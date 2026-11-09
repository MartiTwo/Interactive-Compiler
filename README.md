# Parva Compiler Explorer

An interactive companion to Pat Terry's compilers course (CS301 PLT, Rhodes University),
written in Elm.

The left pane shows one Coco/R production from `Parva.atg`. The right pane shows the Java
that Coco/R generated from it, plus the `Table` and `CodeGen` methods that the production's
semantic actions call into. Stepping through the production's **segments** highlights the
corresponding code on both sides.

A phase selector re-renders **both** panes as the compiler looked at that stage of the
course, so phase 1 shows a genuinely bare grammar and a bare recursive-descent parser, and
each later phase adds material back.

---

## Running it

Three ways, in order of convenience:

1. **Open `index.html`.** It is a complete self-contained page — double-click it. No server,
   no build.
2. **Paste `src/Main.elm` into [Ellie](https://ellie-app.com).** Ellie needs `elm/browser`,
   `elm/core` and `elm/html`, all of which it offers by default.
3. **Build it yourself:**

```bash
elm make src/Main.elm --output=index.html
```

`elm.json` pins Elm 0.19.1, which is what Ellie runs.

---

## What is on screen

| Control | What it does |
|---|---|
| Phase pills 1–5 | Re-render both panes as of that stage. Hovering shows what the phase adds. |
| Production picker | Jump to any production. |
| ◀ prev / next ▶ | Step through the current production's segments. |
| Parser / Table / CodeGen tabs | Parser.java is always shown. Table.java appears at phase 3, CodeGen.java at phase 5. A dot marks a tab holding a method the selected segment calls. |
| what's new | Tints whatever the current phase introduced, so you can see what each stage costs. |
| dark mode | Light by default, which projects better. |

The strip under the panes explains the generation rule behind the selected segment.

---

## Segment kinds

Stepping visits nine kinds of segment. The first four are the ones named in the original
brief; the rest are the EBNF metasymbols, which is where most of the interesting generation
happens.

| Kind | Generates |
|---|---|
| Production head | one `static` method; attributes become parameters, a lone `<out T x>` becomes the return type |
| Non-terminal | a call, with attributes as arguments or an assignment from the result |
| Terminal | `Expect(sym)` in a sequence, but a bare `Get()` inside an alternation |
| Semantic action | copied verbatim, at exactly its position in the production |
| Iteration `{ }` | `while (StartOf(n))`, or a direct `la.kind` test |
| Option `[ ]` | `if` with no else |
| Alternation `( \| )` | `switch` for many alternatives, `if / else-if` for a few |
| `SYNC` | `while (!(StartOf(n))) { SynErr(k); Get(); }` |
| `WEAK` | `ExpectWeak(...)`, or `WeakSeparator(...)` when it heads an iteration |

Paired brackets are **one** segment across two positions: selecting `{` highlights its
matching `}` as well, and stepping visits the pair once.

---

## Coverage

11 of the grammar's 42 productions are populated, chosen so that every generation rule above
appears at least once:

`Parva`, `Statement`, `Block`, `IfStatement`, `WhileStatement`, `VarList`, `OneVar`,
`Designator`, `Primary`, `AddOp`, `Ident`.

The remaining 31 are pure data additions — no engine changes are needed.

---

## How the data is laid out

Everything lives in `src/Main.elm`. The engine is about 450 lines; the rest is data.

### Phase-conditional text

Every fragment of text, grammar and Java alike, is a `Part` carrying the phase range in
which it exists. The upper bound matters: at phase 4 `static void BasicType()` is *replaced*
by `static int BasicType()`, not merely added to.

| Helper | Meaning |
|---|---|
| `core "…"` | every phase |
| `since ScopeRules "…"` | appears at phase 3 and stays |
| `before TypeRules "…"` | phases 1–3, then replaced |
| `onlyIn TypeRules CodeGeneration "…"` | phase 4 only |

### The grammar pane

A production is a list of `GrammarLine`, each a list of `Frag`. A fragment optionally names
the segment it belongs to via `own n`; fragments sharing an owner highlight together. `txt`
makes plain text, `varying` takes explicit parts, `tight` suppresses the leading space, and
`gap` / `pad` insert fixed or phase-conditional indentation so that alternation bars stay
lined up under their opening bracket when `SYNC` shifts the first line right.

The pane is rendered from this structure rather than from source text with spans hidden,
because phase 1 and phase 5 do not share an indentation.

### The Java panes

Each file is a `List Method`, and each method a `List CodeLine`. A segment's `parser` span is
a line range **within its own production's method**, so adding a production never disturbs
anyone else's indices. `table` and `codeGen` are lists of method names; the corresponding tab
renders those bodies, or the whole file when the segment calls nothing.

### Adding a production

1. Add the generated method to `parserSrc` as `Method "Name" nameMethod`, tagging each line's
   phase.
2. Write `nameProduction` with its `lines` and `segments`, and add it to `productions`.
3. Spans are 0-based indices into that method's own line list. Counting them by eye is the
   one error-prone step; the sweep below catches mistakes.

---

## Phase conventions

Deciding which phase a given attribution belongs to is a judgement call. The rules used here:

| Phase | Contains |
|---|---|
| 1 Syntax | EBNF structure, `Expect`, `Get`, the switches and loops, and `default: SynErr`. Coco/R cannot emit a parser without error *detection*; what phase 1 lacks is *recovery*. |
| 2 Error handling | `SYNC` skip loops, `ExpectWeak`, `WeakSeparator`, and every `Warning`. |
| 3 Scope rules | `Entry`, `Table.*`, names, `declared`, `Kinds` as a classification stored on an entry, and stack frame offsets. **Storage allocation lives here**, not at phase 5: `offset` is a field of `Entry`, so allocating it is part of building the symbol table. |
| 4 Kind & type rules | `Types.*`, the `<out … type>` attributes that carry types up the expression grammar, and every `isArith` / `isBool` / `assignable` / `compatible` test. |
| 5 Code generation | `Label` objects and every `CodeGen.*` call. |

Two places resist a clean split, and are tagged phase 5 as a whole because the code cannot
stand without its `CodeGen` calls: the `switch (des.entry.kind)` in `Primary`, and the
`if (entry.kind == Kinds.Var)` guard in `Designator`. Both mix a kind rule with code
generation in a single inseparable action.

---

## Verifying a change

Reload and run this in the browser console. It walks every segment of every production at
every phase and reports any segment that highlights nothing — which is what a wrong span or
a phase mismatch between grammar and code looks like.

```js
const settle = () => new Promise(r => requestAnimationFrame(() => requestAnimationFrame(() => setTimeout(r, 20))));
const names = Array.from(document.querySelector('select').options).map(o => o.text);
const bad = [];
for (const n of names) {
  for (let ph = 1; ph <= 5; ph++) {
    const s = document.querySelector('select');
    s.value = Array.from(s.options).find(o => o.text === n).value;
    s.dispatchEvent(new Event('input', { bubbles: true })); await settle();
    document.querySelectorAll('.pill')[ph - 1].click(); await settle();
    const total = +document.querySelector('.counter').textContent.match(/of (\d+)/)[1];
    for (let i = 0; i < total; i++) {
      const atg = document.querySelectorAll('.pane .frag.sel').length;
      const code = document.querySelectorAll('.pane')[1].querySelectorAll('.line.sel').length;
      if (!atg || !code) bad.push(`${n} phase ${ph} segment ${i + 1}/${total}`);
      if (i < total - 1) {
        Array.from(document.querySelectorAll('.bar.tools button'))
          .find(b => b.textContent.includes('next')).click();
        await settle();
      }
    }
  }
}
console.log(bad.length ? bad.join('\n') : 'all segments map to visible code');
```

It currently checks 578 segment/phase combinations and reports none.

Also worth eyeballing after a change: set phase 1 and step through each production. The left
pane must read as valid bare EBNF — no stray `<…>`, no `SYNC`, no `WEAK`, no `(. .)`, and the
indentation should close up rather than leave gaps.

---

## Deliberate simplifications

- The grammar pane reformats multi-line semantic actions so that `(.` and `.)` sit on their
  own lines. The real `.atg` puts them in a right-hand column, which cannot survive a phase
  change: if the first or last line of an action is filtered out, a column-aligned delimiter
  disappears with it.
- Set indices and error numbers (`StartOf(2)`, `SynErr(55)`) are shown verbatim, as Coco/R
  wrote them.
- There is no click-to-select; navigation is the picker plus prev/next. Adding click would be
  a `Msg` and a handler on the fragment span.

---

## Stepper

`stepper.html` replays a real compilation of a `.pav` program one event at a time: tokens being
scanned, the parse tree growing, symbol-table and type checks, PVM code being emitted and
back-patched. The compiler in `compiler/` records a trace once; the page replays it.

| Need | Version | For |
|---|---|---|
| JDK (`java`, `javac`) | 8 or newer | building the compiler and recording traces |
| Elm | 0.19.1 | building the web pages |
| bash | Git Bash on Windows | `scripts/` |

```bash
scripts/all.sh
```

This builds the compiler, proves tracing changes nothing (`check.sh`), traces every example and
builds both pages. Then open `stepper.html` straight from disk; no server is needed. Without a
JDK, `scripts/build-web.sh` alone rebuilds the pages from the committed traces. To trace your own
program: `scripts/trace.sh path/to/prog.pav`, then **Load trace…** in the page. The format and
its invariants are in [docs/TRACE.md](docs/TRACE.md).

---

Written by Martin Vosloo and Kago Songo, 2026.
Grammar and Java sources from the Prac 8 Parva compiler, P.D. Terry / K. Bradshaw.
