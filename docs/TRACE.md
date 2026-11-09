# Parva step traces

`scripts/trace.sh FILE.pav` compiles a program once and records every meaningful action as an event in a JSON file. The stepper (`stepper.html`) replays it: `state_n = foldl step state_0 (first n events)`.

## Requirements

| Tool | Version | Why |
|---|---|---|
| JDK (`java`, `javac`) | **8 or newer** | `library/*.class` are class version 51 (Java 7); `Trace.java` and the tools use Java 8 |
| Elm | 0.19.1 | both web apps |
| bash | any (Git Bash on Windows) | `scripts/*.sh` |

## 4.1 File shape

The file is pure ASCII JSON. `Trace.java` escapes every char `< 0x20` or `> 0x7E` as `\u00XX`, so encoding never matters.

```json
{
  "format": "parva-trace",
  "version": 1,
  "file": "test10length.pav",
  "source": "void displayList() {\u000d\u000a...",
  "maxT": 53,
  "memSize": 812,
  "terminals": ["EOF", "identifier", "number", "..."],
  "types": ["none", "none[]", "null", "null[]", "int", "int[]", "..."],
  "ops": [ {"name": "", "words": 1}, {"name": "NOP", "words": 1}, {"name": "DSP", "words": 2}, "..." ],
  "events": [
    {"i":0,"ev":"scope.open","at":null,"depth":1},
    {"i":1,"ev":"sym.insert","at":null,"sym":{...},"depth":1,"clash":false},
    "..."
  ]
}
```

| Field | How it is computed |
|---|---|
| `source` | the `.pav` read as bytes and decoded as ISO-8859-1. Coco's `Buffer` counts bytes, so `pos` indexes `source` exactly. |
| `terminals` | index = token kind. Reflection over `Parser`'s `static final int` fields ending in `_Sym` or `_SYM`, suffix stripped; gaps are `"kind<n>"`. Pragmas (kinds > `maxT`) are included. |
| `types` | index = type id. A copy of `Types.typeNames`. |
| `ops` | index = opcode `0 .. PVM.nul`. `name = PVM.mnemonics[op]`. `words = 2` for the two-word opcodes in `PVM.listCode` (`bfalse btrue brn bze sfd sfu efd efu dsp lda ldc ldl stl stlc ston`) plus `prns`, else 1. |
| header | built at finish time, after `PVM.init` and `Table.init` have run. |

The file is written once, at the end: the header, then one event per line.

## 4.2 Events

Every event has `i` (0-based, contiguous), `ev` (the kind) and `at`: `[line, col]` of the last consumed token, or `null` before the first one.

| `ev` | Extra fields | Emitted from |
|---|---|---|
| `scan` | `tok`, `pragma` (`tok.kind > maxT`) | `Parser.frame` `Get()`, right after `la = Scanner.Scan()` |
| `consume` | `tok` | `Parser.frame` `Get()`, first statement. The dummy token `Parse()` primes `la` with (`line == 0`) is skipped. |
| `enter` | `prod` | production method start (added by `Instrument`) |
| `exit` | `prod` | production method `finally` (added by `Instrument`) |
| `scope.open` | `depth` = scopes after opening (the first is 1, from `Table.init`) | `Table.openScope` |
| `scope.close` | `depth` = scopes after closing | `Table.closeScope` |
| `sym.insert` | `sym`, `depth`, `clash` (= `earlierEntry.declared`) | end of `Table.insert` |
| `sym.find` | `name`, `found` (= `entry.declared`), `sym` only if found | `Parva.atg` `Designator`, after `Table.find` |
| `check` | `rule`, `args` (type names), `ok` | `Parva.atg` helpers (`isArith`, `isBool`, `isArray`, `compatible`, `assignable`) and `Trace.that` at inline checks. `ok` always means the rule held. |
| `mem` | `addr`, `old`, `new`, `cause` ∈ `emit` `patch` `fixdsp` `string` | `CodeGen` (5 places) |
| `diag` | `severity` ∈ `syntax` `semantic` `warning`, `line`, `col`, `msg` | `Errors` in `Parser.frame`: exactly the messages the user sees |
| `end` | `ok`, `errors`, `warnings`, `codeLength`, `initSP`, `code` (`mem[0 .. codeLength-1]`), `strings` (`mem[initSP .. memSize-1]`), `selfCheck` | driver (`Parva.frame`), after `listCode` |
| `abort` | `reason`, `selfCheck` | `Trace` itself: uncaught exception, `System.exit` before `end`, event limit (200 000), internal error |

| Sub-shape | Fields |
|---|---|
| `tok` | `{kind, val, line, col, pos}`, copied from `Token` as they are (`col` is in fact 1-based). The EOF token's `val` is Coco's sentinel `"EOF"`. |
| `sym` | `{name, kind: "const"\|"var"\|"func", type: <type name>, value, offset}` |

**Exactly one terminal event (`end` or `abort`) exists, and it is last.**

## 4.3 Invariants: what "a good trace" means

`Trace.java` checks the cheap ones as it goes (its *self-check*). `src/Stepper/Machine.elm` checks all of them again, because a file may have been edited or truncated. A trace is rejected at the first violation, with the event index and a one-line reason. **The view never renders a rejected trace.**

| # | Invariant | Catches |
|---|---|---|
| I1 | `i` equals the event's array index | truncated or merged files |
| I2 | exactly one `end`/`abort`, and it is last | crash or missed finish |
| I3 | LL(1) discipline: a non-pragma `scan` needs no pending lookahead, then becomes pending. A `consume` needs a pending lookahead with equal `pos` and `kind`, then clears it. A pragma `scan` never becomes pending. | wrong hook placement in `Get()` |
| I4 | for `scan` with non-empty `val`: `String.slice pos (pos + length val) source == val`, and `pos` never decreases. **EOF (kind 0)** is checked by position instead: its `pos` equals the source length. | position or encoding bugs |
| I5 | `exit p` requires the open node to be `p` and not the synthetic root | Instrument bugs |
| I6 | `scope.open` depth = current+1. `scope.close` depth = current−1 ≥ 0. `sym.insert` depth = current ≥ 1. (Java also compares with `Table`'s real scope chain.) | missing scope hooks |
| I7 | every `mem.old` equals the current shadow value (memory starts all-zero; `PVM.init` zeroes it) | **a memory write that was not hooked** |
| I8 | `emit` needs `addr == codeTop`, then `codeTop += 1`. `patch`/`fixdsp` need `addr < codeTop`. `patch` needs `new == codeTop` and `old` ∈ {−1} ∪ [0, codeTop). `fixdsp` needs `ops[mem[addr−1]].name == "DSP"`. `string` needs `addr == stringBottom − 1` and `addr ≥ codeTop`. | misread back-patch chain |
| I9 | at `end`: `codeLength == codeTop`; shadow code equals `end.code`; shadow strings equal `end.strings`; only the root of the tree is open; syntax+semantic `diag` count `== errors`; warning count `== warnings`; `selfCheck == "ok"` | **replay must reproduce the compiler's real output** |
| I10 | at `abort`: the state is kept as a partial trace with a banner. It is not an error. | — |

`selfCheck` is `"ok"` or the first Java-side violation of I3, I5, I6, I7 or I9. On a violation `Trace` also prints `TRACE SELF-CHECK FAILED: <reason>` to stderr (never stdout, which `check.sh` compares with the baseline).

## How to add a hook

1. Add a static method to `compiler/Parva/Trace.java`: return at once when `!on`, only read compiler state, and wrap the body in `try { … } catch (Throwable t) { fail(t); }`. A hook that returns a value returns its input unchanged.
2. Call it from a source Coco/R reads (`Parva.atg`, `Parser.frame`, `Parva.frame`) or from `CodeGen.java`/`Table.java`, never a generated file. End the line with `// TRACE` and add a row to `compiler/CHANGES.md`.
3. Add the event to §4.2, a constructor and decoder case to `src/Stepper/Trace.elm`, and its rule to `step` in `src/Stepper/Machine.elm`.
4. Run `scripts/all.sh`. `check.sh` must still pass: same `.cod`, stdout and exit code for every example, and `selfCheck` `"ok"` everywhere.
