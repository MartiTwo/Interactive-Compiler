# Changes to the Prac8 sources for tracing

One row per line ending in `// TRACE` (`grep -rn "// TRACE" compiler` lists them), then one row per new file. Generated `Parva/Parser.java` and `Parva/Parva.java` repeat these lines, plus Instrument's `// TRACE (Instrument)` lines.

| file | line | change | why |
|---|---|---|---|
| Parva.frame | 50 | declare `traceName` | holds the `-t` file name |
| Parva.frame | 61 | parse `-t file.json` | turn tracing on from the command line |
| Parva.frame | 71 | usage line for `-t` | document the flag |
| Parva.frame | 84 | `Trace.start(...)` before `Table.init()` | so the global `scope.open` is recorded |
| Parva.frame | 99 | `Trace.end(...)` after `listCode` | write the trace from final memory, before any `System.exit` |
| Parva.frame | 104 | `else {` becomes `else if (traceName == null) {` | tracing never waits on stdin for `Interpret [y/N]?` |
| Parser.frame | 79 | `Trace.consume(la)` first in `Get()` | every consumed token passes through `Get()` |
| Parser.frame | 83 | `Trace.scan(la, la.kind > maxT)` after `Scanner.Scan()` | every scanned token, pragmas included |
| Parser.frame | 120 | `TRACE-PRODUCTIONS-BEGIN` marker | where Instrument starts |
| Parser.frame | 122 | `TRACE-PRODUCTIONS-END` marker | where Instrument stops |
| Parser.frame | 255 | `Trace.diag("syntax", ...)` in `Errors.SynErr` | log exactly the messages the user sees |
| Parser.frame | 261 | `Trace.diag("semantic", ...)` in `Errors.SemErr` | as above |
| Parser.frame | 267 | `Trace.diag("semantic", ...)` in `Errors.Error(int, int, String)` | as above |
| Parser.frame | 278 | `Trace.diag("warning", ...)` in `Errors.Warn(int, int, String)` | as above |
| Parva/Table.java | 125 | `Trace.insert(...)` at the end of `insert` | each declaration, and whether it clashed |
| Parva/Table.java | 148 | `Trace.scopeOpen()` at the end of `openScope` | scope depth (I6) |
| Parva/Table.java | 154 | `Trace.scopeClose()` at the end of `closeScope` | scope depth (I6) |
| Parva/CodeGen.java | 81 | `Trace.mem(..., "emit")` before the write in `emit` | every code word |
| Parva/CodeGen.java | 170 | `Trace.mem(..., "string")` between `stkTop--` and the write | each string character |
| Parva/CodeGen.java | 172 | `Trace.mem(..., "string")` between `stkTop--` and the write | each string's 0 terminator |
| Parva/CodeGen.java | 257 | `Trace.mem(..., "fixdsp")` in `fixDSP` | the DSP operand patched at the end |
| Parva/CodeGen.java | 309 | `Trace.mem(..., "patch")` in `backPatch` | forward-branch fix-ups |
| Parva.atg | 58 | `isArith` returns `Trace.check("isArith", ...)` | log the helper's verdict |
| Parva.atg | 61 | new private `arith(int)` | untraced copy for use inside `compatible` / `assignable` |
| Parva.atg | 62 | original `isArith` body | moved into `arith` |
| Parva.atg | 63 | closing brace of `arith` | |
| Parva.atg | 66 | `isBool` wraps its body in `Trace.check` | log the helper's verdict |
| Parva.atg | 70 | `isArray` returns `Trace.check("isArray", ...)` | log the helper's verdict |
| Parva.atg | 73 | new private `array(int)` | untraced copy for use inside `compatible` / `assignable` |
| Parva.atg | 74 | original `isArray` body | moved into `array` |
| Parva.atg | 75 | closing brace of `array` | |
| Parva.atg | 79 | `compatible` wraps its expression in `Trace.check(...)` | one log per call |
| Parva.atg | 80 | `isArith` becomes `arith` | no nested logs |
| Parva.atg | 82 | `isArray` becomes `array` | no nested logs |
| Parva.atg | 83 | `isArray` becomes `array`; closes `Trace.check(` | no nested logs |
| Parva.atg | 88 | `assignable` wraps its expression in `Trace.check(...)` | one log per call |
| Parva.atg | 91 | `isArray` becomes `array`; closes `Trace.check(` | no nested logs |
| Parva.atg | 234 | `AssignmentStatement`: `kind != Var` becomes `!Trace.that("assign to a variable", kind == Var)` | inline kind check; same logic, `ok` means the rule held |
| Parva.atg | 248 | `AssignmentStatement` (prefix `++`/`--`): same pattern, `"++/-- on a variable"` | inline kind check |
| Parva.atg | 259 | `Trace.find(name, entry)` after `Table.find` in `Designator` | the parser's own lookups (`Table.insert`'s internal `find` stays silent) |
| Parva.atg | 260 | `Designator`: `!entry.declared` becomes `!Trace.that("declared", entry.declared)` | inline scope check |
| Parva.atg | 276 | `Designator`: subscript kind check, `"subscript a variable"` | inline kind check |
| Parva.atg | 322 | `BreakStatement`: `bl == 0` becomes `!Trace.that("break inside a loop", bl != 0)` | inline check |
| Parva.atg | 350 | `ReadElement`: kind check, `"read into a variable"` | inline kind check |
| Parva.atg | 521 | `Primary` (`cap`/`low`): `type != charType` becomes `!Trace.that("cap/low of a char", type == charType)` | inline type check |
| Parva/Trace.java | new file | the recorder | writes the JSON trace and self-checks it |
| tools/Instrument.java | new file | adds `Trace.enter` / `Trace.exit` to each production method of the generated `Parser.java` | `Parser.java` is generated, so it is never hand-edited |
| tools/EmbedTraces.java | new file | writes `src/Stepper/Examples.elm` from `traces/*.json` | the stepper ships with every example built in |
