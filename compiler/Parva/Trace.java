// Records one compilation as a step trace (JSON) for the Elm stepper.
// Format and invariants: docs/TRACE.md. Hooks only read compiler state, and
// no method here ever throws into the compiler.

package Parva;

import java.io.File;
import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.List;
import java.util.TreeMap;

class Trace {

  static boolean on = false;

  private static final int LIMIT = 200000;

  // Two-word opcodes: the operand cases of PVM.listCode's switch, plus prns.
  private static final int[] TWO_WORD = {
    PVM.bfalse, PVM.btrue, PVM.brn, PVM.bze, PVM.sfd, PVM.sfu, PVM.efd, PVM.efu,
    PVM.dsp, PVM.lda, PVM.ldc, PVM.ldl, PVM.stl, PVM.stlc, PVM.ston, PVM.prns
  };

  private static String sourcePath, outPath, source;
  private static final ArrayList<String> events = new ArrayList<String>();
  private static Token cur;                 // last consumed token: the "at" of each event
  private static Token pending;             // scanned lookahead not yet consumed (I3)
  private static final ArrayDeque<String> prods = new ArrayDeque<String>();
  private static int depth;                 // scope depth (I6)
  private static int[] shadow;              // memory as replayed from mem events (I7, I9)
  private static int codeTop, errors, warnings;
  private static String selfCheck;
  private static boolean finished;

  // ------------------------------------------------------------ lifecycle

  static void start(String sourceFile, String outFile) {
    try {
      sourcePath = sourceFile;
      outPath = outFile;
      // ISO-8859-1 maps byte n to char n. Coco's Buffer counts bytes, so tok.pos indexes this string.
      source = new String(Files.readAllBytes(Paths.get(sourceFile)), "ISO-8859-1");
      shadow = new int[PVM.memSize + 1];
      on = true;
      final Thread.UncaughtExceptionHandler previous = Thread.getDefaultUncaughtExceptionHandler();
      Thread.setDefaultUncaughtExceptionHandler(new Thread.UncaughtExceptionHandler() {
        public void uncaughtException(Thread t, Throwable e) {
          abort("uncaught " + e.getClass().getName() + ": " + e.getMessage());
          if (previous != null) previous.uncaughtException(t, e);
          else {                            // what the JVM would have printed
            System.err.print("Exception in thread \"" + t.getName() + "\" ");
            e.printStackTrace(System.err);
          }
        }
      });
      Runtime.getRuntime().addShutdownHook(new Thread() {
        public void run() {
          if (!finished) abort("exited without end");   // e.g. System.exit in Errors.Exception
        }
      });
    } catch (Throwable t) { fail(t); }
  }

  static void end(boolean ok, int errorCount, int warnCount, int codeLength, int initSP) {
    if (!on) return;
    try {
      // I9: the replayed memory must reproduce what the compiler really produced.
      if (codeLength != codeTop) violate("I9: codeLength is " + codeLength + " but " + codeTop + " words were emitted");
      for (int a = 0; a <= PVM.memSize; a++) {
        if (shadow[a] != PVM.mem[a]) {
          violate("I9: mem[" + a + "] is " + PVM.mem[a] + " but the trace says " + shadow[a]);
          break;
        }
      }
      if (!prods.isEmpty()) violate("I9: production " + prods.peek() + " is still open");
      if (errors != errorCount) violate("I9: " + errors + " error diags but Errors.count is " + errorCount);
      if (warnings != warnCount) violate("I9: " + warnings + " warning diags but Errors.warns is " + warnCount);
      finish("end", "\"ok\":" + ok + ",\"errors\":" + errorCount + ",\"warnings\":" + warnCount
          + ",\"codeLength\":" + codeLength + ",\"initSP\":" + initSP
          + ",\"code\":" + words(0, codeLength) + ",\"strings\":" + words(initSP, PVM.memSize));
    } catch (Throwable t) { fail(t); }
  }

  // ------------------------------------------------------------ tokens and productions

  static void consume(Token t) {
    if (!on) return;
    try {
      if (t == null || t.line == 0) return;   // Parse() primes la with a dummy token (line 0)
      if (pending == null || pending.pos != t.pos || pending.kind != t.kind)
        violate("I3: consume of kind " + t.kind + " at pos " + t.pos + " without a matching pending lookahead");
      pending = null;
      cur = t;
      record("consume", "\"tok\":" + tok(t));
    } catch (Throwable e) { fail(e); }
  }

  static void scan(Token t, boolean pragma) {
    if (!on) return;
    try {
      if (!pragma) {
        if (pending != null) violate("I3: scan at pos " + t.pos + " while the lookahead at pos " + pending.pos + " is pending");
        pending = t;
      }
      record("scan", "\"tok\":" + tok(t) + ",\"pragma\":" + pragma);
    } catch (Throwable e) { fail(e); }
  }

  static void enter(String prod) {
    if (!on) return;
    try {
      prods.push(prod);
      record("enter", "\"prod\":" + json(prod));
    } catch (Throwable e) { fail(e); }
  }

  static void exit(String prod) {
    if (!on) return;
    try {
      if (prods.isEmpty() || !prods.peek().equals(prod))
        violate("I5: exit " + prod + " but the open production is " + (prods.isEmpty() ? "none" : prods.peek()));
      else prods.pop();
      record("exit", "\"prod\":" + json(prod));
    } catch (Throwable e) { fail(e); }
  }

  // ------------------------------------------------------------ symbol table

  static void scopeOpen() {
    if (!on) return;
    try {
      int now = tableDepth();
      if (now != depth + 1) violate("I6: scope.open reached depth " + now + " from " + depth);
      depth = now;
      record("scope.open", "\"depth\":" + depth);
    } catch (Throwable e) { fail(e); }
  }

  static void scopeClose() {
    if (!on) return;
    try {
      int now = tableDepth();
      if (now != depth - 1 || now < 0) violate("I6: scope.close reached depth " + now + " from " + depth);
      depth = now;
      record("scope.close", "\"depth\":" + depth);
    } catch (Throwable e) { fail(e); }
  }

  static void insert(Entry e, boolean clash) {
    if (!on) return;
    try {
      int now = tableDepth();
      if (now != depth || now < 1) violate("I6: sym.insert at depth " + now + " but the trace is at " + depth);
      record("sym.insert", "\"sym\":" + sym(e) + ",\"depth\":" + now + ",\"clash\":" + clash);
    } catch (Throwable t) { fail(t); }
  }

  static void find(String name, Entry e) {
    if (!on) return;
    try {
      record("sym.find", "\"name\":" + json(name) + ",\"found\":" + e.declared + (e.declared ? ",\"sym\":" + sym(e) : ""));
    } catch (Throwable t) { fail(t); }
  }

  // ------------------------------------------------------------ checks (each returns ok unchanged)

  static boolean check(String rule, int type, boolean ok) {
    if (on) {
      try { record("check", "\"rule\":" + json(rule) + ",\"args\":[" + json(typeName(type)) + "],\"ok\":" + ok); }
      catch (Throwable t) { fail(t); }
    }
    return ok;
  }

  static boolean check(String rule, int t1, int t2, boolean ok) {
    if (on) {
      try { record("check", "\"rule\":" + json(rule) + ",\"args\":[" + json(typeName(t1)) + "," + json(typeName(t2)) + "],\"ok\":" + ok); }
      catch (Throwable t) { fail(t); }
    }
    return ok;
  }

  static boolean that(String rule, boolean ok) {
    if (on) {
      try { record("check", "\"rule\":" + json(rule) + ",\"args\":[],\"ok\":" + ok); }
      catch (Throwable t) { fail(t); }
    }
    return ok;
  }

  // ------------------------------------------------------------ memory and diagnostics

  static void mem(int addr, int oldWord, int newWord, String cause) {
    if (!on) return;
    try {
      if (addr < 0 || addr > PVM.memSize) violate("I7: write outside memory at " + addr);
      else {
        if (shadow[addr] != oldWord) violate("I7: mem[" + addr + "] is " + oldWord + " but the trace says " + shadow[addr]);
        shadow[addr] = newWord;
      }
      if (cause.equals("emit")) codeTop++;
      record("mem", "\"addr\":" + addr + ",\"old\":" + oldWord + ",\"new\":" + newWord + ",\"cause\":" + json(cause));
    } catch (Throwable t) { fail(t); }
  }

  static void diag(String severity, int line, int col, String msg) {
    if (!on) return;
    try {
      if (severity.equals("warning")) warnings++; else errors++;
      record("diag", "\"severity\":" + json(severity) + ",\"line\":" + line + ",\"col\":" + col + ",\"msg\":" + json(msg));
    } catch (Throwable t) { fail(t); }
  }

  // ------------------------------------------------------------ internals

  private static void record(String ev, String fields) {
    if (events.size() >= LIMIT - 1) { abort("event limit"); return; }   // keep room for the abort itself
    events.add(line(ev, fields));
  }

  private static String line(String ev, String fields) {
    StringBuilder b = new StringBuilder(96);
    b.append("{\"i\":").append(events.size()).append(",\"ev\":").append(json(ev)).append(",\"at\":");
    if (cur == null) b.append("null");
    else b.append('[').append(cur.line).append(',').append(cur.col).append(']');
    if (fields.length() > 0) b.append(',').append(fields);
    return b.append('}').toString();
  }

  private static void violate(String reason) {
    if (selfCheck != null) return;
    selfCheck = reason + " (event " + events.size() + ")";
    System.err.println("TRACE SELF-CHECK FAILED: " + selfCheck);
  }

  private static void abort(String reason) {
    finish("abort", "\"reason\":" + json(reason));
  }

  private static void fail(Throwable t) {
    if (selfCheck == null) selfCheck = "internal error: " + t;
    abort("internal error: " + t);
  }

  // Appends the single terminal event and writes the file. Never runs twice.
  private static synchronized void finish(String ev, String fields) {
    if (finished) return;
    finished = true;
    on = false;
    try {
      events.add(line(ev, fields + ",\"selfCheck\":" + json(selfCheck == null ? "ok" : selfCheck)));
      write();
    } catch (Throwable t) {
      System.err.println("TRACE: could not write " + outPath + ": " + t);
    }
  }

  private static void write() throws Exception {
    StringBuilder b = new StringBuilder(events.size() * 100 + 8192);
    b.append("{\n");
    b.append("  \"format\": \"parva-trace\",\n");
    b.append("  \"version\": 1,\n");
    b.append("  \"file\": ").append(json(sourcePath == null ? "" : new File(sourcePath).getName())).append(",\n");
    b.append("  \"source\": ").append(json(source)).append(",\n");
    b.append("  \"maxT\": ").append(Parser.maxT).append(",\n");
    b.append("  \"memSize\": ").append(PVM.memSize).append(",\n");
    b.append("  \"terminals\": ").append(strings(terminals())).append(",\n");
    b.append("  \"types\": ").append(strings(Types.typeNames)).append(",\n");
    b.append("  \"ops\": ").append(ops()).append(",\n");
    b.append("  \"events\": [\n");
    for (int k = 0; k < events.size(); k++) {
      b.append("    ").append(events.get(k)).append(k < events.size() - 1 ? ",\n" : "\n");
    }
    b.append("  ]\n}\n");
    Files.write(Paths.get(outPath), b.toString().getBytes("US-ASCII"));
  }

  // Index = token kind, from Parser's *_Sym / *_SYM constants; pragmas included.
  private static List<String> terminals() throws Exception {
    TreeMap<Integer, String> byKind = new TreeMap<Integer, String>();
    for (Field f : Parser.class.getDeclaredFields()) {
      int m = f.getModifiers();
      String n = f.getName();
      if (Modifier.isStatic(m) && Modifier.isFinal(m) && f.getType() == int.class
          && (n.endsWith("_Sym") || n.endsWith("_SYM"))) {
        byKind.put(f.getInt(null), n.substring(0, n.length() - 4));
      }
    }
    List<String> names = new ArrayList<String>();
    int last = byKind.isEmpty() ? -1 : byKind.lastKey();
    for (int k = 0; k <= last; k++) names.add(byKind.containsKey(k) ? byKind.get(k) : "kind" + k);
    return names;
  }

  private static String ops() {
    StringBuilder b = new StringBuilder("[");
    for (int op = 0; op <= PVM.nul; op++) {
      String name = PVM.mnemonics[op] == null ? "" : PVM.mnemonics[op];
      b.append(op == 0 ? "" : ",").append("{\"name\":").append(json(name))
       .append(",\"words\":").append(twoWord(op) ? 2 : 1).append('}');
    }
    return b.append(']').toString();
  }

  private static boolean twoWord(int op) {
    for (int w : TWO_WORD) if (w == op) return true;
    return false;
  }

  private static int tableDepth() {
    int d = 0;
    for (Scope s = Table.topScope; s != null; s = s.outer) d++;
    return d;
  }

  private static String tok(Token t) {
    return "{\"kind\":" + t.kind + ",\"val\":" + json(t.val) + ",\"line\":" + t.line + ",\"col\":" + t.col + ",\"pos\":" + t.pos + "}";
  }

  private static String sym(Entry e) {
    String kind = e.kind >= 0 && e.kind < Kinds.kindNames.length ? Kinds.kindNames[e.kind].trim() : "kind" + e.kind;
    return "{\"name\":" + json(e.name) + ",\"kind\":" + json(kind) + ",\"type\":" + json(typeName(e.type))
        + ",\"value\":" + e.value + ",\"offset\":" + e.offset + "}";
  }

  private static String typeName(int type) {
    return type >= 0 && type < Types.typeNames.size() ? Types.typeNames.get(type) : "type" + type;
  }

  private static String words(int from, int to) {
    StringBuilder b = new StringBuilder("[");
    for (int a = Math.max(0, from); a < to && a <= PVM.memSize; a++) b.append(a > Math.max(0, from) ? "," : "").append(PVM.mem[a]);
    return b.append(']').toString();
  }

  private static String strings(List<String> list) {
    StringBuilder b = new StringBuilder("[");
    for (int k = 0; k < list.size(); k++) b.append(k == 0 ? "" : ",").append(json(list.get(k)));
    return b.append(']').toString();
  }

  // The only serialiser: quotes, backslashes, and every char outside 0x20..0x7E as \\uXXXX.
  private static String json(String s) {
    if (s == null) s = "";
    StringBuilder b = new StringBuilder(s.length() + 2).append('"');
    for (int k = 0; k < s.length(); k++) {
      char c = s.charAt(k);
      if (c == '"' || c == '\\') b.append('\\').append(c);
      else if (c < 0x20 || c > 0x7E) {
        String hex = Integer.toHexString(c);
        b.append("\\u");
        for (int z = hex.length(); z < 4; z++) b.append('0');
        b.append(hex);
      }
      else b.append(c);
    }
    return b.append('"').toString();
  }
}
