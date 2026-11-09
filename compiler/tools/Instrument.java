// Adds Trace.enter/exit to every production method that Coco/R wrote into
// Parser.java between the TRACE-PRODUCTIONS markers. Run after Coco/R.
// Usage: java -cp compiler/tools-bin Instrument compiler/Parva/Parser.java

import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class Instrument {

  // Built at run time (concat, not +, which javac would fold into one constant) so that
  // a grep for the trace marker finds real edits, not this tool or its class file.
  static final String TAG = "// ".concat("TRACE");
  static final String BEGIN = TAG + "-PRODUCTIONS-BEGIN";
  static final String END = TAG + "-PRODUCTIONS-END";
  static final String MARK = "  " + TAG + " (Instrument)";

  static final Pattern OPENING = Pattern.compile("^\\tstatic ([\\w\\[\\]<>]+) (\\w+)\\((.*)\\) \\{$");

  public static void main(String[] args) throws Exception {
    if (args.length != 1) fail("usage: Instrument Parser.java");
    Path file = Paths.get(args[0]);
    String text = new String(Files.readAllBytes(file), "ISO-8859-1");
    String nl = text.contains("\r\n") ? "\r\n" : "\n";
    if (text.contains("Trace.enter(")) fail("already instrumented (contains Trace.enter)");

    String[] lines = text.split("\n", -1);   // -1 keeps a trailing empty element
    for (int k = 0; k < lines.length; k++) {
      if (lines[k].endsWith("\r")) lines[k] = lines[k].substring(0, lines[k].length() - 1);
    }

    List<String> out = new ArrayList<String>();
    List<String> names = new ArrayList<String>();
    boolean inside = false, sawBegin = false, sawEnd = false;
    String open = null;
    for (String line : lines) {
      String trimmed = line.trim();
      if (trimmed.equals(BEGIN)) {
        sawBegin = true;
        inside = true;
        out.add(line);
        continue;
      }
      if (trimmed.equals(END)) {
        if (open != null) fail("region ends while " + open + " is still open");
        sawEnd = true;
        inside = false;
        out.add(line);
        continue;
      }
      if (!inside) {
        out.add(line);
        continue;
      }
      Matcher m = OPENING.matcher(line);
      if (m.matches()) {
        if (open != null) fail(m.group(2) + " starts before " + open + " closed");
        open = m.group(2);
        names.add(open);
        out.add(line);
        out.add("\t\tTrace.enter(\"" + open + "\"); try {" + MARK);
      } else if (line.equals("\t}")) {
        if (open == null) fail("a method closes but none is open");
        // finally also covers <out> productions that end in "return x;"
        out.add("\t\t} finally { Trace.exit(\"" + open + "\"); }" + MARK);
        out.add(line);
        open = null;
      } else {
        out.add(line);
      }
    }
    if (!sawBegin || !sawEnd) fail("markers missing: " + BEGIN + " / " + END);
    if (names.isEmpty()) fail("no production methods found between the markers");

    StringBuilder b = new StringBuilder(text.length() + names.size() * 120);
    for (int k = 0; k < out.size(); k++) b.append(out.get(k)).append(k < out.size() - 1 ? nl : "");
    Files.write(file, b.toString().getBytes("ISO-8859-1"));

    StringBuilder list = new StringBuilder();
    for (String n : names) list.append(list.length() == 0 ? "" : ", ").append(n);
    System.out.println("Instrumented " + names.size() + " productions: " + list);
  }

  static void fail(String why) {
    System.err.println("Instrument: " + why + ". File left untouched.");
    System.exit(2);
  }
}
