// Handle symbol and label tables for Parva level 1 compiler (Java version)
// P.D. Terry, Rhodes University, 2014

package Parva;

import java.util.*;
import library.*;

  class Types {
  // identifier (and expression) types.
    public static final int
      noType    =  0,             // The numbering is significant, as
      nullType  =  2,             // array types are denoted by these
      intType   =  4,             // numbers + 1
      boolType  =  6,
      charType  =  8,
      voidType  = 10;

    static ArrayList<String> typeNames = new ArrayList<String>();

    static int nextType = 0;

    public static int addType(String name) {
    // Generate (and return) next type id, and add to list of type names
      int thisType = nextType;
      nextType += 2;
      typeNames.add(name);        // simple
      typeNames.add(name + "[]"); // matching array type
      return thisType;
    } // Types.addType

    public static String name(int type) {
      return typeNames.get(type);
    } // Types.name

    public static void show(OutFile lst) {
    // For use in debugging
      for (String s : typeNames) lst.write(s + " ");
      lst.writeLine();
    } // Types.show

  } // end Types

  class Kinds {
  // Identifier kinds
    public static final int
      Con   = 0,
      Var   = 1,
      Fun   = 2;

    public static String[] kindNames = { "const", "var  ", "func "};

  } // end Kinds

  class Entry {
  // All fields initialized, but are modified after construction (by semantic analyser)
    public int     kind     = Kinds.Var;
    public String  name     = "";
    public int     type     = Types.noType;
    public int     value    = 0;          // constants
    public int     offset   = 0;          // variables
    public boolean declared = true;       // true for all except sentinel entry
    public Entry   nextInScope = null;    // link to next entry in current scope
  } // end Entry

  class StackFrame {
  // Objects of this type are used to keep track of the space needed by variables as
  // they are declared and come in and out of scope
    public int size = 0;
  } // end StackFrame

  class DesType {
  // Objects of this type are associated with l-value and r-value designators
    public Entry entry;           // the identifier properties
    public int type;              // designator type (not always the entry type)

    public DesType(Entry entry) {
      this.entry  = entry;
      this.type   = entry.type;   // may need modification later
    } // constructor

    public String toString() {
    // Returns string representation of this designator (for debuggng)
      return (entry.name + "        ").substring(0, 8)
             + " type "     + (Types.name(type) + "        ").substring(0, 8)
             + " offset "   + entry.offset;
    } // ToString

  } // end DesType

  class ConstRec {
  // Objects of this type are associated with literal constants
    public int value;             // value of a constant literal
    public int type;              // constant type (determined from the literal)
  } // end ConstRec

  class Scope {
  // Handle scope rules in blocks and functions
    public Scope outer;           // link to enclosing (outer) scope
    public Entry firstInScope;    // link to first identifier entry in this scope
  } // end Scope

  class Table {
  // The table is held as a linked list of scope nodes each pointing to a linked list
  // of entries for the corresponding scopes.  Each list terminates with the dummy sentinel
  // entry to handle undeclared entries in a simple fashion

    static Scope topScope = null;
    static Entry sentinelEntry;   // marker node at end of each scope list

    public static void insert(Entry entry) {
    // Adds entry to symbol table
      Entry earlierEntry = find(entry.name);
      if (earlierEntry.declared)
        Parser.SemError("earlier declaration of " + entry.name + " is still in scope");
      sentinelEntry.name = entry.name;
      Entry look = topScope.firstInScope, previous = null;
      while (!look.name.equals(entry.name)) {
        previous = look; look = look.nextInScope;
      }
      entry.nextInScope = look;
      if (previous == null) topScope.firstInScope = entry;
      else previous.nextInScope = entry;
      entry.declared = true;
      Trace.insert(entry, earlierEntry.declared); // TRACE
    } // Table.insert

    public static Entry find(String name) {
    // Searches table for an entry matching name.  If found then return that
    // entry; otherwise return a sentinel entry (marked as not declared)
      sentinelEntry.name = name;
      Scope scope = topScope;
      while (scope != null) {
        Entry entry = scope.firstInScope;
        while (!entry.name.equals(name)) entry = entry.nextInScope;
        if (entry != sentinelEntry) return entry;
        scope = scope.outer;
      }
      return sentinelEntry;
    } // Table.find

    public static void openScope() {
    // Opens a scope record at the start of parsing a statement block
      Scope newScope = new Scope();
      newScope.outer = topScope;
      newScope.firstInScope = sentinelEntry;
      topScope = newScope;
      Trace.scopeOpen(); // TRACE
    } // Table.openScope

    public static void closeScope() {
    // Closes a scope record at the end of parsing a statement block
      topScope = topScope.outer;
      Trace.scopeClose(); // TRACE
    } // Table.closeScope

    public static String truncate(String str) {
    // Tuuncates str to 8 letters, space fill right if shorter
      return (str + "         ").substring(0, 8);
    } // Table.truncate

    public static void printTable(OutFile lst) {
    // Prints symbol table for diagnostic/debugging purposes
      lst.writeLine();
      lst.writeLine("Symbol table");
      Scope scope = topScope;
      while (scope != null) {
        Entry entry = scope.firstInScope;
        while (entry != sentinelEntry) {
          lst.write(truncate(entry.name) + " ");
          lst.write(truncate(Types.name(entry.type)));
          switch (entry.kind) {
            case Kinds.Con:
              lst.write(" Constant ");
              lst.writeLine(entry.value, 0);
              break;
            case Kinds.Var:
              lst.write(" Variable");
              lst.write(entry.offset, 3);
              lst.writeLine();
              break;
            case Kinds.Fun:
              lst.writeLine(" Function ");
              break;
          }
          entry = entry.nextInScope;
        }
        scope = scope.outer;
      }
      lst.writeLine();
    } // Table.printTable

    public static void init() {
    // Instigate standard simple types, clears table and sets up sentinel entry
      Types.addType("none");
      Types.addType("null");
      Types.addType("int");
      Types.addType("bool");
      Types.addType("char");
      Types.addType("void");
      sentinelEntry = new Entry();
      sentinelEntry.name = "";
      sentinelEntry.type = Types.noType;
      sentinelEntry.kind = Kinds.Var;
      sentinelEntry.nextInScope = null;
      sentinelEntry.declared = false;
      topScope = null;
      openScope();
    } // Table.init

  } // end Table

