// Code Generation for compilers and assemblers targetting the PVM (Java version)
// P.D. Terry, Rhodes University -- modified K Bradshaw 2025
// modified by Martin Vosloo and Kago Songo 2026

package Parva;


import java.util.*;

  class Label {
    private int memAdr;      // address if this.defined, else last forward reference
    private boolean defined; // true once this.memAdr is known

    public Label(boolean known) {
    // Constructor for label, possibly at already known location
      if (known) this.memAdr = CodeGen.getCodeLength();
      else this.memAdr = CodeGen.undefined;  // mark end of forward reference chain
      this.defined = known;
    } // constructor

    public int address() {
    // Returns memAdr if known, otherwise effectively adds to a forward reference
    // chain that will be resolved if and when here() is called and returns the
    // address of the most recent forward reference
      int adr = memAdr;
      if (!defined) memAdr = CodeGen.getCodeLength();
      return adr;
    } // Label.address

    public void here() {
    // Defines memAdr of this label to be at current location counter after fixing
    // any outstanding forward references
      if (defined) Parser.SemError("Compiler error - bad label");
      else CodeGen.backPatch(memAdr);
      memAdr = CodeGen.getCodeLength();
      defined = true;
    } // Label.here

    public boolean isDefined() {
    // Returns true if the location of this label has been established
      return defined;
    } // Label.isDefined

    public String toString() {
      return Integer.toString(memAdr);
    } // Label.toString

  } // end Label

  class CodeGen {
    static boolean generatingCode = true;
    static int codeTop = 0, stkTop = PVM.memSize;
    public static Label[] brkLabels = new Label[20];
    public static int bl = 0;

    public static final int
      undefined  = -1,

      nop  =  1,
      add  =  2,
      sub  =  3,
      mul  =  4,
      div  =  5,
      rem  =  6,
      and  =  7,
      or   =  8,
      ceq  =  9,
      cne  = 10,
      clt  = 11,
      cge  = 12,
      cgt  = 13,
      cle  = 14;

    private static void emit(int word) {
    // Code generator for single word
      if (!generatingCode) return;
      if (codeTop >= stkTop) {
        Parser.SemError("program too long"); generatingCode = false;
      }
      else {
        Trace.mem(codeTop, PVM.mem[codeTop], word, "emit"); // TRACE
        PVM.mem[codeTop] = word; codeTop++;
      }
    } // Codegen.emit

    public static void negateInteger() {
    // Generates code to negate integer value on top of evaluation stack
      emit(PVM.neg);
    } // CodeGen.negateInteger

    public static void negateBoolean() {
    // Generates code to negate boolean value on top of evaluation stack
      emit(PVM.not);
    } // CodeGen.negateBoolean

    public static void binaryOp(int op) {
    // Generates code to pop two values A,B from evaluation stack
    // and push value A op B
      switch (op) {
        case CodeGen.mul:  emit(PVM.mul); break;
        case CodeGen.div:  emit(PVM.div); break;
        case CodeGen.rem:  emit(PVM.rem); break;
        case CodeGen.and:  emit(PVM.and); break;
        case CodeGen.add:  emit(PVM.add); break;
        case CodeGen.sub:  emit(PVM.sub); break;
        case CodeGen.or :  emit(PVM.or);  break;
      }
    } // CodeGen.binaryOp

    public static void booleanOp(Label branch, int op) {
    // Generates code for short circuit Boolean operator op
      switch (op) {
        case CodeGen.and:  emit(PVM.bfalse); break;
        case CodeGen.or :  emit(PVM.btrue); break;
      }
      emit(branch.address());
    } // CodeGen.booleanOp

    public static void comparison(int op, int type) {
    // Generates code to pop two values A,B from evaluation stack
    // and push Boolean value A op B
      switch (op) {
        case CodeGen.ceq:  emit(PVM.ceq); break;
        case CodeGen.cne:  emit(PVM.cne); break;
        case CodeGen.clt:  emit(PVM.clt); break;
        case CodeGen.cle:  emit(PVM.cle); break;
        case CodeGen.cgt:  emit(PVM.cgt); break;
        case CodeGen.cge:  emit(PVM.cge); break;
        case CodeGen.nop:  break;
      }
    } // CodeGen.comparison

    public static void read(int type) {
    // Generates code to read a value of specified type
    // and store it at the address found on top of stack
      switch (type) {
        case Types.intType:  emit(PVM.inpi); break;
        case Types.boolType: emit(PVM.inpb); break;
        case Types.charType: emit(PVM.inpc); break;
      }
    } // CodeGen.read

    public static void readLine() {
    // Generates code to skip to next line of data
      emit(PVM.inpl);
    } // CodeGen.readLine

    public static void write(int type) {
    // Generates code to output value of specified type from top of stack
      switch (type) {
        case Types.intType:  emit(PVM.prni); break;
        case Types.boolType: emit(PVM.prnb); break;
        case Types.charType: emit(PVM.prnc); break;
      }
    } // CodeGen.write

    public static void writeLine() {
    // Generates code to output line mark
      emit(PVM.prnl);
    } // CodeGen.writeLine

    public static void writeString(String str) {
    // Generates code to output a string str stored at known location
      int l = str.length(), first = stkTop - 1;
      if (stkTop <= codeTop + l + 1) {
        Parser.SemError("program too long"); generatingCode = false;
        return;
      }
      for (int i = 0; i < l; i++) {
        stkTop--; Trace.mem(stkTop, PVM.mem[stkTop], str.charAt(i), "string"); PVM.mem[stkTop] = str.charAt(i); // TRACE
      }
      stkTop--; Trace.mem(stkTop, PVM.mem[stkTop], 0, "string"); PVM.mem[stkTop] = 0; // TRACE
      emit(PVM.prns); emit(first);
    } // CodeGen.writeString

    public static void loadConstant(int number) {
    // Generates code to push number onto evaluation stack
      emit(PVM.ldc); emit(number);
    } // CodeGen.loadConstant

    public static void loadAddress(Entry var) {
    // Generates code to push address of variable var onto evaluation stack
      emit(PVM.lda); emit(var.offset);
    } // CodeGen.LoadAddress

    public static void loadValue(Entry var) {
    // Generates code to push value of variable var onto evaluation stack
      emit(PVM.ldl); emit(var.offset);
    } // CodeGen.LoadValue

    public static void index() {
    // Generates code to index an array on the heap
      emit(PVM.ldxa);
    } // CodeGen.index

    public static void allocate() {
    // Generates code to allocate an array on the heap
      emit(PVM.anew);
    } // CodeGen.allocate

    public static void dereference() {
    // Generates code to replace top of evaluation stack by the value found at the
    // address currently stored on top of the stack
      emit(PVM.ldv);
    } // CodeGen.dereference

    public static void incOrDec(boolean inc) {
    // Generates unchecked code to increment or decrement the value found at the
    // address currently stored at the top of the stack
      emit(inc ? PVM.inc : PVM.dec);
    } // CodeGen.IncOrDec

    public static void incOrDec(boolean inc, int type) {
    // Generates code to increment or decrement the value found at the address
    // currently stored at the top of the stack
    // If necessary, apply range check
      if (type == Types.charType) {
        emit(inc ? PVM.incc : PVM.decc);
      }
      else emit(inc ? PVM.inc : PVM.dec);
    } // CodeGen.IncOrDec

 
    public static void assign(int type) {
    // Generates code to store value currently on top-of-stack on the address
    // given by next-to-top, popping these two elements.
      if (type == Types.charType) emit(PVM.stoc);
      else emit(PVM.sto);
    } // CodeGen.assign

    public static void assign2(int type) {
    // Generates code to store value currently on top-of-stack on the address
    // given by next-to-top, popping these two elements
      emit(PVM.sto2);
    } // CodeGen.assign2

    public static void assignN(int n) {
    // Generates code to store value currently on top-of-stack on the address
    // given by next-to-top, popping these two elements.
    // This omits range checking
      if (n > 1) { emit(PVM.ston); emit(n); }
      else emit(PVM.sto);
    } // CodeGen.assignN

    public static void storeValue(Entry var) {
    // Generates code to pop top of stack and store at known offset
      emit(PVM.stl); emit(var.offset);
    } // CodeGen.storeValue

    public static void openStackFrame(int size) {
    // Generates (possibly incomplete) code to reserve space for local variables
      emit(PVM.dsp); emit(size);
    } // CodeGen.openStackFrame

    public static void fixDSP(int location, int size) {
    // Fixes up DSP instruction at location to reserve size space for variables
      Trace.mem(location + 1, PVM.mem[location + 1], size, "fixdsp"); // TRACE
      PVM.mem[location+1] = size;
    } // CodeGen.fixDSP

    public static void leaveProgram() {
    // Generates code needed to leave a program (halt)
      emit(PVM.halt);
    } // CodeGen.leaveProgram

    public static void branch(Label destination) {
    // Generates unconditional branch to destination
      emit(PVM.brn); emit(destination.address());
    } // CodeGen.branch

    public static void branchFalse(Label destination) {
    // Generates branch to destination, conditional on the Boolean
    // value currently on top of the evaluation stack, popping this value
      emit(PVM.bze); emit(destination.address());
    } // CodeGen.branchFalse

    public static void branchTrue(Label destination) {
    // Generates branch to destination if TOS is true (non-zero), popping this value
      emit(PVM.not); emit(PVM.bze); emit(destination.address());
    } // CodeGen.branchTrue

    public static void castToChar() {
    // Generates range-checked integer-to-char conversion
      emit(PVM.i2c);
    } // CodeGen.castToChar

    public static void capChar() {
    // Generates code to convert char on TOS to uppercase
      emit(PVM.capc);
    } // CodeGen.capChar

    public static void lowChar() {
    // Generates code to convert char on TOS to lowercase
      emit(PVM.lowc);
    } // CodeGen.lowChar


    public static void pop(int n) {
    // Generates code to discard top n elements from the stack
      if (n != 0) { emit(PVM.dsp); emit(-n); }
    } // CodeGen.pop

    public static void backPatch(int adr) {
    // Stores the current location counter as the address field of the branch or call
    // instruction currently holding a forward reference to adr and repeatedly
    // works through a linked list of such instructions
      while (adr != undefined) {
        int nextAdr = PVM.mem[adr];
        Trace.mem(adr, nextAdr, codeTop, "patch"); // TRACE
        PVM.mem[adr] = codeTop;
        adr = nextAdr;
      }
    } // CodeGen.backPatch

    public static void stack() {
    // Generates code to dump the current state of the evaluation stack
      emit(PVM.stack);
    } // CodeGen.stack

    public static void heap() {
    // Generates code to dump the current state of the runtime heap
      emit(PVM.heap);
    } // CodeGen.heap

    public static int getCodeLength() {
    // Returns codeTop = length of generated code
      return codeTop;
    } // CodeGen.getCodeLangth

    public static int getInitSP() {
    // Returns stkTop = position for initial stack pointer
      return stkTop;
    } // CodeGen.getInitSP

    public static void oneWord(String mnemonic) {
    // inline assembly of one word instruction with no operand
      emit(PVM.opCode(mnemonic));
    } // CodeGen.oneWord

    public static void twoWord(String mnemonic, int adr) {
    // inline assembly of two word instruction with integer operand
      emit(PVM.opCode(mnemonic)); emit(adr);
    } // CodeGen.twoWord

    public static void branch(String mnemonic, Label adr) {
    // inline assembly of two word branch style instruction with Label operand
      emit(PVM.opCode(mnemonic)); emit(adr.address());
    } // CodeGen.branch

  } // end CodeGen
