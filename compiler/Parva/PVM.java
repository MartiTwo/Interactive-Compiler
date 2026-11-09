// Definition of simple stack machine and simple emulator for Parva level 1 (Java version)
// Uses auxiliary methods push, pop and next
// P.D. Terry, Rhodes University, 2014

package Parva;

import java.util.*;
import library.*;



  class Processor {
    public int sp;            // Stack pointer
    public int hp;            // Heap pointer
    public int gp;            // Global frame pointer
    public int fp;            // Local frame pointer
    public int mp;            // Mark stack pointer
    public int ir;            // Instruction register
    public int pc;            // Program counter
  } // end Processor

  class PVM {

  // Machine opcodes - not all are used

    public static final int
      nop     =   1,
      dsp     =   2,
      ldxa    =   3,
      inpi    =   4,
      inpb    =   5,
      inpc    =   6,
      inpl    =   7,
      prni    =   8,
      prnb    =   9,
      prnc    =  10,
      prns    =  11,
      prnl    =  12,
      neg     =  13,
      add     =  14,
      sub     =  15,
      mul     =  16,
      div     =  17,
      rem     =  18,
      not     =  19,
      and     =  20,
      or      =  21,
      ceq     =  22,
      cne     =  23,
      clt     =  24,
      cle     =  25,
      cgt     =  26,
      cge     =  27,
      brn     =  28,
      bze     =  29,
      anew    =  30,
      halt    =  31,
      stack   =  32,
      btrue   =  33,
      bfalse  =  34,
      ldc     =  35,
      ldc_m1  =  36,
      ldc_0   =  37,
      ldc_1   =  38,
      ldc_2   =  39,
      ldc_3   =  40,
      ldc_4   =  41,
      ldc_5   =  42,
      lda     =  43,
      lda_0   =  44,
      lda_1   =  45,
      lda_2   =  46,
      lda_3   =  47,
      lda_4   =  48,
      lda_5   =  49,
      ldv     =  50,
      sto     =  51,
      stoc    =  52,
      ldl     =  53,
      ldl_0   =  54,
      ldl_1   =  55,
      ldl_2   =  56,
      ldl_3   =  57,
      ldl_4   =  58,
      ldl_5   =  59,
      stl     =  60,
      stlc    =  61,
      stl_0   =  62,
      stl_1   =  63,
      stl_2   =  64,
      stl_3   =  65,
      stl_4   =  66,
      stl_5   =  67,
      inc     =  68,
      dec     =  69,
      incc    =  70,
      decc    =  71,
      i2c     =  72,
      heap    =  73,
      abs     =  74,
      sfu     =  75,
      efu     =  76,
      sfd     =  77,
      efd     =  78,
      ston    =  79,
      sto2    =  80,
      sto2c   =  81,
      cfu     =  82,
      cfd     =  83,
      ckfin   =  84,
      capc    =  85,
      lowc    =  86,

      nul     = 255;                         // leave gap for future

    public static String[] mnemonics = new String[PVM.nul + 1];

  // Memory

    public static final int memSize = 812;   // Limit on memory
    public static final int headerSize = 4;
    public static int[] mem;                 // Simulated memory
    static int stackBase, heapBase;          // Limits on cpu.sp

  // Program status

    static final int
      running  =  0,
      finished =  1,
      badMem   =  2,
      badData  =  3,
      noData   =  4,
      divZero  =  5,
      badOp    =  6,
      badInd   =  7,
      badVal   =  8,
      badAdr   =  9,
      badAll   = 10,
      nullRef  = 11;

    static int ps;

  // The processor

    static Processor cpu = new Processor();

  // Utilities

    static String padding = "                                                               ";
    static final int maxInt = Integer.MAX_VALUE;
    static final int maxChar = 255;

    static void stackDump(int initSP, OutFile results, int pcNow) {
    // Dump local variable and stack area - useful for debugging
      int onLine = 0;
      results.write("\nStack dump at " + pcNow);
      results.write(" SP:"); results.write(cpu.sp, 4);
      results.write(" FP:"); results.write(cpu.fp, 4);
      results.write(" HP:"); results.write(cpu.hp, 4);
      results.write(" HB:"); results.writeLine(heapBase, 4);
      for (int i = stackBase - 1; i >= cpu.sp; i--) {
        results.write(i, 7); results.write(mem[i], 5);
        onLine++; if (onLine % 6 == 0) results.writeLine();
      }
      results.writeLine();
    } // stackDump

    static void heapDump(OutFile results, int pcNow) {
    // Dump heap area - useful for debugging
      int onLine = 0;
      results.write("\nHeap dump at " + pcNow);
      results.write(" SP:"); results.write(cpu.sp, 4);
      results.write(" FP:"); results.write(cpu.fp, 4);
      results.write(" HP:"); results.write(cpu.hp, 4);
      results.write(" HB:"); results.writeLine(heapBase, 4);
      for (int i = heapBase; i < cpu.hp; i++) {
        results.write(i, 7); results.write(mem[i], 5);
        onLine++; if (onLine % 8 == 0) results.writeLine();
      }
      results.writeLine();
    } // PVM.HeapDump

    static void trace(OutFile results, int pcNow) {
    // Simple trace facility for run time debugging
      results.write(" PC:"); results.write(pcNow, 5);
      results.write(" FP:"); results.write(cpu.fp, 5);
      results.write(" SP:"); results.write(cpu.sp, 5);
      results.write(" HP:"); results.write(cpu.hp, 5);
      results.write(" TOS:");
      if (cpu.sp < memSize)
        results.write(mem[cpu.sp], 5);
      else
        results.write(" ????");
      results.write("  " + mnemonics[cpu.ir], -8);
      switch (cpu.ir) {     // two word opcodes
        case PVM.bfalse:
        case PVM.btrue:
        case PVM.brn:
        case PVM.bze:
        case PVM.dsp:
        case PVM.lda:
        case PVM.ldc:
        case PVM.ldl:
        case PVM.stl:
        case PVM.stlc:
        case PVM.ston:
        case PVM.sfd:
        case PVM.sfu:
        case PVM.efd:
        case PVM.efu:
        case PVM.prns:
          results.write(mem[cpu.pc], 7); break;
        default: break;
      }
      results.writeLine();
    } // PVM.trace

    static void postMortem(OutFile results, int pcNow) {
    // Reports run time error and position
      results.writeLine();
      switch (ps) {
        case badMem:  results.write("Memory violation");       break;
        case badData: results.write("Invalid data");           break;
        case noData:  results.write("No more data");           break;
        case divZero: results.write("Division by zero");       break;
        case badOp:   results.write("Illegal opcode");         break;
        case badInd:  results.write("Subscript out of range"); break;
        case badVal:  results.write("Value out of range");     break;
        case badAdr:  results.write("Bad address");            break;
        case badAll:  results.write("Heap allocation error");  break;
        case nullRef: results.write("Null reference");         break;
        default:      results.write("Interpreter error!");     break;
      }
      results.writeLine(" at " + pcNow);
    } // PVM.postMortem

  // The interpreters and utility methods

    static int next() {
    // Fetches next word of program and bumps program counter
      return mem[cpu.pc++];
    } // PVM.next

    static void push(int value) {
    // Bumps stack pointer and pushes value onto stack
      mem[--cpu.sp] = value;
      if (cpu.sp < cpu.hp) ps = badMem;
    } // PVM.push

    static int pop() {
    // Pops and returns top value on stack and bumps stack pointer
      if (cpu.sp == cpu.fp) ps = badMem;
      return mem[cpu.sp++];
    } // PVM.pop

    static boolean inBounds(int p) {
    // Check that memory pointer p does not go out of bounds.  This should not
    // happen with correct code, but it is just as well to check
      if (p == 0) ps = nullRef;
      else if (p < heapBase || p > memSize) ps = badMem;
      return (ps == running);
    } // PVM.inBounds

    public static void emulator(int initPC, int codeLen, int initSP,
                                InFile data, OutFile results, boolean tracing) {
    // Emulates action of the codeLen instructions stored in mem[0 .. codeLen-1], with
    // program counter initialized to initPC, stack pointer initialized to initSP.
    // data and results are used for I/O.  Tracing at the code level may be requested

      int pcNow;                  // current program counter
      int loop;                   // internal loops
      int tos, sos;               // values popped from stack
      int adr, adr1;              // effective addresses for memory accesses
      int target;                 // destination for branches
      stackBase = initSP;
      heapBase = codeLen;         // initialize boundaries
      cpu.hp = heapBase;          // initialize registers
      cpu.sp = stackBase;
      cpu.gp = stackBase;
      cpu.mp = stackBase;
      cpu.fp = stackBase;
      cpu.pc = initPC;            // initialize program counter
      for (int i = heapBase; i < stackBase; i++) mem[i] = 0;  // set entire memory to null or 0

      ps = running;               // prepare to execute
      do {
        pcNow = cpu.pc;           // retain for tracing/postmortem
        if (cpu.pc < 0 || cpu.pc >= codeLen) {
          ps = badAdr;
          break;
        }
        cpu.ir = next();          // fetch
        if (tracing) trace(results, pcNow);
        switch (cpu.ir) {         // execute
          case PVM.nop:           // no operation
            break;
          case PVM.dsp:           // decrement stack pointer (allocate space for variables)
            int localSpace = next();
            cpu.sp -= localSpace;
            if (inBounds(cpu.sp)) // initialize all local variables to zero/null
              for (loop = 0; loop < localSpace; loop++)
                mem[cpu.sp + loop] = 0;
            break;
          case PVM.ldc:           // push constant value
            push(next());
            break;
          case PVM.ldc_m1:        // push constant -1
            push(-1);
            break;
          case PVM.ldc_0:         // push constant 0
            push(0);
            break;
          case PVM.ldc_1:         // push constant 1
            push(1);
            break;
          case PVM.ldc_2:         // push constant 2
            push(2);
            break;
          case PVM.ldc_3:         // push constant 3
            push(3);
            break;
          case PVM.ldc_4:         // push constant 4
            push(4);
            break;
          case PVM.ldc_5:         // push constant 5
            push(5);
            break;
          case PVM.lda:           // push local address
            adr = cpu.fp - 1 - next();
            if (inBounds(adr)) push(adr);
            break;
          case PVM.lda_0:         // push local address 0
            adr = cpu.fp - 1;
            if (inBounds(adr)) push(adr);
            break;
          case PVM.lda_1:         // push local address 1
            adr = cpu.fp - 2;
            if (inBounds(adr)) push(adr);
            break;
          case PVM.lda_2:         // push local address 2
            adr = cpu.fp - 3;
            if (inBounds(adr)) push(adr);
            break;
          case PVM.lda_3:         // push local address 3
            adr = cpu.fp - 4;
            if (inBounds(adr)) push(adr);
            break;
          case PVM.lda_4:         // push local address 4
            adr = cpu.fp - 5;
            if (inBounds(adr)) push(adr);
            break;
          case PVM.lda_5:         // push local address 5
            adr = cpu.fp - 6;
            if (inBounds(adr)) push(adr);
            break;
          case PVM.ldl:           // push local value
            adr = cpu.fp - 1 - next();
            if (inBounds(adr)) push(mem[adr]);
            break;
          case PVM.ldl_0:         // push value of local variable 0
            adr = cpu.fp - 1;
            if (inBounds(adr)) push(mem[adr]);
            break;
          case PVM.ldl_1:         // push value of local variable 1
            adr = cpu.fp - 2;
            if (inBounds(adr)) push(mem[adr]);
            break;
          case PVM.ldl_2:         // push value of local variable 2
            adr = cpu.fp - 3;
            if (inBounds(adr)) push(mem[adr]);
            break;
          case PVM.ldl_3:         // push value of local variable 3
            adr = cpu.fp - 4;
            if (inBounds(adr)) push(mem[adr]);
            break;
          case PVM.ldl_4:         // push value of local variable 4
            adr = cpu.fp - 5;
            if (inBounds(adr)) push(mem[adr]);
            break;
          case PVM.ldl_5:         // push value of local variable 5
            adr = cpu.fp - 6;
            if (inBounds(adr)) push(mem[adr]);
            break;
          case PVM.stl:           // store local value
            adr = cpu.fp - 1 - next();
            if (inBounds(adr)) mem[adr] = pop();
            break;
          case PVM.stlc:          // character checked pop to local variable
            tos = pop(); adr = cpu.fp - 1 - next();
            if (inBounds(adr))
              if (tos >= 0 && tos <= maxChar) mem[adr] = tos;
              else ps = badVal;
            break;
          case PVM.stl_0:         // pop to local variable 0
            adr = cpu.fp - 1;
            if (inBounds(adr)) mem[adr] = pop();
            break;
          case PVM.stl_1:         // pop to local variable 1
            adr = cpu.fp - 2;
            if (inBounds(adr)) mem[adr] = pop();
            break;
          case PVM.stl_2:         // pop to local variable 2
            adr = cpu.fp - 3;
            if (inBounds(adr)) mem[adr] = pop();
            break;
          case PVM.stl_3:         // pop to local variable 3
            adr = cpu.fp - 4;
            if (inBounds(adr)) mem[adr] = pop();
            break;
          case PVM.stl_4:         // pop to local variable 4
            adr = cpu.fp - 5;
            if (inBounds(adr)) mem[adr] = pop();
            break;
          case PVM.stl_5:         // pop to local variable 5
            adr = cpu.fp - 6;
            if (inBounds(adr)) mem[adr] = pop();
            break;
          case PVM.ldv:           // dereference
            adr = pop();
            if (inBounds(adr)) push(mem[adr]);
            break;
          case PVM.sto:           // store
            tos = pop(); adr = pop();
            if (inBounds(adr)) mem[adr] = tos;
            break;
          case PVM.stoc:          // character checked store
            tos = pop(); adr = pop();
            if (inBounds(adr))
              if (tos >= 0 && tos <= maxChar) mem[adr] = tos;
              else ps = badVal;
            break;
          case PVM.sto2:          // store sos on address found on top of stack (do not pop the value)
            adr = pop();
            if (inBounds(adr)) mem[adr] = mem[cpu.sp + 1];
            break;
          case PVM.sto2c:         // character checked store sos
            adr = pop();  sos = mem[cpu.sp + 1];
            if (inBounds(adr))
              if (sos >= 0 && sos <= maxChar) mem[adr] = sos;
              else ps = badVal;
            break;
          case PVM.ston:          // store n values at top of stack on address at second on stack
            int n = next();
            for (int i = n - 1; i >= 0; i--)
              mem[mem[cpu.sp + n + i]] = mem[cpu.sp + i]; // do the assignments
                                                          // then bump stack pointer tp
            cpu.sp = cpu.sp + 2 * n;                      // discard addresses and values
            break;
          case PVM.ldxa:          // heap array indexing
            adr = pop();
            int heapPtr = pop();
            if (heapPtr == 0) ps = nullRef;
            else if (heapPtr < heapBase || heapPtr >= cpu.hp) ps = badMem;
            else if (adr < 0 || adr >= mem[heapPtr]) ps = badInd;
            else push(heapPtr + adr + 1);
            break;
          case PVM.inpi:          // integer input
            adr = pop();
            if (inBounds(adr)) {
              mem[adr] = data.readInt();
              if (data.error()) ps = badData;
            }
            break;
          case PVM.inpb:          // boolean input
            adr = pop();
            if (inBounds(adr)) {
              mem[adr] = data.readBoolean() ? 1 : 0;
              if (data.error()) ps = badData;
            }
            break;
          case PVM.inpc:          // character input
            adr = pop();
            if (inBounds(adr)) {
              mem[adr] = data.readChar();
              if (data.error()) ps = badData;
            }
            break;
          case PVM.inpl:          // skip to end of input line
            data.readLine();
            break;
          case PVM.prni:          // integer output
            if (tracing) results.write(padding);
            results.write(pop(), 0);
            if (tracing) results.writeLine();
            break;
          case PVM.prnb:          // boolean output
            if (tracing) results.write(padding);
            if (pop() != 0) results.write(" true  "); else results.write(" false ");
            if (tracing) results.writeLine();
            break;
          case PVM.prnc:          // character output
            if (tracing) results.write(padding);
            results.write((char) (Math.abs(pop()) % (maxChar + 1)), 1);
            if (tracing) results.writeLine();
            break;
          case PVM.prns:          // string output
            if (tracing) results.write(padding);
            loop = next();
            while (ps == running && mem[loop] != 0) {
              results.write((char) mem[loop]); loop--;
              if (loop < stackBase) ps = badMem;
            }
            if (tracing) results.writeLine();
            break;
          case PVM.prnl:          // newline
            results.writeLine();
            break;
          case PVM.neg:           // integer negation
            push(-pop());
            break;
          case PVM.add:           // integer addition
            tos = pop(); push(pop() + tos);
            break;
          case PVM.sub:           // integer subtraction
            tos = pop(); push(pop() - tos);
            break;
          case PVM.mul:           // integer multiplication
            tos = pop();
            sos = pop();
            if (tos != 0 && Math.abs(sos) > maxInt / Math.abs(tos)) ps = badVal;
            else push(sos * tos);
            break;
          case PVM.div:           // integer division (quotient)
            tos = pop();
            if (tos == 0) ps = divZero;
            else push(pop() / tos);
            break;
          case PVM.rem:           // integer division (remainder)
            tos = pop();
            if (tos == 0) ps = divZero;
            else push(pop() % tos);
            break;
          case PVM.not:           // logical negation
            push(pop() == 0 ? 1 : 0);
            break;
          case PVM.and:           // logical and
            tos = pop(); push(pop() & tos);
            break;
          case PVM.or:            // logical or
            tos = pop(); push(pop() | tos);
            break;
          case PVM.ceq:           // logical equality
            tos = pop(); push(pop() == tos ? 1 : 0);
            break;
          case PVM.cne:           // logical inequality
            tos = pop(); push(pop() != tos ? 1 : 0);
            break;
          case PVM.clt:           // logical less
            tos = pop(); push(pop() <  tos ? 1 : 0);
            break;
          case PVM.cle:           // logical less or equal
            tos = pop(); push(pop() <= tos ? 1 : 0);
            break;
          case PVM.cgt:           // logical greater
            tos = pop(); push(pop() >  tos ? 1 : 0);
            break;
          case PVM.cge:           // logical greater or equal
            tos = pop(); push(pop() >= tos ? 1 : 0);
            break;
          case PVM.cfu:           // check whether a "to" for loop will execute at least once
            push(mem[cpu.sp + 1] <= mem[cpu.sp] ? 1 : 0);
            break;
          case PVM.cfd:           // check whether a "downto" for loop will execute at least once
            push(mem[cpu.sp + 1] >= mem[cpu.sp] ? 1 : 0);
            break;
          case PVM.ckfin:         // check whther a for loop has completed all iterations
            push(pop() != mem[cpu.sp] ? 1 : 0);
            break;
          case PVM.brn:           // unconditional branch
            cpu.pc = next();
            if (cpu.pc < 0 || cpu.pc >= codeLen) ps = badAdr;
            break;
          case PVM.bze:           // pop top of stack, branch if false
            target = next();
            if (pop() == 0) {
              cpu.pc = target;
              if (cpu.pc < 0 || cpu.pc >= codeLen) ps = badAdr;
            }
            break;
          case PVM.bfalse:        // conditional short circuit "and" branch
            target = next();
            if (mem[cpu.sp] == 0) cpu.pc = target; else cpu.sp++;
            break;
          case PVM.btrue:         // conditional short circuit "or" branch
            target = next();
            if (mem[cpu.sp] == 1) cpu.pc = target; else cpu.sp++;
            break;
          case PVM.anew:          // heap array allocation
            int size = pop();
            if (size <= 0 || size + 1 > cpu.sp - cpu.hp - 2)
              ps = badAll;
            else {
              mem[cpu.hp] = size; // first element stores size for bounds checking
              push(cpu.hp);
              cpu.hp += size + 1; // bump heap pointer
                                  // elements are already initialized to 0 / null (why?)
            }
            break;
          case PVM.halt:          // halt
            ps = finished;
            break;
          case PVM.inc:           // integer ++
            adr = pop();
            if (inBounds(adr)) mem[adr]++;
            break;
          case PVM.dec:           // integer --
            adr = pop();
            if (inBounds(adr)) mem[adr]--;
            break;
          case PVM.incc:          // character ++ (apply range check)
            adr = pop();
            if (inBounds(adr))
              if (mem[adr] < maxChar) mem[adr]++;
              else ps = badVal;
            break;
          case PVM.decc:          // character -- (apply range check)
            adr = pop();
            if (inBounds(adr))
              if (mem[adr] > 0) mem[adr]--;
              else ps = badVal;
            break;
          case PVM.i2c:           // checked conversion integer to character
            if (mem[cpu.sp] < 0 || mem[cpu.sp] > maxChar) ps = badVal;
            break;
          case PVM.abs:           // replace TOS by its absolute value
            if (mem[cpu.sp] < 0) mem[cpu.sp] = - mem[cpu.sp];
            break;
          case PVM.stack:         // stack dump (debugging)
            stackDump(initSP, results, pcNow);
            break;
          case PVM.heap:          // heap dump (debugging)
            heapDump(results, pcNow);
            break;
          case PVM.sfu:           // start for loop "to"
            if (mem[cpu.sp + 1] > mem[cpu.sp]) cpu.pc = mem[cpu.pc];  // goto exit
            else {
              mem[mem[cpu.sp + 2]] = mem[cpu.sp + 1]; cpu.pc++;       // assign to control
            }
            break;
          case PVM.sfd:           // start for loop "downto"          // goto exit
            if (mem[cpu.sp + 1] < mem[cpu.sp]) cpu.pc = mem[cpu.pc];
            else {
              mem[mem[cpu.sp + 2]] = mem[cpu.sp + 1]; cpu.pc++;       // assign to control
            }
            break;
          case PVM.efu:           // end for loop "to"
            if (mem[mem[cpu.sp + 2]] == mem[cpu.sp]) cpu.pc++;        // loop complete
            else {
              mem[mem[cpu.sp + 2]]++; cpu.pc = mem[cpu.pc];           // increment control
            }
            break;
          case PVM.efd:           // end for loop "downto"
            if (mem[mem[cpu.sp + 2]] == mem[cpu.sp]) cpu.pc++;        // loop complete
            else {
              mem[mem[cpu.sp + 2]]--; cpu.pc = mem[cpu.pc];           // decrement control
            }
            break;
          case PVM.capc:          // char to uppercase
            if (mem[cpu.sp] >= 'a' && mem[cpu.sp] <= 'z') mem[cpu.sp] -= 32;
            break;
          case PVM.lowc:          // char to lowercase
            if (mem[cpu.sp] >= 'A' && mem[cpu.sp] <= 'Z') mem[cpu.sp] += 32;
            break;
          default:                // unrecognized opcode
            ps = badOp;
            break;
        }
      } while (ps == running);
      if (ps != finished) postMortem(results, pcNow);
    } // PVM.emulator

    public static void interpret(int codeLen, int initSP) {
    // Interactively opens data and results files.  Then interprets the codeLen
    // instructions stored in mem, with stack pointer initialized to initSP
      System.out.print("\nTrace execution (y/N/q)? ");
      char reply = (InFile.StdIn.readLine() + " ").toUpperCase().charAt(0);
      if (reply != 'Q') {
        boolean tracing = reply == 'Y';
        System.out.print("\nData file [STDIN] ? ");
        InFile data = new InFile(InFile.StdIn.readLine());
        System.out.print("\nResults file [STDOUT] ? ");
        String fname = InFile.StdIn.readLine();
        OutFile results = new OutFile(fname);
        emulator(0, codeLen, initSP, data, results, tracing);
        if (!fname.equals("")) results.close();
//        data.close();
      }
    } // PVM.interpret

    public static void listCode(String fileName, int codeLen) {
    // Lists the codeLen instructions stored in mem on a named output file
      int i, j;

      if (fileName == null) return;
      OutFile codeFile = new OutFile(fileName);

      /* ------------- The following may be useful for debugging the interpreter
      i = 0;
      while (i < codeLen) {
        codeFile.write(mem[i], 5);
        if ((i + 1) % 15 == 0) codeFile.writeLine();
        i++;
      }
      codeFile.writeLine();

      ------------- */

      i = 0;
      codeFile.writeLine("ASSEM\nBEGIN");
      while (i < codeLen && mem[i] != PVM.nul) {
        int o = mem[i] % (PVM.nul + 1); // force in range
        codeFile.write("  {");
        codeFile.write(i, 5);
        codeFile.write(" } ");
        codeFile.write(mnemonics[o], -8);
        switch (o) {                    // two word opcodes
          case PVM.bfalse:
          case PVM.btrue:
          case PVM.brn:
          case PVM.bze:
          case PVM.sfd:
          case PVM.sfu:
          case PVM.efd:
          case PVM.efu:
          case PVM.dsp:
          case PVM.lda:
          case PVM.ldc:
          case PVM.ldl:
          case PVM.stl:
          case PVM.stlc:
          case PVM.ston:
            i = (i + 1) % memSize; codeFile.write(mem[i]);
            break;

          case PVM.prns:                // special case
            i = (i + 1) % memSize;
            j = mem[i]; codeFile.write(" \"");
            while (mem[j] != 0) {
              switch (mem[j]) {
                case '\\' : codeFile.write("\\\\"); break;
                case '\"' : codeFile.write("\\\""); break;
                case '\'' : codeFile.write("\\\'"); break;
                case '\b' : codeFile.write("\\b");  break;
                case '\t' : codeFile.write("\\t");  break;
                case '\n' : codeFile.write("\\n");  break;
                case '\f' : codeFile.write("\\f");  break;
                case '\r' : codeFile.write("\\r");  break;
                default   : codeFile.write((char) mem[j]); break;
              }
              j--;
            }
            codeFile.write("\"");
            break;

        } // switch
        i = (i + 1) % memSize;
        codeFile.writeLine();
      } // while (i < codeLen)
      codeFile.writeLine("END.");
      codeFile.close();
    } // PVM.listCode

    public static int opCode(String str) {
    // Maps str to opcode, or to PVM.nul if no match can be found
    // Simple linear search.  A hashtable or dictionary might be a useful improvement!
      int op = 0;
      while (op != PVM.nul && !(str.toUpperCase().equals(mnemonics[op]))) op++;
      return op;
    } // PVM.opCode

    public static void init() {
    // Initializes stack machine
      mem = new int [memSize + 1];                    // virtual machine memory
      for (int i = 0; i <= memSize; i++) mem[i] = 0;  // set entire memory to null or 0

      // Initialize mnemonic table this way for ease of modification in exercises
      // A hashtable or dictionary might be a useful improvement!
      for (int i = 0; i <= PVM.nul; i++) mnemonics[i] = "";

      mnemonics[PVM.abs]      = "ABS";
      mnemonics[PVM.add]      = "ADD";
      mnemonics[PVM.and]      = "AND";
      mnemonics[PVM.anew]     = "ANEW";
      mnemonics[PVM.bfalse]   = "BFALSE";
      mnemonics[PVM.brn]      = "BRN";
      mnemonics[PVM.btrue]    = "BTRUE";
      mnemonics[PVM.bze]      = "BZE";
      mnemonics[PVM.cfd]      = "CFD";
      mnemonics[PVM.ckfin]    = "CKFIN";
      mnemonics[PVM.cfu]      = "CFU";
      mnemonics[PVM.ceq]      = "CEQ";
      mnemonics[PVM.cge]      = "CGE";
      mnemonics[PVM.cgt]      = "CGT";
      mnemonics[PVM.cle]      = "CLE";
      mnemonics[PVM.clt]      = "CLT";
      mnemonics[PVM.cne]      = "CNE";
      mnemonics[PVM.dec]      = "DEC";
      mnemonics[PVM.decc]     = "DECC";
      mnemonics[PVM.div]      = "DIV";
      mnemonics[PVM.dsp]      = "DSP";
      mnemonics[PVM.efd]      = "EFD";
      mnemonics[PVM.efu]      = "EFU";
      mnemonics[PVM.halt]     = "HALT";
      mnemonics[PVM.heap]     = "HEAP";
      mnemonics[PVM.i2c]      = "I2C";
      mnemonics[PVM.inc]      = "INC";
      mnemonics[PVM.incc]     = "INCC";
      mnemonics[PVM.inpb]     = "INPB";
      mnemonics[PVM.inpc]     = "INPC";
      mnemonics[PVM.inpi]     = "INPI";
      mnemonics[PVM.lda]      = "LDA";
      mnemonics[PVM.lda_0]    = "LDA_0";
      mnemonics[PVM.lda_1]    = "LDA_1";
      mnemonics[PVM.lda_2]    = "LDA_2";
      mnemonics[PVM.lda_3]    = "LDA_3";
      mnemonics[PVM.lda_4]    = "LDA_4";
      mnemonics[PVM.lda_5]    = "LDA_5";
      mnemonics[PVM.ldc]      = "LDC";
      mnemonics[PVM.ldc_0]    = "LDC_0";
      mnemonics[PVM.ldc_1]    = "LDC_1";
      mnemonics[PVM.ldc_2]    = "LDC_2";
      mnemonics[PVM.ldc_3]    = "LDC_3";
      mnemonics[PVM.ldc_4]    = "LDC_4";
      mnemonics[PVM.ldc_5]    = "LDC_5";
      mnemonics[PVM.ldc_m1]   = "LDC_M1";
      mnemonics[PVM.ldl]      = "LDL";
      mnemonics[PVM.ldl_0]    = "LDL_0";
      mnemonics[PVM.ldl_1]    = "LDL_1";
      mnemonics[PVM.ldl_2]    = "LDL_2";
      mnemonics[PVM.ldl_3]    = "LDL_3";
      mnemonics[PVM.ldl_4]    = "LDL_4";
      mnemonics[PVM.ldl_5]    = "LDL_5";
      mnemonics[PVM.ldv]      = "LDV";
      mnemonics[PVM.ldxa]     = "LDXA";
      mnemonics[PVM.mul]      = "MUL";
      mnemonics[PVM.neg]      = "NEG";
      mnemonics[PVM.nop]      = "NOP";
      mnemonics[PVM.not]      = "NOT";
      mnemonics[PVM.nul]      = "NUL";
      mnemonics[PVM.or]       = "OR";
      mnemonics[PVM.prnb]     = "PRNB";
      mnemonics[PVM.prnc]     = "PRNC";
      mnemonics[PVM.prni]     = "PRNI";
      mnemonics[PVM.prnl]     = "PRNL";
      mnemonics[PVM.prns]     = "PRNS";
      mnemonics[PVM.rem]      = "REM";
      mnemonics[PVM.sfd]      = "SFD";
      mnemonics[PVM.sfu]      = "SFU";
      mnemonics[PVM.stack]    = "STACK";
      mnemonics[PVM.stl]      = "STL";
      mnemonics[PVM.stlc]     = "STLC";
      mnemonics[PVM.stl_0]    = "STL_0";
      mnemonics[PVM.stl_1]    = "STL_1";
      mnemonics[PVM.stl_2]    = "STL_2";
      mnemonics[PVM.stl_3]    = "STL_3";
      mnemonics[PVM.stl_4]    = "STL_4";
      mnemonics[PVM.stl_5]    = "STL_5";
      mnemonics[PVM.sto]      = "STO";
      mnemonics[PVM.sto2]     = "STO2";
      mnemonics[PVM.stoc]     = "STOC";
      mnemonics[PVM.sto2c]    = "STO2C";
      mnemonics[PVM.ston]     = "STON";
      mnemonics[PVM.sub]      = "SUB";
      mnemonics[PVM.capc]     = "CAPC";
      mnemonics[PVM.lowc]     = "LOWC";
    } // PVM.init

  } // end PVM
