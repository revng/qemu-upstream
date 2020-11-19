#define fLSBOLD(VAL) (fGETBIT(0, VAL)) // Copy rule
#define f8BITSOF(VAL) (VAL ? 0xff : 0x00) // Allows easier parsing
#define fLSBOLDNOT(VAL) (!fGETBIT(0, VAL)) // Negation operator
#define fLSBNEWNOT(PNUM) (!fLSBNEW(PNUM)) // Negation operator
#define fLSBNEW0NOT (!fLSBNEW0) // Negation operator
#define fSATH(VAL) fSATN(16, VAL) // Copy rule
#define fSATUH(VAL) fSATUN(16, VAL) // Copy rule
#define fVSATH(VAL) fVSATN(16, VAL) // Copy rule
#define fVSATUH(VAL) fVSATUN(16, VAL) // Copy rule
#define fSATUB(VAL) fSATUN(8, VAL) // Copy rule
#define fSATB(VAL) fSATN(8, VAL) // Copy rule
#define fVSATUB(VAL) fVSATUN(8, VAL) // Copy rule
#define fVSATB(VAL) fVSATN(8, VAL) // Copy rule
#define fPCALIGN(IMM) (IMM=IMM & ~3) 
#define fWRITE_LR(A) (LR=A) // Becomes an assignment
#define fWRITE_FP(A) (FP=A) // Becomes an assignment
#define fWRITE_SP(A) (SP=A) // Becomes an assignment
#define fREAD_GP() (Constant_extended ? (0) : GP) // Allows easier parsing
#define fBRANCH(LOC,TYPE) (PC=LOC) // Becomes an assignment
#define fJUMPR(REGNO,TARGET,TYPE) (PC=TARGET) // Becomes an assignment
#define fCALL(A) fWRITE_LR(fREAD_NPC()); fWRITE_NPC(A); // Copy rule
#define fCALLR(A) fWRITE_LR(fREAD_NPC()); fWRITE_NPC(A); // Copy rule
#define fWRITE_LOOP_REGS0(START,COUNT) SA0=START; (LC0=COUNT) // Becomes an assignment
#define fWRITE_LOOP_REGS1(START,COUNT) SA1=START; (LC1=COUNT) // Becomes an assignment
#define fWRITE_LC0(VAL) (LC0=VAL) // Becomes an assignment
#define fWRITE_LC1(VAL) (LC1=VAL) // Becomes an assignment
#define fSET_LPCFG(VAL) (USR.LPCFG=VAL) // Becomes an assignment
#define fWRITE_P0(VAL) P0=VAL; // Becomes an assignment
#define fWRITE_P1(VAL) P1=VAL; // Becomes an assignment
#define fWRITE_P3(VAL) P3=VAL; // Becomes an assignment
#define fCAST4_8s(A) fSXTN(32,64,A) // Copy rule
#define fCAST4_8u(A) fZXTN(32,64,A) // Copy rule
#define fCAST2_8s(A) fSXTN(16,64,A) // Copy rule
#define fCAST2_8u(A) fZXTN(16,64,A) // Copy rule
#define fCLIP(DST,SRC,U) (DST=fMIN((1<<U)-1,fMAX(SRC,-(1<<U)))) // Allows easier parsing
#define fRNDN(A,N) ((N==0)?(A):round(A,2**(N-1))) // Allows easier parsing
#define fCRNDN(A,N) ((N==0)?A:convround(A,2**(N-1))>>N) // Allows easier parsing
#define fADD128(A,B) (A+B)
#define fSUB128(A,B) (A-B)
#define fSHIFTR128(A,B) (size8s_t) (A >> B)
#define fSHIFTL128(A,B) (A << B)
#define fAND128(A,B) (A & B)
#define fCAST8S_16S(A) (fSXTN(64,128,A)) // Copy rule
#define fCAST16S_8S(A) (fSXTN(128,64,A)) // Copy rule
#define fEA_RI(REG,IMM) (EA=REG+IMM) // Becomes an assignment
#define fEA_RRs(REG,REG2,SCALE) (EA=REG+(REG2<<SCALE))  // Becomes an assignment
#define fEA_IRs(IMM,REG,SCALE) (EA=IMM+(REG<<SCALE))  // Becomes an assignment
#define fEA_IMM(IMM) (EA=IMM) // Becomes an assignment
#define fEA_REG(REG) (EA=REG) // Becomes an assignment
#define fEA_BREVR(REG) (EA=fbrev(REG)) // Becomes an assignment
#define fEA_GPI(IMM) (EA=fREAD_GP()+IMM) // Becomes an assignment
#define fPM_I(REG,IMM) (REG=REG+IMM) // Becomes an assignment
#define fPM_M(REG,MVAL) (REG=REG+MVAL) // Becomes an assignment
#define fSCALE(N,A) (A<<N)
#define fVSATW(A) fVSATN(32, fCAST8_8s(A)) // Copy rule
#define fSATW(A) fSATN(32, fCAST8_8s(A)) // Copy rule
#define fVSAT(A) fVSATN(32, A) // Copy rule
#define fSAT(A) fSATN(32, A) // Copy rule
#define fBIDIR_ASHIFTL(SRC,SHAMT,REGSTYPE) ((SHAMT>0)?(fCAST##REGSTYPE##s(SRC)<<SHAMT):(fCAST##REGSTYPE##s(SRC)>>-SHAMT)) // Allows easier parsing
#define fBIDIR_LSHIFTL(SRC,SHAMT,REGSTYPE) ((SHAMT>0)?(fCAST##REGSTYPE##u(SRC)<<SHAMT):(fCAST##REGSTYPE##u(SRC)>>>-SHAMT)) // Allows easier parsing
#define fBIDIR_ASHIFTL_SAT(SRC,SHAMT,REGSTYPE) bidir_shiftl(SRC,SHAMT) // Allows easier parsing
#define fBIDIR_ASHIFTR(SRC,SHAMT,REGSTYPE) ((SHAMT>0)?(fCAST##REGSTYPE##s(SRC)>>SHAMT):(fCAST##REGSTYPE##s(SRC)<<-SHAMT)) // Allows easier parsing
#define fBIDIR_LSHIFTR(SRC,SHAMT,REGSTYPE) ((SHAMT>0)?(fCAST##REGSTYPE##u(SRC)>>>SHAMT):(fCAST##REGSTYPE##u(SRC)<<-SHAMT)) // Allows easier parsing
#define fBIDIR_ASHIFTR_SAT(SRC,SHAMT,REGSTYPE) bidir_shiftr(SRC,SHAMT) // Allows easier parsing
#define fASHIFTR(SRC,SHAMT,REGSTYPE) (SRC >> SHAMT)
#define fLSHIFTR(SRC,SHAMT,REGSTYPE) (SRC >>> SHAMT)
#define fROTL(SRC,SHAMT,REGSTYPE) (SRC <<_{R} SHAMT)
#define fASHIFTL(SRC,SHAMT,REGSTYPE) (fCAST##REGSTYPE##s(SRC) << SHAMT)
#define fbrev(REG) REG.h[1] | brev(REG.h[0]) // Allows easier parsing
#define fHIDE(A) // Useful for hiding C language snippets
#define fBRANCH_SPECULATE_STALL(DOTNEWVAL, JUMP_COND, SPEC_DIR, HINTBITNUM, STRBITNUM) // Useful for removing large parts of unused input code
