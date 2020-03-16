#define LIKELY(X) X
#define UNLIKELY(X) X
#define CANCEL NOP
#define STORE_ZERO Zero Store
#define LOAD_CANCEL(EA) NOP
#define STORE_CANCEL(EA) NOP
#define IS_CANCELLED(SLOT)
#define fMAX(A,B) max(A,B)
#define fMIN(A,B) min(A,B)
#define fABS(A) ABS(A)
#define fINSERT_BITS(REG,WIDTH,OFFSET,INVAL) REG[(WIDTH-1+OFFSET):OFFSET]=INVAL
#define fEXTRACTU_BITS(INREG,WIDTH,OFFSET) INREG[(WIDTH+OFFSET-1):OFFSET]
#define fEXTRACTU_BIDIR(INREG,WIDTH,OFFSET) INREG[(WIDTH+OFFSET-1):OFFSET]
#define fEXTRACTU_RANGE(INREG,HIBIT,LOWBIT) INREG[HIBIT:LOWBIT]
#define fINSERT_RANGE(INREG,HIBIT,LOWBIT,INVAL) INREG[HIBIT:LOWBIT]=INVAL
#define f8BITSOF(VAL) VAL ? 0xff : 0x00
#define fLSBOLD(VAL) VAL[0]
#define fLSBNEW(PNUM) PNUM.new[0]
#define fLSBNEW0 P0.new[0]
#define fLSBNEW1 P1.new[0]
#define fLSBOLDNOT(VAL) !VAL[0]
#define fLSBNEWNOT(PNUM) !PNUM.new[0]
#define fLSBNEW0NOT !P0.new[0]
#define fLSBNEW1NOT P1.new[0]
#define fNEWREG(RNUM) RNUM.new
#define fNEWREG_ST(RNUM) RNUM.new
#define fMEMZNEW(RNUM) RNUM.new == 0
#define fMEMNZNEW(RNUM) RNUM.new != 0
#define fVSATUVALN(N,VAL) N-Bit Unsigned Saturation Value for sign of VAL
#define fSATUVALN(N,VAL) N-Bit Unsigned Saturation Value for sign of VAL
#define fSATVALN(N,VAL) N-Bit Saturation Value for sign of VAL
#define fVSATVALN(N,VAL) N-Bit Saturation Value for sign of VAL
#define fZXTN(N,M,VAL) zxt_{N->M}(VAL)
#define fSXTN(N,M,VAL) sxt_{N->M}(VAL)
#define fSATN(N,VAL) sat_##N(VAL)
#define fVSATN(N,VAL) sat_##N(VAL)
#define fADDSAT64(DST,A,B) DST=sat64(A+B)
#define fVSATUN(N,VAL) usat_##N(VAL)
#define fSATUN(N,VAL) usat_##N(VAL)
#define fSATH(VAL) sat_16(VAL)
#define fSATUH(VAL) usat_16(VAL)
#define fVSATH(VAL) sat_16(VAL)
#define fVSATUH(VAL) usat_16(VAL)
#define fSATUB(VAL) usat_8(VAL)
#define fSATB(VAL) sat_8(VAL)
#define fVSATUB(VAL) usat_8(VAL)
#define fVSATB(VAL) sat_8(VAL)
#define fIMMEXT(IMM) apply_extension(IMM)
#define fMUST_IMMEXT(IMM) apply_extension(IMM)
#define fPCALIGN(IMM) IMM=IMM & ~PCALIGN_MASK
#define fGET_EXTENSION extension
#define fVERIFICATION_REGWRITE_NOTE(THREADID,REGNO)
#define fREAD_IREG(VAL) I
#define fREAD_R0() R0
#define fREAD_LR() LR
#define fREAD_SSR() SSR
#define fWRITE_R0(A) R0=A
#define fWRITE_LR(A) LR=A
#define fWRITE_FP(A) FP=A
#define fWRITE_SP(A) SP=A
#define fWRITE_GOSP(A) GOSP=A
#define fWRITE_GP(A) GP=A
#define fREAD_SP() SP
#define fREAD_GOSP() GOSP
#define fREAD_GELR() GELR
#define fREAD_GEVB() GEVB
#define fREAD_CSREG(N) CS N
#define fREAD_LC0 LC0
#define fREAD_LC1 LC1
#define fREAD_SA0 SA0
#define fREAD_SA1 SA1
#define fREAD_FP() FP
#define fREAD_GP() (Constant_extended ? (0) : GP)
#define fREAD_PC() PC
#define fREAD_NPC() NPC
#define fREAD_P0() P0
#define fREAD_P3() P3
#define fNOATTRIB_READ_P3() P3
#define fINVALID() Invalid instruction!
#define fCHECK_PCALIGN(A)
#define fCUREXT()
#define fCUREXT_WRAP(EXT_NO)
#define fWRITE_NPC(A) PC=A
#define fLOOPSTATS(A)
#define fCOF_CALLBACK(LOC,TYPE)
#define fBRANCH(LOC,TYPE) PC=LOC
#define fTIME_JUMPR(REGNO,TARGET,TYPE)
#define fJUMPR(REGNO,TARGET,TYPE) PC=TARGET
#define fHINTJR(TARGET)
#define fBP_RAS_CALL(A)
#define fCALL(A) fWRITE_LR(fREAD_NPC()); fWRITE_NPC(A);
#define fCALLR(A) fWRITE_LR(fREAD_NPC()); fWRITE_NPC(A);
#define fWRITE_LOOP_REGS0(START,COUNT) SA0=START; LC0=COUNT
#define fWRITE_LOOP_REGS1(START,COUNT) SA1=START; LC1=COUNT
#define fWRITE_LC0(VAL) LC0=VAL
#define fWRITE_LC1(VAL) LC1=VAL
#define fCARRY_FROM_ADD(A,B,C) carry_from_add(A,B,C)
#define fSETCV_ADD(A,B,CARRY) SR[CV]=FLAGS(A+B)
#define fSETCV_SUB(A,B,CARRY) SR[CV]=FLAGS(A-B)
#define fSET_OVERFLOW() USR.OVF=1
#define fSET_LPCFG(VAL) USR.LPCFG=VAL
#define fGET_LPCFG USR.LPCFG
#define fWRITE_P0(VAL) P0=VAL
#define fWRITE_P1(VAL) P1=VAL
#define fWRITE_P2(VAL) P2=VAL
#define fWRITE_P3(VAL) P3=VAL
#define fWRITE_P3_LATE(VAL) P3=VAL
#define fPART1(WORK) WORK
#define fCAST4u(A) A.uw[0]
#define fCAST4s(A) A.s32
#define fCAST8u(A) A.u64
#define fCAST8s(A) A.s64
#define fCAST2_2s(A) A
#define fCAST2_2u(A) A
#define fCAST4_4s(A) A
#define fCAST4_4u(A) A
#define fCAST4_8s(A) fSXTN(32,64,A)
#define fCAST4_8u(A) fZXTN(32,64,A)
#define fCAST8_8s(A) A
#define fCAST8_8u(A) A
#define fCAST2_8s(A) fSXTN(16,64,A)
#define fCAST2_8u(A) fZXTN(16,64,A)
#define fZE8_16(A) A
#define fSE8_16(A) A
#define fSE16_32(A) A
#define fZE16_32(A) A
#define fSE32_64(A) A
#define fSE64_128(A) A
#define fZE32_64(A) A
#define fSE8_32(A) A
#define fZE8_32(A) A
#define fMPY8UU(A,B) (A * B)
#define fMPY8US(A,B) (A * B)
#define fMPY8SU(A,B) (A * B)
#define fMPY8SS(A,B) (A * B)
#define fMPY16SS(A,B) (A * B)
#define fMPY16UU(A,B) (A * B)
#define fMPY16SU(A,B) (A * B)
#define fMPY16US(A,B) (A * B)
#define fMPY32SS(A,B) (A * B)
#define fMPY32UU(A,B) (A * B)
#define fMPY32SU(A,B) (A * B)
#define fMPY3216SS(A,B) (A * B)
#define fMPY3216SU(A,B) (A * B)
#define fROUND(A) round(A)
#define fCLIP(DST,SRC,U) DST=MIN((1<<U)-1,MAX(SRC,-(1<<U)))
#define fCRND(A) convround(A)
#define fRNDN(A,N) (N==0)?(A):round(A,2**(N-1))
#define fCRNDN(A,N) (N==0)?A:convround(A,2**(N-1))>>N
#define fCRNDN64(A,N) (N==0)?A:convround(A,2**(N-1))>>N
#define fADD128(A,B) A+B
#define fSUB128(A,B) A-B
#define fSHIFTR128(A,B) (size8s_t) (A >> B)
#define fSHIFTL128(A,B) (A << B)
#define fAND128(A,B) (A & B)
#define fCAST8S_16S(A) sxt_{64->128}(A)
#define fCAST16S_8S(A) sxt_{128->64}(A)
#define fCAST16S_4S(A) sxt_{128->32}(A)
#define fEA_RI(REG,IMM) EA=REG+IMM
#define fEA_RRs(REG,REG2,SCALE) EA=REG+(REG2<<SCALE)
#define fEA_IRs(IMM,REG,SCALE) EA=IMM+(REG<<SCALE)
#define fEA_IMM(IMM) EA=IMM
#define fEA_REG(REG) EA=REG
#define fEA_BREVR(REG) EA=fbrev(REG)
#define fEA_GPI(IMM) EA=fREAD_GP()+IMM
#define fPM_I(REG,IMM) REG=REG+IMM
#define fPM_M(REG,MVAL) REG=REG+MVAL
#define fPM_CIRI(REG,IMM,MVAL) REG=fcirc_add(REG,IMM,MVAL)
#define fPM_CIRR(REG,VAL,MVAL) REG=fcirc_add(REG,VAL,MVAL)
#define fMODCIRCU(N,P) N modulo 2^P
#define fSCALE(N,A) A<<N
#define fVSATW(A) sat_32(A)
#define fSATW(A) sat_32(A)
#define fVSAT(A) sat_32(A)
#define fSAT(A) sat_32(A)
#define fSAT_ORIG_SHL(A,ORIG_REG) sat_32(A)
#define fPASS(A) A
#define fRND(A) ((A)+1)>>1
#define fBIDIR_SHIFTL(SRC,SHAMT,REGSTYPE) bidir_shiftl(SRC,SHAMT)
#define fBIDIR_ASHIFTL(SRC,SHAMT,REGSTYPE) (SHAMT>0)?(fCAST##REGSTYPE##s(SRC)<<SHAMT):(fCAST##REGSTYPE##s(SRC)>>SHAMT)
#define fBIDIR_LSHIFTL(SRC,SHAMT,REGSTYPE) (SHAMT>0)?(fCAST##REGSTYPE##u(SRC)<<SHAMT):(fCAST##REGSTYPE##u(SRC)>>>SHAMT)
#define fBIDIR_ASHIFTL_SAT(SRC,SHAMT,REGSTYPE) bidir_shiftl(SRC,SHAMT)
#define fBIDIR_SHIFTR(SRC,SHAMT,REGSTYPE) bidir_shiftr(SRC,SHAMT)
#define fBIDIR_ASHIFTR(SRC,SHAMT,REGSTYPE) (SHAMT>0)?(fCAST##REGSTYPE##s(SRC)>>SHAMT):(fCAST##REGSTYPE##s(SRC)<<SHAMT)
#define fBIDIR_LSHIFTR(SRC,SHAMT,REGSTYPE) (SHAMT>0)?(fCAST##REGSTYPE##u(SRC)>>>SHAMT):(fCAST##REGSTYPE##u(SRC)<<SHAMT)
#define fBIDIR_ASHIFTR_SAT(SRC,SHAMT,REGSTYPE) bidir_shiftr(SRC,SHAMT)
#define fASHIFTR(SRC,SHAMT,REGSTYPE) SRC >> SHAMT
#define fLSHIFTR(SRC,SHAMT,REGSTYPE) SRC >>> SHAMT
#define fROTL(SRC,SHAMT,REGSTYPE) SRC <<_{R} SHAMT
#define fROTR(SRC,SHAMT,REGSTYPE) SRC >>_{R} SHAMT
#define fASHIFTL(SRC,SHAMT,REGSTYPE) fCAST##REGSTYPE##s(SRC) << SHAMT
#define fFLOAT(A) A
#define fUNFLOAT(A) A
#define fHALF(A) A
#define fUNHALF(A) A
#define fHF_BIAS() 15
#define fHF_MANTBITS() 10
#define fSFNANVAL() NaN
#define fSFINFVAL(A) sign(A) * Inf
#define fSFONEVAL(A) sign(A) * 1.0
#define fCHECKSFNAN(DST,A) if (isnan(A)) DST = NaN;
#define fCHECKSFNAN3(DST,A,B,C) if (isnan(A) || isnan(B) || isnan(C)) DST = NaN;
#define fSF_BIAS() 127
#define fSF_MANTBITS() 23
#define fSF_RECIP_LOOKUP(IDX) recip_lut[IDX]
#define fSF_INVSQRT_LOOKUP(IDX) invsqrt_lut[IDX]
#define fSF_MUL_POW2(A,B) A * 2**B
#define fSF_GETEXP(A) exponent(A)
#define fSF_MAXEXP() 254
#define fSF_RECIP_COMMON(N,D,O,A) (N,D,O,A)=recip_common(N,D)
#define fSF_INVSQRT_COMMON(N,O,A) (N,O,A)=invsqrt_common(N)
#define fFMAFX(A,B,C,ADJ) fmaf(A,B,C) * 2**(ADJ)
#define fFMAF(A,B,C) fmaf(A,B,C)
#define fSFMPY(A,B) A*B
#define fMAKESF(SIGN,EXP,MANT) -1**SIGN * 1.MANT * 2**(EXP-BIAS)
#define fDOUBLE(A) A
#define fUNDOUBLE(A) A
#define fDFNANVAL() NaN
#define fDFINFVAL(A) sign(A) * Inf
#define fDFONEVAL(A) sign(A) * 1.0
#define fCHECKDFNAN(DST,A) if (isnan(A)) DST = NaN;
#define fCHECKDFNAN3(DST,A,B,C) if (isnan(A) || isnan(B) || isnan(C)) DST = NaN;
#define fDF_BIAS() 1023
#define fDF_ISNORMAL(X) is_normal(X)
#define fDF_ISDENORM(X) is_denormal(X)
#define fDF_ISBIG(X) (df_exponent(X) >= 512)
#define fDF_MANTBITS() 52
#define fDF_RECIP_LOOKUP(IDX) recip_lut[IDX]
#define fDF_INVSQRT_LOOKUP(IDX) invsqrt_lut[IDX]
#define fDF_MUL_POW2(A,B) A * 2**B
#define fDF_GETEXP(A) exponent(A)
#define fDF_MAXEXP() 2046
#define fDF_RECIP_COMMON(N,D,O,A) (N,D,O,A)=recip_common(N,D)
#define fDF_INVSQRT_COMMON(N,O,A) (N,O,A)=invsqrt_common(N)
#define fFMA(A,B,C) fma(A,B,C)
#define fDFMPY(A,B) A*B
#define fDF_MPY_HH(A,B,ACC) A*B with partial product ACC
#define fFMAX(A,B,C,ADJ) fma(A,B,C)*2**(2*ADJ)
#define fMAKEDF(SIGN,EXP,MANT) -1**SIGN * 1.MANT * 2**(EXP-BIAS)
#define fFPOP_START() fpop_start
#define fFPOP_END() fpop_end
#define fFPSETROUND_NEAREST() round_to_nearest()
#define fFPSETROUND_CHOP() round_to_zero()
#define fFPCANCELFLAGS() cancel_flags()
#define fISINFPROD(A,B) isinf(A*B)
#define fISZEROPROD(A,B) is_true_zero(A*B)
#define fRAISEFLAGS(A) fpflags |= A
#define fDF_MAX(A,B) fmax(A,B)
#define fDF_MIN(A,B) fmin(A,B)
#define fSF_MAX(A,B) fmaxf(A,B)
#define fSF_MIN(A,B) fmin(A,B)
#define fMMU(ADDR) ADDR
#define fcirc_add(REG,INCR,IMMED) REG=circ_add(REG,INCR,IMMED)
#define fbrev(REG) REG.h[1] | brev(REG.h[0])
#define fLOAD(NUM,SIZE,SIGN,EA,DST) DST = *EA
#define fMEMOP(NUM,SIZE,SIGN,EA,FNTYPE,VALUE) DST = *EA
#define fGET_FRAMEKEY() FRAMEKEY
#define fFRAME_SCRAMBLE(VAL) frame_scramble(VAL)
#define fFRAME_UNSCRAMBLE(VAL) frame_unscramble(VAL)
#define fFRAMECHECK(ADDR,EA) frame_check_limit(ADDR)
#define fLOAD_LOCKED(NUM,SIZE,SIGN,EA,DST) DST = *EA;
#define fLOAD_PHYS(NUM,SIZE,SIGN,SRC1,SRC2,DST) DST = *((SRC1&0x7ff) | (SRC2<<11))
#define fSTORE(NUM,SIZE,EA,SRC) *EA = SRC
#define fSTORE_DMA(NUM,SIZE,EA,SRC) *EA = SRC
#define fSTORE_LOCKED(NUM,SIZE,EA,SRC,PRED) if (lock_valid) { *EA = SRC; PRED = 0xff; lock_valid = 0; } else { PRED = 0; }
#define fVTCM_MEMCPY(DST,SRC,SIZE) for (i = 0; i <= SIZE; i++) { *(DST + i) = *(SRC + i); }
#define fPERMUTEH(SRC0,SRC1,CTRL) permute(SRC0,SRC1,CTRL)
#define fPERMUTEB(SRC0,SRC1,CTRL) permute(SRC0,SRC1,CTRL)
#define fGETBYTE(N,SRC) SRC.b[N]
#define fGETUBYTE(N,SRC) SRC.ub[N]
#define fSETBYTE(N,DST,VAL) DST.b[N]=VAL
#define fGETHALF(N,SRC) SRC.h[N]
#define fGETUHALF(N,SRC) SRC.uh[N]
#define fSETHALF(N,DST,VAL) DST.h[N]=VAL
#define fGETWORD(N,SRC) SRC.w[N]
#define fGETUWORD(N,SRC) SRC.uw[N]
#define fSETWORD(N,DST,VAL) DST.w[N]=VAL
#define fACC()
#define fEXTENSION_AUDIO(A) A
#define fSETBIT(N,DST,VAL) DST.N = VAL
#define fGETBIT(N,SRC) SRC.N
#define fSETBITS(HI,LO,DST,VAL) DST[HI:LO] = VAL
#define fUNDEFINED() UNDEFINED
#define fCOUNTONES_2(VAL) count_ones(VAL)
#define fCOUNTONES_4(VAL) count_ones(VAL)
#define fCOUNTONES_8(VAL) count_ones(VAL)
#define fBREV_8(VAL) reverse_bits(VAL)
#define fBREV_4(VAL) reverse_bits(VAL)
#define fBREV_2(VAL) reverse_bits(VAL)
#define fBREV_1(VAL) reverse_bits(VAL)
#define fCL1_8(VAL) count_leading_ones(VAL)
#define fCL1_4(VAL) count_leading_ones(VAL)
#define fCL1_2(VAL) count_leading_ones(VAL)
#define fCL1_1(VAL) count_leading_ones(VAL)
#define fCLZ_16(VAL) count_leading_zeros(VAL)
#define fINTERLEAVE(ODD,EVEN) interleave(ODD,EVEN)
#define fDEINTERLEAVE(MIXED) deinterleave(ODD,EVEN)
#define fNORM16(VAL) get norm of 16bit value(VAL)
#define fHIDE(A)
#define fASM_MAP(A,B) Assembler mapped to: B
#define fCOND_ASM_MAP(A,C,X,Y) if (C) {Assembler mapped to: X;} else {Assembler mapped to: Y;}@
#define fCONSTLL(A) A
#define fCONSTULL(A) A
#define fECHO(A) A
#define RUNNABLE_THREADS_MAX THREADS_MAX
#define THREAD_IS_ON(PROC,TNUM) THREAD IS ENABLE
#define THREAD_EN_MASK(PROC) THREAD IS ENABLE MASK
#define READ_IMASK(TH) IMASK[TH]
#define WRITE_IMASK(TH,VAL) IMASK[TH]=VAL
#define WRITE_PRIO(TH,VAL) TID[TH].PRIO=VAL
#define DO_IASSIGNW(REG) IASSIGNW(REG)
#define fDO_NMI(SREG) Raise NMI on threads
#define fDO_TRACE(SREG) Send value to ETM trace
#define DO_IASSIGNR(SREG,DREG) DREG=IASSIGNR(SREG)
#define DO_SWI(REG) IPEND |= REG
#define DO_CSWI(REG) IPEND &= ~REG
#define DO_CIAD(VAL) IAD &= ~VAL
#define DO_SIAD(VAL) IAD |= VAL
#define fBREAK() Enter Debug mode
#define fGP_DOCHKPAGECROSS(BASE,SUM)
#define fDOCHKPAGECROSS(BASE,SUM)
#define fTIMING_AIA(OLDVA,NEWVA)
#define fPAUSE(IMM) Pause for IMM cycles
#define fTRAP(TRAPTYPE,IMM) SSR.CAUSE = IMM; TRAP # TRAPTYPE
#define fINTERNAL_CLEAR_SAMEPAGE()
#define fCLEAR_RTE_EX() SSR.SSR_EX = 0
#define fTLB_LOCK_AVAILABLE() SYSCFG.TLBLOCK == 0
#define fK0_LOCK_AVAILABLE() SYSCFG.K0LOCK == 0
#define fSET_TLB_LOCK() if (can_aquire_tlb_lock) {SYSCFG.TLBLOCK = 1;} else {sleep_until_available;}
#define fSET_K0_LOCK() if (can_aquire_k0_lock) {SYSCFG.K0LOCK = 1;} else {sleep_until_available;}
#define fCLEAR_TLB_LOCK() SYSCFG.TLBLOCK = 0
#define fCLEAR_K0_LOCK() SYSCFG.K0LOCK = 0
#define fWRITE_REG_FIELD(REG,FIELD,VAL) REG.FIELD = VAL
#define fALIGN_REG_FIELD_VALUE(FIELD,VAL) VAL << FIELD.OFFSET
#define fGET_REG_FIELD_MASK(FIELD) VAL << FIELD.OFFSET
#define fLOG_REG_FIELD(REG,FIELD,VAL) REG.FIELD = VAL
#define fWRITE_GLOBAL_REG_FIELD(REG,FIELD,VAL) REG.FIELD = VAL
#define fLOG_GLOBAL_REG_FIELD(REG,FIELD,VAL) REG.FIELD = VAL
#define fREAD_REG_FIELD(REG,FIELD) REG.FIELD
#define fREAD_GLOBAL_REG_FIELD(REG,FIELD) REG.FIELD
#define fGET_FIELD(VAL,FIELD) VAL.FIELD
#define fSET_FIELD(VAL,FIELD,NEWVAL) VAL.FIELD
#define fSET_RUN_MODE_NOW(TNUM) modectl[TNUM] = 1
#define fIN_DEBUG_MODE(TNUM) in_debug_mode
#define fIN_DEBUG_MODE_NO_ISDB(TNUM) in_debug_mode
#define fIN_DEBUG_MODE_WARN(TNUM)
#define fCLEAR_RUN_MODE(TNUM) modectl[TNUM] = 0
#define fCLEAR_RUN_MODE_NOW(TNUM) modectl[TNUM] = 0
#define fGET_RUN_MODE(TNUM) modectl[TNUM]
#define fSET_WAIT_MODE(TNUM) modectl[(TNUM+16)] = 1
#define fCLEAR_WAIT_MODE(TNUM) modectl[(TNUM+16)] = 0
#define fGET_WAIT_MODE(TNUM) modectl[(TNUM+16)]
#define fRESET_THREAD(T,NUM) reset_thread(NUM)
#define fREAD_CURRENT_EVB() EVB
#define fREAD_ELR() ELR
#define fPOW2_HELP_ROUNDUP(VAL) helper for pow2_roundup
#define fPOW2_ROUNDUP(VAL) pow2_roundup(VAL)
#define fTLB_IDXMASK(INDEX) INDEX % TLBSIZE
#define fTLB_NONPOW2WRAP(INDEX) INDEX % TLBSIZE
#define fTLBW(INDEX,VALUE) TLB[INDEX] = VALUE
#define fTLB_ENTRY_OVERLAP(VALUE) CHECK_TLB_OVERLAP(VALUE)
#define fTLB_ENTRY_OVERLAP_IDX(VALUE) GET_OVERLAPPING_IDX(VALUE)
#define fTLBR(INDEX) TLB[INDEX]
#define fTLBP(TLBHI) search_TLB(TLBHI)
#define READ_SGP0() SGP0
#define READ_SGP1() SGP1
#define READ_SGP10() SGP
#define READ_UGP() UGP
#define WRITE_SGP0(VAL) SGP0 = VAL
#define WRITE_SGP1(VAL) SGP1 = VAL
#define WRITE_SGP10(VAL) SGP = VAL
#define WRITE_UGP(VAL) UGP = VAL
#define fSTART(REG) start(REG)
#define fRESUME(REG) resume(REG)
#define fGET_TNUM() TNUM
#define fVERIF_MARK_RREG_ACCESS(...)
#define fVERIF_MARK_RREGPAIR_ACCESS(...)
#define fVERIF_MARK_CREG_ACCESS(...)
#define fVERIF_MARK_CREGPAIR_ACCESS(...)
#define fVERIF_MARK_SREG_ACCESS(...)
#define fVERIF_MARK_SREGPAIR_ACCESS(...)
#define fVERIF_MARK_GREG_ACCESS(...)
#define fVERIF_MARK_GREGPAIR_ACCESS(...)
#define fVERIF_MARK_VREG_ACCESS(...)
#define fVERIF_MARK_VREGPAIR_ACCESS(...)
#define fVERIF_MARK_VREGQUAD_ACCESS(...)
#define fBARRIER() memory_barrier
#define fSYNCH() memory_synch
#define fISYNC() instruction_sync
#define fICFETCH(REG) icache_fetch(REG)
#define fDCFETCH(REG) dcache_fetch(REG)
#define fICINVIDX(REG) icache_inv_idx(REG)
#define fICINVA(REG) icache_inv_addr(REG)
#define fICKILL() icache_inv_all()
#define fDCKILL() dcache_inv_all()
#define fL2KILL() l2cache_inv_all()
#define fL2UNLOCK() l2cache_global_unlock()
#define fL2CLEAN() l2cache_global_clean()
#define fL2CLEANINV() l2cache_global_clean_inv()
#define fL2CLEANPA(REG) l2cache_global_clean_range(REG)
#define fL2CLEANINVPA(REG) l2cache_global_clean_inv_range(REG)
#define fL2CLEANINVIDX(REG) l2cache_clean_invalidate_idx(REG)
#define fL2CLEANIDX(REG) l2cache_clean_idx(REG)
#define fL2INVIDX(REG) l2cache_inv_idx(REG)
#define fDCTAGR(INDEX,DST,DSTREGNO) dcache_tag_read(INDEX)
#define fDCTAGW(INDEX,PART2) dcache_tag_write(INDEX,PART2)
#define fICTAGR(INDEX,DST,REGNO) icache_tag_read(INDEX)
#define fICDATAR(INDEX, DST) icache_data_read(INDEX)
#define fICTAGW(INDEX,PART2) icache_tag_write(INDEX,PART2)
#define fICDATAW(INDEX,DATA) icache_data_write(INDEX,DATA)
#define fL2FETCH(ADDR,HEIGHT,WIDTH,STRIDE,FLAGS) l2fetch(ADDR,INFO)
#define fDMSTART(NEWPTR) DM0[31:4] = Rs[31:0];DMA start
#define fDMLINK(CURPTR, NEWPTR) CURPTR->Next = NEWPTR;if (DM0 == 0);  DMA Start
#define fDMPOLL(DST) DST = DM0
#define fDMWAIT(DST) DST = DM0
#define fDMSYNCHT(DST) DST = DM0
#define fDMTLBSYNCH(DST) DST = DM0
#define fDMPAUSE(DST) DST = DM0;DMA = Idle
#define fDMRESUME(PTR) DST = DM0;Restart DMA
#define fDMWAITDESCRIPTOR(SRC,DST) dma_waitdescriptor()
#define fDMCFGRD(DMANUM,DST) DST = DM0
#define fDMCFGWR(DMANUM,DATA) DM[DMANUM]=DATA
#define fL2TAGR(INDEX, DST, DSTREG) l2cache_tag_read(INDEX)
#define fL2LOCKA(VA,DST,PREGDST) DST=l2locka(VA)
#define fL2UNLOCKA(VA) l2unlocka(VA)
#define fL2TAGW(INDEX,PART2) l2cache_tag_write(INDEX,PART2)
#define fDCCLEANIDX(REG) dcache_clean_idx(REG)
#define fDCCLEANA(REG) dcache_clean_addr(REG)
#define fDCCLEANINVIDX(REG) dcache_cleaninv_idx(REG)
#define fDCCLEANINVA(REG) dcache_cleaninv_addr(REG)
#define fDCZEROA(REG) dcache_zero_addr(REG)
#define fDCINVIDX(REG) dcache_inv_idx(REG)
#define fDCINVA(REG) dcache_inv_addr(REG)
#define fCHECKFORPRIV() priv_check();
#define fCHECKFORGUEST() priv_check();
#define fILLEGAL() illegal()
#define fTAKEN_INTERRUPT_EDGECLEAR(proc,intnum) clear_ipend(intnum)
#define fSET_IAD(thread,intnum) set_iad(intnum)
#define fBRANCH_SPECULATED_RIGHT(JC,SD,DOTNEWVAL) branch_speculated_right(JC,SD,DOTNEWVAL)
#define fBRANCH_SPECULATE_STALL(DOTNEWVAL, JUMP_COND, SPEC_DIR, HINTBITNUM, STRBITNUM)
#define CACHE_MODIFY(A, B) cache_modify()
#define SIM_BUSACCESS(A,B,C,D,E,F,G,H,I) sim_busaccess_macro()
#define SIM_BUSACCESS_ACK(A,B,C,D,E,F,G,H,I) sim_busaccess_macro()
#define SET_PMU_EVENT_STATE(_THREAD_, _PMU_EVENT_NUM_) set_pmu_state()
#define CLEAR_PMU_EVENT_STATE(_THREAD_, _PMU_EVENT_NUM_) clear_pmu_state()
#define fNOP_EXECUTED do nothing
#define fSETMREG(IDX, VAL) fSETMREG(IDX,VAL)
#define fGETMREG(IDX) fGETMREG(IDX)
#define fXORBITS(SUM,VAR,VEC) SUM = xor bitsin(VEC)
#define fTIMING(A)
#define IV1DEAD()
#define FAKE()
#define fIN_MONITOR_MODE() in_monitor_mode()
#define fIN_USER_MODE() in_user_mode()
#define fIN_GUEST_MODE() in_guest_mode()
#define fGRE_ENABLED() CCR.GRE
#define fGTE_ENABLED() CCR.GRE
#define fTRAP1_VIRTINSN(IMM) can_handle_trap1_virtinsn(IMM)
#define fTRAP0_TO_GUEST() can_handle_trap0_to_guest(IMM)
#define fVIRTINSN_RTE(IMM,REG) VMRTE
#define fVIRTINSN_SETIE(IMM,REG) VMSETIE
#define fVIRTINSN_GETIE(IMM,REG) VMGETIE
#define fVIRTINSN_SPSWAP(IMM,REG) VMSPSWAP
#define fGUESTTRAP(TRAPTYPE,IMM) GSR.CAUSE = IMM; TRAP # TRAPTYPE
#define fPREDUSE_TIMING() PREDUSE_TIMING
#define INC_GUESTCOMMIT GUEST REGISTER INCREMENT
#define INC_GUESTCYCLE THREAD RUNNING CYCLE REG INCR
#define SHARED_EXT_COMMIT_REGS(...)
#define SHARED_EXT_COMMIT_MEM(...)
#define SHARED_EXT_REWIND(...)
#define SHARED_EXT_ANALYZE_PACKET(...)
#define SHARED_EXT_EXCEPTION(PACKET)
#define SHARED_EXT_EXCEPTION_STUFFED(PACKET)
#define SHARED_EXT_EXCEPTION_BQ(...)
