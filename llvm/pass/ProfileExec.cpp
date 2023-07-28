/*
 *  (C) 2010 by Computer System Laboratory, IIS, Academia Sinica, Taiwan.
 *      See COPYRIGHT in top-level directory.
 */

#include "llvm/Transforms/Utils/Cloning.h"
#include "llvm-debug.h"
#include "llvm-soft-perfmon.h"
#include "llvm-pass.h"
#include "llvm-opc.h"
#include "llvm.h"
#include "utils.h"

#define PASS_NAME  "ProfileExec"

extern LLVMEnv *LLEnv;

/*
 * Profile Pass
 */
class ProfileExec : public FunctionPass {
    enum {
        IDX_LOOP = 0,
        IDX_EXIT,
        IDX_INBR,
    };

    IRFactory *IF;
    const DataLayout *DL;
    MDFactory *MF;
    IntegerType *Int8Ty;
    IntegerType *Int32Ty;
    IntegerType *Int64Ty;
    IntegerType *IntPtrTy;
    PointerType *Int8PtrTy;
    PointerType *Int32PtrTy;
    PointerType *Int64PtrTy;

public:
    static char ID;
    explicit ProfileExec() : FunctionPass(ID) {}
    explicit ProfileExec(IRFactory *IF)
        : FunctionPass(ID), IF(IF), DL(IF->getDL()), MF(IF->getMDFactory())
    {
        LLVMContext &Context = IF->getContext();;
        Int8Ty      = IntegerType::get(Context, 8);
        Int32Ty     = IntegerType::get(Context, 32);
        Int64Ty     = IntegerType::get(Context, 64);
        IntPtrTy    = DL->getIntPtrType(Context);
        Int8PtrTy   = Type::getInt8PtrTy(Context, 0);
        Int32PtrTy  = Type::getInt32PtrTy(Context, 0);
        Int64PtrTy  = Type::getInt64PtrTy(Context, 0);
    }
    bool runOnFunction(Function &F);

    Instruction *getInsertPos(BasicBlock *BB) {
        if (BB == &BB->getParent()->getEntryBlock())
            return &*++BB->begin();
        return BB->getFirstNonPHI();
    }
};

char ProfileExec::ID = 0;
INITIALIZE_PASS(ProfileExec, "profile", "Profile trace execution", false, false)

FunctionPass *llvm::createProfileExec(IRFactory *IF) 
{
    return new ProfileExec(IF);
}

bool ProfileExec::runOnFunction(Function &F)
{
    if (!LLEnv->isTraceMode())
        return false;
    if (!SP->isEnabled())
        return false;

    Instruction *CPU = IF->getDefaultCPU(F);
    if (!CPU) {
        dbg() << DEBUG_PASS << PASS_NAME << ": Cannot find CPU pointer.\n";
        return false;
    }

    TraceInfo *Trace = IF->getTrace();

    for (auto FI = F.begin(), FE = F.end(); FI != FE; FI++) {
        BasicBlock *BB = &*FI;
        if (distance(succ_begin(BB), succ_end(BB)) != 0)
            continue;

        /* Find exit points and indirect branches. */
        Trace->NumExit++;
        if (isa<IndirectBrInst>(BB->getTerminator()))
            Trace->NumIndirectBr++;
    }

    /* Insert code to profile trace exit counts. */
    if (SP->Mode & SPM_EXIT) {
        Instruction *InsertPos = &*++BasicBlock::iterator(CPU);
        Value *NumExitPtr = GetElementPtrInst::CreateInBounds(CPU,
                            CONSTPtr(offsetof(CPUArchState, num_trace_exits)),
                            "", InsertPos);
        NumExitPtr = new BitCastInst(NumExitPtr, Int64PtrTy, "", InsertPos);
        Instruction *NumExits = new LoadInst(NumExitPtr, "", true, InsertPos);
        NumExits = BinaryOperator::Create(Instruction::Add, NumExits,
                                          CONST64(1), "", InsertPos);
        new StoreInst(NumExits, NumExitPtr, true, InsertPos);
    }

    if (!(SP->Mode & SPM_TRACE))
        return false;

    SmallVector<CallInst*, 16> InlineCalls;
    Function *Helper = IF->ResolveFunction("helper_profile_exec");

    /* Prepare counter structures. */
    if (!Trace->ExecCount) {
        Trace->ExecCount = new uint64_t *[MAX_SPM_THREADS];
        for (int i = 0; i < MAX_SPM_THREADS; i++)
            Trace->ExecCount[i] = new uint64_t[3] {0, 0, 0};
    }

    /* Find all profiling point. */
    std::vector<std::pair<Instruction *, int> > ProfilePoint;

    SmallVector<std::pair<const BasicBlock*,const BasicBlock*>, 32> BackEdges;
    FindFunctionBackedges(F, BackEdges);
    for (unsigned i = 0, e = BackEdges.size(); i != e; ++i) {
        auto BackEdgeBB = const_cast<BasicBlock*>(BackEdges[i].first);
        ProfilePoint.push_back(std::make_pair(BackEdgeBB->getTerminator(), IDX_LOOP));
    }

    for (auto FI = F.begin(), FE = F.end(); FI != FE; FI++) {
        BasicBlock *BB = &*FI;
        if (distance(succ_begin(BB), succ_end(BB)) != 0)
            continue;
        bool isIndirectBr = isa<IndirectBrInst>(BB->getTerminator());
        ProfilePoint.push_back(std::make_pair(getInsertPos(BB),
                                    isIndirectBr ? IDX_INBR : IDX_EXIT));
    }

    /* Insert profiling routines. */
    for (unsigned i = 0, e = ProfilePoint.size(); i != e; ++i) {
        Instruction *InsertPos = ProfilePoint[i].first;
        Value *Ty = CONST32(ProfilePoint[i].second);

        Value *Counter = ConstantExpr::getIntToPtr(
                            CONSTPtr((uintptr_t)Trace->ExecCount),
                            PointerType::getUnqual(Int8Ty));

        SmallVector<Value *, 4> Params;
        Type *ParamTy = Helper->getFunctionType()->getParamType(0);
        Value *Env = new BitCastInst(CPU, ParamTy, "", InsertPos);
        Params.push_back(Env);
        Params.push_back(Counter);
        Params.push_back(Ty);

        CallInst *CI = CallInst::Create(Helper, Params, "", InsertPos);
        MF->setConst(CI);
        InlineCalls.push_back(CI);
    }

    while (!InlineCalls.empty())
        InlineFunc(InlineCalls.pop_back_val());

    return true;
}

/*
 * vim: ts=8 sts=4 sw=4 expandtab
 */

