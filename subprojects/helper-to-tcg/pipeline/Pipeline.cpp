//
//  Copyright(c) 2024 rev.ng Labs Srl. All Rights Reserved.
//
//  This program is free software; you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation; either version 2 of the License, or
//  (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program; if not, see <http://www.gnu.org/licenses/>.
//

#include <llvm/ADT/Triple.h>
#include <llvm/Analysis/AliasAnalysis.h>
#include <llvm/Analysis/CGSCCPassManager.h>
#include <llvm/Analysis/LoopAnalysisManager.h>
#include <llvm/Analysis/TargetTransformInfo.h>
#include <llvm/CodeGen/BasicTTIImpl.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/PassManager.h>
#include <llvm/IR/Verifier.h>
#include <llvm/IRReader/IRReader.h>
#include <llvm/InitializePasses.h>
#include <llvm/Linker/Linker.h>
#include <llvm/PassRegistry.h>
#include <llvm/Passes/PassBuilder.h>
#include <llvm/Support/CommandLine.h>
#include <llvm/Support/InitLLVM.h>
#include <llvm/Support/SourceMgr.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Support/ToolOutputFile.h>
#include <llvm/Target/TargetMachine.h>
#include <llvm/Transforms/Scalar/DCE.h>
#include <llvm/Transforms/Scalar/SROA.h>

#include <PrepareForOptPass.h>
#include <PrepareForTcgPass.h>
#include <backend/TcgGenPass.h>
#include <llvm-compat.h>

using namespace llvm;

cl::OptionCategory Cat("helper-to-tcg Options");

// Options for pipeline
cl::opt<std::string> InputFile(cl::Positional, cl::desc("[input LLVM module]"),
                               cl::cat(Cat));

// Options for PrepareForOptPass
cl::opt<bool> TranslateAllHelpers(
    "translate-all-helpers", cl::init(false),
    cl::desc("Translate all functions starting with helper_*"), cl::cat(Cat));

// Options for PrepareForTcgPass
cl::opt<std::string> TcgGlobalMappingsName(
    "tcg-global-mappings",
    cl::desc("Name of global cpu_mappings[] used for mapping accesses"
             "into a struct to TCG globals"),
    cl::init("mappings"), cl::cat(Cat));

// Options for TcgTempAllocation
cl::opt<uint32_t>
    GuestPtrSize("guest-ptr-size",
                 cl::desc("Pointer size of the guest architecture"),
                 cl::init(32), cl::cat(Cat));

// Options for TcgEmit
cl::opt<std::string> MmuIndexFunction(
    "mmu-index-function",
    cl::desc("Name of a (uint32_t tb_flag) -> int function returning the "
             "mmu index from the tb_flags of the current translation block"),
    cl::init("get_tb_mmu_index"), cl::cat(Cat));

cl::opt<std::string>
    TempVectorBlock("temp-vector-block",
                    cl::desc("Name of uint8_t[...] field in CPUArchState used "
                             "for allocating temporary gvec variables"),
                    cl::init("tmp_vmem"), cl::cat(Cat));

// Options for TcgGenPass
cl::opt<std::string> OutputSourceFile("output-source",
                                      cl::desc("output .c file"),
                                      cl::init("helper-to-tcg-emitted.c"),
                                      cl::cat(Cat));

cl::opt<std::string> OutputHeaderFile("output-header",
                                      cl::desc("output .h file"),
                                      cl::init("helper-to-tcg-emitted.h"),
                                      cl::cat(Cat));

cl::opt<std::string>
    OutputEnabledFile("output-enabled",
                      cl::desc("output list of parsed functions"),
                      cl::init("helper-to-tcg-enabled"), cl::cat(Cat));

cl::opt<std::string> OutputLogFile("output-log", cl::desc("output log file"),
                                   cl::init("helper-to-tcg-log"), cl::cat(Cat));

cl::opt<bool>
    ErrorOnTranslationFailure("error-on-translation-failure",
                              cl::desc("Abort translation on first failure"),
                              cl::init(false), cl::cat(Cat));

// Define a TargetTransformInfo (TTI) subclass, this allows for overriding
// common per-llvm-target information expected by other LLVM passes, such
// as the width of the largest scalar/vector registers.  Needed for consistent
// behaviour across different hosts.
class TcgTTI : public BasicTTIImplBase<TcgTTI>
{
    friend class BasicTTIImplBase<TcgTTI>;

    // We need to provide ST, TLI, getST(), getTLI()
    const TargetSubtargetInfo *ST;
    const TargetLoweringBase *TLI;

    const TargetSubtargetInfo *getST() const { return ST; }
    const TargetLoweringBase *getTLI() const { return TLI; }

  public:
    // Initialize ST and TLI from the target machine, e.g. if we're
    // targeting x86 we'll get the Subtarget and TargetLowering to
    // match that architechture.
    TcgTTI(TargetMachine *TM, Function const &F)
        : BasicTTIImplBase(TM, F.getParent()->getDataLayout()),
          ST(TM->getSubtargetImpl(F)), TLI(ST->getTargetLowering())
    {
    }

#if LLVM_VERSION_MAJOR >= 13
    TypeSize getRegisterBitWidth(TargetTransformInfo::RegisterKind K) const
    {
        switch (K) {
        case TargetTransformInfo::RGK_Scalar:
            // We pretend we always support 64-bit registers
            return TypeSize::getFixed(64);
        case TargetTransformInfo::RGK_FixedWidthVector:
            // We pretend we always support 2048-bit vector registers
            return TypeSize::getFixed(2048);
        case TargetTransformInfo::RGK_ScalableVector:
            return TypeSize::getScalable(0);
        default:
            abort();
        }
    }
#else
    unsigned getRegisterBitWidth(bool Vector) const
    {
        if (Vector) {
            return 2048;
        } else {
            return 64;
        }
    }
#endif
};

int main(int argc, char **argv)
{
    InitLLVM X(argc, argv);
    cl::HideUnrelatedOptions(Cat);

    InitializeAllTargets();
    InitializeAllTargetMCs();
    PassRegistry &Registry = *PassRegistry::getPassRegistry();
    initializeCore(Registry);
    initializeScalarOpts(Registry);
    initializeVectorization(Registry);
    initializeAnalysis(Registry);
    initializeTransformUtils(Registry);
    initializeInstCombine(Registry);
    initializeTarget(Registry);

    cl::ParseCommandLineOptions(argc, argv);

    LLVMContext Context;

    SMDiagnostic Err;
    std::unique_ptr<Module> M = parseIRFile(InputFile, Err, Context);

    // Create a new TargetMachine to represent a TCG target,
    // we use x86_64 as a base and derive from that using a
    // TargetTransformInfo to provide allowed scalar and vector
    // register sizes.
    Triple ModuleTriple("x86_64-pc-unknown");
    assert(ModuleTriple.getArch());
    TargetMachine *TM = compat::getTargetMachine(ModuleTriple);

    PipelineTuningOptions PTO;
    PassBuilder PB = compat::createPassBuilder(TM, PTO);
    LoopAnalysisManager LAM;
    FunctionAnalysisManager FAM;
    CGSCCAnalysisManager CGAM;
    ModuleAnalysisManager MAM;

    // Register our TargetIrAnalysis pass using our own TTI
    FAM.registerPass([&] {
        return TargetIRAnalysis(
            [&](const Function &F) { return TcgTTI(TM, F); });
    });
    FAM.registerPass([&] { return LoopAnalysis(); });
    LAM.registerPass([&] { return LoopAccessAnalysis(); });
    // We need to specifically add the aliasing pipeline for LLVM <= 13
    FAM.registerPass([&] { return PB.buildDefaultAAPipeline(); });

    // Register other default LLVM Analyses
    PB.registerFunctionAnalyses(FAM);
    PB.registerModuleAnalyses(MAM);
    PB.registerLoopAnalyses(LAM);
    PB.registerCGSCCAnalyses(CGAM);
    PB.crossRegisterProxies(LAM, FAM, CGAM, MAM);

    ModulePassManager MPM;

    //
    // Start by Filtering out functions we don't want to translate,
    // following by a pass that removes `noinline`s that are inserted
    // by clang on -O0. We finally run a UnifyExitNodesPass to make sure
    // the helpers we parse only has a single exit.
    //

    {
        FunctionPassManager FPM;
#if LLVM_VERSION_MAJOR < 14
        FPM.addPass(SROA());
#else
        FPM.addPass(SROAPass());
#endif
        MPM.addPass(createModuleToFunctionPassAdaptor(std::move(FPM)));
    }

    AnnotationMapTy Annotations;
    MPM.addPass(PrepareForOptPass(Annotations, TranslateAllHelpers));

    {
        FunctionPassManager FPM;
        FPM.addPass(compat::UnifyFunctionExitNodesPass());
        MPM.addPass(createModuleToFunctionPassAdaptor(std::move(FPM)));
    }

    //
    // Run a -Os optimization pass.  In general -Os will prefer loop
    // vectorization over unrolling, as compared to -O3.  In TCG, this
    // translates to more utilization of gvec and possibly smaller TBs.
    //

    // Optimization passes
    MPM.addPass(PB.buildModuleSimplificationPipeline(
        compat::OptimizationLevel::Os, compat::LTOPhase));
    MPM.addPass(
        PB.buildModuleOptimizationPipeline(compat::OptimizationLevel::Os));

    //
    // Next, we run our final transformations, including removing phis and our
    // own instruction combining that prioritizes instructions that map more
    // easily to TCG.
    //

    TcgGlobalMap TcgGlobals;
    MPM.addPass(PrepareForTcgPass(TcgGlobals));
    MPM.addPass(VerifierPass());
    {
        FunctionPassManager FPM;
        FPM.addPass(DCEPass());
        MPM.addPass(createModuleToFunctionPassAdaptor(std::move(FPM)));
    }

    //
    // Finally we run a backend pass that converts from LLVM IR to TCG,
    // and emits the final code.
    //

    std::error_code EC;
    ToolOutputFile OutSource(OutputSourceFile, EC, compat::OpenFlags);
    ToolOutputFile OutHeader(OutputHeaderFile, EC, compat::OpenFlags);
    ToolOutputFile OutEnabled(OutputEnabledFile, EC, compat::OpenFlags);
    ToolOutputFile OutLog(OutputLogFile, EC, compat::OpenFlags);
    assert(!EC);

    MPM.addPass(TcgGenPass(OutSource.os(), OutHeader.os(), OutEnabled.os(),
                           OutLog.os(), OutputHeaderFile, Annotations,
                           TcgGlobals));

    MPM.run(*M, MAM);

    OutSource.keep();
    OutHeader.keep();
    OutEnabled.keep();
    OutLog.keep();

    return 0;
}
