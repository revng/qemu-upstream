// TODO(anjo):
//
// Cut down on includes.. There's a lot of history here, I doubt
// everything is still needed. use-what-you-include?

// IR includes
#include "llvm/IR/IRPrintingPasses.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/PassManager.h"
#include "llvm/IR/Verifier.h"

#include "llvm-compat.h"

// misc
#include "llvm/CodeGen/BasicTTIImpl.h"
#include "llvm/IRReader/IRReader.h"
#include "llvm/InitializePasses.h"
#include "llvm/Linker/Linker.h"
#include "llvm/Passes/PassBuilder.h"
#include "llvm/Target/TargetMachine.h"

// Support
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/InitLLVM.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/ToolOutputFile.h"
#include "llvm/Support/WithColor.h"

// Analysis passes
#include "llvm/Analysis/AliasAnalysis.h"
#include "llvm/Analysis/LoopAccessAnalysis.h"
#include "llvm/Analysis/LoopAnalysisManager.h"
#include "llvm/Analysis/TargetTransformInfo.h"

// Transform passes
#include "llvm/Transforms/Scalar/DCE.h"
#include "llvm/Transforms/Scalar/SROA.h"

// Our includes
#include <FilterFunctionsPass.h>
#include <FinalFilterFunctionsPass.h>
#include <IdMapPass.h>
#include <MapAnnotationsPass.h>
#include <MapGEPPass.h>
#include <RemoveNoinlinePass.h>
#include <RemovePhisPass.h>
#include <TcgGlobalMapPass.h>
#include <TcgInstCombinePass.h>
#include <TcgTempAllocationPass.h>
#include <backend/TcgGenPass.h>

// libc++ includes
#include <memory>

using namespace llvm;

static cl::opt<std::string> OutputSourceFile("output-source",
                                             cl::desc("<output .c file>"),
                                             cl::init("llvm-to-tcg-emitted.c"));
static cl::opt<std::string> OutputHeaderFile("output-header",
                                             cl::desc("<output .h file>"),
                                             cl::init("llvm-to-tcg-emitted.h"));
static cl::opt<std::string>
    OutputEnabledFile("output-enabled",
                      cl::desc("<output list of parsed functions>"),
                      cl::init("llvm-to-tcg-enabled"));
static cl::opt<std::string> OutputLogFile("output-log",
                                          cl::desc("<output log file>"),
                                          cl::init("llvm-to-tcg-log"));
static cl::opt<std::string> DebugDir("debug-dir",
                                     cl::desc("<debug output dir.>"),
                                     cl::init("helper-to-tcg"));
static cl::list<std::string>
    InputFiles(cl::Positional, cl::desc("<input file>"), cl::OneOrMore);

// Here we define our own TargetTransformInfo (TTI) subclass, this allows us
// to override common per-target information expected by other LLVM passes, such
// as the width of the largest scalar/vector registers.
//
// As a result, LLVM can vectorize 2048-bit registers and utilize 64-bit scalar
// registers even if the underlying target this tool is being run on don't
// support it.
class LLVMToTcgTTI : public BasicTTIImplBase<LLVMToTcgTTI> {
  friend class BasicTTIImplBase<LLVMToTcgTTI>;

  // We need to provide ST, TLI, getST(), getTLI()
  const TargetSubtargetInfo *ST;
  const TargetLoweringBase *TLI;

  const TargetSubtargetInfo *getST() const { return ST; }
  const TargetLoweringBase *getTLI() const { return TLI; }

public:
  // Initialize ST and TLI from the target machine, e.g. if we're
  // targeting x86 we'll get the Subtarget and TargetLowering to
  // match that architechture.
  LLVMToTcgTTI(TargetMachine *TM, Function const &F)
      : BasicTTIImplBase(TM, F.getParent()->getDataLayout()),
        ST(TM->getSubtargetImpl(F)), TLI(ST->getTargetLowering()) {}

#if LLVM_VERSION_MAJOR >= 13
  TypeSize getRegisterBitWidth(TargetTransformInfo::RegisterKind K) const {
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
      assert(false);
    }
  }
#else
  unsigned getRegisterBitWidth(bool Vector) const {
    if (Vector) {
      return 2048;
    } else {
      return 64;
    }
  }
#endif
};

static std::unique_ptr<ToolOutputFile> outputIRToFile(ModulePassManager &MPM,
                                                      const char *Filename) {
  std::error_code EC;
  auto Out = std::make_unique<ToolOutputFile>(DebugDir + "/" + Filename, EC,
                                              compat::OpenFlags);
  assert(!EC);
  MPM.addPass(PrintModulePass(Out->os()));
  Out->keep();
  return Out;
}

int main(int argc, char **argv) {
  InitLLVM X(argc, argv);

  InitializeAllTargets();
  InitializeAllTargetMCs();

  cl::ParseCommandLineOptions(argc, argv);

  LLVMContext Context;
  SMDiagnostic Err;

  std::unique_ptr<Module> M;
  if (InputFiles.size() > 1) {
    // If we are passed multiple modules, link them together
    M = std::make_unique<Module>("module", Context);
    Linker L(*M);
    for (const auto &File : InputFiles) {
      auto IndividualM = parseIRFile(File, Err, Context);
      if (!IndividualM.get()) {
        errs() << argv[0] << ": " << File << ": ";
        WithColor::error() << "input module broken!\n";
        return 1;
      }

      if (L.linkInModule(std::move(IndividualM))) {
        errs() << argv[0] << ": " << File << ": ";
        WithColor::error() << "failed to link in module!\n";
        return 1;
      }
    }
  } else {
    // Otherwise just parse the single one
    M = parseIRFile(InputFiles[0], Err, Context);
  }

  // Now we want to fetch the TargetMachine from the module,
  // we need this in our TargetTransformInfo class in other
  // "derive" the target machine, this way we don't have to
  // implement lots of functions.

  Triple ModuleTriple(M->getTargetTriple());
  TargetMachine *TM = nullptr;
  if (ModuleTriple.getArch()) {
    TM = compat::getTargetMachine(ModuleTriple);
  } else if (ModuleTriple.getArchName() != "unknown" &&
             ModuleTriple.getArchName() != "") {
    WithColor::error() << argv[0] << ": unrecognized architecture '"
                       << ModuleTriple.getArchName() << "' provided.\n";
    return 1;
  }

  // We start out with loop unrolling disabled, this is to encourage
  // vectorization of loop blocks over loop unrolling.
  //
  // TODO(anjo): We can probably tune this in the vectorizer instead?
  PipelineTuningOptions PTO;
  PTO.LoopUnrolling = true;

  llvm::PassBuilder PB = compat::createPassBuilder(TM, PTO);
  llvm::LoopAnalysisManager LAM;
  llvm::FunctionAnalysisManager FAM;
  llvm::CGSCCAnalysisManager CGAM;
  llvm::ModuleAnalysisManager MAM;

  // Register our TargetIrAnalysis pass using our own TTI
  FAM.registerPass([&] {
    return TargetIRAnalysis(
        [&](const Function &F) { return LLVMToTcgTTI(TM, F); });
  });
  // We need to specifically add the aliasing pipeline for LLVM <= 13
  FAM.registerPass([&] { return PB.buildDefaultAAPipeline(); });

  // Register other default LLVM Analyses
  PB.registerFunctionAnalyses(FAM);
  PB.registerModuleAnalyses(MAM);
  PB.registerLoopAnalyses(LAM);
  PB.registerCGSCCAnalyses(CGAM);
  PB.crossRegisterProxies(LAM, FAM, CGAM, MAM);

  // Register our analyses
  MAM.registerPass([&] { return MapAnnotationsPass(); });
  MAM.registerPass([&] { return TcgGlobalMapPass(); });
  FAM.registerPass([&] { return LoopAnalysis(); });
  FAM.registerPass([&] { return TcgTempAllocationPass(); });
  FAM.registerPass([&] { return CollectGEPIndices(); });
  LAM.registerPass([&] { return LoopAccessAnalysis(); });

  ModulePassManager MPM;

  assert(!sys::fs::create_directory(DebugDir));

  auto OutInput = outputIRToFile(MPM, "1-input.ll");

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
#elif
    FPM.addPass(SROAPass());
#endif
    MPM.addPass(createModuleToFunctionPassAdaptor(std::move(FPM)));
  }

  MPM.addPass(FilterFunctionsPass());
  {
    FunctionPassManager FPM;
    FPM.addPass(RemoveNoinlinePass());
    FPM.addPass(compat::UnifyFunctionExitNodesPass());
    MPM.addPass(createModuleToFunctionPassAdaptor(std::move(FPM)));
  }

  auto OutFilter = outputIRToFile(MPM, "2-post-filter.ll");

  //
  // Run two -O3 passes with and without optimizations, these
  // perform the bulk of the optimization work, including verctorization.
  // The first -O3 pass without unrolling is to prioritize loop vectorization
  // over unrolling for smaller loops. (Scalar vector instructions and such).
  //

  // Optimization passes
  MPM.addPass(PB.buildModuleSimplificationPipeline(
      compat::OptimizationLevel::Os, compat::LTOPhase));
  MPM.addPass(
      PB.buildModuleOptimizationPipeline(compat::OptimizationLevel::Os));

  auto OutSimpPreOp = outputIRToFile(MPM, "3-post-llvm-O3.ll");

  //
  // Next, we run our final transformations, including removing phis and our
  // own instruction combining that prioritizes instructions that map more
  // easily to TCG.
  //

  MPM.addPass(FinalFilterFunctionsPass());

  {
    FunctionPassManager FPM;
    FPM.addPass(RemovePhisPass());
    MPM.addPass(createModuleToFunctionPassAdaptor(std::move(FPM)));
  }
  MPM.addPass(MapGEPPass());
  MPM.addPass(TcgInstCombinePass());
  {
    FunctionPassManager FPM;
    FPM.addPass(DCEPass());
    MPM.addPass(createModuleToFunctionPassAdaptor(std::move(FPM)));
  }
  MPM.addPass(VerifierPass());
  MPM.addPass(IdMapPass());
  {
    FunctionPassManager FPM;
    FPM.addPass(DCEPass());
    MPM.addPass(createModuleToFunctionPassAdaptor(std::move(FPM)));
  }

  auto OutTcgInst = outputIRToFile(MPM, "4-post-tcg-instcombine.ll");

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
                         OutLog.os(), OutputHeaderFile));

  MPM.run(*M, MAM);

  OutSource.keep();
  OutHeader.keep();
  OutEnabled.keep();
  OutLog.keep();

  return 0;
}
