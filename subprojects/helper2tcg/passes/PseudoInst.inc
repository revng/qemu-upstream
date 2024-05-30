// Identity mapping
PSEUDO_INST_DEF(IdentityMap,        ArgInt, PSEUDO_INST_ARGVEC(ArgInt)),
// Pointer arithmetic
PSEUDO_INST_DEF(PtrAdd,             ArgPtr, PSEUDO_INST_ARGVEC(ArgPtr, ArgInt)),
// Global accesses
PSEUDO_INST_DEF(AccessGlobalArray,  ArgInt, PSEUDO_INST_ARGVEC(ArgInt)),
PSEUDO_INST_DEF(AccessGlobalValue,  ArgInt, PSEUDO_INST_ARGVEC(ArgInt)),
// Conditional branch
PSEUDO_INST_DEF(Brcond,             ArgVoid, PSEUDO_INST_ARGVEC(ArgInt, ArgInt, ArgInt, ArgLabel, ArgLabel)),
// Conditional move
PSEUDO_INST_DEF(Movcond,            ArgInt, PSEUDO_INST_ARGVEC(ArgInt, ArgInt, ArgInt, ArgInt, ArgInt)),
// Vector creation ops
PSEUDO_INST_DEF(VecSplat,           ArgVec, PSEUDO_INST_ARGVEC(ArgInt)),
// Vector unary ops
PSEUDO_INST_DEF(VecNot,             ArgVec, PSEUDO_INST_ARGVEC(ArgVec)),
// Vector scalar binary ops
PSEUDO_INST_DEF(VecAddScalar,       ArgVec, PSEUDO_INST_ARGVEC(ArgVec, ArgInt)),
PSEUDO_INST_DEF(VecSubScalar,       ArgVec, PSEUDO_INST_ARGVEC(ArgVec, ArgInt)),
PSEUDO_INST_DEF(VecMulScalar,       ArgVec, PSEUDO_INST_ARGVEC(ArgVec, ArgInt)),
PSEUDO_INST_DEF(VecXorScalar,       ArgVec, PSEUDO_INST_ARGVEC(ArgVec, ArgInt)),
PSEUDO_INST_DEF(VecOrScalar,        ArgVec, PSEUDO_INST_ARGVEC(ArgVec, ArgInt)),
PSEUDO_INST_DEF(VecAndScalar,       ArgVec, PSEUDO_INST_ARGVEC(ArgVec, ArgInt)),
PSEUDO_INST_DEF(VecShlScalar,       ArgVec, PSEUDO_INST_ARGVEC(ArgVec, ArgInt)),
PSEUDO_INST_DEF(VecLShrScalar,      ArgVec, PSEUDO_INST_ARGVEC(ArgVec, ArgInt)),
PSEUDO_INST_DEF(VecAShrScalar,      ArgVec, PSEUDO_INST_ARGVEC(ArgVec, ArgInt)),
// Vector unary ops that stores to pointer
PSEUDO_INST_DEF(VecNotStore,        ArgVoid, PSEUDO_INST_ARGVEC(ArgPtr, ArgVec)),
// Vector binary ops that stores to pointer
PSEUDO_INST_DEF(VecAddStore,        ArgVoid, PSEUDO_INST_ARGVEC(ArgPtr, ArgVec, ArgVec)),
PSEUDO_INST_DEF(VecSubStore,        ArgVoid, PSEUDO_INST_ARGVEC(ArgPtr, ArgVec, ArgVec)),
PSEUDO_INST_DEF(VecMulStore,        ArgVoid, PSEUDO_INST_ARGVEC(ArgPtr, ArgVec, ArgVec)),
PSEUDO_INST_DEF(VecXorStore,        ArgVoid, PSEUDO_INST_ARGVEC(ArgPtr, ArgVec, ArgVec)),
PSEUDO_INST_DEF(VecOrStore,         ArgVoid, PSEUDO_INST_ARGVEC(ArgPtr, ArgVec, ArgVec)),
PSEUDO_INST_DEF(VecAndStore,        ArgVoid, PSEUDO_INST_ARGVEC(ArgPtr, ArgVec, ArgVec)),
PSEUDO_INST_DEF(VecShlStore,        ArgVoid, PSEUDO_INST_ARGVEC(ArgPtr, ArgVec, ArgVec)),
PSEUDO_INST_DEF(VecLShrStore,       ArgVoid, PSEUDO_INST_ARGVEC(ArgPtr, ArgVec, ArgVec)),
PSEUDO_INST_DEF(VecAShrStore,       ArgVoid, PSEUDO_INST_ARGVEC(ArgPtr, ArgVec, ArgVec)),
PSEUDO_INST_DEF(VecAddScalarStore,  ArgVoid, PSEUDO_INST_ARGVEC(ArgPtr, ArgVec, ArgInt)),
PSEUDO_INST_DEF(VecSubScalarStore,  ArgVoid, PSEUDO_INST_ARGVEC(ArgPtr, ArgVec, ArgInt)),
PSEUDO_INST_DEF(VecMulScalarStore,  ArgVoid, PSEUDO_INST_ARGVEC(ArgPtr, ArgVec, ArgInt)),
PSEUDO_INST_DEF(VecXorScalarStore,  ArgVoid, PSEUDO_INST_ARGVEC(ArgPtr, ArgVec, ArgInt)),
PSEUDO_INST_DEF(VecOrScalarStore,   ArgVoid, PSEUDO_INST_ARGVEC(ArgPtr, ArgVec, ArgInt)),
PSEUDO_INST_DEF(VecAndScalarStore,  ArgVoid, PSEUDO_INST_ARGVEC(ArgPtr, ArgVec, ArgInt)),
PSEUDO_INST_DEF(VecShlScalarStore,  ArgVoid, PSEUDO_INST_ARGVEC(ArgPtr, ArgVec, ArgInt)),
PSEUDO_INST_DEF(VecLShrScalarStore, ArgVoid, PSEUDO_INST_ARGVEC(ArgPtr, ArgVec, ArgInt)),
PSEUDO_INST_DEF(VecAShrScalarStore, ArgVoid, PSEUDO_INST_ARGVEC(ArgPtr, ArgVec, ArgInt)),
// Host memory operations
//                                                     vaddr,  value   sign    size    endian
PSEUDO_INST_DEF(HostLoad,  ArgInt,  PSEUDO_INST_ARGVEC(ArgInt,         ArgInt, ArgInt, ArgInt)),
PSEUDO_INST_DEF(HostStore, ArgVoid, PSEUDO_INST_ARGVEC(ArgInt, ArgInt,         ArgInt, ArgInt)),
// ...
PSEUDO_INST_DEF(VecTruncStore,        ArgVec, PSEUDO_INST_ARGVEC(ArgPtr, ArgVec)),
PSEUDO_INST_DEF(VecSignedSatAddStore, ArgVec, PSEUDO_INST_ARGVEC(ArgPtr, ArgVec, ArgVec)),
PSEUDO_INST_DEF(VecSignedSatSubStore, ArgVec, PSEUDO_INST_ARGVEC(ArgPtr, ArgVec, ArgVec)),
PSEUDO_INST_DEF(VecSelectStore,       ArgVec, PSEUDO_INST_ARGVEC(ArgPtr, ArgVec, ArgVec, ArgVec)),
PSEUDO_INST_DEF(VecFunnelShrStore,    ArgVec, PSEUDO_INST_ARGVEC(ArgPtr, ArgVec, ArgVec, ArgVec)),
PSEUDO_INST_DEF(VecAbsStore,          ArgVec, PSEUDO_INST_ARGVEC(ArgPtr, ArgVec)),
PSEUDO_INST_DEF(VecSignedMaxStore,    ArgVec, PSEUDO_INST_ARGVEC(ArgPtr, ArgVec, ArgVec)),
PSEUDO_INST_DEF(VecUnsignedMaxStore,  ArgVec, PSEUDO_INST_ARGVEC(ArgPtr, ArgVec, ArgVec)),
PSEUDO_INST_DEF(VecSignedMinStore,    ArgVec, PSEUDO_INST_ARGVEC(ArgPtr, ArgVec, ArgVec)),
PSEUDO_INST_DEF(VecUnsignedMinStore,  ArgVec, PSEUDO_INST_ARGVEC(ArgPtr, ArgVec, ArgVec)),
PSEUDO_INST_DEF(VecCtlzStore,         ArgVec, PSEUDO_INST_ARGVEC(ArgPtr, ArgVec)),
PSEUDO_INST_DEF(VecCttzStore,         ArgVec, PSEUDO_INST_ARGVEC(ArgPtr, ArgVec)),
PSEUDO_INST_DEF(VecCtpopStore,        ArgVec, PSEUDO_INST_ARGVEC(ArgPtr, ArgVec)),

PSEUDO_INST_DEF(SignExtract,          ArgInt, PSEUDO_INST_ARGVEC(ArgInt, ArgInt, ArgInt)),
PSEUDO_INST_DEF(Extract,              ArgInt, PSEUDO_INST_ARGVEC(ArgInt, ArgInt, ArgInt)),
