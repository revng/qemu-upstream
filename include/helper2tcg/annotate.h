#ifndef ANNOTATE_H
#define ANNOTATE_H

#ifdef TCGG
#define LLVM_ANNOTATE(str) __attribute__((annotate (str)))
#else
#define LLVM_ANNOTATE(str) /* Empty */
#endif

#endif /* ANNOTATE_H */
