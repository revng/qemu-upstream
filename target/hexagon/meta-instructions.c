#include "macdesc_s.h"

#define PCALIGN 4
#define PCALIGN_MASK (PCALIGN - 1)
#define fGEN_TCG_ALL

#define DEF_TCG_FUNC(TAG, GENFN) #TAG,GENFN

#include "tcg_funcs_generated.h"
