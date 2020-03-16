#include "macdesc_s.h"

#define PCALIGN 4
#define PCALIGN_MASK (PCALIGN - 1)
#define fGEN_TCG_ALL

#define DEF_QEMU(TAG, SHORTCODE, HELPER, GENFN, HELPFN) #TAG,GENFN
// #define DEF_QEMU(TAG, SHORTCODE, HELPER, GENFN, HELPFN) DEF_QEMU(TAG, SHORTCODE, HELPER, GENFN, HELPFN)
#include "qemu_def_generated.h"

