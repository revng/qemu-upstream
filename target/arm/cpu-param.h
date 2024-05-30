/*
 * ARM cpu parameters for qemu.
 *
 * Copyright (c) 2003 Fabrice Bellard
 * SPDX-License-Identifier: LGPL-2.0+
 */

#ifndef ARM_CPU_PARAM_H
#define ARM_CPU_PARAM_H

#ifdef TARGET_AARCH64
# define TARGET_LONG_BITS             64
# define TARGET_PHYS_ADDR_SPACE_BITS  52
# define TARGET_VIRT_ADDR_SPACE_BITS  52
#else
# define TARGET_LONG_BITS             32
# define TARGET_PHYS_ADDR_SPACE_BITS  40
# define TARGET_VIRT_ADDR_SPACE_BITS  32
#endif

#define TARGET_PAGE_BITS_VARY
#ifdef CONFIG_USER_ONLY
# ifdef TARGET_AARCH64
#  define TARGET_TAGGED_ADDRESSES
/* Allow user-only to vary page size from 4k */
#  define TARGET_PAGE_BITS_VARY_NEW_TEMP
# endif
# define TARGET_PAGE_BITS_MIN 12
#else /* !CONFIG_USER_ONLY */
/*
 * ARMv7 and later CPUs have 4K pages minimum, but ARMv5 and v6
 * have to support 1K tiny pages.
 */
# define TARGET_PAGE_BITS_VARY_NEW_TEMP
# define TARGET_PAGE_BITS_MIN  10
#endif /* !CONFIG_USER_ONLY */

/* ARM processors have a weak memory model */
#define TCG_GUEST_DEFAULT_MO      (0)

#endif
