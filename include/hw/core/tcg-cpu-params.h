/*
 * Target independent access to TCG CPU-specific parameters
 *
 * Copyright (C) 2024 Anton Johansson <anjo@rev.ng>
 *
 * SPDX-License-Identifier: GPL-2.0-or-later
 */

#ifndef TCG_CPU_PARAMS_H
#define TCG_CPU_PARAMS_H

#include <stdint.h>

struct TCGCPUParams {
    uint32_t virt_addr_space_bits;
    int long_bits;
};

#endif /* TCG_CPU_PARAMS_H */
