/*
 *  Copyright(c) 2024 rev.ng Labs Srl. All Rights Reserved.
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, see <http://www.gnu.org/licenses/>.
 */

#ifndef ANNOTATE_H
#define ANNOTATE_H

/* HELPER_TO_TCG can be defined when generating LLVM IR. */
#ifdef HELPER_TO_TCG
#define LLVM_ANNOTATE(str) __attribute__((annotate (str)))
#else
#define LLVM_ANNOTATE(str) /* str */
#endif

#endif /* ANNOTATE_H */
