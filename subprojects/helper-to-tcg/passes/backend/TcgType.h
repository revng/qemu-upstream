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

#pragma once

#include <llvm/ADT/Optional.h>
#include <llvm/ADT/StringRef.h>

#include <assert.h>
#include <stdint.h>
#include <string>

enum TcgVKind : uint8_t {
    IrValue,
    IrConst,
    IrEnv,
    IrImmediate,
    IrPtr,
    IrPtrToOffset,
    IrLabel,
};

// Counter incremented for every TcgV created, also used in the creation of
// unique names (e.g. varr_10 for an array).
extern uint32_t VarIndex;

struct TcgV {
    uint16_t Id;
    std::string Name;

    uint32_t TcgSize;
    uint8_t LlvmSize;
    uint8_t VectorElementCount;

    TcgVKind Kind;

    bool ConstantExpression = false;

    static TcgV makeVector(uint32_t VectorWidthBits, uint32_t ElementWidthBits,
                           uint32_t ElementCount)
    {
        return TcgV("", VectorWidthBits, ElementWidthBits, ElementCount,
                    IrPtrToOffset);
    }

    static TcgV makeImmediate(llvm::StringRef Name, uint32_t TcgWidth,
                              uint32_t LlvmWidth)
    {
        return TcgV(Name.str(), TcgWidth, LlvmWidth, 1, IrImmediate);
    }

    static TcgV makeTemp(uint32_t TcgWidth, uint32_t LlvmWidth, TcgVKind Kind)
    {
        return TcgV("", TcgWidth, LlvmWidth, 1, Kind);
    }

    static TcgV makeConstantExpression(llvm::StringRef Expression,
                                       uint32_t TcgWidth, uint32_t LlvmWidth,
                                       TcgVKind Kind)
    {
        TcgV Tcg(Expression.str(), TcgWidth, LlvmWidth, 1, Kind);
        Tcg.ConstantExpression = true;
        return Tcg;
    }

    static TcgV makeLabel() { return TcgV("", 32, 32, 1, IrLabel); }

    TcgV(std::string Name, uint32_t TcgSize, uint32_t LlvmSize,
         uint32_t VectorElementCount, TcgVKind Kind)
        : Id(VarIndex++), Name(Name), TcgSize(TcgSize), LlvmSize(LlvmSize),
          VectorElementCount(VectorElementCount), Kind(Kind)
    {
        assert(verifySize());
    }

    // We make the following assumptions about TcgSize and LLvmSize:
    //   - TcgSize either 32- or 64-bit;
    //   - LlvmSize either 1-,8-,16-,32-,64-,or 128-bit.
    // We also assume that there are only these valid combinations of
    // (TcgSize, LlvmSize):
    //   - (64, 64) uint64_t
    //   - (64, 1)  bool
    //   - (32, 32) uint32_t
    //   - (32, 16) uint16_t
    //   - (32, 8)  uint8_t
    //   - (32, 1)  bool
    // So we try to fit the variables in the smallest possible TcgSize,
    // with the exception of booleans which need to able to be 64-bit
    // when dealing with conditions.
    bool verifySize()
    {
        return (LlvmSize == 1 || LlvmSize == 8 || LlvmSize == 16 ||
                LlvmSize == 32 || LlvmSize == 64) &&
               (LlvmSize <= TcgSize);
    }

    bool operator==(const TcgV &Other) const { return Other.Id == Id; }
    bool operator!=(const TcgV &Other) const { return !operator==(Other); }
};

inline uint64_t llvmToTcgSize(uint64_t LlvmSize)
{
    return (LlvmSize <= 32) ? 32 : 64;
}

inline uint32_t vectorSizeInBytes(const TcgV &Vec)
{
    assert(Vec.Kind == IrPtrToOffset);
    return Vec.LlvmSize * Vec.VectorElementCount / 8;
}

struct TcgBinOp {
    std::string Code;
};

struct TcgVecBinOp {
    std::string Code;
    llvm::Optional<uint32_t> RequiredOp2Size;
};
