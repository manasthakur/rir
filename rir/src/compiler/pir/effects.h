#pragma once

#include "utils/EnumSet.h"
#include <cstdint>
#include <iostream>

namespace rir {
namespace pir {

// Effect that can be produced by an instruction.
enum class Effect : uint8_t {
    // Changes R_Visible
    Visibility,
    // Instruction might produce a warning. Example: AsTest warns if the
    // vector used in an if condition has length > 1
    Warn,
    // Instruction might produce an error. Example: ForSeqSize raises an
    // error if the collection to loop over is not indexable.
    Error,
    // Instruction might force promises
    Force,
    // Instruction might use reflection
    Reflection,
    // Instruction might leak some of it's arguments
    LeakArg,

    ChangesContexts,
    ReadsEnv,
    WritesEnv,
    LeaksEnv,

    TriggerDeopt,

    // Instruction might execute more R code
    ExecuteCode,

    FIRST = Visibility,
    LAST = ExecuteCode,
};
typedef EnumSet<Effect, unsigned> Effects;

void printEffects(std::ostream& out, Effects effects);

} // namespace pir
} // namespace rir
