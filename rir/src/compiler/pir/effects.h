#pragma once

#include "utils/EnumSet.h"
#include <iostream>

namespace rir {
namespace pir {

// Effect that can be produced by an instruction.
enum class Effect : uint8_t {
    // Immediately breaks sandbox (warning: might not cover all cases, currently
    // just used for speculation)
    NotSandboxable,
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

    // If we speculatively optimize an instruction then we must set this flag
    // to avoid it getting hoisted over its assumption. Take care when removing
    // or masking this flag. Most of the time it is not correct to remove it,
    // e.g. the type of inputs to an instructions might already be based on
    // assumptions.
    DependsOnAssume,

    FIRST = NotSandboxable,
    LAST = DependsOnAssume,
};
typedef EnumSet<Effect, unsigned> Effects;

static const Effects errorEffects =
    Effects(Effect::Visibility) | Effect::Warn | Effect::Error;

void printEffects(std::ostream& out, Effects effects);

} // namespace pir
} // namespace rir
