#include "effects.h"

namespace rir {
namespace pir {

void printEffects(std::ostream& out, Effects effects) {
    const size_t totalEffs = (size_t)Effect::LAST - (size_t)Effect::FIRST;
    Effects eff;
    if (effects.count() > totalEffs / 2) {
        out << "!";
        eff = ~effects;
    } else {
        eff = effects;
    }
    for (auto it = eff.begin(); it != eff.end(); ++it) {
        Effect effect = *it;
        switch (effect) {
#define CASE(Name, Str)                                                        \
    case Effect::Name:                                                         \
        out << Str;                                                            \
        break;
            CASE(Visibility, "v")
            CASE(Warn, "w")
            CASE(Error, "e")
            CASE(Force, "f")
            CASE(Reflection, "r")
            CASE(LeakArg, "l")
            CASE(ChangesContexts, "C")
            CASE(ReadsEnv, "R")
            CASE(WritesEnv, "W")
            CASE(LeaksEnv, "L")
            CASE(TriggerDeopt, "D")
            CASE(ExecuteCode, "X")
#undef CASE
        default:
            assert(false);
        }
    }
}

} // namespace pir
} // namespace rir
