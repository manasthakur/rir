#ifndef PIR_PARAMETER_H
#define PIR_PARAMETER_H

#include <stddef.h>

namespace rir {
namespace pir {

struct Parameter {
    static bool DEBUG_DEOPTS;
    static bool DEOPT_CHAOS;
    static bool DEOPT_CHAOS_SEED;
    static size_t MAX_INPUT_SIZE;
    static unsigned RIR_WARMUP;
    // Function must be called 2^n - 1 times before args are safe forced
    static unsigned RIR_SAFE_FORCE_WARMUP;

    static size_t INLINER_MAX_SIZE;
    static size_t INLINER_MAX_INLINEE_SIZE;
    static size_t INLINER_INITIAL_FUEL;
};
} // namespace pir
} // namespace rir

#endif
