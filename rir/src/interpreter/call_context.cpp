#include "call_context.h"

namespace rir {

// Function must be called 2^n - 1 times before args are safe forced
unsigned pir::Parameter::RIR_SAFE_FORCE_WARMUP =
    getenv("RIR_SAFE_FORCE_WARMUP") ? atoi(getenv("RIR_SAFE_FORCE_WARMUP")) : 4;

bool CallContext::shouldSafeForce() {
    assert(sfCounter != nullptr);
    size_t sfCount =
        *sfCounter & (((size_t)1 << pir::Parameter::RIR_SAFE_FORCE_WARMUP) - 1);
    return (sfCount ==
            ((size_t)1 << pir::Parameter::RIR_SAFE_FORCE_WARMUP) - 1);
}

void CallContext::incSafeForceCounter() {
    assert(!shouldSafeForce());
    (*sfCounter)++;
}

} // namespace rir
