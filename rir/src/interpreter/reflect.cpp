#include "reflect.h"
#include "compiler/parameter.h"
#include "utils/capture_out.h"
#include <R/Symbols.h>
#include <iostream>

// #define DEBUG_REFLECT

extern "C" SEXP R_ReplaceFunsTable;

namespace rir {

static ReflectGuard parseReflectGuard(std::string guard) {
    if (guard == "off") {
        return ReflectGuard::None;
    } else if (guard == "warn") {
        return ReflectGuard::Warn;
    } else if (guard == "error") {
        return ReflectGuard::Error;
    } else {
        Rf_error("Invalid RIR_REFLECT_GUARD: %s. Valid options are: off, warn, "
                 "error",
                 guard.c_str());
    }
}

ReflectGuard pir::Parameter::RIR_REFLECT_GUARD =
    getenv("RIR_REFLECT_GUARD") ? (ReflectGuard)parseReflectGuard(
                                      std::string(getenv("RIR_REFLECT_GUARD")))
                                : ReflectGuard::None;

ReflectGuard curReflectGuard = ReflectGuard::None;

inline static SEXP currentEnv() {
    if (R_ExternalEnvStack == NULL)
        return R_BaseEnv;
    else
        return R_ExternalEnvStack->env;
}

inline static bool canEnvAccess(SEXP x, SEXP target) {
    SLOWASSERT(TYPEOF(target) == ENVSXP);
    if (x == R_BaseEnv || x == R_BaseNamespace || target == R_ReplaceFunsTable)
        return true;
    for (SEXP cur = x; cur != R_EmptyEnv; cur = ENCLOS(cur)) {
        if (cur == symbol::delayedEnv)
            cur = R_GlobalEnv;
        SLOWASSERT(TYPEOF(cur) == ENVSXP);
        if (cur == target)
            return true;
    }
    return false;
}

void willPerformReflection(SEXP env, EnvAccessType typ) {
    switch (curReflectGuard) {
    case ReflectGuard::None:
        break;
    case ReflectGuard::Warn:
        Rf_warning("warning: closure tried to perform reflection (type %d)",
                   typ);
        break;
    case ReflectGuard::Error:
        Rf_error("error: closure tried to perform reflection (type %d)", typ);
        break;
    default:
        assert(false);
        break;
    }
}

void willAccessEnv(SEXP env, EnvAccessType typ) {
    if (typ == EnvAccessType::Get)
        return;
    // Fastcase no-guard
    if (curReflectGuard != ReflectGuard::None &&
        !canEnvAccess(currentEnv(), env))
        willPerformReflection(env, typ);
}

} // namespace rir
