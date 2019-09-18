#include "reflect.h"
#include "compiler/parameter.h"
#include "runtime/DispatchTable.h"
#include "runtime/Function.h"
#include "utils/Pool.h"
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
    } else if (guard == "retry") {
        return ReflectGuard::Retry;
    } else {
        Rf_error("Invalid RIR_REFLECT_GUARD: %s. Valid options are: off, warn, "
                 "error, retry",
                 guard.c_str());
    }
}

ReflectGuard pir::Parameter::RIR_REFLECT_GUARD =
    getenv("RIR_REFLECT_GUARD") ? (ReflectGuard)parseReflectGuard(
                                      std::string(getenv("RIR_REFLECT_GUARD")))
                                : ReflectGuard::None;

bool canPerformReflection(ReflectGuard guard) {
    return guard != ReflectGuard::Error && guard != ReflectGuard::Retry;
}

inline static SEXP currentEnv() {
    if (R_ExternalEnvStack == NULL)
        return R_BaseEnv;
    else
        return R_ExternalEnvStack->env;
}

inline static ReflectGuard curReflectGuard() {
    return (ReflectGuard)R_GlobalContext->curReflectGuard;
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

// Mark the topmost RIR function non-reflective, and remove all RIR functions in
// the call stack from their dispatch table. Also jumps out of these functions'
// execution
static void taintCallStack() {
    bool taintedTopmost = false;
    RCNTXT* prevRctx;
    RCNTXT* rctx;
    for (prevRctx = NULL, rctx = R_GlobalContext;
         rctx->callflag != CTXT_TOPLEVEL;
         prevRctx = rctx, rctx = rctx->nextcontext) {
        if (Function* fun = (Function*)rctx->rirCallFun) {
            DispatchTable* table = DispatchTable::unpack(BODY(rctx->callfun));
            // TODO: this version is still reachable from static call inline
            // caches (it will not be called though). We could delete the
            // function body to save some space.
            Pool::insert(fun->container());
            table->remove(fun->body());
            if (!taintedTopmost) {
                if (table->reflectGuard == ReflectGuard::Retry) {
                    table->reflectGuard = ReflectGuard::None;
                }
                if (fun->reflectGuard == ReflectGuard::Retry) {
                    taintedTopmost = true;
                }
            }
        }
    }
    assert(taintedTopmost);
    if (prevRctx != NULL)
        R_jumpctxt(prevRctx, 0, R_MissingArg);
}

void willPerformReflection(SEXP env, EnvAccessType typ) {
    switch (curReflectGuard()) {
    case ReflectGuard::None:
        break;
    case ReflectGuard::Warn:
        Rf_warning("closure tried to perform reflection (type %d)", typ);
        break;
    case ReflectGuard::Error:
        Rf_error("closure tried to perform reflection (type %d)", typ);
        break;
    case ReflectGuard::Retry:
        std::cerr << "RIR assertion failure: closure tried to perform "
                     "reflection, marked reflective for next time\n";
        taintCallStack();
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
    if (curReflectGuard() != ReflectGuard::None &&
        !canEnvAccess(currentEnv(), env))
        willPerformReflection(env, typ);
}

std::ostream& operator<<(std::ostream& buf, ReflectGuard guard) {
    switch (guard) {
    case ReflectGuard::None:
        buf << "none";
        break;
    case ReflectGuard::Warn:
        buf << "warn";
        break;
    case ReflectGuard::Error:
        buf << "error";
        break;
    case ReflectGuard::Retry:
        buf << "retry";
        break;
    default:
        assert(false);
        break;
    }
    return buf;
}

} // namespace rir
