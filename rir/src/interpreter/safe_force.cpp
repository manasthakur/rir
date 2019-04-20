#include "safe_force.h"
#include "R/RList.h"
#include "R/Sexp.h"
#include "R/Symbols.h"
#include "R/r.h"

#include <assert.h>

namespace rir {

SEXP safeEval(SEXP e, SEXP rho) {
    if (e == R_UnboundValue) {
#ifdef DEBUG_SAFE_EVAL
        std::cout << "safe eval failed on unbound\n";
#endif
        return R_UnboundValue;
    } else if (e == R_MissingArg) {
#ifdef DEBUG_SAFE_EVAL
        std::cout << "safe eval missing\n";
#endif
        return e;
    }
    SEXPTYPE t = TYPEOF(e);
    if (t == LANGSXP || t == BCODESXP || t == EXTERNALSXP) {
#ifdef DEBUG_SAFE_EVAL
        std::cout << "safe eval failed: ";
        Rf_PrintValue(e);
#endif
        return R_UnboundValue;
    } else if (t == PROMSXP) {
#ifdef DEBUG_SAFE_EVAL
        std::cout << "safe eval promise -> ";
#endif
        return safeForcePromise(e);
    } else if (t == SYMSXP) {
        if (rho == nullptr)
            return R_UnboundValue;
#ifdef DEBUG_SAFE_EVAL
        std::cout << "safe eval lookup " << CHAR(PRINTNAME(e));
#endif
        SEXP f = Rf_findVar(e, rho);
        if (f == R_UnboundValue) {
#ifdef DEBUG_SAFE_EVAL
            std::cout << " failed\n";
#endif
            return R_UnboundValue;
        }
#ifdef DEBUG_SAFE_EVAL
        std::cout << " -> ";
#endif
        return safeEval(f, rho);
    } else {
        // Constant
#ifdef DEBUG_SAFE_EVAL
        std::cout << "safe eval constant: ";
        Rf_PrintValue(e);
#endif
        return e;
    }
}

SEXP safeForcePromise(SEXP e) {
    if (PRVALUE(e) == R_UnboundValue) {
        SEXP val = safeEval(PRCODE(e), PRENV(e));
        if (val != R_UnboundValue) {
            SET_PRVALUE(e, val);
            ENSURE_NAMEDMAX(val);
            SET_PRENV(e, R_NilValue);
        }
        return val;
    } else {
        return PRVALUE(e);
    }
}

} // namespace rir
