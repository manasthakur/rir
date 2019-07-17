#include "safe_force.h"
#include "instance.h"
#include "R/RList.h"
#include "R/Sexp.h"
#include "R/Symbols.h"
#include "R/r.h"

#include <assert.h>

namespace rir {

SEXP safeEval(SEXP e, SEXP rho) {
    SEXPTYPE t = TYPEOF(e);
    if (t == LANGSXP || t == SYMSXP || t == PROMSXP || t == BCODESXP ||
        t == EXTERNALSXP) {
        return R_UnboundValue;
    } else {
        // Constant
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

static SEXP promiseEval(SEXP e, SEXP env, InterpreterInstance* ctx,
                        SandboxMode mode) {
// #define DEBUG_EVAL
#ifdef DEBUG_EVAL
    std::cout << "Custom eval of " << TYPEOF(e) << ": ";
    Rf_PrintValue(e);
#endif
    // From GNU-R eval
    static unsigned evalcount = 0;
    R_Visible = (Rboolean) true;
    /* this is needed even for self-evaluating objects or something like
       'while (TRUE) NULL' will not be interruptable */
    if (evalcount++ > 1000) { /* was 100 before 2.8.0 */
        R_CheckUserInterrupt();
#ifndef IMMEDIATE_FINALIZERS
        /* finalizers are run here since this should only be called at
           points where running arbitrary code should be safe */
        R_RunPendingFinalizers();
#endif
        evalcount = 0;
    }
    /* handle self-evaluating objects with minimal overhead */
    switch (TYPEOF(e)) {
    case NILSXP:
    case LISTSXP:
    case LGLSXP:
    case INTSXP:
    case REALSXP:
    case STRSXP:
    case CPLXSXP:
    case RAWSXP:
    case S4SXP:
    case SPECIALSXP:
    case BUILTINSXP:
    case ENVSXP:
    case CLOSXP:
    case VECSXP:
    case EXTPTRSXP:
    case WEAKREFSXP:
    case EXPRSXP:
        /* Make sure constants in expressions are NAMED before being
           used as values.  Setting NAMED to NAMEDMAX makes sure weird calls
           to replacement functions won't modify constants in
           expressions.  */
        ENSURE_NAMEDMAX(e);
        return e;
    default:
        break;
    }
    int bcintactivesave = R_BCIntActive;
    R_BCIntActive = 0;
    SEXP srcrefsave = R_Srcref;
    int depthsave = R_EvalDepth++;
    if (R_EvalDepth > R_Expressions) {
        R_Expressions = R_Expressions_keep + 500;
        errorcall(R_NilValue, "evaluation nested too deeply: infinite "
                              "recursion / options(expressions=)?");
    }
    R_CheckStack();

    // Not from GNU-R
    SEXP res = NULL;
    switch (TYPEOF(e)) {
    case EXTERNALSXP:
        res = rirEval(e, env, mode);
        break;
    default:
        if (mode == SandboxMode::Record)
            ctx->recordUnsafe();
        if (mode == SandboxMode::Sandbox) // Don't actually evaluate
            res = NULL;
        else
            res = Rf_eval(e, env);
        break;
    }

    R_EvalDepth = depthsave;
    R_Srcref = srcrefsave;
    R_BCIntActive = bcintactivesave;
    return res;
}

SEXP rirForcePromise(SEXP e, InterpreterInstance* ctx, SandboxMode mode) {
    // From GNU-R
    if (PRVALUE(e) != R_UnboundValue)
        return PRVALUE(e);
    RPRSTACK prstack;
    SEXP val;
    if (PRSEEN(e)) {
        if (PRSEEN(e) == 1)
            Rf_errorcall(R_GlobalContext->call,
                         "promise already under evaluation: recursive default "
                         "argument reference or earlier problems?");
        else {
            /* set PRSEEN to 1 to avoid infinite recursion */
            SET_PRSEEN(e, 1);
            warningcall(R_GlobalContext->call,
                        "restarting interrupted promise evaluation");
        }
    }
    /* Mark the promise as under evaluation and push it on a stack
       that can be used to unmark pending promises if a jump out
       of the evaluation occurs. */
    SET_PRSEEN(e, 1);
    prstack.promise = e;
    prstack.next = R_PendingPromises;
    R_PendingPromises = &prstack;

    // The original code is eval(PRCODE(e), PRENV(e))
    //
    val = promiseEval(PRCODE(e), PRENV(e), ctx, mode);

    /* Pop the stack, unmark the promise and set its value field.
       Also set the environment to R_NilValue to allow GC to
       reclaim the promise environment; this is also useful for
       fancy games with delayedAssign() */
    R_PendingPromises = prstack.next;
    SET_PRSEEN(e, 0);
    if (val == NULL) {
        assert(mode == SandboxMode::Sandbox);
        return NULL;
    }
    SET_PRVALUE(e, val);
    ENSURE_NAMEDMAX(val);
    SET_PRENV(e, R_NilValue);
    return PRVALUE(e);
}

} // namespace rir
