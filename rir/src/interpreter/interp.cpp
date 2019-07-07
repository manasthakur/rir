#include "interp.h"
#include "ArgsLazyData.h"
#include "LazyEnvironment.h"
#include "R/Funtab.h"
#include "R/RList.h"
#include "R/Symbols.h"
#include "cache.h"
#include "compiler/parameter.h"
#include "compiler/translations/rir_2_pir/rir_2_pir_compiler.h"
#include "event_counters.h"
#include "ir/Deoptimization.h"
#include "runtime/TypeFeedback_inl.h"
#include "safe_force.h"
#include "utils/Pool.h"

#include <assert.h>
#include <deque>
#include <set>

#define NOT_IMPLEMENTED assert(false)

#undef eval

extern "C" {
extern SEXP Rf_NewEnvironment(SEXP, SEXP, SEXP);
}

namespace rir {

#define PRINT_INTERP 0
#if PRINT_INTERP
static void printInterp(Opcode* pc, Code* c) {
    BC bc = BC::decode(pc, c);
    std::cout << "#";
    bc.print(std::cout);
}

static void printLastop() { std::cout << "> lastop\n"; }
#endif

static RIR_INLINE SEXP getSrcAt(Code* c, Opcode* pc, InterpreterInstance* ctx) {
    unsigned sidx = c->getSrcIdxAt(pc, true);
    if (sidx == 0)
        return src_pool_at(ctx, c->src);
    return src_pool_at(ctx, sidx);
}

static RIR_INLINE SEXP getSrcForCall(Code* c, Opcode* pc,
                                     InterpreterInstance* ctx) {
    unsigned sidx = c->getSrcIdxAt(pc, false);
    return src_pool_at(ctx, sidx);
}

#define PC_BOUNDSCHECK(pc, c)                                                  \
    SLOWASSERT((pc) >= (c)->code() && (pc) < (c)->endCode());

#define HANDLE_SANDBOX()                                                       \
    ctx->recordPure(BC::isPure(*pc)); /* usually noop, else does 1 set */

#ifdef THREADED_CODE
#define BEGIN_MACHINE NEXT();
#define INSTRUCTION(name)                                                      \
    op_##name: /* debug(c, pc, #name, ostack_length(ctx) - bp, ctx); */
#if PRINT_INTERP
#define NEXT()                                                                 \
    (__extension__({                                                           \
        printInterp(pc, c);                                                    \
        HANDLE_SANDBOX();                                                      \
        goto* opAddr[static_cast<uint8_t>(advanceOpcode())];                   \
    }))
#define LASTOP                                                                 \
    { printLastop(); }
#else
#define NEXT()                                                                 \
    (__extension__({                                                           \
        HANDLE_SANDBOX();                                                      \
        goto* opAddr[static_cast<uint8_t>(advanceOpcode())];                   \
    }))
#define LASTOP                                                                 \
    {}
#endif
#else
#define BEGIN_MACHINE                                                          \
    HANDLE_SANDBOX();                                                          \
    loop:                                                                      \
    switch (advanceOpcode())
#define INSTRUCTION(name)                                                      \
    case Opcode::name:                                                         \
        /* debug(c, pc, #name, ostack_length(ctx) - bp, ctx); */
#define NEXT()                                                                 \
    HANDLE_SANDBOX();                                                          \
    goto loop
#define LASTOP                                                                 \
    default:                                                                   \
        assert(false && "wrong or unimplemented opcode")
#endif

// bytecode accesses
#define advanceOpcode() (*(pc++))
#define readImmediate() (*(Immediate*)pc)
#define readSignedImmediate() (*(SignedImmediate*)pc)
#define readJumpOffset() (*(JumpOffset*)(pc))
#define advanceImmediate() pc += sizeof(Immediate)
#define advanceImmediateN(n) pc += n * sizeof(Immediate)
#define advanceJump() pc += sizeof(JumpOffset)

#define readConst(ctx, idx) (cp_pool_at(ctx, idx))

void initClosureContext(SEXP ast, RCNTXT* cntxt, SEXP rho, SEXP sysparent,
                        SEXP arglist, SEXP op) {
    /*  If we have a generic function we need to use the sysparent of
       the generic as the sysparent of the method because the method
       is a straight substitution of the generic.  */

    if (R_GlobalContext->callflag == CTXT_GENERIC)
        Rf_begincontext(cntxt, CTXT_RETURN, ast, rho,
                        R_GlobalContext->sysparent, arglist, op);
    else
        Rf_begincontext(cntxt, CTXT_RETURN, ast, rho, sysparent, arglist, op);
}

static void endClosureContext(RCNTXT* cntxt, SEXP result) {
    cntxt->returnValue = result;
    Rf_endcontext(cntxt);
}

static RIR_INLINE SEXP createPromise(Code* code, SEXP env) {
    SEXP p = Rf_mkPROMISE(code->container(), env);
    return p;
}

static RIR_INLINE SEXP promiseValue(SEXP promise, InterpreterInstance* ctx,
                                    bool sandboxed = false) {
    // if already evaluated, return the value
    if (PRVALUE(promise) && PRVALUE(promise) != R_UnboundValue) {
        promise = PRVALUE(promise);
        assert(TYPEOF(promise) != PROMSXP);
        return promise;
    } else {
        SEXP res = rirForcePromise(promise, ctx, sandboxed);
        assert(res != NULL || sandboxed);
        assert((res == NULL || TYPEOF(res) != PROMSXP) &&
               "promise returned promise");
        return res;
    }
}

static void jit(SEXP cls, SEXP name, InterpreterInstance* ctx) {
    assert(TYPEOF(cls) == CLOSXP);
    if (TYPEOF(BODY(cls)) == EXTERNALSXP)
        return;
    SEXP cmp = ctx->closureCompiler(cls, name);
    SET_BODY(cls, BODY(cmp));
}

static void closureDebug(SEXP call, SEXP op, SEXP rho, SEXP newrho,
                         RCNTXT* cntxt) {
    // TODO!!!
}

static void endClosureDebug(SEXP call, SEXP op, SEXP rho) {
    // TODO!!!
}

/** Given argument code offsets, creates the argslist from their promises.
 */
// TODO unnamed only at this point
static RIR_INLINE void __listAppend(SEXP* front, SEXP* last, SEXP value,
                                    SEXP name) {
    SLOWASSERT(TYPEOF(*front) == LISTSXP || TYPEOF(*front) == NILSXP);
    SLOWASSERT(TYPEOF(*last) == LISTSXP || TYPEOF(*last) == NILSXP);

    SEXP app = CONS_NR(value, R_NilValue);

    SET_TAG(app, name);

    if (*front == R_NilValue) {
        *front = app;
        PROTECT(*front);
    }

    if (*last != R_NilValue)
        SETCDR(*last, app);
    *last = app;
}

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wcast-align"

SEXP createEnvironment(InterpreterInstance* ctx, SEXP wrapper_) {
    auto wrapper = LazyEnvironment::unpack(wrapper_);

    SEXP arglist = R_NilValue;
    auto names = wrapper->names;
    for (size_t i = 0; i < wrapper->nargs; ++i) {
        SEXP val = wrapper->getArg(i);
        SEXP name = cp_pool_at(ctx, names[i]);
        arglist = CONS_NR(val, arglist);
        SET_TAG(arglist, name);
        SET_MISSING(arglist, val == R_MissingArg ? 2 : 0);
    }

    SEXP environment =
        Rf_NewEnvironment(R_NilValue, arglist, wrapper->getParent());
    auto finger = R_BCNodeStackTop;
    while (finger > wrapper->frameEnd) {
        if (finger->tag == 0 && finger->u.sxpval == wrapper_)
            finger->u.sxpval = environment;
        finger--;
    }
    return environment;
}

static SEXP materializeCallerEnv(CallContext& callCtx,
                                 InterpreterInstance* ctx) {
    if (LazyEnvironment::check(callCtx.callerEnv))
        callCtx.callerEnv = createEnvironment(ctx, callCtx.callerEnv);
    SLOWASSERT(callCtx.callerEnv == symbol::delayedEnv ||
               TYPEOF(callCtx.callerEnv) == ENVSXP);
    return callCtx.callerEnv;
}

SEXP createLegacyArgsListFromStackValues(CallContext& call, bool eagerCallee,
                                         InterpreterInstance* ctx) {
    SEXP result = R_NilValue;
    SEXP pos = result;

    for (size_t i = 0; i < call.suppliedArgs; ++i) {

        SEXP name = call.hasNames() ? call.name(i, ctx) : R_NilValue;

        SEXP arg = call.stackArg(i);

        if (eagerCallee && TYPEOF(arg) == PROMSXP) {
            arg = Rf_eval(arg, materializeCallerEnv(call, ctx));
        }
        __listAppend(&result, &pos, arg, name);
    }

    if (result != R_NilValue)
        UNPROTECT(1);

    return result;
}

static SEXP createLegacyArgsList(CallContext& call, bool eagerCallee,
                                 InterpreterInstance* ctx) {
    SEXP result = R_NilValue;
    SEXP pos = result;

    // loop through the arguments and create a promise, unless it is a missing
    // argument
    for (size_t i = 0; i < call.suppliedArgs; ++i) {
        unsigned argi = call.implicitArgIdx(i);
        SEXP name = call.hasNames() ? call.name(i, ctx) : R_NilValue;

        // if the argument is an ellipsis, then retrieve it from the environment
        // and
        // flatten the ellipsis
        if (argi == DOTS_ARG_IDX) {
            SEXP ellipsis =
                Rf_findVar(R_DotsSymbol, materializeCallerEnv(call, ctx));
            if (TYPEOF(ellipsis) == DOTSXP) {
                while (ellipsis != R_NilValue) {
                    name = TAG(ellipsis);
                    if (eagerCallee) {
                        SEXP arg = CAR(ellipsis);
                        if (arg != R_MissingArg)
                            arg = Rf_eval(CAR(ellipsis),
                                          materializeCallerEnv(call, ctx));
                        assert(TYPEOF(arg) != PROMSXP);
                        __listAppend(&result, &pos, arg, name);
                    } else {
                        SEXP promise = Rf_mkPROMISE(
                            CAR(ellipsis), materializeCallerEnv(call, ctx));
                        __listAppend(&result, &pos, promise, name);
                    }
                    ellipsis = CDR(ellipsis);
                }
            }
        } else if (argi == MISSING_ARG_IDX) {
            if (eagerCallee)
                Rf_errorcall(call.ast, "argument %d is empty", i + 1);
            __listAppend(&result, &pos, R_MissingArg, R_NilValue);
        } else {
            if (eagerCallee) {
                SEXP arg = evalRirCodeExtCaller(
                    call.implicitArg(i), ctx, materializeCallerEnv(call, ctx));
                assert(TYPEOF(arg) != PROMSXP);
                __listAppend(&result, &pos, arg, name);
            } else {
                Code* arg = call.implicitArg(i);
                SEXP promise =
                    createPromise(arg, materializeCallerEnv(call, ctx));
                __listAppend(&result, &pos, promise, name);
            }
        }
    }

    if (result != R_NilValue)
        UNPROTECT(1);
    return result;
}

SEXP materialize(void* rirDataWrapper) {
    if (auto promargs = ArgsLazyData::cast(rirDataWrapper)) {
        return promargs->createArgsLists();
    } else if (LazyEnvironment::check((SEXP)rirDataWrapper)) {
        return createEnvironment(globalContext(), (SEXP)rirDataWrapper);
    }
    assert(false);
    return nullptr;
}

SEXP* keepAliveSEXPs(void* rirDataWrapper) {
    assert(false);
    return nullptr;
}

SEXP lazyPromargsCreation(void* rirDataWrapper) {
    return ArgsLazyData::cast(rirDataWrapper)->createArgsLists();
}

static RIR_INLINE SEXP createLegacyLazyArgsList(CallContext& call,
                                                InterpreterInstance* ctx) {
    if (call.hasStackArgs()) {
        return createLegacyArgsListFromStackValues(call, false, ctx);
    } else {
        return createLegacyArgsList(call, false, ctx);
    }
}

static RIR_INLINE SEXP createLegacyArgsList(CallContext& call,
                                            InterpreterInstance* ctx) {
    if (call.hasStackArgs()) {
        return createLegacyArgsListFromStackValues(call, call.hasEagerCallee(),
                                                   ctx);
    } else {
        return createLegacyArgsList(call, call.hasEagerCallee(), ctx);
    }
}

SEXP evalRirCode(Code*, InterpreterInstance*, SEXP, const CallContext*, Opcode*,
                 R_bcstack_t*, BindingCache*);
SEXP evalRirCodeSandboxed(Code* c, InterpreterInstance* ctx, SEXP env);

static SEXP rirCallTrampoline_(RCNTXT& cntxt, const CallContext& call,
                               Code* code, SEXP env, InterpreterInstance* ctx) {
    if ((SETJMP(cntxt.cjmpbuf))) {
        if (R_ReturnedValue == R_RestartToken) {
            cntxt.callflag = CTXT_RETURN; /* turn restart off */
            R_ReturnedValue = R_NilValue; /* remove restart token */
            return evalRirCode(code, ctx, cntxt.cloenv, &call);
        } else {
            return R_ReturnedValue;
        }
    }
    return evalRirCode(code, ctx, env, &call);
}

static RIR_INLINE SEXP rirCallTrampoline(const CallContext& call, Function* fun,
                                         SEXP env, SEXP arglist,
                                         InterpreterInstance* ctx) {
    assert(TYPEOF(env) == ENVSXP ||
           fun->signature().envCreation ==
               FunctionSignature::Environment::CalleeCreated);

    RCNTXT cntxt;

    // This code needs to be protected, because its slot in the dispatch table
    // could get overwritten while we are executing it.
    PROTECT(fun->container());

    initClosureContext(call.ast, &cntxt, env, call.callerEnv, arglist,
                       call.callee);
    R_Srcref = getAttrib(call.callee, symbol::srcref);

    closureDebug(call.ast, call.callee, env, R_NilValue, &cntxt);

    // Warning: call.popArgs() between initClosureContext and trampoline will
    // result in broken stack on non-local returns.

    Code* code = fun->body();
    // Pass &cntxt.cloenv, to let evalRirCode update the env of the current
    // context
    SEXP result = rirCallTrampoline_(cntxt, call, code, env, ctx);
    PROTECT(result);

    endClosureDebug(call.ast, call.callee, env);
    endClosureContext(&cntxt, result);

    R_Srcref = cntxt.srcref;
    R_ReturnedValue = R_NilValue;

    UNPROTECT(2);
    return result;
}

static RIR_INLINE SEXP rirCallTrampoline(const CallContext& call, Function* fun,
                                         SEXP arglist,
                                         InterpreterInstance* ctx) {
    return rirCallTrampoline(call, fun, symbol::delayedEnv, arglist, ctx);
}

#define UI_COUNT_DELTA 1000

static unsigned int count = 0;

// Interrupt Signal Checker - Allows for Ctrl - C functionality to exit out
// of infinite loops
void checkUserInterrupt() {
    if (++count > UI_COUNT_DELTA) {
        R_CheckUserInterrupt();
        R_RunPendingFinalizers();
        count = 0;
    }
}

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-variable"
const static SEXP loopTrampolineMarker = (SEXP)0x7007;
static void loopTrampoline(Code* c, InterpreterInstance* ctx, SEXP env,
                           const CallContext* callCtxt, Opcode* pc,
                           R_bcstack_t* localsBase, BindingCache* cache) {
    assert(env);

    RCNTXT cntxt;
    Rf_begincontext(&cntxt, CTXT_LOOP, R_NilValue, env, R_BaseEnv, R_NilValue,
                    R_NilValue);

    if (int s = SETJMP(cntxt.cjmpbuf)) {
        // incoming non-local break/continue:
        if (s == CTXT_BREAK) {
            Rf_endcontext(&cntxt);
            return;
        }
        // continue case: fall through to do another iteration
    }

    // execute the loop body
    SEXP res = evalRirCode(c, ctx, env, callCtxt, pc, localsBase, cache);
    assert(res == loopTrampolineMarker);
    Rf_endcontext(&cntxt);
}
#pragma GCC diagnostic pop

static SEXP inlineContextTrampoline(Code* c, const CallContext* callCtx,
                                    SEXP ast, SEXP sysparent, SEXP op,
                                    InterpreterInstance* ctx, Opcode* pc,
                                    R_bcstack_t* localsBase,
                                    BindingCache* cache) {
    RCNTXT cntxt;
    // The first env should be the callee env, but that will be done by the
    // callee. We store sysparent there, because our optimizer may actually
    // delay instructions into the inlinee and might assume that we still have
    // to outer env.
    initClosureContext(ast, &cntxt, symbol::delayedEnv, sysparent,
                       symbol::delayedArglist, op);
    auto trampoline = [&]() {
        if ((SETJMP(cntxt.cjmpbuf))) {
            if (R_ReturnedValue == R_RestartToken) {
                cntxt.callflag = CTXT_RETURN; /* turn restart off */
                R_ReturnedValue = R_NilValue; /* remove restart token */
                return evalRirCode(c, ctx, cntxt.cloenv, callCtx, pc, nullptr,
                                   nullptr);
            } else {
                return R_ReturnedValue;
            }
        }
        return evalRirCode(c, ctx, sysparent, callCtx, pc, localsBase, cache);
    };

    // execute the inlined function
    auto res = trampoline();
    endClosureContext(&cntxt, res);
    return res;
}

static RIR_INLINE SEXP legacySpecialCall(CallContext& call,
                                         InterpreterInstance* ctx) {
    assert(call.ast != R_NilValue);

    // get the ccode
    CCODE f = getBuiltin(call.callee);
    int flag = getFlag(call.callee);
    R_Visible = static_cast<Rboolean>(flag != 1);
    // call it with the AST only
    SEXP result = f(call.ast, call.callee, CDR(call.ast),
                    materializeCallerEnv(call, ctx));
    if (flag < 2)
        R_Visible = static_cast<Rboolean>(flag != 1);
    return result;
}

static RIR_INLINE SEXP legacyCallWithArgslist(CallContext& call, SEXP argslist,
                                              InterpreterInstance* ctx) {
    if (TYPEOF(call.callee) == BUILTINSXP) {
        // get the ccode
        CCODE f = getBuiltin(call.callee);
        int flag = getFlag(call.callee);
        if (flag < 2)
            R_Visible = static_cast<Rboolean>(flag != 1);
        // call it
        SEXP result =
            f(call.ast, call.callee, argslist, materializeCallerEnv(call, ctx));
        if (flag < 2)
            R_Visible = static_cast<Rboolean>(flag != 1);
        return result;
    }

    assert(TYPEOF(call.callee) == CLOSXP &&
           TYPEOF(BODY(call.callee)) != EXTERNALSXP);
    return Rf_applyClosure(call.ast, call.callee, argslist,
                           materializeCallerEnv(call, ctx), R_NilValue);
}

static RIR_INLINE SEXP legacyCall(CallContext& call, InterpreterInstance* ctx) {
    // create the argslist
    SEXP argslist = createLegacyArgsList(call, ctx);
    PROTECT(argslist);
    SEXP res = legacyCallWithArgslist(call, argslist, ctx);
    UNPROTECT(1);
    return res;
}

static SEXP closureArgumentAdaptor(const CallContext& call, SEXP arglist,
                                   SEXP suppliedvars) {
    SEXP op = call.callee;
    if (FORMALS(op) == R_NilValue && arglist == R_NilValue)
        return Rf_NewEnvironment(R_NilValue, R_NilValue, CLOENV(op));

    /*  Set up a context with the call in it so error has access to it */
    RCNTXT cntxt;
    initClosureContext(call.ast, &cntxt, CLOENV(op), call.callerEnv, arglist,
                       op);

    /*  Build a list which matches the actual (unevaluated) arguments
        to the formal paramters.  Build a new environment which
        contains the matched pairs.  Ideally this environment sould be
        hashed.  */
    SEXP newrho, a, f;

    SEXP actuals = Rf_matchArgs(FORMALS(op), arglist, call.ast);
    PROTECT(newrho = Rf_NewEnvironment(FORMALS(op), actuals, CLOENV(op)));

    /* Turn on reference counting for the binding cells so local
       assignments arguments increment REFCNT values */
    for (a = actuals; a != R_NilValue; a = CDR(a))
        ENABLE_REFCNT(a);

    /*  Use the default code for unbound formals.  FIXME: It looks like
        this code should preceed the building of the environment so that
        this will also go into the hash table.  */

    /* This piece of code is destructively modifying the actuals list,
       which is now also the list of bindings in the frame of newrho.
       This is one place where internal structure of environment
       bindings leaks out of envir.c.  It should be rewritten
       eventually so as not to break encapsulation of the internal
       environment layout.  We can live with it for now since it only
       happens immediately after the environment creation.  LT */

    f = FORMALS(op);
    a = actuals;
    // get the first Code that is a compiled default value of a formal arg
    // (or nullptr if no such exist)
    Function* fun = DispatchTable::unpack(BODY(op))->baseline();
    size_t pos = 0;
    while (f != R_NilValue) {
        Code* c = fun->defaultArg(pos++);
        if (CAR(f) != R_MissingArg) {
            if (CAR(a) == R_MissingArg) {
                assert(c != nullptr && "No more compiled formals available.");
                SETCAR(a, createPromise(c, newrho));
                SET_MISSING(a, 2);
            }
            // Either just used the compiled formal or it was not needed.
            // Skip over current compiled formal and find the next default arg.
        }
        assert(CAR(f) != R_DotsSymbol || TYPEOF(CAR(a)) == DOTSXP);
        f = CDR(f);
        a = CDR(a);
    }

    /*  Fix up any extras that were supplied by usemethod. */

    if (suppliedvars != R_NilValue)
        Rf_addMissingVarsToNewEnv(newrho, suppliedvars);

    if (R_envHasNoSpecialSymbols(newrho))
        SET_NO_SPECIAL_SYMBOLS(newrho);

    endClosureContext(&cntxt, R_NilValue);

    UNPROTECT(1);

    return newrho;
};

static SEXP findRootPromise(SEXP p) {
    if (TYPEOF(p) == PROMSXP) {
        while (TYPEOF(PREXPR(p)) == PROMSXP) {
            p = PREXPR(p);
        }
    }
    return p;
}

static void addDynamicAssumptionsFromContext(CallContext& call) {
    Assumptions& given = call.givenAssumptions;

    if (!call.hasNames())
        given.add(Assumption::CorrectOrderOfArguments);

    given.add(Assumption::NoExplicitlyMissingArgs);
    if (call.hasStackArgs()) {
        // Always true in this case, since we will pad missing args on the stack
        // later with R_MissingArg's
        given.add(Assumption::NotTooFewArguments);

        auto testArg = [&](size_t i) {
            SEXP arg = call.stackArg(i);
            bool notObj = true;
            bool isEager = true;
            if (TYPEOF(arg) == PROMSXP) {
                arg = PRVALUE(arg);
                if (arg == R_UnboundValue) {
                    notObj = false;
                    isEager = false;
                }
            }
            if (arg == R_MissingArg) {
                given.remove(Assumption::NoExplicitlyMissingArgs);
                isEager = false;
            }
            if (isObject(arg)) {
                notObj = false;
            }
            if (isEager)
                given.setEager(i);
            if (notObj)
                given.setNotObj(i);
            if (isEager && notObj && IS_SIMPLE_SCALAR(arg, REALSXP))
                given.setSimpleReal(i);
            if (isEager && notObj && IS_SIMPLE_SCALAR(arg, INTSXP))
                given.setSimpleInt(i);
        };

        for (size_t i = 0; i < call.suppliedArgs; ++i) {
            testArg(i);
        }
    } else {
        for (size_t i = 0; i < call.suppliedArgs; ++i) {
            if (call.missingArg(i))
                given.remove(Assumption::NoExplicitlyMissingArgs);
        }
    }
}

static RIR_INLINE Assumptions addDynamicAssumptionsForOneTarget(
    const CallContext& call, const FunctionSignature& signature) {
    Assumptions given = call.givenAssumptions;

    if (call.suppliedArgs <= signature.formalNargs()) {
        given.numMissing(signature.formalNargs() - call.suppliedArgs);
    }

    if (!call.hasStackArgs()) {
        if (call.suppliedArgs >= signature.expectedNargs())
            given.add(Assumption::NotTooFewArguments);
    }

    if (call.suppliedArgs <= signature.formalNargs())
        given.add(Assumption::NotTooManyArguments);

    return given;
}

static RIR_INLINE bool matches(const CallContext& call,
                               const FunctionSignature& signature) {
    // TODO: look at the arguments of the function signature and not just at the
    // global assumptions list. This only becomes relevant as soon as we want to
    // optimize based on argument types.

    // Baseline always matches!
    if (signature.optimization ==
        FunctionSignature::OptimizationLevel::Baseline) {
#ifdef DEBUG_DISPATCH
        std::cout << "BL\n";
#endif
        return true;
    }

    assert(signature.envCreation ==
           FunctionSignature::Environment::CalleeCreated);

    if (!call.hasStackArgs()) {
        // We can't materialize ... in optimized code yet
        for (size_t i = 0; i < call.suppliedArgs; ++i)
            if (call.implicitArgIdx(i) == DOTS_ARG_IDX)
                return false;
    }

    Assumptions given = addDynamicAssumptionsForOneTarget(call, signature);

#ifdef DEBUG_DISPATCH
    std::cout << "have   " << given << "\n";
    std::cout << "trying " << signature.assumptions << "\n";
    std::cout << " -> " << signature.assumptions.subtype(given) << "\n";
#endif
    // Check if given assumptions match required assumptions
    return signature.assumptions.subtype(given);
}

// Watch out: this changes call.nargs! To clean up after the call, you need to
// pop call.nargs number of arguments (which now might be more than the number
// of actually supplied arguments).
static RIR_INLINE void supplyMissingArgs(CallContext& call,
                                         const Function* fun) {
    auto signature = fun->signature();
    assert(call.hasStackArgs());
    if (signature.expectedNargs() > call.suppliedArgs) {
        for (size_t i = 0; i < signature.expectedNargs() - call.suppliedArgs;
             ++i)
            ostack_push(ctx, R_MissingArg);
        call.passedArgs = signature.expectedNargs();
    }
}

static Function* dispatch(const CallContext& call, DispatchTable* vt) {
    // Find the most specific version of the function that can be called given
    // the current call context.
    Function* fun = nullptr;
    for (int i = vt->size() - 1; i >= 0; i--) {
        auto candidate = vt->get(i);
        if (matches(call, candidate->signature())) {
            fun = candidate;
            break;
        }
    }
    assert(fun);

    return fun;
};

unsigned pir::Parameter::RIR_WARMUP =
    getenv("PIR_WARMUP") ? atoi(getenv("PIR_WARMUP")) : 3;

static unsigned serializeCounter = 0;

// Call a RIR function. Arguments are still untouched.
RIR_INLINE SEXP rirCall(CallContext& call, InterpreterInstance* ctx) {
    SEXP body = BODY(call.callee);
    bool bodyPreserved = false;
    if (pir::Parameter::RIR_SERIALIZE_CHAOS) {
        serializeCounter++;
        if (serializeCounter == pir::Parameter::RIR_SERIALIZE_CHAOS) {
            body = copyBySerial(body);
            PROTECT(body);
            bodyPreserved = true;
            serializeCounter = 0;
        }
    }
    assert(DispatchTable::check(body));

    auto table = DispatchTable::unpack(body);

    addDynamicAssumptionsFromContext(call);
    Function* fun = dispatch(call, table);
    fun->registerInvocation();

    if (!fun->unoptimizable &&
        fun->invocationCount() % pir::Parameter::RIR_WARMUP == 0) {
        Assumptions given =
            addDynamicAssumptionsForOneTarget(call, fun->signature());
        // addDynamicAssumptionForOneTarget compares arguments with the
        // signature of the current dispatch target. There the number of
        // arguments might be off. But we want to force compiling a new version
        // exactly for this number of arguments, thus we need to add this as an
        // explicit assumption.
        given.add(Assumption::NotTooFewArguments);
        if (fun == table->baseline() || given != fun->signature().assumptions) {
            if (Assumptions(given).includes(
                    pir::Rir2PirCompiler::minimalAssumptions)) {
                // More assumptions are available than this version uses. Let's
                // try compile a better matching version.
#ifdef DEBUG_DISPATCH
                std::cout << "Optimizing for new context:";
                std::cout << given << " vs " << fun->signature().assumptions
                          << "\n";
#endif
                SEXP lhs = CAR(call.ast);
                SEXP name = R_NilValue;
                if (TYPEOF(lhs) == SYMSXP)
                    name = lhs;
                ctx->closureOptimizer(call.callee, given, name);
                fun = dispatch(call, table);
            }
        }
    }

    bool needsEnv = fun->signature().envCreation ==
                    FunctionSignature::Environment::CallerProvided;
    SEXP result = nullptr;
    auto arglist = call.arglist;
    if (needsEnv) {
        if (!arglist)
            arglist = createLegacyLazyArgsList(call, ctx);
        PROTECT(arglist);
        SEXP env = closureArgumentAdaptor(call, arglist, R_NilValue);
        PROTECT(env);
        result = rirCallTrampoline(call, fun, env, arglist, ctx);
        UNPROTECT(2);
    } else {
        if (call.hasStackArgs()) {
            // Instead of a SEXP with the argslist we create an
            // structure with the information needed to recreate
            // the list lazily if the gnu-r interpreter needs it
            ArgsLazyData lazyArgs(&call, ctx);
            if (!arglist)
                arglist = (SEXP)&lazyArgs;
            supplyMissingArgs(call, fun);
            result = rirCallTrampoline(call, fun, arglist, ctx);
        } else {
            if (!arglist)
                arglist = createLegacyArgsList(call, ctx);
            PROTECT(arglist);
            result = rirCallTrampoline(call, fun, arglist, ctx);
            UNPROTECT(1);
        }
    }

    if (bodyPreserved)
        UNPROTECT(1);

    assert(result);

    assert(!fun->deopt);
    return result;
}

#ifdef DEBUG_SLOWCASES

class SlowcaseCounter {
  public:
    std::unordered_map<std::string, size_t> counter;

    void count(const std::string& kind, CallContext& call,
               InterpreterInstance* ctx) {
        std::stringstream message;
        message << "Fast case " << kind << " failed for "
                << getBuiltinName(getBuiltinNr(call.callee)) << " ("
                << getBuiltinNr(call.callee) << ") "
                << "nargs : " << call.suppliedArgs;
        if (call.suppliedArgs > 0) {
            auto arg = call.stackArg(0);
            if (TYPEOF(arg) == PROMSXP)
                arg = safeForcePromise(arg);
            if (arg == R_UnboundValue)
                message << "arg0 lazy";
            else if (arg == R_MissingArg)
                message << "arg0 missing";
            else
                message << " arg0 : " << type2char(TYPEOF(arg)) << " a "
                        << (ATTRIB(arg) != R_NilValue);
        }
        if (!counter.count(message.str()))
            counter[message.str()] = 0;
        counter[message.str()]++;
    }

    static constexpr size_t TRESHOLD = 100;
    ~SlowcaseCounter() {
        std::map<size_t, std::set<std::string>> order;
        for (auto& e : counter)
            if (e.second > TRESHOLD)
                order[e.second].insert(e.first);
        for (auto& o : order) {
            for (auto& e : o.second) {
                std::cout << o.first << " times: " << e << "\n";
            }
        }
    }
};
SlowcaseCounter SLOWCASE_COUNTER;
#endif

static RIR_INLINE SEXP builtinCall(CallContext& call,
                                   InterpreterInstance* ctx) {
    if (call.hasStackArgs() && !call.hasNames()) {
        SEXP res = tryFastBuiltinCall(call, ctx);
        if (res)
            return res;
#ifdef DEBUG_SLOWCASES
        SLOWCASE_COUNTER.count("builtin", call, ctx);
#endif
    }
    return legacyCall(call, ctx);
}

static RIR_INLINE SEXP specialCall(CallContext& call,
                                   InterpreterInstance* ctx) {
    if (call.hasStackArgs() && !call.hasNames()) {
        SEXP res = tryFastSpecialCall(call, ctx);
        if (res)
            return res;
#ifdef DEBUG_SLOWCASES
        SLOWCASE_COUNTER.count("special", call, ctx);
#endif
    }
    return legacySpecialCall(call, ctx);
}

static SEXP doCall(CallContext& call, InterpreterInstance* ctx) {
    assert(call.callee);

    switch (TYPEOF(call.callee)) {
    case SPECIALSXP:
        return specialCall(call, ctx);
    case BUILTINSXP:
        return builtinCall(call, ctx);
    case CLOSXP: {
        if (TYPEOF(BODY(call.callee)) != EXTERNALSXP)
            return legacyCall(call, ctx);
        return rirCall(call, ctx);
    }
    default:
        Rf_error("Invalid Callee");
        break;
    };
    return R_NilValue;
}

static SEXP dispatchApply(SEXP ast, SEXP obj, SEXP actuals, SEXP selector,
                          SEXP callerEnv, InterpreterInstance* ctx) {
    SEXP op = SYMVALUE(selector);

    // ===============================================
    // First try S4
    if (IS_S4_OBJECT(obj) && R_has_methods(op)) {
        SEXP result = R_possible_dispatch(ast, op, actuals, callerEnv, TRUE);
        if (result)
            return result;
    }

    // ===============================================
    // Then try S3
    const char* generic = CHAR(PRINTNAME(selector));
    SEXP rho1 = Rf_NewEnvironment(R_NilValue, R_NilValue, callerEnv);
    PROTECT(rho1);
    RCNTXT cntxt;
    initClosureContext(ast, &cntxt, rho1, callerEnv, actuals, op);
    SEXP result;
    bool success = Rf_usemethod(generic, obj, ast, actuals, rho1, callerEnv,
                                R_BaseEnv, &result);
    UNPROTECT(1);
    endClosureContext(&cntxt, success ? result : R_NilValue);
    if (success)
        return result;

    return nullptr;
}

#define R_INT_MAX INT_MAX
#define R_INT_MIN -INT_MAX
// .. relying on fact that NA_INTEGER is outside of these

static R_INLINE int R_integer_plus(int x, int y, Rboolean* pnaflag) {
    if (x == NA_INTEGER || y == NA_INTEGER)
        return NA_INTEGER;

    if (((y > 0) && (x > (R_INT_MAX - y))) ||
        ((y < 0) && (x < (R_INT_MIN - y)))) {
        if (pnaflag != NULL)
            *pnaflag = TRUE;
        return NA_INTEGER;
    }
    return x + y;
}

static R_INLINE int R_integer_minus(int x, int y, Rboolean* pnaflag) {
    if (x == NA_INTEGER || y == NA_INTEGER)
        return NA_INTEGER;

    if (((y < 0) && (x > (R_INT_MAX + y))) ||
        ((y > 0) && (x < (R_INT_MIN + y)))) {
        if (pnaflag != NULL)
            *pnaflag = TRUE;
        return NA_INTEGER;
    }
    return x - y;
}

#define GOODIPROD(x, y, z) ((double)(x) * (double)(y) == (z))
static R_INLINE int R_integer_times(int x, int y, Rboolean* pnaflag) {
    if (x == NA_INTEGER || y == NA_INTEGER)
        return NA_INTEGER;
    else {
        int z = x * y;
        if (GOODIPROD(x, y, z) && z != NA_INTEGER)
            return z;
        else {
            if (pnaflag != NULL)
                *pnaflag = TRUE;
            return NA_INTEGER;
        }
    }
}

enum op { PLUSOP, MINUSOP, TIMESOP, DIVOP, POWOP, MODOP, IDIVOP };
#define INTEGER_OVERFLOW_WARNING "NAs produced by integer overflow"

static SEXPREC createFakeSEXP(SEXPTYPE t) {
    SEXPREC res;
    res.attrib = R_NilValue;
    res.gengc_next_node = R_NilValue;
    res.gengc_prev_node = R_NilValue;
    res.sxpinfo.gcgen = 1;
    res.sxpinfo.mark = 1;
    res.sxpinfo.named = 2;
    res.sxpinfo.type = t;
    return res;
}

static SEXPREC createFakeCONS(SEXP cdr) {
    auto res = createFakeSEXP(LISTSXP);
    res.u.listsxp.carval = R_NilValue;
    res.u.listsxp.tagval = R_NilValue;
    res.u.listsxp.cdrval = cdr;
    return res;
}

#define CHECK_INTEGER_OVERFLOW(ans, naflag)                                    \
    do {                                                                       \
        if (naflag) {                                                          \
            PROTECT(ans);                                                      \
            SEXP call = getSrcForCall(c, pc - 1, ctx);                         \
            Rf_warningcall(call, INTEGER_OVERFLOW_WARNING);                    \
            UNPROTECT(1);                                                      \
        }                                                                      \
    } while (0)

#define BINOP_FALLBACK(op)                                                     \
    do {                                                                       \
        static SEXP prim = NULL;                                               \
        static CCODE blt;                                                      \
        static int flag;                                                       \
        if (!prim) {                                                           \
            prim = Rf_findFun(Rf_install(op), R_GlobalEnv);                    \
            blt = getBuiltin(prim);                                            \
            flag = getFlag(prim);                                              \
        }                                                                      \
                                                                               \
        if (flag < 2)                                                          \
            R_Visible = static_cast<Rboolean>(flag != 1);                      \
        SEXP call = getSrcForCall(c, pc - 1, ctx);                             \
                                                                               \
        if (!env || !(isObject(lhs) || isObject(rhs))) {                       \
            SEXPREC arglist2 = createFakeCONS(R_NilValue);                     \
            SEXPREC arglist = createFakeCONS(&arglist2);                       \
            arglist.u.listsxp.carval = lhs;                                    \
            arglist2.u.listsxp.carval = rhs;                                   \
            res = blt(call, prim, &arglist, env);                              \
        } else {                                                               \
            SEXP arglist = CONS_NR(lhs, CONS_NR(rhs, R_NilValue));             \
            ostack_push(ctx, arglist);                                         \
            res = blt(call, prim, arglist, env);                               \
            ostack_pop(ctx);                                                   \
        }                                                                      \
                                                                               \
        if (flag < 2)                                                          \
            R_Visible = static_cast<Rboolean>(flag != 1);                      \
    } while (false)

#define DO_FAST_BINOP(op, op2)                                                 \
    do {                                                                       \
        if (IS_SIMPLE_SCALAR(lhs, REALSXP)) {                                  \
            if (IS_SIMPLE_SCALAR(rhs, REALSXP)) {                              \
                res_type = REALSXP;                                            \
                real_res = (*REAL(lhs) == NA_REAL || *REAL(rhs) == NA_REAL)    \
                               ? NA_REAL                                       \
                               : *REAL(lhs) op * REAL(rhs);                    \
            } else if (IS_SIMPLE_SCALAR(rhs, INTSXP)) {                        \
                res_type = REALSXP;                                            \
                real_res =                                                     \
                    (*REAL(lhs) == NA_REAL || *INTEGER(rhs) == NA_INTEGER)     \
                        ? NA_REAL                                              \
                        : *REAL(lhs) op * INTEGER(rhs);                        \
            }                                                                  \
        } else if (IS_SIMPLE_SCALAR(lhs, INTSXP)) {                            \
            if (IS_SIMPLE_SCALAR(rhs, INTSXP)) {                               \
                Rboolean naflag = FALSE;                                       \
                switch (op2) {                                                 \
                case PLUSOP:                                                   \
                    int_res =                                                  \
                        R_integer_plus(*INTEGER(lhs), *INTEGER(rhs), &naflag); \
                    break;                                                     \
                case MINUSOP:                                                  \
                    int_res = R_integer_minus(*INTEGER(lhs), *INTEGER(rhs),    \
                                              &naflag);                        \
                    break;                                                     \
                case TIMESOP:                                                  \
                    int_res = R_integer_times(*INTEGER(lhs), *INTEGER(rhs),    \
                                              &naflag);                        \
                    break;                                                     \
                }                                                              \
                res_type = INTSXP;                                             \
                CHECK_INTEGER_OVERFLOW(R_NilValue, naflag);                    \
            } else if (IS_SIMPLE_SCALAR(rhs, REALSXP)) {                       \
                res_type = REALSXP;                                            \
                real_res =                                                     \
                    (*INTEGER(lhs) == NA_INTEGER || *REAL(rhs) == NA_REAL)     \
                        ? NA_REAL                                              \
                        : *INTEGER(lhs) op * REAL(rhs);                        \
            }                                                                  \
        }                                                                      \
    } while (false)

#define STORE_BINOP(res_type, int_res, real_res)                               \
    do {                                                                       \
        SEXP a = ostack_at(ctx, 0);                                            \
        SEXP b = ostack_at(ctx, 1);                                            \
        if (NO_REFERENCES(a)) {                                                \
            TYPEOF(a) = res_type;                                              \
            res = a;                                                           \
            ostack_pop(ctx);                                                   \
            ostack_at(ctx, 0) = a;                                             \
        } else if (NO_REFERENCES(b)) {                                         \
            TYPEOF(b) = res_type;                                              \
            res = b;                                                           \
            ostack_pop(ctx);                                                   \
        } else {                                                               \
            ostack_pop(ctx);                                                   \
            ostack_at(ctx, 0) = res = Rf_allocVector(res_type, 1);             \
        }                                                                      \
        switch (res_type) {                                                    \
        case INTSXP:                                                           \
            INTEGER(res)[0] = int_res;                                         \
            break;                                                             \
        case REALSXP:                                                          \
            REAL(res)[0] = real_res;                                           \
            break;                                                             \
        }                                                                      \
    } while (false)

#define DO_BINOP(op, op2)                                                      \
    do {                                                                       \
        int int_res = -1;                                                      \
        double real_res = -2.0;                                                \
        int res_type = 0;                                                      \
        DO_FAST_BINOP(op, op2);                                                \
        if (res_type) {                                                        \
            STORE_BINOP(res_type, int_res, real_res);                          \
            R_Visible = (Rboolean) true;                                       \
        } else {                                                               \
            BINOP_FALLBACK(#op);                                               \
            ostack_pop(ctx);                                                   \
            ostack_set(ctx, 0, res);                                           \
        }                                                                      \
    } while (false)

static double myfloor(double x1, double x2) {
    double q = x1 / x2, tmp;

    if (x2 == 0.0)
        return q;
    tmp = x1 - floor(q) * x2;
    return floor(q) + floor(tmp / x2);
}

static double myfmod(InterpreterInstance* ctx, double x1, double x2) {
    if (x2 == 0.0)
        return R_NaN;
    double q = x1 / x2, tmp = x1 - floor(q) * x2;
    if (R_FINITE(q) && (fabs(q) > 1 / R_AccuracyInfo.eps))
        Rf_warning("probable complete loss of accuracy in modulus");
    q = floor(tmp / x2);
    return tmp - q * x2;
}

static R_INLINE int R_integer_uplus(int x, Rboolean* pnaflag) {
    if (x == NA_INTEGER)
        return NA_INTEGER;

    return x;
}

static R_INLINE int R_integer_uminus(int x, Rboolean* pnaflag) {
    if (x == NA_INTEGER)
        return NA_INTEGER;

    return -x;
}

#define UNOP_FALLBACK(op)                                                      \
    do {                                                                       \
        static SEXP prim = NULL;                                               \
        static CCODE blt;                                                      \
        static int flag;                                                       \
        if (!prim) {                                                           \
            prim = Rf_findFun(Rf_install(op), R_GlobalEnv);                    \
            blt = getBuiltin(prim);                                            \
            flag = getFlag(prim);                                              \
        }                                                                      \
        SEXP call = getSrcForCall(c, pc - 1, ctx);                             \
        SEXP argslist = CONS_NR(val, R_NilValue);                              \
        ostack_push(ctx, argslist);                                            \
        if (flag < 2)                                                          \
            R_Visible = static_cast<Rboolean>(flag != 1);                      \
        res = blt(call, prim, argslist, env);                                  \
        if (flag < 2)                                                          \
            R_Visible = static_cast<Rboolean>(flag != 1);                      \
        ostack_pop(ctx);                                                       \
    } while (false)

#define DO_UNOP(op, op2)                                                       \
    do {                                                                       \
        if (IS_SIMPLE_SCALAR(val, REALSXP)) {                                  \
            res = Rf_allocVector(REALSXP, 1);                                  \
            *REAL(res) = (*REAL(val) == NA_REAL) ? NA_REAL : op * REAL(val);   \
            R_Visible = (Rboolean) true;                                       \
        } else if (IS_SIMPLE_SCALAR(val, INTSXP)) {                            \
            Rboolean naflag = FALSE;                                           \
            res = Rf_allocVector(INTSXP, 1);                                   \
            switch (op2) {                                                     \
            case PLUSOP:                                                       \
                *INTEGER(res) = R_integer_uplus(*INTEGER(val), &naflag);       \
                break;                                                         \
            case MINUSOP:                                                      \
                *INTEGER(res) = R_integer_uminus(*INTEGER(val), &naflag);      \
                break;                                                         \
            }                                                                  \
            CHECK_INTEGER_OVERFLOW(res, naflag);                               \
            R_Visible = (Rboolean) true;                                       \
        } else {                                                               \
            UNOP_FALLBACK(#op);                                                \
        }                                                                      \
        ostack_set(ctx, 0, res);                                               \
    } while (false)

#define DO_RELOP(op)                                                           \
    do {                                                                       \
        if (IS_SIMPLE_SCALAR(lhs, LGLSXP)) {                                   \
            if (IS_SIMPLE_SCALAR(rhs, LGLSXP)) {                               \
                if (*LOGICAL(lhs) == NA_LOGICAL ||                             \
                    *LOGICAL(rhs) == NA_LOGICAL) {                             \
                    res = R_LogicalNAValue;                                    \
                } else {                                                       \
                    res = *LOGICAL(lhs) op * LOGICAL(rhs) ? R_TrueValue        \
                                                          : R_FalseValue;      \
                }                                                              \
                break;                                                         \
            }                                                                  \
        } else if (IS_SIMPLE_SCALAR(lhs, REALSXP)) {                           \
            if (IS_SIMPLE_SCALAR(rhs, REALSXP)) {                              \
                if (*REAL(lhs) == NA_REAL || *REAL(rhs) == NA_REAL) {          \
                    res = R_LogicalNAValue;                                    \
                } else {                                                       \
                    res = *REAL(lhs) op * REAL(rhs) ? R_TrueValue              \
                                                    : R_FalseValue;            \
                }                                                              \
                break;                                                         \
            } else if (IS_SIMPLE_SCALAR(rhs, INTSXP)) {                        \
                if (*REAL(lhs) == NA_REAL || *INTEGER(rhs) == NA_INTEGER) {    \
                    res = R_LogicalNAValue;                                    \
                } else {                                                       \
                    res = *REAL(lhs) op * INTEGER(rhs) ? R_TrueValue           \
                                                       : R_FalseValue;         \
                }                                                              \
                break;                                                         \
            }                                                                  \
        } else if (IS_SIMPLE_SCALAR(lhs, INTSXP)) {                            \
            if (IS_SIMPLE_SCALAR(rhs, INTSXP)) {                               \
                if (*INTEGER(lhs) == NA_INTEGER ||                             \
                    *INTEGER(rhs) == NA_INTEGER) {                             \
                    res = R_LogicalNAValue;                                    \
                } else {                                                       \
                    res = *INTEGER(lhs) op * INTEGER(rhs) ? R_TrueValue        \
                                                          : R_FalseValue;      \
                }                                                              \
                break;                                                         \
            } else if (IS_SIMPLE_SCALAR(rhs, REALSXP)) {                       \
                if (*INTEGER(lhs) == NA_INTEGER || *REAL(rhs) == NA_REAL) {    \
                    res = R_LogicalNAValue;                                    \
                } else {                                                       \
                    res = *INTEGER(lhs) op * REAL(rhs) ? R_TrueValue           \
                                                       : R_FalseValue;         \
                }                                                              \
                break;                                                         \
            }                                                                  \
        }                                                                      \
        BINOP_FALLBACK(#op);                                                   \
    } while (false)

static SEXP seq_int(int n1, int n2) {
    int n = n1 <= n2 ? n2 - n1 + 1 : n1 - n2 + 1;
    SEXP ans = Rf_allocVector(INTSXP, n);
    int* data = INTEGER(ans);
    if (n1 <= n2) {
        while (n1 <= n2)
            *data++ = n1++;
    } else {
        while (n1 >= n2)
            *data++ = n1--;
    }
    return ans;
}

RIR_INLINE static void castInt(bool ceil_, Code* c, Opcode* pc,
                               InterpreterInstance* ctx) {
    SEXP val = ostack_top(ctx);
    // Scalar integers (already done)
    if (IS_SIMPLE_SCALAR(val, INTSXP) && *INTEGER(val) != NA_INTEGER) {
        return;
    } else if (IS_SIMPLE_SCALAR(val, REALSXP) && NO_REFERENCES(val) &&
               !ISNAN(*REAL(val))) {
        double r = *REAL(val);
        TYPEOF(val) = INTSXP;
        *INTEGER(val) = (int)(ceil_ ? ceil(r) : floor(r));
        return;
    } else if (IS_SIMPLE_SCALAR(val, LGLSXP) && NO_REFERENCES(val) &&
               *LOGICAL(val) != NA_LOGICAL) {
        TYPEOF(val) = INTSXP;
        return;
    }
    int x = -20;
    bool isNaOrNan = false;
    if (TYPEOF(val) == INTSXP || TYPEOF(val) == REALSXP ||
        TYPEOF(val) == LGLSXP) {
        if (XLENGTH(val) == 0) {
            Rf_errorcall(getSrcAt(c, pc - 1, ctx), "argument of length 0");
            x = NA_INTEGER;
            isNaOrNan = false;
        } else {
            switch (TYPEOF(val)) {
            case INTSXP: {
                int i = *INTEGER(val);
                if (i == NA_INTEGER) {
                    x = NA_INTEGER;
                    isNaOrNan = true;
                } else {
                    x = i;
                    isNaOrNan = false;
                }
                break;
            }
            case REALSXP: {
                double r = *REAL(val);
                if (ISNAN(r)) {
                    x = NA_INTEGER;
                    isNaOrNan = true;
                } else {
                    x = (int)(ceil_ ? ceil(r) : floor(r));
                    isNaOrNan = false;
                }
                break;
            }
            case LGLSXP: {
                int l = *LOGICAL(val);
                if (l == NA_LOGICAL) {
                    x = NA_INTEGER;
                    isNaOrNan = true;
                } else {
                    x = (int)l;
                    isNaOrNan = false;
                }
                break;
            }
            default:
                assert(false);
            }
            if (XLENGTH(val) > 1) {
                Rf_warningcall(getSrcAt(c, pc - 1, ctx),
                               "numerical expression has multiple "
                               "elements: only the first used");
            }
        }
    } else { // Everything else
        x = NA_INTEGER;
        isNaOrNan = true;
    }
    if (isNaOrNan) {
        Rf_errorcall(getSrcAt(c, pc - 1, ctx), "NA/NaN argument");
    }
    SEXP res = Rf_allocVector(INTSXP, 1);
    *INTEGER(res) = x;
    ostack_pop(ctx);
    ostack_push(ctx, res);
}

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wcast-align"

// This happens since enabling -fno-exceptions, but the error message is
// terrible, can't find out where in the evalRirCode function
#pragma GCC diagnostic ignored "-Wstrict-overflow"

/*
 * This function takes some deopt metadata and stack frame contents on the
 * interpreter stack. It first recursively reconstructs a context for each
 * inlined frame, bottom up. This means either reuse the already existing
 * context, or synthesize a new one. Then the frames are executed (in the
 * deoptimized version), top down. At the end we long-jump out of the outermost
 * context and thus return from the R function that triggered this deopt
 * routine.
 */
void deoptFramesWithContext(InterpreterInstance* ctx,
                            const CallContext* callCtxt,
                            DeoptMetadata* deoptData, SEXP sysparent,
                            size_t pos, size_t stackHeight,
                            bool outerHasContext) {
    size_t excessStack = stackHeight;

    FrameInfo& f = deoptData->frames[pos];
    stackHeight -= f.stackSize + 1;
    SEXP deoptEnv = ostack_at(ctx, stackHeight);
    auto code = f.code;

    bool outermostFrame = pos == deoptData->numFrames - 1;
    bool innermostFrame = pos == 0;

    RCNTXT fake;
    RCNTXT* cntxt;
    auto originalCntxt = findFunctionContextFor(deoptEnv);
    if (originalCntxt) {
        assert(outerHasContext &&
               "Frame with context after frame without context");
        cntxt = originalCntxt;
    } else {
        // NOTE: this assert triggers if we can't find the context of the
        // current function. Usually the reason is that a wrong environment is
        // stored in the context.
        assert(!outermostFrame && "Cannot find outermost function context");
        // If the inlinee had no context, we need to synthesize one
        // TODO: need to add ast and closure to the deopt metadata to create a
        // complete context
        cntxt = &fake;
        initClosureContext(R_NilValue, cntxt, deoptEnv, sysparent,
                           FRAME(sysparent), R_NilValue);
    }

    if (LazyEnvironment::check(deoptEnv)) {
        deoptEnv = createEnvironment(globalContext(), deoptEnv);
        cntxt->cloenv = deoptEnv;
    }
    assert(TYPEOF(deoptEnv) == ENVSXP);

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-variable"
    auto frameBaseSize = ostack_length(ctx) - excessStack;
#pragma GCC diagnostic pop

    auto trampoline = [&]() {
        // 1. Set up our (outer) context
        //
        // The inlinees need to be bound to a new trampoline, or they could
        // long-jump out of this deopt routine into the inlineContextTrampoline.
        // The outermost frame is the caller, not an inlinee, thus we need not
        // change its context.
        if (!outermostFrame) {
            // The longjump is initialized, when we are still reconstructing
            // the frames. But if we restart from here, we need to remove
            // all the extra stuff from the stack used for reconstruction.
            cntxt->nodestack = ostack_cell_at(ctx, excessStack - 1);
            if ((SETJMP(cntxt->cjmpbuf))) {
                assert((size_t)ostack_length(ctx) == frameBaseSize);
                if (R_ReturnedValue == R_RestartToken) {
                    cntxt->callflag = CTXT_RETURN; /* turn restart off */
                    R_ReturnedValue = R_NilValue;  /* remove restart token */
                    return evalRirCode(code, ctx, cntxt->cloenv, callCtxt);
                } else {
                    return R_ReturnedValue;
                }
            }
        }

        // 2. Execute the inner frames
        if (!innermostFrame) {
            deoptFramesWithContext(ctx, callCtxt, deoptData, deoptEnv, pos - 1,
                                   stackHeight, originalCntxt);
        }

        // 3. Execute our frame
        //
        // This wrapper consumes the environment from the deopt metadata and the
        // result of the previous frame.
        assert((size_t)ostack_length(ctx) ==
               frameBaseSize + f.stackSize + (innermostFrame ? 1 : 2));
        SEXP res = nullptr;
        if (!innermostFrame)
            res = ostack_pop(ctx);
        assert(ostack_top() == deoptEnv);
        ostack_pop(ctx);
        if (!innermostFrame)
            ostack_push(ctx, res);
        code->registerInvocation();
        return evalRirCode(code, ctx, cntxt->cloenv, callCtxt, f.pc, nullptr,
                           nullptr);
    };

    SEXP res = trampoline();
    assert((size_t)ostack_length(ctx) == frameBaseSize);

    if (!outermostFrame) {
        endClosureContext(cntxt, res);
    } else {
        assert(findFunctionContextFor(deoptEnv) == cntxt);
        // long-jump out of all the inlined contexts
        Rf_findcontext(CTXT_BROWSER | CTXT_FUNCTION, cntxt->cloenv, res);
        assert(false);
    }

    ostack_push(ctx, res);
}

#ifdef ENABLE_EVENT_COUNTERS
static unsigned EnvAllocated =
    EventCounters::instance().registerCounter("env allocated");
static unsigned EnvStubAllocated =
    EventCounters::instance().registerCounter("envstub allocated");
#endif

SEXP evalRirCode(Code* c, InterpreterInstance* ctx, SEXP env,
                 const CallContext* callCtxt, Opcode* initialPC,
                 R_bcstack_t* localsBase, BindingCache* cache){
#define PURE_INSTRUCTION(op) INSTRUCTION(op)
#define IMPURE_INSTRUCTION(op) INSTRUCTION(op)
#define SANDBOX_NOOP_INSTRUCTION(op) INSTRUCTION(op)
#define FORCE_PROMISE(res, ctx) res = promiseValue(res, ctx)
#include "bc_machine.h"
#undef PURE_INSTRUCTION
#undef IMPURE_INSTRUCTION
#undef SANDBOX_NOOP_INSTRUCTION
#undef FORCE_PROMISE
}

SEXP evalRirCodeSandboxed(Code* c, InterpreterInstance* ctx, SEXP env,
                          const CallContext* callCtxt, Opcode* initialPC,
                          R_bcstack_t* localsBase, BindingCache* cache) {
#define PURE_INSTRUCTION(op) INSTRUCTION(op)
#define IMPURE_INSTRUCTION(op) INSTRUCTION(op) return NULL;
#define SANDBOX_NOOP_INSTRUCTION(op) INSTRUCTION(op) if (false)
#define FORCE_PROMISE(res, ctx)                                                \
    res = promiseValue(res, ctx, true);                                        \
    if (res == NULL)                                                           \
        return NULL;
#include "bc_machine.h"
#undef PURE_INSTRUCTION
#undef IMPURE_INSTRUCTION
#undef SANDBOX_NOOP_INSTRUCTION
#undef FORCE_PROMISE
}

#pragma GCC diagnostic pop

SEXP evalRirCodeExtCaller(Code* c, InterpreterInstance* ctx, SEXP env,
                          bool sandboxed) {
    if (sandboxed)
        return evalRirCodeSandboxed(c, ctx, env, NULL, NULL, NULL, NULL);
    else
        return evalRirCode(c, ctx, env, nullptr);
}

SEXP evalRirCode(Code* c, InterpreterInstance* ctx, SEXP env,
                 const CallContext* callCtxt) {
    return evalRirCode(c, ctx, env, callCtxt, nullptr, nullptr, nullptr);
}

SEXP rirApplyClosure(SEXP ast, SEXP op, SEXP arglist, SEXP rho,
                     SEXP suppliedvars) {
    auto ctx = globalContext();

    RList args(arglist);
    size_t nargs = 0;
    std::vector<Immediate> names;
    for (auto arg = args.begin(), end = args.end(); arg != end; ++arg) {
        ostack_push(ctx, *arg);
        if (arg.hasTag()) {
            names.resize(nargs + 1);
            names[nargs] = Pool::insert(arg.tag());
        }
        nargs++;
    }
    if (!names.empty()) {
        names.resize(nargs);
    }
    // Add extra arguments from object dispatching
    if (suppliedvars != R_NilValue) {
        auto extra = RList(suppliedvars);
        for (auto a = extra.begin(); a != extra.end(); ++a) {
            if (a.hasTag()) {
                auto var = Pool::insert(a.tag());
                if (std::find(names.begin(), names.end(), var) == names.end()) {
                    ostack_push(ctx, *a);
                    names.resize(nargs + 1);
                    names[nargs] = var;
                    nargs++;
                }
            }
        }
    }

    CallContext call(nullptr, op, nargs, ast, ostack_cell_at(ctx, nargs - 1),
                     nullptr, names.empty() ? nullptr : names.data(), rho,
                     Assumptions(), ctx);
    call.arglist = arglist;
    call.safeForceArgs();

    auto res = rirCall(call, ctx);
    ostack_popn(ctx, call.passedArgs);
    return res;
}

SEXP rirEval_f(SEXP what, SEXP env) { return rirEval(what, env, false); }

SEXP rirEval(SEXP what, SEXP env, bool sandboxed) {
    assert(TYPEOF(what) == EXTERNALSXP);

    // TODO: do we not need an RCNTXT here?

    if (auto code = Code::check(what)) {
        return evalRirCodeExtCaller(code, globalContext(), env, sandboxed);
    }

    if (auto table = DispatchTable::check(what)) {
        // TODO: add an adapter frame to be able to call something else than
        // the baseline version!
        Function* fun = table->baseline();
        fun->registerInvocation();

        return evalRirCodeExtCaller(fun->body(), globalContext(), env,
                                    sandboxed);
    }

    if (auto fun = Function::check(what)) {
        fun->registerInvocation();
        return evalRirCodeExtCaller(fun->body(), globalContext(), env,
                                    sandboxed);
    }

    assert(false && "Expected a code object or a dispatch table");
    return nullptr;
}
} // namespace rir
