assert(env != symbol::delayedEnv || (callCtxt != nullptr));

#ifdef THREADED_CODE
static void* opAddr[static_cast<uint8_t>(Opcode::num_of)] = {
#define DEF_INSTR(name, ...) (__extension__ && op_##name),
#include "ir/insns.h"
#undef DEF_INSTR
};
#endif

assert(c->info.magic == CODE_MAGIC);
bool existingLocals = localsBase;

BindingCache* bindingCache;
if (cache) {
    bindingCache = cache;
} else {
    bindingCache = (BindingCache*)(alloca(sizeof(BindingCache) +
                                          sizeof(BindingCacheEntry) *
                                              c->bindingCacheSize));
    bindingCache->length = c->bindingCacheSize;
    // Optimized functions explicitly manage the cache
    if (env != symbol::delayedEnv)
        clearCache(bindingCache);

#ifdef ENABLE_EVENT_COUNTERS
    if (ENABLE_EVENT_COUNTERS && env != symbol::delayedEnv)
        EventCounters::instance().count(EnvAllocated);
#endif
}

if (!existingLocals) {
#ifdef TYPED_STACK
    // Zero the region of the locals to avoid keeping stuff alive and to
    // zero all the type tags. Note: this trick does not work with the stack
    // in general, since there intermediate callees might set the type tags
    // to something else.
    memset(R_BCNodeStackTop, 0, sizeof(*R_BCNodeStackTop) * c->localsCount);
#endif
    localsBase = R_BCNodeStackTop;
}
Locals locals(localsBase, c->localsCount, existingLocals);

// make sure there is enough room on the stack
// there is some slack of 5 to make sure the call instruction can store
// some intermediate values on the stack
ostack_ensureSize(ctx, c->stackLength + 5);

Opcode* pc = initialPC ? initialPC : c->code();
SEXP res;

auto changeEnv = [&](SEXP e) {
    assert((TYPEOF(e) == ENVSXP || LazyEnvironment::check(e)) &&
           "Expected an environment");
    if (e != env)
        env = e;
};
R_Visible = TRUE;

checkUserInterrupt();

// main loop

BEGIN_MACHINE {

    PURE_INSTRUCTION(invalid_) assert(false && "wrong or unimplemented opcode");

    PURE_INSTRUCTION(nop_) NEXT();

    IMPURE_INSTRUCTION(push_context_) {
        SEXP ast = ostack_at(ctx, 1);
        SEXP op = ostack_at(ctx, 0);
        assert(LazyEnvironment::check(env) || TYPEOF(env) == ENVSXP);
        assert(TYPEOF(op) == CLOSXP);
        ostack_popn(ctx, 2);
        int offset = readJumpOffset();
        advanceJump();
        // Recursively call myself through a inlineContextTrampoline. The
        // trampoline creates an RCNTXT, and then continues executing the
        // same code.
        inlineContextTrampoline(c, callCtxt, ast, env, op, ctx, pc, localsBase,
                                bindingCache);
        // After returning from the inlined context we need to skip all the
        // instructions inside the context. Otherwise we would execute them
        // twice. Effectively this updates our pc to match the one the
        // pop_context_ had in the inlined context.
        pc += offset;
        assert(*pc == Opcode::pop_context_);
        advanceOpcode();
        NEXT();
    }

    IMPURE_INSTRUCTION(pop_context_) { return ostack_pop(ctx); }

    IMPURE_INSTRUCTION(mk_env_) {
        size_t n = readImmediate();
        advanceImmediate();
        int contextPos = readSignedImmediate();
        advanceImmediate();
        SEXP parent = ostack_pop(ctx);
        PROTECT(parent);
        assert(TYPEOF(parent) == ENVSXP &&
               "Non-environment used as environment parent.");
        SEXP arglist = R_NilValue;
        auto names = (Immediate*)pc;
        advanceImmediateN(n);
        bool hasMissing = false;
        for (long i = n - 1; i >= 0; --i) {
            SEXP val = ostack_pop(ctx);
            ENSURE_NAMED(val);
            SEXP name = cp_pool_at(ctx, names[i]);
            arglist = CONS_NR(val, arglist);
            SET_TAG(arglist, name);
            hasMissing = hasMissing || val == R_MissingArg;
            SET_MISSING(arglist, val == R_MissingArg ? 2 : 0);
        }
        res = Rf_NewEnvironment(R_NilValue, arglist, parent);

        if (contextPos > 0) {
            if (auto cptr = getFunctionContext(contextPos - 1)) {
                cptr->cloenv = res;
                if (cptr->promargs == symbol::delayedArglist) {
                    auto promargs = arglist;
                    if (hasMissing) {
                        // For the promargs we need to strip missing
                        // arguments from the list, otherwise nargs()
                        // reports the wrong value.
                        promargs = Rf_shallow_duplicate(arglist);
                        // Need to test for R_MissingArg because
                        // shallowDuplicate does not copy the missing flag.
                        while (CAR(promargs) == R_MissingArg &&
                               promargs != R_NilValue) {
                            promargs = CDR(promargs);
                        }
                        auto p = promargs;
                        auto prev = p;
                        while (p != R_NilValue) {
                            if (CAR(p) == R_MissingArg)
                                SETCDR(prev, CDR(p));
                            prev = p;
                            p = CDR(p);
                        }
                    }
                    cptr->promargs = promargs;
                }
            }
        }
        ostack_push(ctx, res);
        UNPROTECT(1);

#ifdef ENABLE_EVENT_COUNTERS
        if (ENABLE_EVENT_COUNTERS)
            EventCounters::instance().count(EnvAllocated);
#endif

        NEXT();
    }

    PURE_INSTRUCTION(clear_binding_cache_) {
        size_t start = readImmediate();
        advanceImmediate();
        size_t len = readImmediate();
        advanceImmediate();
        if (len) {
            SLOWASSERT(start + len <= bindingCache->length);
            memset(&bindingCache->entry[start], 0,
                   sizeof(BindingCacheEntry) * len);
        }
        NEXT();
    }

    PURE_INSTRUCTION(mk_stub_env_) {
        size_t n = readImmediate();
        advanceImmediate();
        int contextPos = readSignedImmediate();
        advanceImmediate();
        // Do we need to preserve parent and the arg vals?
        SEXP parent = ostack_pop(ctx);
        assert(TYPEOF(parent) == ENVSXP &&
               "Non-environment used as environment parent.");
        auto names = pc;
        advanceImmediateN(n);
        SEXP wrapper = Rf_allocVector(EXTERNALSXP, sizeof(LazyEnvironment) +
                                                       sizeof(SEXP) * (n + 1));
        new (DATAPTR(wrapper))
            LazyEnvironment(parent, (Immediate*)names, n, localsBase, ctx);

        ostack_push(ctx, wrapper);
        if (contextPos > 0) {
            if (auto cptr = getFunctionContext(contextPos - 1))
                cptr->cloenv = wrapper;
        }

#ifdef ENABLE_EVENT_COUNTERS
        if (ENABLE_EVENT_COUNTERS)
            EventCounters::instance().count(EnvStubAllocated);
#endif
        NEXT();
    }

    IMPURE_INSTRUCTION(parent_env_) {
        // Can only be used for pir. In pir we always have a closure that
        // stores the lexical envrionment
        assert(callCtxt);
        ostack_push(ctx, CLOENV(callCtxt->callee));
        NEXT();
    }

    IMPURE_INSTRUCTION(get_env_) {
        assert(env);
        ostack_push(ctx, env);
        NEXT();
    }

    IMPURE_INSTRUCTION(set_env_) {
        SEXP e = ostack_pop(ctx);
        changeEnv(e);
        NEXT();
    }

    PURE_INSTRUCTION(ldfun_) {
        SEXP sym = readConst(ctx, readImmediate());
        advanceImmediate();
        res = Rf_findFun(sym, env);

        // TODO something should happen here
        if (res == R_UnboundValue)
            assert(false && "Unbound var");
        if (res == R_MissingArg)
            assert(false && "Missing argument");

        switch (TYPEOF(res)) {
        case CLOSXP:
            // Note: JIT will not exit sandbox
            jit(res, sym, ctx);
            break;
        case SPECIALSXP:
        case BUILTINSXP:
            // special and builtin functions are ok
            break;
        default:
            Rf_error("attempt to apply non-function");
        }
        ostack_push(ctx, res);
        NEXT();
    }

    PURE_INSTRUCTION(ldvar_for_update_) {
        Immediate id = readImmediate();
        advanceImmediate();
        R_varloc_t loc = R_findVarLocInFrame(env, cp_pool_at(ctx, id));
        bool isLocal = !R_VARLOC_IS_NULL(loc);
        SEXP res = nullptr;

        if (isLocal && CAR(loc.cell) != R_UnboundValue) {
            res = CAR(loc.cell);
        } else {
            SEXP sym = cp_pool_at(ctx, id);
            res = Rf_findVar(sym, ENCLOS(env));
        }

        if (res == R_UnboundValue) {
            SEXP sym = cp_pool_at(ctx, id);
            Rf_error("object \"%s\" not found", CHAR(PRINTNAME(sym)));
        } else if (res == R_MissingArg) {
            SEXP sym = cp_pool_at(ctx, id);
            Rf_error("argument \"%s\" is missing, with no default",
                     CHAR(PRINTNAME(sym)));
        }

        // if promise, evaluate & return
        if (TYPEOF(res) == PROMSXP) {
            FORCE_PROMISE(res, ctx);
        }

        if (res != R_NilValue) {
            if (isLocal)
                ENSURE_NAMED(res);
            else if (NAMED(res) < 2)
                SET_NAMED(res, 2);
        }

        ostack_push(ctx, res);
        NEXT();
    }

    PURE_INSTRUCTION(ldvar_for_update_cache_) {
        Immediate id = readImmediate();
        advanceImmediate();
        Immediate cacheIndex = readImmediate();
        advanceImmediate();
        SEXP loc = getCellFromCache(env, id, cacheIndex, ctx, bindingCache);
        bool isLocal = loc;
        SEXP res = nullptr;

        if (isLocal && CAR(loc) != R_UnboundValue) {
            res = CAR(loc);
        } else {
            SEXP sym = cp_pool_at(ctx, id);
            res = Rf_findVar(sym, ENCLOS(env));
        }

        if (res == R_UnboundValue) {
            SEXP sym = cp_pool_at(ctx, id);
            Rf_error("object \"%s\" not found", CHAR(PRINTNAME(sym)));
        } else if (res == R_MissingArg) {
            SEXP sym = cp_pool_at(ctx, id);
            Rf_error("argument \"%s\" is missing, with no default",
                     CHAR(PRINTNAME(sym)));
        }

        // if promise, evaluate & return
        if (TYPEOF(res) == PROMSXP) {
            FORCE_PROMISE(res, ctx);
        }

        if (res != R_NilValue) {
            if (isLocal)
                ENSURE_NAMED(res);
            else if (NAMED(res) < 2)
                SET_NAMED(res, 2);
        }

        ostack_push(ctx, res);
        NEXT();
    }

    PURE_INSTRUCTION(ldvar_noforce_stubbed_) {
        unsigned pos = readImmediate();
        advanceImmediate();

        auto le = LazyEnvironment::check(env);
        assert(le);

        auto res = le->getArg(pos);

        if (res == R_UnboundValue) {
            Rf_error("object \"%s\" not found",
                     CHAR(PRINTNAME(Pool::get(le->names[pos]))));
        } else if (res == R_MissingArg) {
            Rf_error("argument \"%s\" is missing, with no default",
                     CHAR(PRINTNAME(Pool::get(le->names[pos]))));
        }

        if (res != R_NilValue)
            ENSURE_NAMED(res);

        ostack_push(ctx, res);
        NEXT();
    }

    PURE_INSTRUCTION(ldvar_) {
        SEXP sym = readConst(ctx, readImmediate());
        advanceImmediate();
        res = Rf_findVar(sym, env);

        if (res == R_UnboundValue) {
            Rf_error("object \"%s\" not found", CHAR(PRINTNAME(sym)));
        } else if (res == R_MissingArg) {
            Rf_error("argument \"%s\" is missing, with no default",
                     CHAR(PRINTNAME(sym)));
        } else if (TYPEOF(res) == PROMSXP) {
            // if promise, evaluate and return
            FORCE_PROMISE(res, ctx);
        }

        if (res != R_NilValue)
            ENSURE_NAMED(res);

        ostack_push(ctx, res);
        NEXT();
    }

    PURE_INSTRUCTION(ldvar_cached_) {
        Immediate id = readImmediate();
        advanceImmediate();
        Immediate cacheIndex = readImmediate();
        advanceImmediate();
        res = cachedGetVar(env, id, cacheIndex, ctx, bindingCache);

        if (res == R_UnboundValue) {
            SEXP sym = cp_pool_at(ctx, id);
            Rf_error("object \"%s\" not found", CHAR(PRINTNAME(sym)));
        } else if (res == R_MissingArg) {
            SEXP sym = cp_pool_at(ctx, id);
            Rf_error("argument \"%s\" is missing, with no default",
                     CHAR(PRINTNAME(sym)));
        }

        // if promise, evaluate & return
        if (TYPEOF(res) == PROMSXP) {
            FORCE_PROMISE(res, ctx);
        }

        if (res != R_NilValue)
            ENSURE_NAMED(res);

        ostack_push(ctx, res);
        NEXT();
    }

    PURE_INSTRUCTION(ldvar_noforce_) {
        SEXP sym = readConst(ctx, readImmediate());
        advanceImmediate();
        res = Rf_findVar(sym, env);

        if (res == R_UnboundValue) {
            Rf_error("object \"%s\" not found", CHAR(PRINTNAME(sym)));
        } else if (res == R_MissingArg) {
            Rf_error("argument \"%s\" is missing, with no default",
                     CHAR(PRINTNAME(sym)));
        }

        if (res != R_NilValue)
            ENSURE_NAMED(res);

        ostack_push(ctx, res);
        NEXT();
    }

    PURE_INSTRUCTION(ldvar_noforce_cached_) {
        Immediate id = readImmediate();
        advanceImmediate();
        Immediate cacheIndex = readImmediate();
        advanceImmediate();
        res = cachedGetVar(env, id, cacheIndex, ctx, bindingCache);

        if (res == R_UnboundValue) {
            SEXP sym = cp_pool_at(ctx, id);
            Rf_error("object \"%s\" not found", CHAR(PRINTNAME(sym)));
        } else if (res == R_MissingArg) {
            SEXP sym = cp_pool_at(ctx, id);
            Rf_error("argument \"%s\" is missing, with no default",
                     CHAR(PRINTNAME(sym)));
        }

        if (res != R_NilValue)
            ENSURE_NAMED(res);

        ostack_push(ctx, res);
        NEXT();
    }

    PURE_INSTRUCTION(ldvar_super_) {
        SEXP sym = readConst(ctx, readImmediate());
        advanceImmediate();
        res = Rf_findVar(sym, ENCLOS(env));

        if (res == R_UnboundValue) {
            Rf_error("object \"%s\" not found", CHAR(PRINTNAME(sym)));
        } else if (res == R_MissingArg) {
            Rf_error("argument \"%s\" is missing, with no default",
                     CHAR(PRINTNAME(sym)));
        }

        // if promise, evaluate & return
        if (TYPEOF(res) == PROMSXP) {
            FORCE_PROMISE(res, ctx);
        }

        if (res != R_NilValue)
            ENSURE_NAMED(res);

        ostack_push(ctx, res);
        NEXT();
    }

    PURE_INSTRUCTION(ldvar_noforce_super_) {
        SEXP sym = readConst(ctx, readImmediate());
        advanceImmediate();
        res = Rf_findVar(sym, ENCLOS(env));

        if (res == R_UnboundValue) {
            Rf_error("object \"%s\" not found", CHAR(PRINTNAME(sym)));
        } else if (res == R_MissingArg) {
            Rf_error("argument \"%s\" is missing, with no default",
                     CHAR(PRINTNAME(sym)));
        }

        if (res != R_NilValue)
            ENSURE_NAMED(res);

        ostack_push(ctx, res);
        NEXT();
    }

    PURE_INSTRUCTION(ldddvar_) {
        SEXP sym = readConst(ctx, readImmediate());
        advanceImmediate();
        res = Rf_ddfindVar(sym, env);

        if (res == R_UnboundValue) {
            Rf_error("object \"%s\" not found", CHAR(PRINTNAME(sym)));
        } else if (res == R_MissingArg) {
            Rf_error("argument \"%s\" is missing, with no default",
                     CHAR(PRINTNAME(sym)));
        }

        // if promise, evaluate & return
        if (TYPEOF(res) == PROMSXP) {
            FORCE_PROMISE(res, ctx);
        }

        if (res != R_NilValue)
            ENSURE_NAMED(res);

        ostack_push(ctx, res);
        NEXT();
    }

    PURE_INSTRUCTION(ldarg_) {
        Immediate idx = readImmediate();
        advanceImmediate();
        assert(callCtxt);

        if (callCtxt->hasStackArgs()) {
            ostack_push(ctx, callCtxt->stackArg(idx));
        } else {
            if (callCtxt->missingArg(idx)) {
                res = R_MissingArg;
            } else {
                Code* arg = callCtxt->implicitArg(idx);
                assert(!LazyEnvironment::check(callCtxt->callerEnv));
                res = createPromise(arg, callCtxt->callerEnv);
            }
            ostack_push(ctx, res);
        }
        NEXT();
    }

    PURE_INSTRUCTION(ldloc_) {
        Immediate offset = readImmediate();
        advanceImmediate();
        res = locals.load(offset);
        ostack_push(ctx, res);
        NEXT();
    }

    IMPURE_INSTRUCTION(stvar_stubbed_) {
        unsigned pos = readImmediate();
        advanceImmediate();
        SEXP val = ostack_top(ctx);

        auto le = LazyEnvironment::check(env);
        assert(le);
        le->setArg(pos, val);
        ostack_pop(ctx);
        NEXT();
    }

    IMPURE_INSTRUCTION(stvar_) {
        SEXP sym = readConst(ctx, readImmediate());
        advanceImmediate();
        SLOWASSERT(TYPEOF(sym) == SYMSXP);
        SEXP val = ostack_top(ctx);

        assert(!LazyEnvironment::check(env));

        rirDefineVarWrapper(sym, val, env);
        ostack_pop(ctx);
        NEXT();
    }

    IMPURE_INSTRUCTION(stvar_cached_) {
        Immediate id = readImmediate();
        advanceImmediate();
        Immediate cacheIndex = readImmediate();
        advanceImmediate();
        SEXP val = ostack_pop(ctx);

        assert(!LazyEnvironment::check(env));

        cachedSetVar(val, env, id, cacheIndex, ctx, bindingCache);
        NEXT();
    }

    IMPURE_INSTRUCTION(starg_) {
        Immediate id = readImmediate();
        advanceImmediate();
        SEXP val = ostack_top(ctx);

        assert(!LazyEnvironment::check(env));

        SEXP sym = cp_pool_at(ctx, id);
        // In case there is a local binding we must honor missingness which
        // defineVar does not
        if (env != R_BaseEnv && env != R_BaseNamespace) {
            R_varloc_t loc = R_findVarLocInFrame(env, sym);
            if (!R_VARLOC_IS_NULL(loc) && !BINDING_IS_LOCKED(loc.cell) &&
                !IS_ACTIVE_BINDING(loc.cell)) {
                SEXP cur = CAR(loc.cell);
                if (cur != val) {
                    INCREMENT_NAMED(val);
                    SETCAR(loc.cell, val);
                }
                ostack_pop(ctx);
                NEXT();
            }
        }

        rirDefineVarWrapper(sym, val, env);
        ostack_pop(ctx);

        NEXT();
    }

    IMPURE_INSTRUCTION(starg_cached_) {
        Immediate id = readImmediate();
        advanceImmediate();
        Immediate cacheIndex = readImmediate();
        advanceImmediate();
        SEXP val = ostack_pop(ctx);

        assert(!LazyEnvironment::check(env));
        cachedSetVar(val, env, id, cacheIndex, ctx, bindingCache, true);

        NEXT();
    }

    IMPURE_INSTRUCTION(stvar_super_) {
        SEXP sym = readConst(ctx, readImmediate());
        advanceImmediate();
        SLOWASSERT(TYPEOF(sym) == SYMSXP);
        SEXP val = ostack_pop(ctx);
        rirSetVarWrapper(sym, val, ENCLOS(env));
        NEXT();
    }

    IMPURE_INSTRUCTION(stloc_) {
        Immediate offset = readImmediate();
        advanceImmediate();
        locals.store(offset, ostack_top(ctx));
        ostack_pop(ctx);
        NEXT();
    }

    IMPURE_INSTRUCTION(movloc_) {
        Immediate target = readImmediate();
        advanceImmediate();
        Immediate source = readImmediate();
        advanceImmediate();
        locals.store(target, locals.load(source));
        NEXT();
    }

    IMPURE_INSTRUCTION(named_call_implicit_) {
#ifdef ENABLE_SLOWASSERT
        auto lll = ostack_length(ctx);
        int ttt = R_PPStackTop;
#endif

        // Callee is TOS
        // Arguments and names are immediate given as promise code indices.
        size_t n = readImmediate();
        advanceImmediate();
        size_t ast = readImmediate();
        advanceImmediate();
        Assumptions given(pc);
        pc += sizeof(Assumptions);
        auto arguments = (Immediate*)pc;
        advanceImmediateN(n);
        auto names = (Immediate*)pc;
        advanceImmediateN(n);
        CallContext call(c, ostack_top(ctx), n, ast, arguments, names, env,
                         given, ctx);
        res = doCall(call, ctx);
        ostack_pop(ctx); // callee
        ostack_push(ctx, res);

        SLOWASSERT(ttt == R_PPStackTop);
        SLOWASSERT(lll == ostack_length(ctx));
        NEXT();
    }

    PURE_INSTRUCTION(record_call_) {
        ObservedCallees* feedback = (ObservedCallees*)pc;
        SEXP callee = ostack_top(ctx);
        feedback->record(c, callee);
        pc += sizeof(ObservedCallees);
        NEXT();
    }

    PURE_INSTRUCTION(record_type_) {
        ObservedValues* feedback = (ObservedValues*)pc;
        SEXP t = ostack_top(ctx);
        feedback->record(t);
        pc += sizeof(ObservedValues);
        NEXT();
    }

    IMPURE_INSTRUCTION(call_implicit_) {
#ifdef ENABLE_SLOWASSERT
        auto lll = ostack_length(ctx);
        int ttt = R_PPStackTop;
#endif

        // Callee is TOS
        // Arguments are immediate given as promise code indices.
        size_t n = readImmediate();
        advanceImmediate();
        size_t ast = readImmediate();
        advanceImmediate();
        Assumptions given(pc);
        pc += sizeof(Assumptions);
        auto arguments = (Immediate*)pc;
        advanceImmediateN(n);
        CallContext call(c, ostack_top(ctx), n, ast, arguments, env, given,
                         ctx);
        res = doCall(call, ctx);
        ostack_pop(ctx); // callee
        ostack_push(ctx, res);

        SLOWASSERT(ttt == R_PPStackTop);
        SLOWASSERT(lll == ostack_length(ctx));
        NEXT();
    }

    IMPURE_INSTRUCTION(call_) {
#ifdef ENABLE_SLOWASSERT
        auto lll = ostack_length(ctx);
        int ttt = R_PPStackTop;
#endif

        // Stack contains [callee, arg1, ..., argn]
        Immediate n = readImmediate();
        advanceImmediate();
        size_t ast = readImmediate();
        advanceImmediate();
        Assumptions given(pc);
        pc += sizeof(Assumptions);
        CallContext call(c, ostack_at(ctx, n), n, ast,
                         ostack_cell_at(ctx, n - 1), env, given, ctx);
        res = doCall(call, ctx);
        ostack_popn(ctx, call.passedArgs + 1);
        ostack_push(ctx, res);

        SLOWASSERT(ttt == R_PPStackTop);
        SLOWASSERT(lll - call.suppliedArgs == (unsigned)ostack_length(ctx));
        NEXT();
    }

    IMPURE_INSTRUCTION(named_call_) {
#ifdef ENABLE_SLOWASSERT
        auto lll = ostack_length(ctx);
        int ttt = R_PPStackTop;
#endif

        // Stack contains [callee, arg1, ..., argn]
        Immediate n = readImmediate();
        advanceImmediate();
        size_t ast = readImmediate();
        advanceImmediate();
        Assumptions given(pc);
        pc += sizeof(Assumptions);
        auto names = (Immediate*)pc;
        advanceImmediateN(n);
        CallContext call(c, ostack_at(ctx, n), n, ast,
                         ostack_cell_at(ctx, n - 1), names, env, given, ctx);
        res = doCall(call, ctx);
        ostack_popn(ctx, call.passedArgs + 1);
        ostack_push(ctx, res);

        SLOWASSERT(ttt == R_PPStackTop);
        SLOWASSERT(lll - call.suppliedArgs == (unsigned)ostack_length(ctx));
        NEXT();
    }

    IMPURE_INSTRUCTION(call_builtin_) {
#ifdef ENABLE_SLOWASSERT
        auto lll = ostack_length(ctx);
        int ttt = R_PPStackTop;
#endif

        // Stack contains [arg1, ..., argn], callee is immediate
        Immediate n = readImmediate();
        advanceImmediate();
        Immediate ast = readImmediate();
        advanceImmediate();
        SEXP callee = cp_pool_at(ctx, readImmediate());
        advanceImmediate();
        CallContext call(c, callee, n, ast, ostack_cell_at(ctx, n - 1), env,
                         Assumptions(), ctx);
        res = builtinCall(call, ctx);
        ostack_popn(ctx, call.passedArgs);
        ostack_push(ctx, res);

        SLOWASSERT(ttt == R_PPStackTop);
        SLOWASSERT(lll - call.suppliedArgs + 1 == (unsigned)ostack_length(ctx));
        NEXT();
    }

    IMPURE_INSTRUCTION(static_call_) {
#ifdef ENABLE_SLOWASSERT
        auto lll = ostack_length(ctx);
        int ttt = R_PPStackTop;
#endif

        // Stack contains [arg1, ..., argn], callee is immediate
        Immediate n = readImmediate();
        advanceImmediate();
        Immediate ast = readImmediate();
        advanceImmediate();
        Assumptions given(pc);
        pc += sizeof(Assumptions);
        SEXP callee = cp_pool_at(ctx, readImmediate());
        advanceImmediate();
        SEXP version = cp_pool_at(ctx, readImmediate());
        CallContext call(c, callee, n, ast, ostack_cell_at(ctx, n - 1), env,
                         given, ctx);
        auto fun = Function::unpack(version);
        addDynamicAssumptionsFromContext(call);
        bool dispatchFail = !fun->dead && !matches(call, fun->signature());
        if (fun->invocationCount() % pir::Parameter::RIR_WARMUP == 0) {
            Assumptions assumptions =
                addDynamicAssumptionsForOneTarget(call, fun->signature());
            if (assumptions != fun->signature().assumptions)
                // We have more assumptions available, let's recompile
                dispatchFail = true;
        }

        if (dispatchFail) {
            auto dt = DispatchTable::unpack(BODY(callee));
            fun = dispatch(call, dt);
            // Patch inline cache
            (*(Immediate*)pc) = Pool::insert(fun->container());
        }
        advanceImmediate();

        if (fun->signature().envCreation ==
            FunctionSignature::Environment::CallerProvided) {
            res = doCall(call, ctx);
        } else {
            ArgsLazyData lazyArgs(&call, ctx);
            fun->registerInvocation();
            supplyMissingArgs(call, fun);
            res = rirCallTrampoline(call, fun, symbol::delayedEnv,
                                    (SEXP)&lazyArgs, ctx);
        }
        ostack_popn(ctx, call.passedArgs);
        ostack_push(ctx, res);

        SLOWASSERT(ttt == R_PPStackTop);
        SLOWASSERT(lll - call.suppliedArgs + 1 == (unsigned)ostack_length(ctx));
        NEXT();
    }

    PURE_INSTRUCTION(close_) {
        SEXP srcref = ostack_at(ctx, 0);
        SEXP body = ostack_at(ctx, 1);
        SEXP formals = ostack_at(ctx, 2);
        res = Rf_allocSExp(CLOSXP);
        assert(DispatchTable::check(body));
        SET_FORMALS(res, formals);
        SET_BODY(res, body);
        SET_CLOENV(res, env);
        Rf_setAttrib(res, Rf_install("srcref"), srcref);
        ostack_popn(ctx, 3);
        ostack_push(ctx, res);
        NEXT();
    }

    PURE_INSTRUCTION(isfun_) {
        SEXP val = ostack_top(ctx);

        switch (TYPEOF(val)) {
        case CLOSXP:
            jit(val, R_NilValue, ctx);
            break;
        case SPECIALSXP:
        case BUILTINSXP:
            // builtins and specials are fine
            // TODO for now - we might be fancier here later
            break;
        default:
            Rf_error("attempt to apply non-function");
        }
        NEXT();
    }

    PURE_INSTRUCTION(promise_) {
        Immediate id = readImmediate();
        advanceImmediate();
        SEXP prom = Rf_mkPROMISE(c->getPromise(id)->container(), env);
        SET_PRVALUE(prom, ostack_pop(ctx));
        ostack_push(ctx, prom);
        NEXT();
    }

    PURE_INSTRUCTION(force_) {
        if (TYPEOF(ostack_top(ctx)) == PROMSXP) {
            SEXP res = ostack_pop(ctx);
            // If the promise is already evaluated then push the value
            // inside the promise onto the stack, otherwise push the value
            // from forcing the promise
            FORCE_PROMISE(res, ctx);
            ostack_push(ctx, res);
        }
        NEXT();
    }

    IMPURE_INSTRUCTION(force_sb_) {
        bool succeed = true;
#define PRINT_SANDBOX PRINT_INTERP
#if PRINT_SANDBOX
        std::cout << "** begin sandbox\n";
#endif
        ctx->beginSandbox();
        if (TYPEOF(ostack_top(ctx)) == PROMSXP) {
            SEXP val = ostack_pop(ctx);
            // If the promise is already evaluated then push the value
            // inside the promise onto the stack, otherwise push the value
            // from forcing the promise
            SEXP res = promiseValue(val, ctx, SandboxMode::Sandbox);
            if (res == NULL) {
                succeed = false;
                res = val;
            }
            ostack_push(ctx, res);
        }
#if PRINT_SANDBOX
        if (succeed)
            std::cout << "** sandbox success\n";
        else
            std::cout << "** sandbox fail\n";
#endif
        ctx->endSandbox(succeed);
        ostack_push(ctx, succeed ? R_TrueValue : R_FalseValue);
        NEXT();
    }

    PURE_INSTRUCTION(push_) {
        res = readConst(ctx, readImmediate());
        advanceImmediate();
        ostack_push(ctx, res);
        NEXT();
    }

    PURE_INSTRUCTION(push_code_) {
        Immediate n = readImmediate();
        advanceImmediate();
        ostack_push(ctx, c->getPromise(n)->container());
        NEXT();
    }

    PURE_INSTRUCTION(dup_) {
        ostack_push(ctx, ostack_top(ctx));
        NEXT();
    }

    PURE_INSTRUCTION(dup2_) {
        ostack_push(ctx, ostack_at(ctx, 1));
        ostack_push(ctx, ostack_at(ctx, 1));
        NEXT();
    }

    PURE_INSTRUCTION(pop_) {
        ostack_pop(ctx);
        NEXT();
    }

    PURE_INSTRUCTION(popn_) {
        Immediate i = readImmediate();
        advanceImmediate();
        ostack_popn(ctx, i);
        NEXT();
    }

    PURE_INSTRUCTION(swap_) {
        SEXP lhs = ostack_pop(ctx);
        SEXP rhs = ostack_pop(ctx);
        ostack_push(ctx, lhs);
        ostack_push(ctx, rhs);
        NEXT();
    }

    PURE_INSTRUCTION(put_) {
        Immediate i = readImmediate();
        advanceImmediate();
        R_bcstack_t* pos = ostack_cell_at(ctx, 0);
#ifdef TYPED_STACK
        SEXP val = pos->u.sxpval;
        while (i--) {
            pos->u.sxpval = (pos - 1)->u.sxpval;
            pos--;
        }
        pos->u.sxpval = val;
#else
        SEXP val = *pos;
        while (i--) {
            *pos = *(pos - 1);
            pos--;
        }
        *pos = val;
#endif
        NEXT();
    }

    PURE_INSTRUCTION(pick_) {
        Immediate i = readImmediate();
        advanceImmediate();
        R_bcstack_t* pos = ostack_cell_at(ctx, i);
#ifdef TYPED_STACK
        SEXP val = pos->u.sxpval;
        while (i--) {
            pos->u.sxpval = (pos + 1)->u.sxpval;
            pos++;
        }
        pos->u.sxpval = val;
#else
        SEXP val = *pos;
        while (i--) {
            *pos = *(pos + 1);
            pos++;
        }
        *pos = val;
#endif
        NEXT();
    }

    PURE_INSTRUCTION(pull_) {
        Immediate i = readImmediate();
        advanceImmediate();
        SEXP val = ostack_at(ctx, i);
        ostack_push(ctx, val);
        NEXT();
    }

    IMPURE_INSTRUCTION(add_) {
        SEXP lhs = ostack_at(ctx, 1);
        SEXP rhs = ostack_at(ctx, 0);
        DO_BINOP(+, PLUSOP);
        NEXT();
    }

    IMPURE_INSTRUCTION(uplus_) {
        SEXP val = ostack_at(ctx, 0);
        DO_UNOP(+, PLUSOP);
        NEXT();
    }

    IMPURE_INSTRUCTION(inc_) {
        SEXP val = ostack_top(ctx);
        SLOWASSERT(TYPEOF(val) == INTSXP);
        if (MAYBE_REFERENCED(val)) {
            int i = INTEGER(val)[0];
            ostack_pop(ctx);
            SEXP n = Rf_allocVector(INTSXP, 1);
            INTEGER(n)[0] = i + 1;
            ostack_push(ctx, n);
        } else {
            INTEGER(val)[0]++;
        }
        NEXT();
    }

    IMPURE_INSTRUCTION(dec_) {
        SEXP val = ostack_top(ctx);
        SLOWASSERT(TYPEOF(val) == INTSXP);
        if (MAYBE_REFERENCED(val)) {
            int i = INTEGER(val)[0];
            ostack_pop(ctx);
            SEXP n = Rf_allocVector(INTSXP, 1);
            INTEGER(n)[0] = i - 1;
            ostack_push(ctx, n);
        } else {
            INTEGER(val)[0]--;
        }
        NEXT();
    }

    IMPURE_INSTRUCTION(sub_) {
        SEXP lhs = ostack_at(ctx, 1);
        SEXP rhs = ostack_at(ctx, 0);
        DO_BINOP(-, MINUSOP);
        NEXT();
    }

    IMPURE_INSTRUCTION(uminus_) {
        SEXP val = ostack_at(ctx, 0);
        DO_UNOP(-, MINUSOP);
        NEXT();
    }

    IMPURE_INSTRUCTION(mul_) {
        SEXP lhs = ostack_at(ctx, 1);
        SEXP rhs = ostack_at(ctx, 0);
        DO_BINOP(*, TIMESOP);
        NEXT();
    }

    IMPURE_INSTRUCTION(div_) {
        SEXP lhs = ostack_at(ctx, 1);
        SEXP rhs = ostack_at(ctx, 0);

        if (IS_SIMPLE_SCALAR(lhs, REALSXP) && IS_SIMPLE_SCALAR(rhs, REALSXP)) {
            double real_res = (*REAL(lhs) == NA_REAL || *REAL(rhs) == NA_REAL)
                                  ? NA_REAL
                                  : *REAL(lhs) / *REAL(rhs);
            STORE_BINOP(REALSXP, 0, real_res);
        } else if (IS_SIMPLE_SCALAR(lhs, REALSXP) &&
                   IS_SIMPLE_SCALAR(rhs, INTSXP)) {
            double real_res;
            int r = *INTEGER(rhs);
            if (*REAL(lhs) == NA_REAL || r == NA_INTEGER)
                real_res = NA_REAL;
            else
                real_res = *REAL(lhs) / (double)r;
            STORE_BINOP(REALSXP, 0, real_res);
        } else if (IS_SIMPLE_SCALAR(lhs, INTSXP) &&
                   IS_SIMPLE_SCALAR(rhs, REALSXP)) {
            double real_res;
            int l = *INTEGER(lhs);
            if (l == NA_INTEGER || *REAL(rhs) == NA_REAL)
                real_res = NA_REAL;
            else
                real_res = (double)l / *REAL(rhs);
            STORE_BINOP(REALSXP, 0, real_res);
        } else if (IS_SIMPLE_SCALAR(lhs, INTSXP) &&
                   IS_SIMPLE_SCALAR(rhs, INTSXP)) {
            double real_res;
            int l = *INTEGER(lhs);
            int r = *INTEGER(rhs);
            if (l == NA_INTEGER || r == NA_INTEGER)
                real_res = NA_REAL;
            else
                real_res = (double)l / (double)r;
            STORE_BINOP(REALSXP, 0, real_res);
        } else {
            BINOP_FALLBACK("/");
            ostack_popn(ctx, 2);
            ostack_push(ctx, res);
        }
        NEXT();
    }

    IMPURE_INSTRUCTION(idiv_) {
        SEXP lhs = ostack_at(ctx, 1);
        SEXP rhs = ostack_at(ctx, 0);

        if (IS_SIMPLE_SCALAR(lhs, REALSXP) && IS_SIMPLE_SCALAR(rhs, REALSXP)) {
            double real_res = myfloor(*REAL(lhs), *REAL(rhs));
            STORE_BINOP(REALSXP, 0, real_res);
        } else if (IS_SIMPLE_SCALAR(lhs, REALSXP) &&
                   IS_SIMPLE_SCALAR(rhs, INTSXP)) {
            double real_res = myfloor(*REAL(lhs), (double)*INTEGER(rhs));
            STORE_BINOP(REALSXP, 0, real_res);
        } else if (IS_SIMPLE_SCALAR(lhs, INTSXP) &&
                   IS_SIMPLE_SCALAR(rhs, REALSXP)) {
            double real_res = myfloor((double)*INTEGER(lhs), *REAL(rhs));
            STORE_BINOP(REALSXP, 0, real_res);
        } else if (IS_SIMPLE_SCALAR(lhs, INTSXP) &&
                   IS_SIMPLE_SCALAR(rhs, INTSXP)) {
            int int_res;
            int l = *INTEGER(lhs);
            int r = *INTEGER(rhs);
            /* This had x %/% 0 == 0 prior to 2.14.1, but
               it seems conventionally to be undefined */
            if (l == NA_INTEGER || r == NA_INTEGER || r == 0)
                int_res = NA_INTEGER;
            else
                int_res = (int)floor((double)l / (double)r);
            STORE_BINOP(INTSXP, int_res, 0);
        } else {
            BINOP_FALLBACK("%/%");
            ostack_popn(ctx, 2);
            ostack_push(ctx, res);
        }
        NEXT();
    }

    IMPURE_INSTRUCTION(mod_) {
        SEXP lhs = ostack_at(ctx, 1);
        SEXP rhs = ostack_at(ctx, 0);

        if (IS_SIMPLE_SCALAR(lhs, REALSXP) && IS_SIMPLE_SCALAR(rhs, REALSXP)) {
            double real_res = myfmod(ctx, *REAL(lhs), *REAL(rhs));
            STORE_BINOP(REALSXP, 0, real_res);
        } else if (IS_SIMPLE_SCALAR(lhs, REALSXP) &&
                   IS_SIMPLE_SCALAR(rhs, INTSXP)) {
            double real_res = myfmod(ctx, *REAL(lhs), (double)*INTEGER(rhs));
            STORE_BINOP(REALSXP, 0, real_res);
        } else if (IS_SIMPLE_SCALAR(lhs, INTSXP) &&
                   IS_SIMPLE_SCALAR(rhs, REALSXP)) {
            double real_res = myfmod(ctx, (double)*INTEGER(lhs), *REAL(rhs));
            STORE_BINOP(REALSXP, 0, real_res);
        } else if (IS_SIMPLE_SCALAR(lhs, INTSXP) &&
                   IS_SIMPLE_SCALAR(rhs, INTSXP)) {
            int int_res;
            int l = *INTEGER(lhs);
            int r = *INTEGER(rhs);
            if (l == NA_INTEGER || r == NA_INTEGER || r == 0) {
                int_res = NA_INTEGER;
            } else {
                int_res = (l >= 0 && r > 0)
                              ? l % r
                              : (int)myfmod(ctx, (double)l, (double)r);
            }
            STORE_BINOP(INTSXP, int_res, 0);
        } else {
            BINOP_FALLBACK("%%");
            ostack_popn(ctx, 2);
            ostack_push(ctx, res);
        }
        NEXT();
    }

    IMPURE_INSTRUCTION(pow_) {
        SEXP lhs = ostack_at(ctx, 1);
        SEXP rhs = ostack_at(ctx, 0);
        BINOP_FALLBACK("^");
        ostack_popn(ctx, 2);
        ostack_push(ctx, res);
        NEXT();
    }

    IMPURE_INSTRUCTION(lt_) {
        SEXP lhs = ostack_at(ctx, 1);
        SEXP rhs = ostack_at(ctx, 0);
        DO_RELOP(<);
        ostack_popn(ctx, 2);
        ostack_push(ctx, res);
        NEXT();
    }

    IMPURE_INSTRUCTION(gt_) {
        SEXP lhs = ostack_at(ctx, 1);
        SEXP rhs = ostack_at(ctx, 0);
        DO_RELOP(>);
        ostack_popn(ctx, 2);
        ostack_push(ctx, res);
        NEXT();
    }

    IMPURE_INSTRUCTION(le_) {
        SEXP lhs = ostack_at(ctx, 1);
        SEXP rhs = ostack_at(ctx, 0);
        DO_RELOP(<=);
        ostack_popn(ctx, 2);
        ostack_push(ctx, res);
        NEXT();
    }

    IMPURE_INSTRUCTION(ge_) {
        SEXP lhs = ostack_at(ctx, 1);
        SEXP rhs = ostack_at(ctx, 0);
        DO_RELOP(>=);
        ostack_popn(ctx, 2);
        ostack_push(ctx, res);
        NEXT();
    }

    IMPURE_INSTRUCTION(eq_) {
        SEXP lhs = ostack_at(ctx, 1);
        SEXP rhs = ostack_at(ctx, 0);
        DO_RELOP(==);
        ostack_popn(ctx, 2);
        ostack_push(ctx, res);
        NEXT();
    }

    PURE_INSTRUCTION(identical_noforce_) {
        SEXP rhs = ostack_pop(ctx);
        SEXP lhs = ostack_pop(ctx);
        // This instruction does not force, but we should still compare
        // the actual promise value if it is already forced.
        // Especially important since all the inlined functions are probably
        // behind lazy loading stub promises.
        if (TYPEOF(rhs) == PROMSXP && PRVALUE(rhs) != R_UnboundValue)
            rhs = PRVALUE(rhs);
        if (TYPEOF(lhs) == PROMSXP && PRVALUE(lhs) != R_UnboundValue)
            lhs = PRVALUE(lhs);
        // Special case for closures: (level 1) deep compare with body
        // expression instead of body object, to ensure that a compiled
        // closure is equal to the uncompiled one
        if (lhs != rhs && TYPEOF(lhs) == CLOSXP && TYPEOF(rhs) == CLOSXP &&
            CLOENV(lhs) == CLOENV(rhs) && FORMALS(lhs) == FORMALS(rhs) &&
            BODY_EXPR(lhs) == BODY_EXPR(rhs))
            ostack_push(ctx, R_TrueValue);
        else
            ostack_push(ctx, rhs == lhs ? R_TrueValue : R_FalseValue);

        NEXT();
    }

    IMPURE_INSTRUCTION(ne_) {
        assert(R_PPStackTop >= 0);
        SEXP lhs = ostack_at(ctx, 1);
        SEXP rhs = ostack_at(ctx, 0);
        DO_RELOP(!=);
        ostack_popn(ctx, 2);
        ostack_push(ctx, res);
        NEXT();
    }

    IMPURE_INSTRUCTION(not_) {
        SEXP val = ostack_at(ctx, 0);

        if (IS_SIMPLE_SCALAR(val, LGLSXP)) {
            if (*LOGICAL(val) == NA_LOGICAL) {
                res = R_LogicalNAValue;
            } else {
                res = *LOGICAL(val) == 0 ? R_TrueValue : R_FalseValue;
            }
        } else if (IS_SIMPLE_SCALAR(val, REALSXP)) {
            if (*REAL(val) == NA_REAL) {
                res = R_LogicalNAValue;
            } else {
                res = *REAL(val) == 0.0 ? R_TrueValue : R_FalseValue;
            }
        } else if (IS_SIMPLE_SCALAR(val, INTSXP)) {
            if (*INTEGER(val) == NA_INTEGER) {
                res = R_LogicalNAValue;
            } else {
                res = *INTEGER(val) == 0 ? R_TrueValue : R_FalseValue;
            }
        } else {
            UNOP_FALLBACK("!");
        }

        ostack_popn(ctx, 1);
        ostack_push(ctx, res);
        NEXT();
    }

    IMPURE_INSTRUCTION(lgl_or_) {
        SEXP s2 = ostack_pop(ctx);
        SEXP s1 = ostack_pop(ctx);
        assert(TYPEOF(s2) == LGLSXP);
        assert(TYPEOF(s1) == LGLSXP);
        int x2 = XLENGTH(s2) == 0 ? NA_LOGICAL : LOGICAL(s2)[0];
        int x1 = XLENGTH(s1) == 0 ? NA_LOGICAL : LOGICAL(s1)[0];
        assert(x1 == 1 || x1 == 0 || x1 == NA_LOGICAL);
        assert(x2 == 1 || x2 == 0 || x2 == NA_LOGICAL);
        if (x1 == 1 || x2 == 1)
            ostack_push(ctx, R_TrueValue);
        else if (x1 == 0 && x2 == 0)
            ostack_push(ctx, R_FalseValue);
        else
            ostack_push(ctx, R_LogicalNAValue);
        NEXT();
    }

    IMPURE_INSTRUCTION(lgl_and_) {
        SEXP s2 = ostack_pop(ctx);
        SEXP s1 = ostack_pop(ctx);
        assert(TYPEOF(s2) == LGLSXP);
        assert(TYPEOF(s1) == LGLSXP);
        int x2 = XLENGTH(s2) == 0 ? NA_LOGICAL : LOGICAL(s2)[0];
        int x1 = XLENGTH(s1) == 0 ? NA_LOGICAL : LOGICAL(s1)[0];
        assert(x1 == 1 || x1 == 0 || x1 == NA_LOGICAL);
        assert(x2 == 1 || x2 == 0 || x2 == NA_LOGICAL);
        if (x1 == 1 && x2 == 1)
            ostack_push(ctx, R_TrueValue);
        else if (x1 == 0 || x2 == 0)
            ostack_push(ctx, R_FalseValue);
        else
            ostack_push(ctx, R_LogicalNAValue);
        NEXT();
    }

    PURE_INSTRUCTION(aslogical_) {
        SEXP val = ostack_top(ctx);
        int x1 = Rf_asLogical(val);
        assert(x1 == 1 || x1 == 0 || x1 == NA_LOGICAL);
        res = Rf_ScalarLogical(x1);
        ostack_pop(ctx);
        ostack_push(ctx, res);
        NEXT();
    }

    PURE_INSTRUCTION(asbool_) {
        SEXP val = ostack_top(ctx);
        int cond = NA_LOGICAL;
        if (XLENGTH(val) > 1)
            Rf_warningcall(getSrcAt(c, pc - 1, ctx),
                           "the condition has length > 1 and only the first "
                           "element will be used");

        if (XLENGTH(val) > 0) {
            switch (TYPEOF(val)) {
            case LGLSXP:
                cond = LOGICAL(val)[0];
                break;
            case INTSXP:
                cond = INTEGER(val)[0]; // relies on NA_INTEGER == NA_LOGICAL
                break;
            default:
                cond = Rf_asLogical(val);
            }
        }

        if (cond == NA_LOGICAL) {
            const char* msg =
                XLENGTH(val)
                    ? (isLogical(val)
                           ? ("missing value where TRUE/FALSE needed")
                           : ("argument is not interpretable as logical"))
                    : ("argument is of length zero");
            Rf_errorcall(getSrcAt(c, pc - 1, ctx), msg);
        }

        ostack_pop(ctx);
        ostack_push(ctx, cond ? R_TrueValue : R_FalseValue);
        NEXT();
    }

    PURE_INSTRUCTION(ceil_) {
        castInt(true, c, pc, ctx);
        NEXT();
    }

    PURE_INSTRUCTION(floor_) {
        castInt(false, c, pc, ctx);
        NEXT();
    }

    PURE_INSTRUCTION(asast_) {
        SEXP val = ostack_pop(ctx);
        assert(TYPEOF(val) == PROMSXP);
        res = PRCODE(val);
        // if the code is EXTERNALSXP then it is rir Code object, get its
        // ast
        if (TYPEOF(res) == EXTERNALSXP)
            res = cp_pool_at(ctx, Code::unpack(res)->src);
        // otherwise return whatever we had, make sure we do not see
        // bytecode
        assert(TYPEOF(res) != BCODESXP);
        ostack_push(ctx, res);
        NEXT();
    }

    PURE_INSTRUCTION(is_) {
        SEXP val = ostack_pop(ctx);
        Immediate i = readImmediate();
        advanceImmediate();
        bool res;
        switch (i) {
        case NILSXP:
        case LGLSXP:
        case REALSXP:
            res = TYPEOF(val) == i;
            break;

        case VECSXP:
            res = TYPEOF(val) == VECSXP || TYPEOF(val) == LISTSXP;
            break;

        case LISTSXP:
            res = TYPEOF(val) == LISTSXP || TYPEOF(val) == NILSXP;
            break;

        case static_cast<Immediate>(TypeChecks::RealNonObject):
            res = TYPEOF(val) == REALSXP && !isObject(val);
            break;
        case static_cast<Immediate>(TypeChecks::RealSimpleScalar):
            res = IS_SIMPLE_SCALAR(val, REALSXP);
            break;
        case static_cast<Immediate>(TypeChecks::IntegerNonObject):
            res = TYPEOF(val) == INTSXP && !isObject(val);
            break;
        case static_cast<Immediate>(TypeChecks::IntegerSimpleScalar):
            res = IS_SIMPLE_SCALAR(val, INTSXP);
            break;

        default:
            assert(false);
            res = false;
            break;
        }
        ostack_push(ctx, res ? R_TrueValue : R_FalseValue);
        NEXT();
    }

    PURE_INSTRUCTION(isobj_) {
        SEXP val = ostack_pop(ctx);
        ostack_push(ctx, isObject(val) ? R_TrueValue : R_FalseValue);
        NEXT();
    }

    PURE_INSTRUCTION(isstubenv_) {
        SEXP val = ostack_pop(ctx);
        ostack_push(ctx,
                    LazyEnvironment::check(val) ? R_TrueValue : R_FalseValue);
        NEXT();
    }

    PURE_INSTRUCTION(missing_) {
        SEXP sym = readConst(ctx, readImmediate());
        advanceImmediate();
        SLOWASSERT(TYPEOF(sym) == SYMSXP);
        SLOWASSERT(!DDVAL(sym));
        assert(env);
        SEXP val = R_findVarLocInFrame(env, sym).cell;
        if (val == NULL)
            Rf_errorcall(getSrcAt(c, pc - 1, ctx),
                         "'missing' can only be used for arguments");

        if (MISSING(val) || CAR(val) == R_MissingArg) {
            ostack_push(ctx, R_TrueValue);
            NEXT();
        }

        val = CAR(val);

        if (TYPEOF(val) != PROMSXP) {
            ostack_push(ctx, R_FalseValue);
            NEXT();
        }

        val = findRootPromise(val);
        if (!isSymbol(PREXPR(val)))
            ostack_push(ctx, R_FalseValue);
        else {
            ostack_push(ctx, R_isMissing(PREXPR(val), PRENV(val))
                                 ? R_TrueValue
                                 : R_FalseValue);
        }
        NEXT();
    }

    PURE_INSTRUCTION(check_missing_) {
        SEXP val = ostack_top(ctx);
        if (val == R_MissingArg)
            Rf_error("argument is missing, with no default");
        NEXT();
    }

    PURE_INSTRUCTION(brobj_) {
        JumpOffset offset = readJumpOffset();
        advanceJump();
        if (isObject(ostack_top(ctx))) {
            checkUserInterrupt();
            pc += offset;
        }
        PC_BOUNDSCHECK(pc, c);
        NEXT();
    }

    PURE_INSTRUCTION(brtrue_) {
        JumpOffset offset = readJumpOffset();
        advanceJump();
        if (ostack_pop(ctx) == R_TrueValue) {
            checkUserInterrupt();
            pc += offset;
        }
        PC_BOUNDSCHECK(pc, c);
        NEXT();
    }

    PURE_INSTRUCTION(brfalse_) {
        JumpOffset offset = readJumpOffset();
        advanceJump();
        if (ostack_pop(ctx) == R_FalseValue) {
            checkUserInterrupt();
            pc += offset;
        }
        PC_BOUNDSCHECK(pc, c);
        NEXT();
    }

    PURE_INSTRUCTION(br_) {
        JumpOffset offset = readJumpOffset();
        advanceJump();
        checkUserInterrupt();
        pc += offset;
        PC_BOUNDSCHECK(pc, c);
        NEXT();
    }

    IMPURE_INSTRUCTION(extract1_1_) {
        SEXP val = ostack_at(ctx, 1);
        SEXP idx = ostack_at(ctx, 0);

        SEXP args = CONS_NR(val, CONS_NR(idx, R_NilValue));
        ostack_push(ctx, args);

        if (isObject(val)) {
            SEXP call = getSrcForCall(c, pc - 1, ctx);
            res = dispatchApply(call, val, args, symbol::Bracket, env, ctx);
            if (!res)
                res = do_subset_dflt(R_NilValue, symbol::Bracket, args, env);
        } else {
            res = do_subset_dflt(R_NilValue, symbol::Bracket, args, env);
        }

        ostack_popn(ctx, 3);

        ostack_push(ctx, res);
        NEXT();
    }

    IMPURE_INSTRUCTION(extract1_2_) {
        SEXP val = ostack_at(ctx, 2);
        SEXP idx = ostack_at(ctx, 1);
        SEXP idx2 = ostack_at(ctx, 0);

        SEXP args = CONS_NR(val, CONS_NR(idx, CONS_NR(idx2, R_NilValue)));
        ostack_push(ctx, args);

        if (isObject(val)) {
            SEXP call = getSrcForCall(c, pc - 1, ctx);
            res = dispatchApply(call, val, args, symbol::Bracket, env, ctx);
            if (!res)
                res = do_subset_dflt(R_NilValue, symbol::Bracket, args, env);
        } else {
            res = do_subset_dflt(R_NilValue, symbol::Bracket, args, env);
        }

        ostack_popn(ctx, 4);

        ostack_push(ctx, res);
        NEXT();
    }

    IMPURE_INSTRUCTION(extract2_1_) {
        SEXP val = ostack_at(ctx, 1);
        SEXP idx = ostack_at(ctx, 0);
        int i = -1;

        if (ATTRIB(val) != R_NilValue || ATTRIB(idx) != R_NilValue)
            goto fallback;

        switch (TYPEOF(idx)) {
        case REALSXP:
            if (STDVEC_LENGTH(idx) != 1 || *REAL(idx) == NA_REAL)
                goto fallback;
            i = (int)*REAL(idx) - 1;
            break;
        case INTSXP:
            if (STDVEC_LENGTH(idx) != 1 || *INTEGER(idx) == NA_INTEGER)
                goto fallback;
            i = *INTEGER(idx) - 1;
            break;
        case LGLSXP:
            if (STDVEC_LENGTH(idx) != 1 || *LOGICAL(idx) == NA_LOGICAL)
                goto fallback;
            i = (int)*LOGICAL(idx) - 1;
            break;
        default:
            goto fallback;
        }

        if (i >= XLENGTH(val) || i < 0)
            goto fallback;

        switch (TYPEOF(val)) {

#define SIMPLECASE(vectype, vecaccess)                                         \
    case vectype: {                                                            \
        if (XLENGTH(val) == 1 && NO_REFERENCES(val)) {                         \
            res = val;                                                         \
        } else if (XLENGTH(idx) == 1 && NO_REFERENCES(idx)) {                  \
            TYPEOF(idx) = vectype;                                             \
            res = idx;                                                         \
            vecaccess(res)[0] = vecaccess(val)[i];                             \
        } else {                                                               \
            res = Rf_allocVector(vectype, 1);                                  \
            vecaccess(res)[0] = vecaccess(val)[i];                             \
        }                                                                      \
        break;                                                                 \
    }

            SIMPLECASE(REALSXP, REAL);
            SIMPLECASE(INTSXP, INTEGER);
            SIMPLECASE(LGLSXP, LOGICAL);
#undef SIMPLECASE

        case VECSXP: {
            res = VECTOR_ELT(val, i);
            break;
        }

        default:
            goto fallback;
        }

        ostack_popn(ctx, 2);
        ostack_push(ctx, res);
        R_Visible = (Rboolean) true;
        NEXT();

    // ---------
    fallback : {
        SEXP args = CONS_NR(val, CONS_NR(idx, R_NilValue));
        ostack_push(ctx, args);
        if (isObject(val)) {
            SEXP call = getSrcAt(c, pc - 1, ctx);
            res =
                dispatchApply(call, val, args, symbol::DoubleBracket, env, ctx);
            if (!res)
                res = do_subset2_dflt(call, symbol::DoubleBracket, args, env);
        } else {
            res = do_subset2_dflt(R_NilValue, symbol::DoubleBracket, args, env);
        }
        ostack_popn(ctx, 3);

        ostack_push(ctx, res);
        NEXT();
    }
    }

    IMPURE_INSTRUCTION(extract2_2_) {
        SEXP val = ostack_at(ctx, 2);
        SEXP idx = ostack_at(ctx, 1);
        SEXP idx2 = ostack_at(ctx, 0);

        SEXP args = CONS_NR(val, CONS_NR(idx, CONS_NR(idx2, R_NilValue)));
        ostack_push(ctx, args);

        if (isObject(val)) {
            SEXP call = getSrcForCall(c, pc - 1, ctx);
            res =
                dispatchApply(call, val, args, symbol::DoubleBracket, env, ctx);
            if (!res)
                res = do_subset2_dflt(call, symbol::DoubleBracket, args, env);
        } else {
            res = do_subset2_dflt(R_NilValue, symbol::DoubleBracket, args, env);
        }
        ostack_popn(ctx, 4);

        ostack_push(ctx, res);
        NEXT();
    }

    IMPURE_INSTRUCTION(subassign1_1_) {
        SEXP idx = ostack_at(ctx, 0);
        SEXP vec = ostack_at(ctx, 1);
        SEXP val = ostack_at(ctx, 2);

        // Destructively modifies TOS, even if the refcount is 1. This is
        // intended, to avoid copying. Care need to be taken if `vec` is
        // used multiple times as a temporary.
        if (MAYBE_SHARED(vec)) {
            vec = Rf_duplicate(vec);
            ostack_set(ctx, 1, vec);
        }

        SEXP args = CONS_NR(vec, CONS_NR(idx, CONS_NR(val, R_NilValue)));
        SET_TAG(CDDR(args), symbol::value);
        PROTECT(args);

        res = nullptr;
        SEXP call = getSrcForCall(c, pc - 1, ctx);
        RCNTXT assignContext;
        Rf_begincontext(&assignContext, CTXT_RETURN, call, env, ENCLOS(env),
                        args, symbol::AssignBracket);
        if (isObject(vec)) {
            res =
                dispatchApply(call, vec, args, symbol::AssignBracket, env, ctx);
        }
        if (!res) {
            res = do_subassign_dflt(call, symbol::AssignBracket, args, env);
            // We duplicated the vector above, and there is a stvar
            // following
            SET_NAMED(res, 0);
        }
        Rf_endcontext(&assignContext);
        ostack_popn(ctx, 3);
        UNPROTECT(1);

        ostack_push(ctx, res);
        NEXT();
    }

    IMPURE_INSTRUCTION(subassign1_2_) {
        SEXP idx2 = ostack_at(ctx, 0);
        SEXP idx1 = ostack_at(ctx, 1);
        SEXP mtx = ostack_at(ctx, 2);
        SEXP val = ostack_at(ctx, 3);

        // Destructively modifies TOS, even if the refcount is 1. This is
        // intended, to avoid copying. Care need to be taken if `vec` is
        // used multiple times as a temporary.
        if (MAYBE_SHARED(mtx)) {
            mtx = Rf_duplicate(mtx);
            ostack_set(ctx, 2, mtx);
        }

        SEXP args = CONS_NR(
            mtx, CONS_NR(idx1, CONS_NR(idx2, CONS_NR(val, R_NilValue))));
        SET_TAG(CDDDR(args), symbol::value);
        PROTECT(args);

        res = nullptr;
        SEXP call = getSrcForCall(c, pc - 1, ctx);
        RCNTXT assignContext;
        Rf_begincontext(&assignContext, CTXT_RETURN, call, env, ENCLOS(env),
                        args, symbol::AssignBracket);
        if (isObject(mtx)) {
            res =
                dispatchApply(call, mtx, args, symbol::AssignBracket, env, ctx);
        }

        if (!res) {
            res = do_subassign_dflt(call, symbol::AssignBracket, args, env);
            // We duplicated the matrix above, and there is a stvar
            // following
            SET_NAMED(res, 0);
        }
        Rf_endcontext(&assignContext);
        ostack_popn(ctx, 4);
        UNPROTECT(1);

        ostack_push(ctx, res);
        NEXT();
    }

    IMPURE_INSTRUCTION(subassign2_1_) {
        SEXP idx = ostack_at(ctx, 0);
        SEXP vec = ostack_at(ctx, 1);
        SEXP val = ostack_at(ctx, 2);

        // Fast case
        if (NOT_SHARED(vec) && !isObject(vec)) {
            SEXPTYPE vectorT = TYPEOF(vec);
            SEXPTYPE valT = TYPEOF(val);
            SEXPTYPE idxT = TYPEOF(idx);

            // Fast case only if
            // 1. index is numerical and scalar
            // 2. vector is real and shape of value fits into real
            //      or vector is int and shape of value is int
            //      or vector is generic
            // 3. value fits into one cell of the vector
            if ((idxT == INTSXP || idxT == REALSXP) &&
                (XLENGTH(idx) == 1) && // 1
                ((vectorT == REALSXP &&
                  (valT == REALSXP || valT == INTSXP)) || // 2
                 (vectorT == INTSXP && (valT == INTSXP)) ||
                 (vectorT == VECSXP)) &&
                (XLENGTH(val) == 1 || vectorT == VECSXP)) { // 3

                int idx_ = -1;

                if (idxT == REALSXP) {
                    if (*REAL(idx) != NA_REAL)
                        idx_ = (int)*REAL(idx) - 1;
                } else {
                    if (*INTEGER(idx) != NA_INTEGER)
                        idx_ = *INTEGER(idx) - 1;
                }

                if (idx_ >= 0 && idx_ < XLENGTH(vec)) {
                    switch (vectorT) {
                    case REALSXP:
                        REAL(vec)
                        [idx_] = valT == REALSXP ? *REAL(val)
                                                 : (double)*INTEGER(val);
                        break;
                    case INTSXP:
                        INTEGER(vec)[idx_] = *INTEGER(val);
                        break;
                    case VECSXP:
                        // Avoid recursive vectors
                        if (val == vec)
                            val = Rf_shallow_duplicate(val);
                        SET_VECTOR_ELT(vec, idx_, val);
                        break;
                    }
                    ostack_popn(ctx, 3);

                    ostack_push(ctx, vec);
                    NEXT();
                }
            }
        }

        // Destructively modifies TOS, even if the refcount is 1. This is
        // intended, to avoid copying. Care need to be taken if `vec` is
        // used multiple times as a temporary.
        if (MAYBE_SHARED(vec)) {
            vec = Rf_duplicate(vec);
            ostack_set(ctx, 1, vec);
        }

        SEXP args = CONS_NR(vec, CONS_NR(idx, CONS_NR(val, R_NilValue)));
        SET_TAG(CDDR(args), symbol::value);
        PROTECT(args);

        res = nullptr;
        SEXP call = getSrcForCall(c, pc - 1, ctx);

        RCNTXT assignContext;
        Rf_begincontext(&assignContext, CTXT_RETURN, call, env, ENCLOS(env),
                        args, symbol::AssignDoubleBracket);
        if (isObject(vec)) {
            res = dispatchApply(call, vec, args, symbol::AssignDoubleBracket,
                                env, ctx);
        }

        if (!res) {
            res = do_subassign2_dflt(call, symbol::AssignDoubleBracket, args,
                                     env);
            // We duplicated the vector above, and there is a stvar
            // following
            SET_NAMED(res, 0);
        }
        Rf_endcontext(&assignContext);
        ostack_popn(ctx, 3);
        UNPROTECT(1);

        ostack_push(ctx, res);
        NEXT();
    }

    IMPURE_INSTRUCTION(subassign2_2_) {
        SEXP idx2 = ostack_at(ctx, 0);
        SEXP idx1 = ostack_at(ctx, 1);
        SEXP mtx = ostack_at(ctx, 2);
        SEXP val = ostack_at(ctx, 3);

        // Fast case
        if (NOT_SHARED(mtx) && !isObject(mtx)) {
            SEXPTYPE matrixT = TYPEOF(mtx);
            SEXPTYPE valT = TYPEOF(val);
            SEXPTYPE idx1T = TYPEOF(idx1);
            SEXPTYPE idx2T = TYPEOF(idx2);

            // Fast case only if
            // 1. index is numerical and scalar
            // 2. matrix is real and shape of value fits into real
            //      or matrix is int and shape of value is int
            //      or matrix is generic
            // 3. value fits into one cell of the matrix
            if ((idx1T == INTSXP || idx1T == REALSXP) &&
                (XLENGTH(idx1) == 1) && // 1
                (idx2T == INTSXP || idx2T == REALSXP) && (XLENGTH(idx2) == 1) &&
                ((matrixT == REALSXP &&
                  (valT == REALSXP || valT == INTSXP)) || // 2
                 (matrixT == INTSXP && (valT == INTSXP)) ||
                 (matrixT == VECSXP)) &&
                (XLENGTH(val) == 1 || matrixT == VECSXP)) { // 3

                int idx1_ = -1;
                int idx2_ = -1;

                if (idx1T == REALSXP) {
                    if (*REAL(idx1) != NA_REAL)
                        idx1_ = (int)*REAL(idx1) - 1;
                } else {
                    if (*INTEGER(idx1) != NA_INTEGER)
                        idx1_ = *INTEGER(idx1) - 1;
                }

                if (idx2T == REALSXP) {
                    if (*REAL(idx2) != NA_REAL)
                        idx2_ = (int)*REAL(idx1) - 1;
                } else {
                    if (*INTEGER(idx2) != NA_INTEGER)
                        idx2_ = *INTEGER(idx2) - 1;
                }

                if (idx1_ >= 0 && idx1_ < Rf_ncols(mtx) && idx2_ >= 0 &&
                    idx2_ < Rf_nrows(mtx)) {
                    int idx_ = idx1_ + (idx2_ * Rf_nrows(mtx));
                    SEXPTYPE mtxT = TYPEOF(mtx);
                    switch (mtxT) {
                    case REALSXP:
                        REAL(mtx)
                        [idx_] = valT == REALSXP ? *REAL(val)
                                                 : (double)*INTEGER(val);
                        break;
                    case INTSXP:
                        INTEGER(mtx)[idx_] = *INTEGER(val);
                        break;
                    case VECSXP:
                        // Avoid recursive vectors
                        if (val == mtx)
                            val = Rf_shallow_duplicate(val);
                        SET_VECTOR_ELT(mtx, idx_, val);
                        break;
                    }
                    ostack_popn(ctx, 4);

                    ostack_push(ctx, mtx);
                    NEXT();
                }
            }
        }

        // Destructively modifies TOS, even if the refcount is 1. This is
        // intended, to avoid copying. Care need to be taken if `vec` is
        // used multiple times as a temporary.
        if (MAYBE_SHARED(mtx)) {
            mtx = Rf_duplicate(mtx);
            ostack_set(ctx, 2, mtx);
        }

        SEXP args = CONS_NR(
            mtx, CONS_NR(idx1, CONS_NR(idx2, CONS_NR(val, R_NilValue))));
        SET_TAG(CDDDR(args), symbol::value);
        PROTECT(args);

        res = nullptr;
        SEXP call = getSrcForCall(c, pc - 1, ctx);
        RCNTXT assignContext;
        Rf_begincontext(&assignContext, CTXT_RETURN, call, env, ENCLOS(env),
                        args, symbol::AssignDoubleBracket);
        if (isObject(mtx)) {
            res = dispatchApply(call, mtx, args, symbol::AssignDoubleBracket,
                                env, ctx);
        }

        if (!res) {
            res = do_subassign2_dflt(call, symbol::AssignDoubleBracket, args,
                                     env);
            // We duplicated the matrix above, and there is a stvar
            // following
            SET_NAMED(res, 0);
        }
        Rf_endcontext(&assignContext);
        ostack_popn(ctx, 4);
        UNPROTECT(1);

        ostack_push(ctx, res);
        NEXT();
    }

    PURE_INSTRUCTION(guard_fun_) {
        SEXP sym = readConst(ctx, readImmediate());
        advanceImmediate();
        res = readConst(ctx, readImmediate());
        advanceImmediate();
        advanceImmediate();
        if (res != Rf_findFun(sym, env))
            Rf_error("Invalid Callee");
        NEXT();
    }

    IMPURE_INSTRUCTION(deopt_) {
        SEXP r = readConst(ctx, readImmediate());
        advanceImmediate();
        assert(TYPEOF(r) == RAWSXP);
        assert(XLENGTH(r) >= (int)sizeof(DeoptMetadata));
        auto m = (DeoptMetadata*)DATAPTR(r);

#if 0
        size_t pos = 0;
        for (size_t i = 0; i < m->numFrames; ++i) {
            std::cout << "Code " << m->frames[i].code << "\n";
            std::cout << "Frame " << i << ":\n";
            std::cout << "  - env (" << pos << ")\n";
            Rf_PrintValue(ostack_at(ctx, pos++));
            for( size_t j = 0; j < m->frames[i].stackSize; ++j) {
                std::cout << "  - stack (" << pos << ") " << j << "\n";
                Rf_PrintValue(ostack_at(ctx, pos++));
            }
        }
#endif

        if (!pir::Parameter::DEOPT_CHAOS) {
            // TODO: this version is still reachable from static call inline
            // caches. Thus we need to preserve it forever. We need some
            // dependency management here.
            Pool::insert(c->container());
            // remove the deoptimized function. Unless on deopt chaos,
            // always recompiling would just blow testing time...
            auto dt = DispatchTable::unpack(BODY(callCtxt->callee));
            // TODO: report deoptimization reason.
            // For example if we deopt because of stubenv was materialized
            // we should prevent pir from stubbing the env in the future.
            dt->remove(c);
        }
        assert(m->numFrames >= 1);
        size_t stackHeight = 0;
        for (size_t i = 0; i < m->numFrames; ++i)
            stackHeight += m->frames[i].stackSize + 1;
        deoptFramesWithContext(ctx, callCtxt, m, R_NilValue, m->numFrames - 1,
                               stackHeight, true);
        assert(false);
    }

    PURE_INSTRUCTION(seq_) {
        static SEXP prim = NULL;
        if (!prim) {
            // TODO: we could call seq.default here, but it messes up the
            // error call :(
            prim = Rf_findFun(Rf_install("seq"), R_GlobalEnv);
        }

        // TODO: add a real guard here...
        assert(prim == Rf_findFun(Rf_install("seq"), env));

        SEXP from = ostack_at(ctx, 2);
        SEXP to = ostack_at(ctx, 1);
        SEXP by = ostack_at(ctx, 0);
        res = NULL;

        if (IS_SIMPLE_SCALAR(from, INTSXP) && IS_SIMPLE_SCALAR(to, INTSXP) &&
            IS_SIMPLE_SCALAR(by, INTSXP)) {
            int f = *INTEGER(from);
            int t = *INTEGER(to);
            int b = *INTEGER(by);
            if (f != NA_INTEGER && t != NA_INTEGER && b != NA_INTEGER) {
                if ((f < t && b > 0) || (t < f && b < 0)) {
                    int size = 1 + (t - f) / b;
                    res = Rf_allocVector(INTSXP, size);
                    int v = f;
                    for (int i = 0; i < size; ++i) {
                        INTEGER(res)[i] = v;
                        v += b;
                    }
                } else if (f == t) {
                    res = Rf_allocVector(INTSXP, 1);
                    *INTEGER(res) = f;
                }
            }
        }

        if (!res) {
            SLOWASSERT(!isObject(from));
            SEXP call = getSrcForCall(c, pc - 1, ctx);
            SEXP argslist = CONS_NR(from, CONS_NR(to, CONS_NR(by, R_NilValue)));
            ostack_push(ctx, argslist);
            res = Rf_applyClosure(call, prim, argslist, env, R_NilValue);
            ostack_pop(ctx);
        }

        ostack_popn(ctx, 3);
        ostack_push(ctx, res);
        NEXT();
    }

    IMPURE_INSTRUCTION(colon_) {

        SEXP lhs = ostack_at(ctx, 1);
        SEXP rhs = ostack_at(ctx, 0);
        res = NULL;

        if (IS_SIMPLE_SCALAR(lhs, INTSXP)) {
            int from = *INTEGER(lhs);
            if (IS_SIMPLE_SCALAR(rhs, INTSXP)) {
                int to = *INTEGER(rhs);
                if (from != NA_INTEGER && to != NA_INTEGER) {
                    res = seq_int(from, to);
                }
            } else if (IS_SIMPLE_SCALAR(rhs, REALSXP)) {
                double to = *REAL(rhs);
                if (from != NA_INTEGER && to != NA_REAL && R_FINITE(to) &&
                    INT_MIN <= to && INT_MAX >= to && to == (int)to) {
                    res = seq_int(from, (int)to);
                }
            }
        } else if (IS_SIMPLE_SCALAR(lhs, REALSXP)) {
            double from = *REAL(lhs);
            if (IS_SIMPLE_SCALAR(rhs, INTSXP)) {
                int to = *INTEGER(rhs);
                if (from != NA_REAL && to != NA_INTEGER && R_FINITE(from) &&
                    INT_MIN <= from && INT_MAX >= from && from == (int)from) {
                    res = seq_int((int)from, to);
                }
            } else if (IS_SIMPLE_SCALAR(rhs, REALSXP)) {
                double to = *REAL(rhs);
                if (from != NA_REAL && to != NA_REAL && R_FINITE(from) &&
                    R_FINITE(to) && INT_MIN <= from && INT_MAX >= from &&
                    INT_MIN <= to && INT_MAX >= to && from == (int)from &&
                    to == (int)to) {
                    res = seq_int((int)from, (int)to);
                }
            }
        }

        if (res != NULL) {
            R_Visible = (Rboolean) true;
        } else {
            BINOP_FALLBACK(":");
        }

        ostack_popn(ctx, 2);
        ostack_push(ctx, res);
        NEXT();
    }

    PURE_INSTRUCTION(names_) {
        ostack_push(ctx, Rf_getAttrib(ostack_pop(ctx), R_NamesSymbol));
        NEXT();
    }

    PURE_INSTRUCTION(set_names_) {
        SEXP val = ostack_pop(ctx);
        if (!isNull(val))
            Rf_setAttrib(ostack_top(ctx), R_NamesSymbol, val);
        NEXT();
    }

    PURE_INSTRUCTION(alloc_) {
        SEXP val = ostack_pop(ctx);
        assert(TYPEOF(val) == INTSXP);
        int type = readSignedImmediate();
        advanceImmediate();
        res = Rf_allocVector(type, INTEGER(val)[0]);
        ostack_push(ctx, res);
        NEXT();
    }

    PURE_INSTRUCTION(length_) {
        SEXP val = ostack_pop(ctx);
        R_xlen_t len = XLENGTH(val);
        ostack_push(ctx, Rf_allocVector(INTSXP, 1));
        INTEGER(ostack_top(ctx))[0] = len;
        NEXT();
    }

    PURE_INSTRUCTION(for_seq_size_) {
        SEXP seq = ostack_at(ctx, 0);
        // TODO: we should extract the length just once at the begining of
        // the loop and generally have somthing more clever here...
        SEXP value = Rf_allocVector(INTSXP, 1);
        if (Rf_isVector(seq)) {
            INTEGER(value)[0] = LENGTH(seq);
        } else if (Rf_isList(seq) || isNull(seq)) {
            INTEGER(value)[0] = Rf_length(seq);
        } else {
            Rf_errorcall(R_NilValue, "invalid for() loop sequence");
        }
        // TODO: Even when the for loop sequence is an object, R won't
        // dispatch on it. Since in RIR we use the normals extract2_1
        // BC on it, we would. To prevent this we strip the object
        // flag here. What we should do instead, is use a non-dispatching
        // extract BC.
        if (isObject(seq)) {
            seq = Rf_duplicate(seq);
            SET_OBJECT(seq, 0);
            ostack_set(ctx, 0, seq);
        }
        ostack_push(ctx, value);
        NEXT();
    }

    PURE_INSTRUCTION(visible_) {
        R_Visible = TRUE;
        NEXT();
    }

    PURE_INSTRUCTION(invisible_) {
        R_Visible = FALSE;
        NEXT();
    }

    IMPURE_INSTRUCTION(ensure_named_) {
        SEXP val = ostack_top(ctx);
        ENSURE_NAMED(val);
        NEXT();
    }

    IMPURE_INSTRUCTION(set_shared_) {
        SEXP val = ostack_top(ctx);
        if (NAMED(val) < 2)
            SET_NAMED(val, 2);
        NEXT();
    }

    PURE_INSTRUCTION(beginloop_) {
        SLOWASSERT(env);
        int offset = readJumpOffset();
        advanceJump();
        loopTrampoline(c, ctx, env, callCtxt, pc, localsBase, bindingCache);
        pc += offset;
        checkUserInterrupt();
        assert(*pc == Opcode::endloop_);
        advanceOpcode();
        NEXT();
    }

    PURE_INSTRUCTION(endloop_) { return loopTrampolineMarker; }

    IMPURE_INSTRUCTION(return_) {
        res = ostack_top(ctx);
        // this restores stack pointer to the value from the target context
        Rf_findcontext(CTXT_BROWSER | CTXT_FUNCTION, env, res);
        // not reached
        assert(false);
    }

    PURE_INSTRUCTION(ret_) { goto eval_done; }

    PURE_INSTRUCTION(int3_) {
        asm("int3");
        NEXT();
    }

    PURE_INSTRUCTION(printInvocation_) {
        printf("Invocation count: %d\n", c->funInvocationCount);
        NEXT();
    }

    PURE_INSTRUCTION(assert_type_) {
        assert(pir::Parameter::RIR_CHECK_PIR_TYPES);
        SEXP val = ostack_top(ctx);
        pir::PirType typ(pc);
        pc += sizeof(pir::PirType);
        int instrIdx = readSignedImmediate();
        const char* instr = NULL;
        if (instrIdx == -1) {
            instr = "not generated, set RIR_CHECK_PIR_TYPES=2";
        } else {
            instr = CHAR(Rf_asChar(Pool::get((unsigned)instrIdx)));
        }
        advanceImmediate();
        if (!typ.isInstance(val)) {
            std::cerr << "type assert failed in:\n" << instr << "\n";
            std::cerr << "type " << typ << " not accurate for value ("
                      << pir::PirType(val) << "):\n";
            Rf_PrintValue(val);
            assert(false);
        }
        NEXT();
    }

    PURE_INSTRUCTION(begin_sandbox_record_) {
        ctx->startRecordingSafe();
        NEXT();
    }

    PURE_INSTRUCTION(end_sandbox_record_) {
        ObservedSafe* feedback = (ObservedSafe*)pc;
        bool isSafe = ctx->stopRecordingSafe();
        if (isSafe && *feedback == ObservedSafe::Unknown)
            *feedback = ObservedSafe::Safe;
        else if (!isSafe)
            *feedback = ObservedSafe::Unsafe;
        advanceImmediate();
        NEXT();
    }

    LASTOP;
}

eval_done : return ostack_pop(ctx);
