#include <stack>

#include <cassert>

#include "BC.h"
#include "CodeVerifier.h"
#include "R/Symbols.h"

#include "simple_instruction_list.h"

namespace rir {

namespace {

/** State for verifying the stack layout and calculating max ostack
 * size.
 */
class State {
  public:
    static_assert(sizeof(SEXP) == 8, "Invalid ptr size");
    static_assert(sizeof(unsigned) == 4, "Invalid unsigned size");

    Opcode* pc;
    int ostack;

    State(Opcode* pc = nullptr, int ostack = 0) : pc(pc), ostack(ostack) {}

    State(State const& from, Opcode* pc) : pc(pc), ostack(from.ostack) {}

    bool operator!=(State const& other) const {
        assert(pc == other.pc and
               "It is meaningless to compare different states");
        return pc != other.pc or ostack != other.ostack;
    }

    State& operator=(State const& from) = default;

    /** Updates own ostack if the other stack has greater
     * requirements.
     */
    void updateMax(State const& other) {
        if (other.ostack > ostack)
            ostack = other.ostack;
    }

    void check() const { assert(ostack >= 0 and "Too many pops"); }

    void advance(Code* code,
                 std::unordered_map<Opcode*, int32_t>& pushContextStackHeight) {
        auto oldPc = pc;
        BC bc = BC::advance(&pc, code);
        // Those two instructions deal with cleanly returning from the function
        // themselves, so we can ignore leftover values on the stack. Note that
        // ret_ should not be added here, as it requires the stack to have
        // *only* the result value left.
        if (bc.bc == Opcode::return_ || bc.bc == Opcode::deopt_)
            ostack = 0;
        else
            ostack -= bc.popCount();
        check();

        if (bc.bc == Opcode::push_context_) {
            uint32_t popCtxt = *reinterpret_cast<Immediate*>(oldPc + 1);
            auto popCtxtPos = pc + popCtxt;
            assert(*popCtxtPos == Opcode::pop_context_ ||
                   *popCtxtPos == Opcode::int3_);
            assert(!pushContextStackHeight.count(popCtxtPos) ||
                   pushContextStackHeight.at(popCtxtPos) == ostack);
            pushContextStackHeight[popCtxtPos] = ostack;
        }

        if (bc.bc == Opcode::pop_context_) {
            int32_t* stackOffset = reinterpret_cast<int32_t*>(oldPc + 1);
            if (*stackOffset == INT_MAX) {
                assert(pushContextStackHeight.count(oldPc));
                *stackOffset = ostack - pushContextStackHeight.at(oldPc);
            }
        }

        ostack += bc.pushCount();
    }

    BC cur(Code* code) { return BC::decode(pc, code); }

    void checkClear() const {
        assert(ostack == 0 and "Stack imbalance when exitting the function");
    }
};

enum class Sources {
    Required,
    May,
    NotNeeded,
};

static Sources hasSources(Opcode bc) {
    switch (bc) {
    case Opcode::extract1_1_:
    case Opcode::extract1_2_:
    case Opcode::extract2_1_:
    case Opcode::extract2_2_:
    case Opcode::extract1_3_:
    case Opcode::add_:
    case Opcode::mul_:
    case Opcode::div_:
    case Opcode::idiv_:
    case Opcode::mod_:
    case Opcode::pow_:
    case Opcode::sub_:
    case Opcode::uplus_:
    case Opcode::uminus_:
    case Opcode::not_:
    case Opcode::lt_:
    case Opcode::gt_:
    case Opcode::le_:
    case Opcode::ge_:
    case Opcode::eq_:
    case Opcode::ne_:
    case Opcode::colon_:
    case Opcode::subassign1_1_:
    case Opcode::subassign2_1_:
    case Opcode::subassign1_2_:
    case Opcode::subassign2_2_:
    case Opcode::subassign1_3_:
        return Sources::Required;

    case Opcode::inc_:
    case Opcode::dec_:
    case Opcode::identical_noforce_:
    case Opcode::push_:
    case Opcode::ldfun_:
    case Opcode::ldddvar_:
    case Opcode::ldvar_:
    case Opcode::ldvar_cached_:
    case Opcode::ldvar_for_update_cache_:
    case Opcode::ldvar_for_update_:
    case Opcode::ldvar_noforce_:
    case Opcode::ldvar_noforce_cached_:
    case Opcode::ldvar_super_:
    case Opcode::ldvar_noforce_super_:
    case Opcode::starg_:
    case Opcode::stvar_:
    case Opcode::starg_cached_:
    case Opcode::stvar_cached_:
    case Opcode::stvar_super_:
    case Opcode::guard_fun_:
    case Opcode::call_:
    case Opcode::call_dots_:
    case Opcode::named_call_:
    case Opcode::static_call_:
    case Opcode::call_builtin_:
    case Opcode::mk_promise_:
    case Opcode::mk_eager_promise_:
    case Opcode::push_code_:
    case Opcode::br_:
    case Opcode::brtrue_:
    case Opcode::beginloop_:
    case Opcode::brfalse_:
    case Opcode::pick_:
    case Opcode::pull_:
    case Opcode::is_:
    case Opcode::istype_:
    case Opcode::put_:
    case Opcode::alloc_:
    case Opcode::ldarg_:
    case Opcode::stloc_:
    case Opcode::movloc_:
    case Opcode::nop_:
    case Opcode::mk_env_:
    case Opcode::mk_stub_env_:
    case Opcode::mk_dotlist_:
    case Opcode::get_env_:
    case Opcode::parent_env_:
    case Opcode::set_env_:
    case Opcode::materialize_env_:
    case Opcode::ret_:
    case Opcode::length_:
    case Opcode::names_:
    case Opcode::set_names_:
    case Opcode::force_:
    case Opcode::pop_:
    case Opcode::popn_:
    case Opcode::close_:
    case Opcode::asast_:
    case Opcode::dup_:
    case Opcode::dup2_:
    case Opcode::for_seq_size_:
    case Opcode::swap_:
    case Opcode::set_shared_:
    case Opcode::ensure_named_:
    case Opcode::return_:
    case Opcode::isfun_:
    case Opcode::invisible_:
    case Opcode::visible_:
    case Opcode::endloop_:
    case Opcode::isstubenv_:
    case Opcode::check_missing_:
    case Opcode::lgl_and_:
    case Opcode::lgl_or_:
    case Opcode::record_call_:
    case Opcode::record_type_:
    case Opcode::record_test_:
    case Opcode::deopt_:
    case Opcode::record_deopt_:
    case Opcode::pop_context_:
    case Opcode::push_context_:
    case Opcode::ceil_:
    case Opcode::floor_:
    case Opcode::clear_binding_cache_:
    case Opcode::ldvar_noforce_stubbed_:
    case Opcode::stvar_stubbed_:
    case Opcode::starg_stubbed_:
    case Opcode::assert_type_:
    case Opcode::update_promise_:
        return Sources::NotNeeded;

    case Opcode::ldloc_:
    case Opcode::aslogical_:
    case Opcode::asbool_:
    case Opcode::missing_:
#define V(NESTED, name, Name)\
    case Opcode::name ## _:\
        return Sources::May;
SIMPLE_INSTRUCTIONS(V, _)
#undef V

    case Opcode::invalid_:
    case Opcode::num_of: {}
    }
    assert(false);
    return Sources::NotNeeded;
}
} // unnamed namespace

void CodeVerifier::calculateAndVerifyStack(Code* code) {
    State max; // max state
    std::map<Opcode*, State> state;
    std::unordered_map<Opcode*, int32_t> pushContextStackHeight;
    std::stack<State> q;

    Opcode* cptr = code->code();
    q.push(State(cptr));

    while (not q.empty()) {
        State i = q.top();
        q.pop();
        if (state.find(i.pc) != state.end()) {
            assert(i.pc >= code->code() && i.pc < code->endCode());
            State current = state[i.pc];
            if (current != i)
                assert(false and "Stack imbalance detected");
            continue;
        }
        while (true) {
            state[i.pc] = i;
            Opcode* pc = i.pc;
            assert(pc >= code->code() && pc < code->endCode());
            BC cur = BC::decode(pc, code);
            i.advance(code, pushContextStackHeight);
            max.updateMax(i);
            if (cur.isExit()) {
                i.checkClear();
                break;
            } else if (cur.bc == Opcode::br_) {
                q.push(State(i, BC::jmpTarget(pc)));
                break;
            } else if (cur.isJmp()) {
                q.push(State(i, BC::jmpTarget(pc)));
                // no break because we want to continue verification in current
                // sequence as well
            }
        }
    }
    code->stackLength = max.ostack;
}

void CodeVerifier::verifyFunctionLayout(SEXP sexp, InterpreterInstance* ctx) {
    if (TYPEOF(sexp) != EXTERNALSXP)
        Rf_error("RIR Verifier: Invalid SEXPTYPE");
    Function* f = Function::unpack(sexp);

    // get the code objects
    std::vector<Code*> objs;
    objs.push_back(f->body());
    for (size_t i = 0; i < f->numArgs; ++i)
        if (f->defaultArg(i))
            objs.push_back(f->defaultArg(i));

    if (f->size > XLENGTH(sexp))
        Rf_error("RIR Verifier: Reported size must be smaller than the size of "
                 "the vector");

    // check that the call instruction has proper arguments and number of
    // instructions is valid
    while (!objs.empty()) {
        auto c = objs.back();
        objs.pop_back();

        if (c->info.magic != CODE_MAGIC)
            Rf_error("RIR Verifier: Invalid code magic number");
        if (c->src == 0)
            Rf_error("RIR Verifier: Code must have AST");
        unsigned oldo = c->stackLength;
        calculateAndVerifyStack(c);
        if (oldo != c->stackLength)
            Rf_error("RIR Verifier: Invalid stack layout reported");

        if (((uintptr_t)(c + 1) + pad4(c->codeSize) +
             c->srcLength * sizeof(Code::SrclistEntry)) == 0)
            Rf_error("RIR Verifier: Invalid code length reported");

        Opcode* cptr = c->code();
        Opcode* start = cptr;
        Opcode* end = start + c->codeSize;
        while (true) {
            if (cptr > end)
                Rf_error("RIR Verifier: Bytecode overflow");
            BC cur = BC::decode(cptr, c);
            switch (hasSources(cur.bc)) {
            case Sources::Required:
                if (c->getSrcIdxAt(cptr, true) == 0)
                    Rf_error("RIR Verifier: Source required but not found");
                break;
            case Sources::NotNeeded:
                if (c->getSrcIdxAt(cptr, true) != 0)
                    Rf_error("RIR Verifier: Sources not needed but stored");
                break;
            case Sources::May: {
            }
            }
            if (*cptr == Opcode::br_ || *cptr == Opcode::brtrue_ ||
                *cptr == Opcode::brfalse_) {
                int off = *reinterpret_cast<int*>(cptr + 1);
                if (cptr + cur.size() + off < start ||
                    cptr + cur.size() + off > end)
                    Rf_error("RIR Verifier: Branch outside closure");
            }
            if (*cptr == Opcode::ldvar_ || *cptr == Opcode::ldvar_noforce_ ||
                *cptr == Opcode::ldvar_super_ ||
                *cptr == Opcode::ldvar_noforce_super_ ||
                *cptr == Opcode::ldvar_for_update_) {
                unsigned* argsIndex = reinterpret_cast<Immediate*>(cptr + 1);
                if (*argsIndex >= cp_pool_length(ctx))
                    Rf_error("RIR Verifier: Invalid arglist index");
                SEXP sym = cp_pool_at(ctx, *argsIndex);
                if (TYPEOF(sym) != SYMSXP)
                    Rf_error("RIR Verifier: load/store binding not a symbol");
                if (!(strlen(CHAR(PRINTNAME(sym)))))
                    Rf_error("RIR Verifier: load/store empty binding name");
            }
            if (*cptr == Opcode::ldvar_cached_ ||
                *cptr == Opcode::stvar_cached_ ||
                *cptr == Opcode::ldvar_noforce_cached_ ||
                *cptr == Opcode::starg_cached_ ||
                *cptr == Opcode::ldvar_for_update_cache_) {
                unsigned* argsIndex = reinterpret_cast<Immediate*>(cptr + 1);
                if (*argsIndex >= cp_pool_length(ctx))
                    Rf_error("RIR Verifier: Invalid arglist index");
                SEXP sym = cp_pool_at(ctx, *argsIndex);
                if (TYPEOF(sym) != SYMSXP)
                    Rf_error("RIR Verifier: load/store binding not a symbol");
                if (!(strlen(CHAR(PRINTNAME(sym)))))
                    Rf_error("RIR Verifier: load/store empty binding name");
                unsigned cacheIdx = *(argsIndex + 1);
                if (cacheIdx >= c->bindingCacheSize)
                    Rf_error(
                        "RIR Verifier: cached load/store with invalid index");
            }
            if (*cptr == Opcode::clear_binding_cache_) {
                unsigned* argsIndex = reinterpret_cast<Immediate*>(cptr + 1);
                unsigned cacheIdxStart = *(argsIndex);
                unsigned cacheIdxSize = *(argsIndex + 1);
                if (cacheIdxStart + cacheIdxSize > c->bindingCacheSize)
                    Rf_error("RIR Verifier: cached clear_binding_cache_ with "
                             "invalid index");
            }

            if (*cptr == Opcode::mk_promise_ ||
                *cptr == Opcode::mk_eager_promise_) {
                unsigned* promidx = reinterpret_cast<Immediate*>(cptr + 1);
                objs.push_back(c->getPromise(*promidx));
            }
            if (*cptr == Opcode::named_call_) {
                uint32_t nargs = *reinterpret_cast<Immediate*>(cptr + 1);
                for (size_t i = 0, e = nargs; i != e; ++i) {
                    uint32_t offset = cur.callExtra().callArgumentNames[i];
                    if (offset) {
                        SEXP name = cp_pool_at(ctx, offset);
                        if (TYPEOF(name) != SYMSXP && name != R_NilValue)
                            Rf_error(
                                "RIR Verifier: Calling target not a symbol");
                    }
                }
            }
            if (*cptr == Opcode::mk_env_ || *cptr == Opcode::mk_stub_env_) {
                uint32_t nargs = *reinterpret_cast<Immediate*>(cptr + 1);
                for (size_t i = 0, e = nargs; i != e; ++i) {
                    uint32_t offset = cur.mkEnvExtra().names[i];
                    SEXP name = cp_pool_at(ctx, offset);
                    if (TYPEOF(name) != SYMSXP && (TYPEOF(name) != LISTSXP &&
                                                   TYPEOF(CAR(name)) != SYMSXP))
                        Rf_error(
                            "RIR Verifier: environment argument not a symbol");
                }
            }

            cptr += cur.size();
            if (cptr == start + c->codeSize) {
                if (!(cur.isJmp() && cur.immediate.offset < 0) &&
                    !(cur.isExit()))
                    Rf_error("RIR Verifier: Last opcode should jump backwards "
                             "or exit");
                break;
            }
        }
    }
}
} // namespace rir
