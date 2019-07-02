#ifndef DEF_INSTR
#error "DEF_INSTR must be defined before including insns.h"
#endif

// DEF_INSTR(name, imm, pop, push, maybe_pure)
// pure = no effects except visibility
// Functions marked maybe_pure=true can explicitly signal "impure" at certain
// points - e.g. ldvar is marked pure, and if it fails to lookup the variable or
// lookup triggers potentially-impure code, it will signal "impure". Functions
// marked maybe_pure=false will signal "impure" immediately before they're
// interpreted

/**
 * invalid_:: Opcode 0 acts as a sentinnel for unintialiazed Code.
 */
DEF_INSTR(invalid_, 0, 0, 0, false)

/**
 * nop_:: do nothing.
 */
DEF_INSTR(nop_, 0, 0, 0, true)

DEF_INSTR(push_context_, 1, 2, 0, false)
DEF_INSTR(pop_context_, 0, 1, 0, false)

/**
 * mk_env_:: create a new environment with the parent and all locals taken
 * from stack and the argument names as immediates.
 */
DEF_INSTR(mk_env_, 2, -1, 1, false)

/*
 * clear_binding_cache_. Clear binding cache entries from start to start+size
 * (two
 * immediates).
 */
DEF_INSTR(clear_binding_cache_, 2, 0, 0, true)

/**
 * make_stub_env_:: create a fake environment for speculative purposes
 */
DEF_INSTR(mk_stub_env_, 2, -1, 1, true)

/**
 * parent_env_:: push lexically outer env to tos.
 * Parent env doesn't cause effects because they're only for local env
 */
DEF_INSTR(parent_env_, 0, 0, 1, false)

/**
 * get_env_:: push current env to tos
 */
DEF_INSTR(get_env_, 0, 0, 1, false)

/**
 * set_env_:: make tos environment the current env
 */
DEF_INSTR(set_env_, 0, 1, 0, false)

/**
 * ldfun_:: take immediate CP index of symbol, find function bound to that name
 * and push it on stack.
 */
DEF_INSTR(ldfun_, 1, 0, 1, true)

/**
 * ldvar_:: take immediate CP index of symbol, finding binding in env and push.
 */
DEF_INSTR(ldvar_, 1, 0, 1, true)

/**
 * ldvar_:: load from the stubbed env at a fixed offset
 */
DEF_INSTR(ldvar_noforce_stubbed_, 1, 0, 1, true)

/**
 * ldvar_:: like ldvar.
 * Stores an additional immediate with a unique number for the cache bindings.
 */
DEF_INSTR(ldvar_cached_, 2, 0, 1, true)

/**
 * ldvar_:: like ldvar.
 * Additionally Increment named count if the variable is not local.
 */
DEF_INSTR(ldvar_for_update_cache_, 2, 0, 1, true)
DEF_INSTR(ldvar_for_update_, 1, 0, 1, true)

/**
 * ldvar_noforce_:: like ldvar_ but don't force if promise or fail if missing
 */
DEF_INSTR(ldvar_noforce_, 1, 0, 1, true)

/**
 * ldvar_noforce_cache:: like ldvar_cache but additionaly stores a unique cache
 * binding number.
 */
DEF_INSTR(ldvar_noforce_cached_, 2, 0, 1, true)

/**
 * ldvar_super_:: take immediate CP index of symbol, finding binding in
 * enclosing env and push.
 */
DEF_INSTR(ldvar_super_, 1, 0, 1, true)

/**
 * ldvar_noforce_super_:: like ldvar_super_ but no force
 */
DEF_INSTR(ldvar_noforce_super_, 1, 0, 1, true)

/**
 * ldddvar_:: loads the ellipsis values (such as ..1, ..2) and pushes them on
 * stack.
 */
DEF_INSTR(ldddvar_, 1, 0, 1, true)

/**
 * ldarg_:: load argument
 */
DEF_INSTR(ldarg_, 1, 0, 1, true)

/**
 * ldloc_:: push local variable on stack
 */
DEF_INSTR(ldloc_, 1, 0, 1, true)

/**
 * stvar_:: assign tos to the immediate symbol.
 */
DEF_INSTR(starg_, 1, 1, 0, false)

/**
 * stvar_:: assign tos to the immediate symbol. May be in cache
 */
DEF_INSTR(starg_cached_, 2, 1, 0, false)

/**
 * stvar_:: assign tos to the immediate symbol. We know it was not previously
 * cached.
 */
DEF_INSTR(stvar_, 1, 1, 0, false)

/**
 * stvar_:: assign tos to the stubbed environment at a fixed offset
 */
DEF_INSTR(stvar_stubbed_, 1, 1, 0, false)

/**
 * stvar_cache:: like stvar but the var may be in the cache.
 */
DEF_INSTR(stvar_cached_, 2, 1, 0, false)

/**
 * stvar_super_:: assign tos to the immediate symbol, lookup starts in the
 * enclosing environment
 */
DEF_INSTR(stvar_super_, 1, 1, 0, false)

/**
 * stloc_:: store top of stack to local variable
 */
DEF_INSTR(stloc_, 1, 1, 0, false)

/**
 * movloc_:: copy one local into another
 */
DEF_INSTR(movloc_, 2, 0, 0, false)

/**
 * call_implicit_:: Takes a list of code objects, which represent the arguments,
 *                  decides on eager/lazy evaluation and does the right thing
 *                  with the code objs
 *                  Expects the callee on TOS
 *                  code objects are passed as immediate arguments
 *
 *                  THIS IS A VARIABLE LENGTH INSTRUCTION
 *                  the actual number of immediates is 3 + nargs
 */
DEF_INSTR(call_implicit_, 4, 1, 1, false)
/*
 * Same as above, but with names for the arguments as immediates
 *
 *                  THIS IS A VARIABLE LENGTH INSTRUCTION
 *                  the actual number of immediates is 3 + 2 * nargs
 */
DEF_INSTR(named_call_implicit_, 4, 1, 1, false)

/**
 * call_:: Like call_implicit_, but expects arguments on stack
 *         on top of the callee; these arguments can be both
 *         values and promises (even preseeded w/ a value)
 */
DEF_INSTR(call_, 4, -1, 1, false)

/*
 * Same as above, but with names for the arguments as immediates
 *
 *                  THIS IS A VARIABLE LENGTH INSTRUCTION
 *                  the actual number of immediates is 3 + nargs
 */
DEF_INSTR(named_call_, 4, -1, 1, false)

/**
 * static_call_:: Like call_, but the callee is statically known
 *                and is accessed via the immediate callsite
 */
DEF_INSTR(static_call_, 6, -1, 1, false)

/**
 * call_builtin_:: Like static call, but calls a builtin
 */
DEF_INSTR(call_builtin_, 3, -1, 1, false)

/**
 * close_:: pop body and argument list, create closure, and push on object stack
 */
DEF_INSTR(close_, 0, 3, 1, true)

/**
 * isfun_:: pop object stack, convert to RIR code or assert error, push code to
 * object stack
 */
DEF_INSTR(isfun_, 0, 1, 1, true)

/**
 * promise_:: take immediate CP index of Code, create promise & push on object
 * stack
 */
DEF_INSTR(promise_, 1, 1, 1, true)

/**
 * force_:: pop from objet stack, evaluate, push promise's value
 */
DEF_INSTR(force_, 0, 1, 1, true)

/**
 * push_:: take immediate CP index, and push CP value on object stack.
 */
DEF_INSTR(push_, 1, 0, 1, true)

/**
 * push_code_:: take immediate code object index, and push code object onto obj
 * stack
 */
DEF_INSTR(push_code_, 1, 0, 1, true)

/**
 * dup_:: pop value from object stack, push it twice
 */
DEF_INSTR(dup_, 0, 1, 2, true)

/**
 * dup2_:: a b -> a b a b
 */
DEF_INSTR(dup2_, 0, 2, 4, true)

/**
 * pop_:: pop from object stack
 */
DEF_INSTR(pop_, 0, 1, 0, true)

/**
 * popn_:: pop n elements from object stack
 */
DEF_INSTR(popn_, 1, -1, 0, true)

/**
 * swap_:: swap two elements tos
 */
DEF_INSTR(swap_, 0, 2, 2, true)

/**
 * put_:: put tos at the n-th pos in the stack
 */
DEF_INSTR(put_, 1, 0, 0, true)

/**
 * pick_:: remove n-th element on stack and push it tos
 */
DEF_INSTR(pick_, 1, 0, 0, true)

/**
 * pull_ :: copy a value from the stack. examples: pull(0) == dup(), pull(1)
 takes 2nd element on stack and pushes it
 */
DEF_INSTR(pull_, 1, 0, 1, true)

/**
 * add_:: pop two values from object stack, add them, push result on object
 * stack. Works on any SEXP.
 */
DEF_INSTR(add_, 0, 2, 1, false)

/**
 * uplus_:: unary plus
 */
DEF_INSTR(uplus_, 0, 1, 1, false)

/**
 * inc_ :: increment tos integer
 */
DEF_INSTR(inc_, 0, 1, 1, true)

/**
 * dec_ :: decrement tos integer
 */
DEF_INSTR(dec_, 0, 1, 1, true)

DEF_INSTR(sub_, 0, 2, 1, false)
DEF_INSTR(uminus_, 0, 1, 1, false)
DEF_INSTR(mul_, 0, 2, 1, false)
DEF_INSTR(div_, 0, 2, 1, false)
DEF_INSTR(idiv_, 0, 2, 1, false)
DEF_INSTR(mod_, 0, 2, 1, false)
DEF_INSTR(pow_, 0, 2, 1, false)

/**
 * lt_:: relational operator <
 */
DEF_INSTR(lt_, 0, 2, 1, false)
DEF_INSTR(gt_, 0, 2, 1, false)
DEF_INSTR(le_, 0, 2, 1, false)
DEF_INSTR(ge_, 0, 2, 1, false)
DEF_INSTR(eq_, 0, 2, 1, false)
DEF_INSTR(ne_, 0, 2, 1, false)

DEF_INSTR(identical_noforce_, 0, 2, 1, true)

/**
 * not_:: unary negation operator !
 */
DEF_INSTR(not_, 0, 1, 1, false)

/**
 * lgl_or_:: computes the logical (ternary) or of the two tos vals
 */
DEF_INSTR(lgl_or_, 0, 2, 1, true)

/**
 * lgl_and_:: computes the logical (ternary) and of the two tos vals
 */
DEF_INSTR(lgl_and_, 0, 2, 1, true)

/**
 * aslogical_:: converts tos to a (ternary) logical
 */
DEF_INSTR(aslogical_, 0, 1, 1, true)

/**
 * asbool_:: pop object stack, convert to Logical vector of size 1 and push on
 * object stack. Throws an error if the result would be NA.
 */
DEF_INSTR(asbool_, 0, 1, 1, true)

/**
 * ceil_ / floor_ :: pop object stack, convert to integer scalar and push. Ceils
 *                   or floors if real, 0 or 1 if logical, throws an NA error if
 *                   another type. For simple ranges.
 */
DEF_INSTR(ceil_, 0, 1, 1, true)
DEF_INSTR(floor_, 0, 1, 1, true)

/**
 * asast_:: pop a promise off the object stack, push its AST on object stack

 TODO: we do not use now, might not work... why?
 */
DEF_INSTR(asast_, 0, 1, 1, false)

/**
 * is_:: immediate type tag (SEXPTYPE), push T/F
 */
DEF_INSTR(is_, 1, 1, 1, true)

/**
 * isobj_:: check if TOS is any kind of object, push T/F
 */
DEF_INSTR(isobj_, 0, 1, 1, true)

/**
 * isstubenv_:: check if TOS is an env stub, push T/F
 */
DEF_INSTR(isstubenv_, 0, 1, 1, true)

/**
 * missing_ :: check if symb is missing
 */
DEF_INSTR(missing_, 1, 0, 1, true)

/**
 * check_missing_ :: check if TOS is missing
 */
DEF_INSTR(check_missing_, 0, 0, 0, true)

/**
 * brobj_:: branch if tos is object
 */
DEF_INSTR(brobj_, 1, 0, 0, true)

/**
 * brtrue_:: pop object stack, if TRUE branch to immediate offset
 */
DEF_INSTR(brtrue_, 1, 1, 0, true)

/**
 * brfalse_:: pop object stack, if FALSE branch to immediate offset
 */
DEF_INSTR(brfalse_, 1, 1, 0, true)

/**
 * br_:: branch to immediate offset
 */
DEF_INSTR(br_, 1, 0, 0, true)

/**
 * extract1_1_:: do a[b], where a and b are on the stack and a is no obj
 */
DEF_INSTR(extract1_1_, 0, 2, 1, false)

/**
 * extract1_2_:: do a[b,c], where a, b and c are on the stack and a is no obj
 */
DEF_INSTR(extract1_2_, 0, 3, 1, false)

/**
 * subassign1_1_ :: a[b] <- c
 *
 * this instruction creates the rhs part of a <- `[<-(a,b,c)` and still needs
 * to be assigned.
 *
 * Warning: on named == 1 it updates the array in-place! This is not the same
 * as GNUR's subassign, and it's not equivalent to `[<-(a,b,c)` itself
 */
DEF_INSTR(subassign1_1_, 0, 3, 1, false)

/**
 * subassign1_2_ :: a[b,c] <- d
 *
 * this instruction creates the rhs part of a <- `[<-(a,b,c,d)` and still needs
 * to be assigned.
 *
 * Warning: on named == 1 it updates the array in-place! This is not the same
 * as GNUR's subassign, and it's not equivalent to `[<-(a,b,c, d)` itself
 */
DEF_INSTR(subassign1_2_, 0, 4, 1, false)

/**
 * extract2_1_:: do a[[b]], where a and b are on the stack and a is no obj
 */
DEF_INSTR(extract2_1_, 0, 2, 1, false)

/**
 * extract2_2_:: do a[[b,c]], where a, b and c are on the stack and a is no obj
 */
DEF_INSTR(extract2_2_, 0, 3, 1, false)

/**
 * subassign2_1 :: a[[b]] <- c
 *
 * this instruction creates the rhs part of a <- `[[<-(a,b,c)` and still needs
 * to be assigned.
 *
 * Warning: on named == 1 it updates the array in-place! This is not the same
 * as GNUR's subassign, and it's not equivalent to `[[<-(a,b,c)` itself
 */
DEF_INSTR(subassign2_1_, 0, 3, 1, false)

/**
 * subassign2_2_ :: a[[b,c]] <- d
 *
 * this instruction creates the rhs part of a <- `[[<-(a,b,c,d)` and still needs
 * to be assigned.
 *
 * Warning: on named == 1 it updates the array in-place! This is not the same
 * as GNUR's subassign, and it's not equivalent to `[[<-(a,b,c,d)` itself
 */
DEF_INSTR(subassign2_2_, 0, 4, 1, false)

/**
 * guard_fun_:: takes symbol, target, id, checks findFun(symbol) == target
 */
DEF_INSTR(guard_fun_, 3, 0, 0, true)

/**
 * seq_ :: seq(scalar, scalar, scalar)
 */
DEF_INSTR(seq_, 0, 3, 1, false)

/**
 * colon_:: takes two bounds a and b and pushes a:b
 */
DEF_INSTR(colon_, 0, 2, 1, false)

/**
 * names_ :: read out names of a vector
 */
DEF_INSTR(names_, 0, 1, 1, true)

/**
 * set_names_ :: set names of a vector, takes vector and names, puts vector back
 */
DEF_INSTR(set_names_, 0, 2, 1, true)

/**
 * alloc_ :: allocate vector. type immediate, length as integer on stack
 */
DEF_INSTR(alloc_, 1, 1, 1, true)

/**
 * length_ :: get length of a vector
 */
DEF_INSTR(length_, 0, 1, 1, true)

/**
 * for_seq_size_ :: get size of the for loop sequence
 */
DEF_INSTR(for_seq_size_, 0, 0, 1, true)

/**
 * visible_:: reset invisible flag
 */
DEF_INSTR(visible_, 0, 0, 0, true)

/**
 * invisible_:: set invisible flag, so that value will not be printed. This is a
 * global flag, many operations implicitly clear it.
 */
DEF_INSTR(invisible_, 0, 0, 0, true)

/**
 * set_shared:: ensures tos has named >= 2
 */
DEF_INSTR(set_shared_, 0, 1, 1, true)

/**
 * ensure_named_:: ensures tos has named >= 1
 */
DEF_INSTR(ensure_named_, 0, 1, 1, true)

/**
 * beginloop_:: begins loop context, break and continue target immediate (this
 * is the target for break and next long jumps)
 */
DEF_INSTR(beginloop_, 1, 0, 0, true)

/**
 * endloop_:: end marker for a loop with context

 */
DEF_INSTR(endloop_, 0, 0, 0, true)

/**
 * return_ :: return instruction. Non-local return instruction as opposed to
 * ret_.
 */
DEF_INSTR(return_, 0, 1, 0, false)

/**
 * ret_:: terminates execution and pops result off object stack
 */
DEF_INSTR(ret_, 0, 1, 0, false)

/**
 * deopt_ :: jumps to the immediate bc location
 */
DEF_INSTR(deopt_, 1, -1, 0, false)

/*
 * recording bytecodes are used to collect information
 * They keep a struct from RuntimeFeedback.h inline, that's why they are quite
 * heavy in size.
 */
DEF_INSTR(record_call_, 4, 1, 1, true)
DEF_INSTR(record_type_, 1, 1, 1, true)

/*
 * pushes a "pure frame" for record_effects or check_effects
 */
DEF_INSTR(start_recording_pure_, 0, 0, 0, true)

/*
 * pops an "pure frame" pushed by a force, and records the effects
 */
DEF_INSTR(record_pure_, 1, 0, 0, true)

/*
 * will stop running code and push true if an impure function is reached
 * within the sandbox. Otherwise pushes false. If these are nested sandboxes,
 * jumps to the outer sandbox on failure
 */
DEF_INSTR(begin_sandbox_, 0, 0, 1, true)

/**
 * ends the sandbox, so unless there's an outer sandbox, impure code can be run
 */
DEF_INSTR(end_sandbox_, 0, 0, 0, true)

DEF_INSTR(int3_, 0, 0, 0, true)
DEF_INSTR(printInvocation_, 0, 0, 0, true)

/*
 * assert_type_ :: asserts that tos has the immediate PIR type
 */
DEF_INSTR(assert_type_, 3, 1, 1, true)

#undef DEF_INSTR
