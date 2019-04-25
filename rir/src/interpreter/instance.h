#ifndef interpreter_context_h
#define interpreter_context_h

#include "R/r.h"
#include "ir/BC_inc.h"
#include "runtime/Assumptions.h"

#include "interp_incl.h"

#include <stdio.h>

#include <assert.h>
#include <functional>
#include <stdint.h>

#include <unordered_map>
namespace rir {

// --- Misc Declarations

const static SEXP loopTrampolineMarker = (SEXP)0x7007;

/** Compiler API. Given a language object, compiles it and returns the
  EXTERNALSXP containing the Function and its Code objects.

  The idea is to call this if we want on demand compilation of closures.
 */
typedef std::function<SEXP(SEXP expr, SEXP env)> ExprCompiler;
typedef std::function<SEXP(SEXP closure, SEXP name)> ClosureCompiler;
typedef std::function<SEXP(SEXP closure, const rir::Assumptions& assumptions,
                           SEXP name)>
    ClosureOptimizer;

#define POOL_CAPACITY 4096
#define STACK_CAPACITY 4096

/** Resizeable R list.

 Allocates large list and then tricks R into believing that the list is actually
 smaller.

 This works because R uses non-moving GC and is used for constant and source
 pools as well as the interpreter object stack.
 */
typedef struct {
    SEXP list;
    size_t capacity;
} ResizeableList;

#define CONTEXT_INDEX_CP 0
#define CONTEXT_INDEX_SRC 1

/** Interpreter's context.

 Interpreter's context is a list (so that it will be marked by R's gc) that
 contains the SEXP pools and stack as well as other stacks that do not need to
 be gc'd.

 */

struct InterpreterInstance {
    SEXP list;
    ResizeableList cp;
    ResizeableList src;
    ExprCompiler exprCompiler;
    ClosureCompiler closureCompiler;
    ClosureOptimizer closureOptimizer;
};

// --- Resizable List

// TODO we might actually need to do more for the lengths (i.e. true length vs
// length)

RIR_INLINE size_t rl_length(ResizeableList* l) { return Rf_length(l->list); }

RIR_INLINE void rl_setLength(ResizeableList* l, size_t length) {
    ((VECSEXP)l->list)->vecsxp.length = length;
    ((VECSEXP)l->list)->vecsxp.truelength = length;
}

RIR_INLINE void rl_grow(ResizeableList* l, SEXP parent, size_t index) {
    int oldsize = rl_length(l);
    SEXP n = Rf_allocVector(VECSXP, l->capacity * 2);
    memcpy(DATAPTR(n), DATAPTR(l->list), l->capacity * sizeof(SEXP));
    SET_VECTOR_ELT(parent, index, n);
    l->list = n;
    rl_setLength(l, oldsize);
    l->capacity *= 2;
}

RIR_INLINE void rl_append(ResizeableList* l, SEXP val, SEXP parent,
                          size_t index) {
    size_t i = rl_length(l);
    if (i == l->capacity) {
        PROTECT(val);
        rl_grow(l, parent, index);
        UNPROTECT(1);
    }
    rl_setLength(l, i + 1);
    SET_VECTOR_ELT(l->list, i, val);
}

// --- Stack

// Determines whether simple scalars are represented unboxed
// Otherwise they'll be STACK_OBJ_SEXPs like other expressions

#ifdef PROFILE_TYPED_STACK

class TSProfile {
  public:
    std::unordered_map<std::string, size_t> counter;

    void mark(const std::string& event) { counter[event]++; }

    ~TSProfile() {
        std::cout << "Typed stack profile\n";
        for (auto& o : counter) {
            std::cout << o.first << ": " << o.second << "\n";
        }
    }
};

TSProfile TSPROFILE;

#endif

#define INTEGER_TO_REAL(x) ((x) == NA_INTEGER ? NA_REAL : (x))
#define INTEGER_TO_LOGICAL(x)                                                  \
    ((x) == NA_INTEGER ? NA_LOGICAL : (x) ? TRUE : FALSE)
#define LOGICAL_TO_REAL(x) ((x) == NA_LOGICAL ? NA_REAL : (x))

typedef enum {
    STACK_OBJ_SEXP = 0,
    STACK_OBJ_INT = INTSXP,
    STACK_OBJ_REAL = REALSXP,
    STACK_OBJ_LOGICAL = LGLSXP,
    STACK_OBJ_NULL = -1,
} stackObjType;

static const R_bcstack_t nullStackObj = {STACK_OBJ_NULL, {0xBAD}};
typedef union {
    double dval;
    int ival;
} scalar_value_t;

RIR_INLINE R_bcstack_t sexpStackObj(SEXP value) {
    R_bcstack_t res;
    res.tag = STACK_OBJ_SEXP;
    res.u.sxpval = value;
    return res;
}

RIR_INLINE R_bcstack_t intStackObj(int x) {
    return sexpStackObj(ScalarInteger(x));
}

RIR_INLINE R_bcstack_t realStackObj(double x) {
    return sexpStackObj(ScalarReal(x));
}

RIR_INLINE R_bcstack_t logicalStackObj(int x) {
    return sexpStackObj(ScalarLogical(x));
}

RIR_INLINE R_bcstack_t sexpStackObjTryUnbox(SEXP x) { return sexpStackObj(x); }

RIR_INLINE SEXP stackObjToSexp(R_bcstack_t* stackCell) {
    return stackCell->u.sxpval;
}

RIR_INLINE SEXP stackObjAsSexp(R_bcstack_t* stackCell) {
    return stackCell->u.sxpval;
}

// Returns NA_INTEGER if not an integer, doesn't consider reals integers
RIR_INLINE int stackObjAsInteger(R_bcstack_t* stackCell) {
    if (IS_SIMPLE_SCALAR(stackCell->u.sxpval, INTSXP)) {
        return *INTEGER(stackCell->u.sxpval);
    } else {
        return NA_INTEGER;
    }
}

// Returns NA_REAL if not a real, doesn't consider integers reals
RIR_INLINE double stackObjAsReal(R_bcstack_t* stackCell) {
    if (IS_SIMPLE_SCALAR(stackCell->u.sxpval, REALSXP)) {
        return *REAL(stackCell->u.sxpval);
    } else {
        return NA_REAL;
    }
}

// Returns NA_LOGICAL if not a logical
RIR_INLINE int stackObjAsLogical(R_bcstack_t* stackCell) {
    if (IS_SIMPLE_SCALAR(stackCell->u.sxpval, LGLSXP)) {
        return *LOGICAL(stackCell->u.sxpval);
    } else {
        return NA_LOGICAL;
    }
}

RIR_INLINE int stackObjAsLglScalar(R_bcstack_t* stackCell) {
    return Rf_asLogical(stackCell->u.sxpval);
}

// Doesn't consider reals integers
RIR_INLINE bool stackObjIsInteger(R_bcstack_t* stackCell) {
    return IS_SIMPLE_SCALAR(stackCell->u.sxpval, INTSXP);
}

// Doesn't consider integers reals
RIR_INLINE bool stackObjIsReal(R_bcstack_t* stackCell) {
    return IS_SIMPLE_SCALAR(stackCell->u.sxpval, REALSXP);
}

RIR_INLINE bool stackObjIsLogical(R_bcstack_t* stackCell) {
    return IS_SIMPLE_SCALAR(stackCell->u.sxpval, LGLSXP);
}

RIR_INLINE bool stackObjIsSimpleScalar(R_bcstack_t* stackCell, SEXPTYPE type) {
    return IS_SIMPLE_SCALAR(stackCell->u.sxpval, type);
}

RIR_INLINE bool stackObjIsScalar(R_bcstack_t* stackCell) {
    return stackCell->u.sxpval->sxpinfo.scalar;
}

RIR_INLINE SEXPTYPE stackObjIsBoxed(R_bcstack_t* stackCell) { return true; }

RIR_INLINE bool stackObjIsVector(R_bcstack_t* stackCell) {
    return Rf_isVector(stackCell->u.sxpval);
}

RIR_INLINE bool stackObjIsObject(R_bcstack_t* stackCell) {
    return isObject(stackCell->u.sxpval);
}

RIR_INLINE bool stackObjsIdentical(R_bcstack_t* x, R_bcstack_t* y) {
    return x->u.sxpval == y->u.sxpval;
}

// Fails if not a logical or NA
RIR_INLINE int tryStackObjToLogicalNa(R_bcstack_t* stackCell) {
    return XLENGTH(stackCell->u.sxpval) == 0 ? NA_LOGICAL
                                             : *LOGICAL(stackCell->u.sxpval);
}

// Returns regular if int, truncated if real, -1 otherwise
RIR_INLINE int tryStackObjToIdx(R_bcstack_t* stackCell) {
    if (stackObjIsInteger(stackCell)) {
        return stackObjAsInteger(stackCell) - 1;
    } else if (stackObjIsReal(stackCell)) {
        return (int)stackObjAsReal(stackCell) - 1;
    } else {
        return -1;
    }
}

RIR_INLINE SEXPTYPE stackObjTypeof(R_bcstack_t* stackCell) {
    return TYPEOF(stackCell->u.sxpval);
}

static R_INLINE int tryStackScalar(R_bcstack_t* stackCell, scalar_value_t* v,
                                   SEXP* pv) {
    return 0;
}

static R_INLINE int tryStackScalarReal(R_bcstack_t* stackCell,
                                       scalar_value_t* v, SEXP* pv) {
    return 0;
}

RIR_INLINE R_xlen_t stackObjLength(R_bcstack_t* stackCell) {
    return XLENGTH(stackCell->u.sxpval);
}

#define ostackLength(c) (R_BCNodeStackTop - R_BCNodeStackBase)

#define ostackTop(c) *(R_BCNodeStackTop - 1)
#define ostackCellTop(c) ostackCellAt(c, 0)
#define ostackAt(c, i) *(R_BCNodeStackTop - 1 - (i))
#define ostackCellAt(c, i) (R_BCNodeStackTop - 1 - (i))

#define ostackSet(c, i, v) *(R_BCNodeStackTop - 1 - (i)) = (v)

#define ostackSetInt(c, i, v) ostackSetSexp(c, i, ScalarInteger(v))

#define ostackSetReal(c, i, v) ostackSetSexp(c, i, ScalarReal(v))

#define ostackSetLogical(c, i, v) ostackSetSexp(c, i, ScalarLogical(v))

#define ostackSetSexp(c, i, v)                                                 \
    do {                                                                       \
        R_bcstack_t* stk = R_BCNodeStackTop - 1 - (i);                         \
        stk->u.sxpval = (v);                                                   \
        stk->tag = STACK_OBJ_SEXP;                                             \
    } while (0)

#define ostackPopn(c, p)                                                       \
    do {                                                                       \
        R_BCNodeStackTop -= (p);                                               \
    } while (0)

#define ostackPop(c) (*(--R_BCNodeStackTop))
#define ostackCellPop(c) (--R_BCNodeStackTop)

#define ostackPush(c, v)                                                       \
    do {                                                                       \
        *R_BCNodeStackTop = (v);                                               \
        ++R_BCNodeStackTop;                                                    \
    } while (0)

#define ostackPushInt(c, v) ostackPushSexp(c, ScalarInteger(v))

#define ostackPushReal(c, v) ostackPushSexp(c, ScalarReal(v))

#define ostackPushLogical(c, v) ostackPushSexp(c, ScalarLogical(v))

#define ostackPushSexp(c, v)                                                   \
    do {                                                                       \
        R_BCNodeStackTop->u.sxpval = (v);                                      \
        R_BCNodeStackTop->tag = STACK_OBJ_SEXP;                                \
        ++R_BCNodeStackTop;                                                    \
    } while (0)

RIR_INLINE SEXP ostackSexpAt(InterpreterInstance* ctx, unsigned idx) {
    return stackObjToSexp(ostackCellAt(ctx, idx));
}

RIR_INLINE SEXP ostackPopSexp(InterpreterInstance* ctx) {
    return stackObjToSexp(ostackCellPop(ctx));
}

RIR_INLINE SEXP ostackTopSexp(InterpreterInstance* ctx) {
    return stackObjToSexp(ostackCellTop(ctx));
}

RIR_INLINE void ostackEnsureSize(InterpreterInstance* ctx, unsigned minFree) {
    if ((R_BCNodeStackTop + minFree) >= R_BCNodeStackEnd) {
        // TODO....
        assert(false);
    }
}

// --- Locals

class Locals final {
    // NOTE: must not own any resources, because the destructor is not
    // called if there is a longjmp from the evalRirCode call
  private:
    R_bcstack_t* base;
    unsigned localsCount;
    bool existingLocals;

  public:
    explicit Locals(R_bcstack_t* base, unsigned count, bool existingLocals)
        : base(base), localsCount(count), existingLocals(existingLocals) {
        if (!existingLocals)
            R_BCNodeStackTop += localsCount;
    }

    ~Locals() {
        if (!existingLocals)
            R_BCNodeStackTop -= localsCount;
    }

    R_bcstack_t load(unsigned offset) {
        SLOWASSERT(offset < localsCount &&
                   "Attempt to load invalid local variable.");
        return *(base + offset);
    }

    void store(unsigned offset, R_bcstack_t val) {
        SLOWASSERT(offset < localsCount &&
                   "Attempt to store invalid local variable.");
        *(base + offset) = val;
    }

    Locals(Locals const&) = delete;
    Locals(Locals&&) = delete;
    Locals& operator=(Locals const&) = delete;
    Locals& operator=(Locals&&) = delete;
    static void* operator new(size_t) = delete;
};

// --- Context

InterpreterInstance* context_create();

#define cp_pool_length(c) (rl_length(&(c)->cp))
#define src_pool_length(c) (rl_length(&(c)->src))

RIR_INLINE size_t cp_pool_add(InterpreterInstance* c, SEXP v) {
    size_t result = rl_length(&c->cp);
    rl_append(&c->cp, v, c->list, CONTEXT_INDEX_CP);
    return result;
}

RIR_INLINE size_t src_pool_add(InterpreterInstance* c, SEXP v) {
    size_t result = rl_length(&c->src);
    rl_append(&c->src, v, c->list, CONTEXT_INDEX_SRC);
    return result;
}

RIR_INLINE SEXP cp_pool_at(InterpreterInstance* c, unsigned index) {
    SLOWASSERT(c->cp.capacity > index);
    return VECTOR_ELT(c->cp.list, index);
}

RIR_INLINE SEXP src_pool_at(InterpreterInstance* c, unsigned index) {
    SLOWASSERT(c->src.capacity > index);
    return VECTOR_ELT(c->src.list, index);
}

RIR_INLINE void cp_pool_set(InterpreterInstance* c, unsigned index, SEXP e) {
    SLOWASSERT(c->cp.capacity > index);
    SET_VECTOR_ELT(c->cp.list, index, e);
}

} // namespace rir

#endif // interpreter_context_h
