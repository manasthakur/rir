#ifndef API_H_
#define API_H_

#include "R/r.h"
#include "compiler/debugging/debugging.h"
#include "runtime/Assumptions.h"
#include <stdint.h>

#define REXPORT extern "C"

extern int R_ENABLE_JIT;

REXPORT SEXP rir_invocation_count(SEXP what);
REXPORT SEXP rir_eval(SEXP exp, SEXP env);
REXPORT SEXP pir_compile(SEXP closure, SEXP name, SEXP debugFlags,
                         SEXP debugStyle);
REXPORT SEXP rir_compile(SEXP what, SEXP env);
SEXP pirCompile(SEXP closure,
                const std::vector<rir::Assumptions>& assumptionsVec,
                const std::string& name, const rir::pir::DebugOptions&);
extern SEXP rirOptDefaultOpts(SEXP closure,
                              const std::vector<rir::Assumptions>& assumptions,
                              SEXP name, bool dryRun);

#endif // API_H_
