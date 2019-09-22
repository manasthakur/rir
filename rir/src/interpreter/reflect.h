/**
 * reflect.h - Reflection tracking
 */

#pragma once

#include "ir/ReflectGuard.h"
#include <R/r.h>

namespace rir {

extern bool freezeEnabled;

enum class EnvAccessType : unsigned {
    Delete = 0,
    Define = 1,
    Set = 2,
    Get = 3,
};

void willAccessEnv(SEXP env, EnvAccessType typ);

} // namespace rir
