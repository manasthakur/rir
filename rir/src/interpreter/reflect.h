#pragma once

#include "compiler/parameter.h"
#include <R/r.h>
#include <stdio.h>

namespace rir {

enum class EnvAccessType : unsigned {
    Delete = 0,
    Define = 1,
    Set = 2,
    Get = 3,
};

bool canPerformReflection(ReflectGuard guard);

void willAccessEnv(SEXP env, EnvAccessType typ);

std::ostream& operator<<(std::ostream& buf, ReflectGuard guard);

} // namespace rir
