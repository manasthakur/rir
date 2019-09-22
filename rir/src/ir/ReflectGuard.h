#pragma once

#include "compiler/parameter.h"
#include <R/r.h>
#include <stdio.h>

namespace rir {

// If the guard is broken, will retry the expression if in "rir.freeze",
// otherwise will abort. Higher values imply lower ones.
enum class ReflectGuard : unsigned {
    None = 0,
    // Block delete/define/set but allow get
    Introspect = 1
};

// The reflect guard applied to new compiled closures
ReflectGuard newReflectGuard();
bool canIntrospect(ReflectGuard guard);

std::ostream& operator<<(std::ostream& buf, ReflectGuard guard);

} // namespace rir
