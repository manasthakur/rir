#include "ReflectGuard.h"
#include "interpreter/reflect.h"
#include <iostream>

namespace rir {

ReflectGuard newReflectGuard() {
    return freezeEnabled ? ReflectGuard::Introspect : ReflectGuard::None;
}

bool canIntrospect(ReflectGuard guard) {
    return guard < ReflectGuard::Introspect;
}

std::ostream& operator<<(std::ostream& buf, ReflectGuard guard) {
    switch (guard) {
    case ReflectGuard::None:
        buf << "none";
        break;
    case ReflectGuard::Introspect:
        buf << "introspect";
        break;
    default:
        assert(false);
        break;
    }
    return buf;
}

} // namespace rir
