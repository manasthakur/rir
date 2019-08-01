#include "FormalArgs.h"
#include "../R/RList.h"

namespace rir {

FormalArgs::FormalArgs(SEXP formals)
    : hasDefaultArgs_(false), hasDots_(false), original_(formals) {
    for (auto it = RList(formals).begin(); it != RList::end(); ++it) {
        names_.push_back(it.tag());
        defaultArgs_.push_back(*it);

        if (it.tag() == R_DotsSymbol)
            hasDots_ = true;
        if (*it != R_MissingArg)
            hasDefaultArgs_ = true;
    }
}

} // namespace rir
