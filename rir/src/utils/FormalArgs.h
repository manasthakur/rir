#pragma once

#include "R/r.h"
#include <vector>

namespace rir {

class FormalArgs {
    std::vector<SEXP> names_;
    std::vector<SEXP> defaultArgs_;
    bool hasDefaultArgs_, hasDots_;
    SEXP original_;

  public:
    FormalArgs(const FormalArgs&) = delete;
    FormalArgs& operator=(const FormalArgs&) = delete;

    explicit FormalArgs(SEXP formals);

    const std::vector<SEXP>& names() const { return names_; }

    const std::vector<SEXP>& defaultArgs() const { return defaultArgs_; }

    bool hasDefaultArgs() const { return hasDefaultArgs_; }

    bool hasDots() const { return hasDots_; }

    size_t nargs() const { return names_.size(); }

    SEXP original() const { return original_; }
};

} // namespace rir
