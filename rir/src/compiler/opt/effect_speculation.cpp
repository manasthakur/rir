#include "../analysis/available_checkpoints.h"
#include "../pir/pir_impl.h"
#include "../transform/bb.h"
#include "../util/cfg.h"
#include "../util/visitor.h"
#include "R/r.h"
#include "pass_definitions.h"

#include <unordered_map>

namespace rir {
namespace pir {

// Derived from TypeSpeculation
void EffectSpeculation::apply(RirCompiler&, ClosureVersion* function,
                              LogStream& log) const {

    AvailableCheckpoints checkpoint(function, log);

    Visitor::run(function->entry, [&](BB* bb) {
        auto ip = bb->begin();
        while (ip != bb->end()) {
            auto next = ip + 1;
            auto i = *ip;
            if (Force::Cast(i) && i->hasPureFeedback && !i->isSandboxed()) {
                if (auto cp = checkpoint.next(i)) {
                    i->sandbox(cp);
                }
            }
            ip = next;
        }
    });
}
} // namespace pir
} // namespace rir
