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
        // inSandbox could be part of an analysis but it's very simple
        bool inSandbox = false;
        auto ip = bb->begin();
        while (ip != bb->end()) {
            auto i = *ip;

            if (BeginSandbox::Cast(i))
                inSandbox = true;
            else if (EndSandbox::Cast(i))
                inSandbox = false;
            else if (i->isSandboxable() && !inSandbox &&
                     i->effects.intersects(
                         Effects(Effect::Reflection) | Effect::ReadsEnv |
                         Effect::WritesEnv | Effect::LeaksEnv |
                         Effect::ExecuteCode) &&
                     i->hasPureFeedback) {
                if (auto cp = checkpoint.at(i)) {
                    i->sandbox(cp);
                    ip = bb->insert(ip, new BeginSandbox());
                    ip++;
                    ip++;
                    BBTransform::insertAssume(new EndSandbox(), cp, bb, ip,
                                              true);
                }
            }
            ip++;
        }
        assert(!inSandbox);
    });
}
} // namespace pir
} // namespace rir
