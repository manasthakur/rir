#include "../analysis/available_checkpoints.h"
#include "../analysis/query.h"
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
void Sandbox::apply(RirCompiler&, ClosureVersion* function,
                    LogStream& log) const {

    AvailableCheckpoints checkpoint(function, log);

    Visitor::run(function->entry, [&](BB* bb) {
        // inSandbox could be part of an analysis but it's very simple
        bool inSandbox = false;
        auto ip = bb->begin();
        auto shouldSandbox = [&](Instruction* i) {
            if (!inSandbox &&
                i->effects.intersects(Effects(Effect::Reflection) |
                                      Effect::ReadsEnv | Effect::WritesEnv |
                                      Effect::LeaksEnv | Effect::ExecuteCode) &&
                i->safeFeedback == ObservedSafe::Safe) {
                if (auto f = Force::Cast(i)) {
                    if (auto mkarg = MkArg::Cast(f->followCastsAndForce())) {
                        auto prom = mkarg->prom();
                        return Query::sandboxable(prom);
                    } else {
                        return true;
                    }
                }
            }
            return false;
        };
        while (ip != bb->end()) {
            auto i = *ip;

            if (BeginSandbox::Cast(i))
                inSandbox = true;
            else if (EndSandbox::Cast(i))
                inSandbox = false;
            else if (shouldSandbox(i)) {
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
