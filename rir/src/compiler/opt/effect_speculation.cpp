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

    std::unordered_map<Checkpoint*, std::unordered_map<, PirType>> speculate;

    Visitor::run(function->entry, [&](BB* bb) {
        auto ip = bb->begin();
        Checkpoint* sandboxCp = NULL;
        BeginSandbox* sandboxBegin = NULL;
        auto tryEnterSandbox = [&]() {
            assert(sandboxCp == NULL);
            if (auto cp = checkpoint.next(*ip)) {
                // Later instructions will clear the checkpoint because of
                // effects, but since we're sandboxing them those effects will
                // be guarded. So we want the checkpoint at sandbox enter.
                sandboxCp = cp;
                sandboxBegin = new BeginSandbox();
                ip = bb->insert(ip, sandboxBegin);
                ip++;
            }
        };
        auto tryExitSandbox = [&]() {
            if (sandboxCp != NULL) {
                ip++;
                ip = bb->insert(ip, new Assume(sandboxBegin, sandboxCp)->Not());
                sandboxCp = NULL;
            }
        };
        while (ip != bb->end()) {
            auto next = ip + 1;
            auto i = *ip;
            bool sandboxInstr = i->effects.intersects(Effects(Effect::Reflection) | Effect::ReadsEnv | Effect::WritesEnv | Effect::LeaksEnv | Effect::ExecuteCode) &&
                i->hasPureFeedback && !i->isSandboxed()) {
                if (auto cp = checkpoint.next(i)) {
                    i->sandbox(cp);
                }
            }
            ip = next;
        }
        if (inSandbox)
            exitSandbox();
    });
}
} // namespace pir
} // namespace rir
