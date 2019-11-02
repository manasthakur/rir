#ifndef PIR_DEAD_H
#define PIR_DEAD_H

#include "compiler/pir/pir.h"
#include "compiler/pir/tag.h"
#include <unordered_set>

namespace rir {
namespace pir {

constexpr static std::initializer_list<Tag> TypecheckInstrsList = {
    Tag::IsObject, Tag::IsType, Tag::CastType, Tag::FrameState};
class DeadInstructions {
    std::unordered_set<Instruction*> unused_;

  public:
    enum DeadInstructionsMode {
        CountAll,
        IgnoreUpdatePromise,
        IgnoreTypeTests,
    };

    DeadInstructions(Code*, uint8_t maxBurstSize,
                     DeadInstructionsMode mode = CountAll);
    bool isAlive(Value* v);
    bool isAlive(Instruction* v);
    bool isDead(Value* v);
    bool isDead(Instruction* v);
};

} // namespace pir
} // namespace rir

#endif
