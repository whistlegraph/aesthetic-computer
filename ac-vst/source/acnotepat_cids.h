#pragma once

#include "pluginterfaces/base/funknown.h"
#include "pluginterfaces/vst/vsttypes.h"

namespace AestheticComputer {

// Unique IDs for processor and controller
// Generated UUIDs for AC Notepat
static const Steinberg::FUID kACNotepatProcessorUID(0x12345678, 0xABCD1234, 0x56789ABC, 0xDEF01234);
static const Steinberg::FUID kACNotepatControllerUID(0x87654321, 0xDCBA4321, 0xCBA98765, 0x4321FEDC);

// Parameter IDs
enum ACNotepatParams : Steinberg::Vst::ParamID
{
    kParamVolume = 0,
    kParamRoom = 1,
};

} // namespace AestheticComputer
