#include "acnotepat.h"
#include "acnotepat_controller.h"
#include "acnotepat_cids.h"
#include "version.h"

#include "public.sdk/source/main/pluginfactory.h"

#define stringSubCategory Steinberg::Vst::PlugType::kInstrumentSynth

BEGIN_FACTORY_DEF(stringCompanyName, stringCompanyURL, stringCompanyEmail)

    // Register the audio processor
    DEF_CLASS2(
        INLINE_UID_FROM_FUID(AestheticComputer::kACNotepatProcessorUID),
        PClassInfo::kManyInstances,
        kVstAudioEffectClass,
        stringPluginName,
        Vst::kDistributable,
        stringSubCategory,
        FULL_VERSION_STR,
        kVstVersionString,
        AestheticComputer::ACNotepatProcessor::createInstance)

    // Register the edit controller
    DEF_CLASS2(
        INLINE_UID_FROM_FUID(AestheticComputer::kACNotepatControllerUID),
        PClassInfo::kManyInstances,
        kVstComponentControllerClass,
        stringPluginName "Controller",
        0,  // not used for controllers
        "",  // not used for controllers
        FULL_VERSION_STR,
        kVstVersionString,
        AestheticComputer::ACNotepatController::createInstance)

END_FACTORY
