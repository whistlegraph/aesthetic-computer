#pragma once

#include "public.sdk/source/vst/vstaudioeffect.h"
#include <array>
#include <atomic>

namespace AestheticComputer {

//------------------------------------------------------------------------
// ACNotepatProcessor - Audio Processing Component
//------------------------------------------------------------------------
class ACNotepatProcessor : public Steinberg::Vst::AudioEffect
{
public:
    ACNotepatProcessor();
    ~ACNotepatProcessor() override;

    // Create instance
    static Steinberg::FUnknown* createInstance(void*) 
    { 
        return static_cast<Steinberg::Vst::IAudioProcessor*>(new ACNotepatProcessor()); 
    }

    // AudioEffect overrides
    Steinberg::tresult PLUGIN_API initialize(Steinberg::FUnknown* context) override;
    Steinberg::tresult PLUGIN_API terminate() override;
    Steinberg::tresult PLUGIN_API setActive(Steinberg::TBool state) override;
    Steinberg::tresult PLUGIN_API process(Steinberg::Vst::ProcessData& data) override;
    Steinberg::tresult PLUGIN_API setState(Steinberg::IBStream* state) override;
    Steinberg::tresult PLUGIN_API getState(Steinberg::IBStream* state) override;
    Steinberg::tresult PLUGIN_API setupProcessing(Steinberg::Vst::ProcessSetup& setup) override;
    Steinberg::tresult PLUGIN_API canProcessSampleSize(Steinberg::int32 symbolicSampleSize) override;

    // Ring buffer for audio from WebView
    static constexpr size_t kRingBufferSize = 16384;
    std::array<float, kRingBufferSize> ringBufferLeft{};
    std::array<float, kRingBufferSize> ringBufferRight{};
    std::atomic<int> writeIndex{0};
    std::atomic<int> readIndex{0};
    std::atomic<bool> bridgeActive{false};

    void writeAudioSamples(const float* left, const float* right, int numSamples);

protected:
    float volume = 1.0f;
    float room = 0.5f;
    double sampleRate = 44100.0;
};

} // namespace AestheticComputer
