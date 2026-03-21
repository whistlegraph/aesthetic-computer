#include "acnotepat.h"
#include "acnotepat_cids.h"

#include "base/source/fstreamer.h"
#include "pluginterfaces/vst/ivstparameterchanges.h"
#include "pluginterfaces/vst/ivstevents.h"

using namespace Steinberg;
using namespace Steinberg::Vst;

namespace AestheticComputer {

//------------------------------------------------------------------------
ACNotepatProcessor::ACNotepatProcessor()
{
    setControllerClass(kACNotepatControllerUID);
}

//------------------------------------------------------------------------
ACNotepatProcessor::~ACNotepatProcessor()
{
}

//------------------------------------------------------------------------
tresult PLUGIN_API ACNotepatProcessor::initialize(FUnknown* context)
{
    tresult result = AudioEffect::initialize(context);
    if (result != kResultOk)
        return result;

    // Add stereo audio output (this is a synth - no input needed)
    addAudioOutput(STR16("Stereo Out"), SpeakerArr::kStereo);

    // Add event input for MIDI
    addEventInput(STR16("Event In"), 1);

    return kResultOk;
}

//------------------------------------------------------------------------
tresult PLUGIN_API ACNotepatProcessor::terminate()
{
    return AudioEffect::terminate();
}

//------------------------------------------------------------------------
tresult PLUGIN_API ACNotepatProcessor::setActive(TBool state)
{
    if (state)
    {
        // Reset ring buffer on activation
        writeIndex.store(0);
        readIndex.store(0);
    }
    return AudioEffect::setActive(state);
}

//------------------------------------------------------------------------
tresult PLUGIN_API ACNotepatProcessor::setupProcessing(ProcessSetup& setup)
{
    sampleRate = setup.sampleRate;
    return AudioEffect::setupProcessing(setup);
}

//------------------------------------------------------------------------
tresult PLUGIN_API ACNotepatProcessor::canProcessSampleSize(int32 symbolicSampleSize)
{
    // We only support 32-bit float
    return symbolicSampleSize == kSample32 ? kResultTrue : kResultFalse;
}

//------------------------------------------------------------------------
tresult PLUGIN_API ACNotepatProcessor::process(ProcessData& data)
{
    // Handle parameter changes
    if (data.inputParameterChanges)
    {
        int32 numParamsChanged = data.inputParameterChanges->getParameterCount();
        for (int32 i = 0; i < numParamsChanged; i++)
        {
            IParamValueQueue* paramQueue = data.inputParameterChanges->getParameterData(i);
            if (paramQueue)
            {
                ParamValue value;
                int32 sampleOffset;
                int32 numPoints = paramQueue->getPointCount();
                
                if (paramQueue->getPoint(numPoints - 1, sampleOffset, value) == kResultTrue)
                {
                    switch (paramQueue->getParameterId())
                    {
                        case kParamVolume:
                            volume = static_cast<float>(value);
                            break;
                        case kParamRoom:
                            room = static_cast<float>(value);
                            break;
                    }
                }
            }
        }
    }

    // Process audio
    if (data.numOutputs == 0 || data.outputs[0].numChannels < 2)
        return kResultOk;

    float* outLeft = data.outputs[0].channelBuffers32[0];
    float* outRight = data.outputs[0].channelBuffers32[1];
    int32 numSamples = data.numSamples;

    if (bridgeActive.load())
    {
        // Read from ring buffer (audio from WebView)
        int rIdx = readIndex.load();
        int wIdx = writeIndex.load();

        for (int32 i = 0; i < numSamples; ++i)
        {
            if (rIdx != wIdx)
            {
                outLeft[i] = ringBufferLeft[rIdx] * volume;
                outRight[i] = ringBufferRight[rIdx] * volume;
                rIdx = (rIdx + 1) % kRingBufferSize;
            }
            else
            {
                // Buffer underrun
                outLeft[i] = 0.0f;
                outRight[i] = 0.0f;
            }
        }
        readIndex.store(rIdx);
    }
    else
    {
        // No bridge active - silence
        for (int32 i = 0; i < numSamples; ++i)
        {
            outLeft[i] = 0.0f;
            outRight[i] = 0.0f;
        }
    }

    // Process MIDI events if needed
    if (data.inputEvents)
    {
        int32 numEvents = data.inputEvents->getEventCount();
        for (int32 i = 0; i < numEvents; ++i)
        {
            Event event;
            if (data.inputEvents->getEvent(i, event) == kResultOk)
            {
                // TODO: Forward MIDI to WebView
                if (event.type == Event::kNoteOnEvent)
                {
                    // Note on: event.noteOn.pitch, event.noteOn.velocity
                }
                else if (event.type == Event::kNoteOffEvent)
                {
                    // Note off: event.noteOff.pitch
                }
            }
        }
    }

    return kResultOk;
}

//------------------------------------------------------------------------
void ACNotepatProcessor::writeAudioSamples(const float* left, const float* right, int numSamples)
{
    int wIdx = writeIndex.load();
    
    for (int i = 0; i < numSamples; ++i)
    {
        ringBufferLeft[wIdx] = left[i];
        ringBufferRight[wIdx] = right[i];
        wIdx = (wIdx + 1) % kRingBufferSize;
    }
    
    writeIndex.store(wIdx);
}

//------------------------------------------------------------------------
tresult PLUGIN_API ACNotepatProcessor::setState(IBStream* state)
{
    IBStreamer streamer(state, kLittleEndian);
    
    float savedVolume = 0.f;
    if (!streamer.readFloat(savedVolume))
        return kResultFalse;
    volume = savedVolume;

    float savedRoom = 0.f;
    if (!streamer.readFloat(savedRoom))
        return kResultFalse;
    room = savedRoom;

    return kResultOk;
}

//------------------------------------------------------------------------
tresult PLUGIN_API ACNotepatProcessor::getState(IBStream* state)
{
    IBStreamer streamer(state, kLittleEndian);
    
    streamer.writeFloat(volume);
    streamer.writeFloat(room);

    return kResultOk;
}

} // namespace AestheticComputer
