/*
  ==============================================================================
    AC Notepat VST Plugin - Audio Processor Implementation
  ==============================================================================
*/

#include "PluginProcessor.h"
#include "PluginEditor.h"

ACNotepatProcessor::ACNotepatProcessor()
    : AudioProcessor(BusesProperties()
                     .withOutput("Output", juce::AudioChannelSet::stereo(), true))
    , parameters(*this, nullptr, "PARAMETERS", createParameterLayout())
{
}

ACNotepatProcessor::~ACNotepatProcessor()
{
}

juce::AudioProcessorValueTreeState::ParameterLayout ACNotepatProcessor::createParameterLayout()
{
    std::vector<std::unique_ptr<juce::RangedAudioParameter>> params;
    
    // Room/Reverb amount (0-100%)
    params.push_back(std::make_unique<juce::AudioParameterFloat>(
        "room", "Room", 0.0f, 100.0f, 50.0f));
    
    // Wave type (0=sine, 1=triangle, 2=sawtooth, 3=square)
    params.push_back(std::make_unique<juce::AudioParameterChoice>(
        "wave", "Wave Type", 
        juce::StringArray{"Sine", "Triangle", "Sawtooth", "Square"}, 0));
    
    // Octave offset (-2 to +2)
    params.push_back(std::make_unique<juce::AudioParameterInt>(
        "octave", "Octave Offset", -2, 2, 0));
    
    // Slide mode
    params.push_back(std::make_unique<juce::AudioParameterBool>(
        "slide", "Slide Mode", false));

    return { params.begin(), params.end() };
}

void ACNotepatProcessor::prepareToPlay(double sampleRate, int samplesPerBlock)
{
    // Prepare for playback
}

void ACNotepatProcessor::releaseResources()
{
    // Release resources
}

void ACNotepatProcessor::processBlock(juce::AudioBuffer<float>& buffer, juce::MidiBuffer& midiMessages)
{
    // Get write pointers for output
    auto* leftChannel = buffer.getWritePointer(0);
    auto* rightChannel = buffer.getWritePointer(1);
    int numSamples = buffer.getNumSamples();
    
    // Read audio from ring buffer if VST bridge is active
    if (vstBridgeActive.load())
    {
        int readIdx = audioReadIndex.load();
        int writeIdx = audioWriteIndex.load();
        
        for (int i = 0; i < numSamples; ++i)
        {
            // Check if we have samples available
            if (readIdx != writeIdx)
            {
                leftChannel[i] = audioRingBufferLeft[readIdx];
                rightChannel[i] = audioRingBufferRight[readIdx];
                readIdx = (readIdx + 1) % audioRingBufferSize;
            }
            else
            {
                // Buffer underrun - output silence
                leftChannel[i] = 0.0f;
                rightChannel[i] = 0.0f;
            }
        }
        
        audioReadIndex.store(readIdx);
    }
    else
    {
        // No VST bridge - clear output
        buffer.clear();
    }
    
    // Process MIDI messages and convert to keyboard events for notepat
    for (const auto metadata : midiMessages)
    {
        const auto msg = metadata.getMessage();
        
        if (msg.isNoteOn())
        {
            int note = msg.getNoteNumber();
            if (activeNotes.find(note) == activeNotes.end())
            {
                activeNotes.insert(note);
                auto key = midiNoteToNotepatKey(note);
                if (key.isNotEmpty())
                {
                    juce::ScopedLock sl(keyEventLock);
                    pendingKeyEvents.push({key, true});
                }
            }
        }
        else if (msg.isNoteOff())
        {
            int note = msg.getNoteNumber();
            activeNotes.erase(note);
            auto key = midiNoteToNotepatKey(note);
            if (key.isNotEmpty())
            {
                juce::ScopedLock sl(keyEventLock);
                pendingKeyEvents.push({key, false});
            }
        }
        else if (msg.isController())
        {
            int cc = msg.getControllerNumber();
            int value = msg.getControllerValue();
            
            // CC1 (Mod Wheel) -> Room
            if (cc == 1)
            {
                if (auto* param = parameters.getParameter("room"))
                    param->setValueNotifyingHost(value / 127.0f);
            }
            // CC74 (Brightness) -> Cycle wave type
            else if (cc == 74 && value > 64)
            {
                if (auto* param = parameters.getParameter("wave"))
                {
                    int current = (int)param->getValue() * 3;
                    param->setValueNotifyingHost(((current + 1) % 4) / 3.0f);
                }
            }
        }
    }
}

juce::String ACNotepatProcessor::midiNoteToNotepatKey(int midiNote)
{
    // Get octave offset parameter
    int octaveOffset = 0;
    if (auto* param = parameters.getRawParameterValue("octave"))
        octaveOffset = (int)*param;
    
    // Adjust note by octave offset
    midiNote += octaveOffset * 12;
    
    // Base octave starts at C3 (MIDI 48)
    // Notepat keyboard layout:
    // Lower row: z x c v b n m , . /  (C D E F G A B C D E)
    // Upper row: a s d f g h j k l ;  (C# D# F# G# A# C# D# F# G# A#)
    
    int noteInOctave = midiNote % 12;
    int octave = (midiNote / 12) - 4; // Relative to C4
    
    // Map note to key based on octave
    static const char* naturalKeys[] = {"z", "x", "c", "v", "b", "n", "m", ",", ".", "/"};
    static const char* sharpKeys[] = {"a", "s", "", "d", "f", "", "g", "h", "", "j", "k", ""};
    
    // Natural notes: C(0), D(2), E(4), F(5), G(7), A(9), B(11)
    // Sharp notes: C#(1), D#(3), F#(6), G#(8), A#(10)
    
    bool isSharp = (noteInOctave == 1 || noteInOctave == 3 || noteInOctave == 6 || 
                    noteInOctave == 8 || noteInOctave == 10);
    
    if (isSharp)
    {
        int sharpIndex = 0;
        switch (noteInOctave)
        {
            case 1: sharpIndex = 0; break;  // C#
            case 3: sharpIndex = 1; break;  // D#
            case 6: sharpIndex = 3; break;  // F#
            case 8: sharpIndex = 4; break;  // G#
            case 10: sharpIndex = 6; break; // A#
        }
        // Adjust for octave
        sharpIndex += octave * 7;
        if (sharpIndex >= 0 && sharpIndex < 10)
            return sharpKeys[sharpIndex];
    }
    else
    {
        int naturalIndex = 0;
        switch (noteInOctave)
        {
            case 0: naturalIndex = 0; break;  // C
            case 2: naturalIndex = 1; break;  // D
            case 4: naturalIndex = 2; break;  // E
            case 5: naturalIndex = 3; break;  // F
            case 7: naturalIndex = 4; break;  // G
            case 9: naturalIndex = 5; break;  // A
            case 11: naturalIndex = 6; break; // B
        }
        // Adjust for octave
        naturalIndex += octave * 7;
        if (naturalIndex >= 0 && naturalIndex < 10)
            return naturalKeys[naturalIndex];
    }
    
    return {};
}

void ACNotepatProcessor::writeAudioSamples(const float* left, const float* right, int numSamples)
{
    int writeIdx = audioWriteIndex.load();
    
    for (int i = 0; i < numSamples; ++i)
    {
        audioRingBufferLeft[writeIdx] = left[i];
        audioRingBufferRight[writeIdx] = right[i];
        writeIdx = (writeIdx + 1) % audioRingBufferSize;
    }
    
    audioWriteIndex.store(writeIdx);
    vstBridgeActive.store(true);
}

juce::AudioProcessorEditor* ACNotepatProcessor::createEditor()
{
    return new ACNotepatEditor(*this);
}

void ACNotepatProcessor::getStateInformation(juce::MemoryBlock& destData)
{
    auto state = parameters.copyState();
    std::unique_ptr<juce::XmlElement> xml(state.createXml());
    copyXmlToBinary(*xml, destData);
}

void ACNotepatProcessor::setStateInformation(const void* data, int sizeInBytes)
{
    std::unique_ptr<juce::XmlElement> xmlState(getXmlFromBinary(data, sizeInBytes));
    if (xmlState.get() != nullptr)
        if (xmlState->hasTagName(parameters.state.getType()))
            parameters.replaceState(juce::ValueTree::fromXml(*xmlState));
}

// Plugin instantiation
juce::AudioProcessor* JUCE_CALLTYPE createPluginFilter()
{
    return new ACNotepatProcessor();
}
