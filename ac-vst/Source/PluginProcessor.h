/*
  ==============================================================================
    AC Notepat VST Plugin - Audio Processor
    
    This is the main audio processing class. It handles:
    - MIDI input from the DAW
    - Audio generation (via embedded WebView running notepat)
    - Parameter state management
  ==============================================================================
*/

#pragma once

#include <juce_audio_processors/juce_audio_processors.h>
#include <juce_gui_extra/juce_gui_extra.h>

class ACNotepatProcessor : public juce::AudioProcessor
{
public:
    ACNotepatProcessor();
    ~ACNotepatProcessor() override;

    // Audio processing
    void prepareToPlay(double sampleRate, int samplesPerBlock) override;
    void releaseResources() override;
    void processBlock(juce::AudioBuffer<float>&, juce::MidiBuffer&) override;

    // Editor
    juce::AudioProcessorEditor* createEditor() override;
    bool hasEditor() const override { return true; }

    // Plugin info
    const juce::String getName() const override { return JucePlugin_Name; }
    bool acceptsMidi() const override { return true; }
    bool producesMidi() const override { return false; }
    bool isMidiEffect() const override { return false; }
    double getTailLengthSeconds() const override { return 0.0; }

    // Programs (presets)
    int getNumPrograms() override { return 1; }
    int getCurrentProgram() override { return 0; }
    void setCurrentProgram(int index) override {}
    const juce::String getProgramName(int index) override { return {}; }
    void changeProgramName(int index, const juce::String& newName) override {}

    // State save/load
    void getStateInformation(juce::MemoryBlock& destData) override;
    void setStateInformation(const void* data, int sizeInBytes) override;

    // Parameters
    juce::AudioProcessorValueTreeState& getParameters() { return parameters; }
    
    // MIDI to notepat key conversion
    juce::String midiNoteToNotepatKey(int midiNote);
    
    // Pending key events for the editor to dispatch
    std::queue<std::pair<juce::String, bool>> pendingKeyEvents; // key, isDown
    juce::CriticalSection keyEventLock;

private:
    juce::AudioProcessorValueTreeState parameters;
    
    // MIDI note tracking
    std::set<int> activeNotes;
    
    // Create parameter layout
    juce::AudioProcessorValueTreeState::ParameterLayout createParameterLayout();

    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR(ACNotepatProcessor)
};
