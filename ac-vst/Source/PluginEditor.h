/*
  ==============================================================================
    AC Notepat VST Plugin - Editor (UI)
    
    Embeds aesthetic.computer/notepat directly in the plugin window.
  ==============================================================================
*/

#pragma once

#include <juce_audio_processors/juce_audio_processors.h>
#include <juce_gui_extra/juce_gui_extra.h>
#include "PluginProcessor.h"

class ACNotepatEditor : public juce::AudioProcessorEditor,
                        private juce::Timer
{
public:
    explicit ACNotepatEditor(ACNotepatProcessor&);
    ~ACNotepatEditor() override;

    void paint(juce::Graphics&) override;
    void resized() override;

private:
    void timerCallback() override;
    void dispatchKeyEvent(const juce::String& key, bool isDown);
    void setupAudioBridge();
    
    ACNotepatProcessor& processorRef;
    
    // WebView for embedding notepat
    std::unique_ptr<juce::WebBrowserComponent> webView;
    
    // URL to load - use localhost:8888 for dev, production URL otherwise
    #if JUCE_DEBUG
    juce::String notepatUrl = "https://localhost:8888/notepat?vst=true";
    #else
    juce::String notepatUrl = "https://aesthetic.computer/notepat?vst=true";
    #endif

    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR(ACNotepatEditor)
};
