/*
  ==============================================================================
    AC Notepat VST Plugin - Editor (UI)
    
    Uses native WKWebView directly for proper DAW embedding.
  ==============================================================================
*/

#pragma once

#include <juce_audio_processors/juce_audio_processors.h>
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
    void createNativeWebView();
    void destroyNativeWebView();
    void dispatchKeyEvent(const juce::String& key, bool isDown);
    
    ACNotepatProcessor& processorRef;
    
    // Native WebView (macOS) - stored as void* to avoid ObjC in header
    void* webView = nullptr;
    void* webViewDelegate = nullptr;
    
    // URL to load
    #if JUCE_DEBUG
    juce::String notepatUrl = "https://localhost:8888/notepat?vst=true";
    #else
    juce::String notepatUrl = "https://aesthetic.computer/notepat?vst=true";
    #endif

    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR(ACNotepatEditor)
};
