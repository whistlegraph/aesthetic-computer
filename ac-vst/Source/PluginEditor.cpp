/*
  ==============================================================================
    AC Notepat VST Plugin - Editor Implementation
    
    Uses native WKWebView on macOS for inline embedding.
  ==============================================================================
*/

#include "PluginEditor.h"

ACNotepatEditor::ACNotepatEditor(ACNotepatProcessor& p)
    : AudioProcessorEditor(&p), processorRef(p)
{
    // Set plugin window size - good for DAW integration
    setSize(600, 400);
    setResizable(true, true);
    setResizeLimits(300, 200, 1920, 1080);
    
    // Create WebView with native embedding options
    juce::WebBrowserComponent::Options options;
    options = options
        .withBackend(juce::WebBrowserComponent::Options::Backend::webview2) // Use modern backend
        .withNativeIntegrationEnabled()  // Enable native window integration
        .withKeepPageLoadedWhenBrowserIsHidden();
    
    webView = std::make_unique<juce::WebBrowserComponent>(options);
    
    // CRITICAL: Add as child component for inline display
    addAndMakeVisible(*webView);
    
    // Load notepat
    webView->goToURL(notepatUrl);
    
    // Start timer to poll for MIDI->key events
    startTimerHz(60);
}

ACNotepatEditor::~ACNotepatEditor()
{
    stopTimer();
    webView = nullptr;
}

void ACNotepatEditor::paint(juce::Graphics& g)
{
    // Dark background matching AC aesthetic
    g.fillAll(juce::Colour(0xff111111));
}

void ACNotepatEditor::resized()
{
    // WebView fills entire editor area
    if (webView)
        webView->setBounds(getLocalBounds());
}

void ACNotepatEditor::timerCallback()
{
    // Process pending key events from MIDI
    juce::ScopedLock sl(processorRef.keyEventLock);
    while (!processorRef.pendingKeyEvents.empty())
    {
        auto [key, isDown] = processorRef.pendingKeyEvents.front();
        processorRef.pendingKeyEvents.pop();
        dispatchKeyEvent(key, isDown);
    }
}

void ACNotepatEditor::dispatchKeyEvent(const juce::String& key, bool isDown)
{
    if (!webView) return;
    
    // Dispatch keyboard event to the WebView via JavaScript
    juce::String eventType = isDown ? "keydown" : "keyup";
    
    // Build key code - handle special keys
    int keyCode = key.length() == 1 ? (int)key[0] : 0;
    juce::String code = key.length() == 1 ? "Key" + key.toUpperCase() : key;
    
    juce::String jsCode = 
        "(function() {"
        "  var event = new KeyboardEvent('" + eventType + "', {"
        "    key: '" + key + "',"
        "    code: '" + code + "',"
        "    keyCode: " + juce::String(keyCode) + ","
        "    which: " + juce::String(keyCode) + ","
        "    bubbles: true,"
        "    cancelable: true"
        "  });"
        "  document.dispatchEvent(event);"
        "  var canvas = document.querySelector('canvas');"
        "  if (canvas) canvas.dispatchEvent(event);"
        "})();";
    
    webView->evaluateJavascript(jsCode, nullptr);
}
