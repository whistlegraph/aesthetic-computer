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
    setSize(800, 600);
    setResizable(true, true);
    setResizeLimits(400, 300, 1920, 1080);
    
    // Create WebView with options optimized for plugin hosting
    juce::WebBrowserComponent::Options options;
    
    #if JUCE_MAC
    // macOS: Use default WebKit backend
    options = options
        .withNativeIntegrationEnabled()
        .withKeepPageLoadedWhenBrowserIsHidden();
    #else
    // Windows: Use WebView2
    options = options
        .withBackend(juce::WebBrowserComponent::Options::Backend::webview2)
        .withNativeIntegrationEnabled()
        .withKeepPageLoadedWhenBrowserIsHidden();
    #endif
    
    // Add native function binding for audio data from JS
    options = options.withNativeFunction("sendAudioSamples", 
        [this](const juce::Array<juce::var>& args, 
               juce::WebBrowserComponent::NativeFunctionCompletion completion) {
            // Receive audio samples from JavaScript and write to processor's ring buffer
            if (args.size() >= 2) {
                auto* leftArray = args[0].getArray();
                auto* rightArray = args[1].getArray();
                if (leftArray && rightArray) {
                    // Convert to float arrays
                    std::vector<float> leftSamples, rightSamples;
                    leftSamples.reserve(leftArray->size());
                    rightSamples.reserve(rightArray->size());
                    
                    for (int i = 0; i < leftArray->size(); ++i) {
                        leftSamples.push_back((float)(*leftArray)[i]);
                        rightSamples.push_back((float)(*rightArray)[i]);
                    }
                    
                    // Write to processor's ring buffer
                    processorRef.writeAudioSamples(leftSamples.data(), rightSamples.data(), 
                                                   (int)leftSamples.size());
                }
            }
            completion({});
        });
    
    // Notify plugin that audio bridge is ready
    options = options.withNativeFunction("vstReady",
        [this](const juce::Array<juce::var>& args,
               juce::WebBrowserComponent::NativeFunctionCompletion completion) {
            DBG("VST Audio Bridge Ready!");
            setupAudioBridge();
            completion(juce::var(true));
        });
    
    webView = std::make_unique<juce::WebBrowserComponent>(options);
    
    // CRITICAL: Add as child component BEFORE going to URL
    addAndMakeVisible(*webView);
    
    // Load notepat with VST mode parameter
    DBG("Loading URL: " + notepatUrl);
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

void ACNotepatEditor::setupAudioBridge()
{
    if (!webView) return;
    
    // Tell the web app to enable VST audio bridge mode
    // This will route audio samples back to the plugin instead of Web Audio output
    juce::String jsCode = R"(
        (function() {
            if (window.AC && window.AC.enableVSTBridge) {
                window.AC.enableVSTBridge(function(leftSamples, rightSamples) {
                    // Send samples to native plugin
                    window.webkit.messageHandlers.sendAudioSamples.postMessage([leftSamples, rightSamples]);
                });
                console.log('VST Audio Bridge enabled!');
            } else {
                console.log('Waiting for AC to load...');
                setTimeout(arguments.callee, 100);
            }
        })();
    )";
    
    webView->evaluateJavascript(jsCode, nullptr);
}
