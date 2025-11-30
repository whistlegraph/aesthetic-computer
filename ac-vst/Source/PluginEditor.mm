/*
  ==============================================================================
    AC Notepat VST Plugin - Editor Implementation
    
    Uses native WKWebView via Objective-C++ for proper inline embedding in DAWs.
  ==============================================================================
*/

#include "PluginEditor.h"

#if JUCE_MAC
#import <WebKit/WebKit.h>
#import <Cocoa/Cocoa.h>

// Custom message handler for JS -> Native communication
@interface ACWebViewMessageHandler : NSObject <WKScriptMessageHandler>
@property (nonatomic, assign) ACNotepatProcessor* processor;
@end

@implementation ACWebViewMessageHandler

- (void)userContentController:(WKUserContentController*)userContentController
      didReceiveScriptMessage:(WKScriptMessage*)message
{
    if ([message.name isEqualToString:@"audioSamples"]) {
        // Receive audio samples from JavaScript
        if ([message.body isKindOfClass:[NSDictionary class]]) {
            NSDictionary* data = (NSDictionary*)message.body;
            NSArray* left = data[@"left"];
            NSArray* right = data[@"right"];
            
            if (left && right && self.processor) {
                std::vector<float> leftSamples, rightSamples;
                leftSamples.reserve(left.count);
                rightSamples.reserve(right.count);
                
                for (NSNumber* sample in left) {
                    leftSamples.push_back([sample floatValue]);
                }
                for (NSNumber* sample in right) {
                    rightSamples.push_back([sample floatValue]);
                }
                
                self.processor->writeAudioSamples(leftSamples.data(), rightSamples.data(),
                                                  (int)leftSamples.size());
            }
        }
    }
    else if ([message.name isEqualToString:@"vstReady"]) {
        NSLog(@"VST Audio Bridge Ready!");
    }
}

@end

#endif // JUCE_MAC

ACNotepatEditor::ACNotepatEditor(ACNotepatProcessor& p)
    : AudioProcessorEditor(&p), processorRef(p)
{
    // Set plugin window size
    setSize(800, 600);
    setResizable(true, true);
    setResizeLimits(400, 300, 1920, 1080);
    
    // Create native WebView
    createNativeWebView();
    
    // Start timer to poll for MIDI->key events
    startTimerHz(60);
}

ACNotepatEditor::~ACNotepatEditor()
{
    stopTimer();
    destroyNativeWebView();
}

void ACNotepatEditor::createNativeWebView()
{
#if JUCE_MAC
    @autoreleasepool {
        // Get the native NSView from JUCE
        NSView* parentView = (NSView*)getWindowHandle();
        if (!parentView) {
            // Defer creation until we have a window
            return;
        }
        
        // Create WKWebView configuration
        WKWebViewConfiguration* config = [[WKWebViewConfiguration alloc] init];
        
        // Enable JavaScript
        WKPreferences* prefs = [[WKPreferences alloc] init];
        config.preferences = prefs;
        
        // Create message handler for JS -> Native bridge
        ACWebViewMessageHandler* handler = [[ACWebViewMessageHandler alloc] init];
        handler.processor = &processorRef;
        webViewDelegate = (__bridge_retained void*)handler;
        
        [config.userContentController addScriptMessageHandler:handler name:@"audioSamples"];
        [config.userContentController addScriptMessageHandler:handler name:@"vstReady"];
        
        // Inject JavaScript bridge code that maps to our message handlers
        NSString* bridgeScript = @"window.webkit = window.webkit || {};"
                                  "window.webkit.messageHandlers = window.webkit.messageHandlers || {};"
                                  "window.AC = window.AC || {};"
                                  "window.AC.isVSTMode = function() { return true; };"
                                  "window.AC.sendAudioSamples = function(left, right) {"
                                  "  window.webkit.messageHandlers.audioSamples.postMessage({left: left, right: right});"
                                  "};";
        
        WKUserScript* script = [[WKUserScript alloc] initWithSource:bridgeScript
                                                      injectionTime:WKUserScriptInjectionTimeAtDocumentStart
                                                   forMainFrameOnly:YES];
        [config.userContentController addUserScript:script];
        
        // Create the WebView - IMPORTANT: use bounds of parent for inline embedding
        CGRect frame = CGRectMake(0, 0, getWidth(), getHeight());
        WKWebView* wkWebView = [[WKWebView alloc] initWithFrame:frame configuration:config];
        
        // Make it resize with parent
        wkWebView.autoresizingMask = NSViewWidthSizable | NSViewHeightSizable;
        
        // Set transparent background to blend with plugin
        [wkWebView setValue:@NO forKey:@"drawsBackground"];
        
        // Add as subview of the JUCE component's native view
        [parentView addSubview:wkWebView];
        
        // Store reference
        webView = (__bridge_retained void*)wkWebView;
        
        // Load the URL
        NSString* urlString = [NSString stringWithUTF8String:notepatUrl.toRawUTF8()];
        NSURL* url = [NSURL URLWithString:urlString];
        NSURLRequest* request = [NSURLRequest requestWithURL:url];
        [wkWebView loadRequest:request];
        
        DBG("Native WKWebView created and loading: " + notepatUrl);
    }
#endif
}

void ACNotepatEditor::destroyNativeWebView()
{
#if JUCE_MAC
    @autoreleasepool {
        if (webView) {
            WKWebView* wkWebView = (__bridge_transfer WKWebView*)webView;
            [wkWebView removeFromSuperview];
            webView = nullptr;
        }
        
        if (webViewDelegate) {
            ACWebViewMessageHandler* handler = (__bridge_transfer ACWebViewMessageHandler*)webViewDelegate;
            handler.processor = nullptr;
            webViewDelegate = nullptr;
        }
    }
#endif
}

void ACNotepatEditor::paint(juce::Graphics& g)
{
    // Dark background matching AC aesthetic
    g.fillAll(juce::Colour(0xff111111));
    
#if JUCE_MAC
    // Create WebView if we don't have one yet (deferred from constructor)
    if (!webView && getWindowHandle()) {
        createNativeWebView();
    }
#endif
}

void ACNotepatEditor::resized()
{
#if JUCE_MAC
    @autoreleasepool {
        if (webView) {
            WKWebView* wkWebView = (__bridge WKWebView*)webView;
            CGRect frame = CGRectMake(0, 0, getWidth(), getHeight());
            wkWebView.frame = frame;
        }
    }
#endif
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
#if JUCE_MAC
    @autoreleasepool {
        if (!webView) return;
        
        WKWebView* wkWebView = (__bridge WKWebView*)webView;
        
        // Dispatch keyboard event to the WebView via JavaScript
        NSString* eventType = isDown ? @"keydown" : @"keyup";
        NSString* keyStr = [NSString stringWithUTF8String:key.toRawUTF8()];
        
        // Build key code
        int keyCode = key.length() == 1 ? (int)key[0] : 0;
        NSString* code = key.length() == 1 ? 
            [NSString stringWithFormat:@"Key%@", [keyStr uppercaseString]] : keyStr;
        
        NSString* jsCode = [NSString stringWithFormat:
            @"(function() {"
            "  var event = new KeyboardEvent('%@', {"
            "    key: '%@',"
            "    code: '%@',"
            "    keyCode: %d,"
            "    which: %d,"
            "    bubbles: true,"
            "    cancelable: true"
            "  });"
            "  document.dispatchEvent(event);"
            "  var canvas = document.querySelector('canvas');"
            "  if (canvas) canvas.dispatchEvent(event);"
            "})();",
            eventType, keyStr, code, keyCode, keyCode];
        
        [wkWebView evaluateJavaScript:jsCode completionHandler:nil];
    }
#endif
}
