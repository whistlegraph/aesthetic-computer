#include "acnotepat_controller.h"
#include "acnotepat_cids.h"
#include "base/source/fstreamer.h"

#import <Cocoa/Cocoa.h>
#import <WebKit/WebKit.h>

using namespace Steinberg;
using namespace Steinberg::Vst;

// Forward declaration
namespace AestheticComputer {
    class ACNotepatController;
}

//------------------------------------------------------------------------
// Objective-C message handler for WKWebView -> Native communication
// Must be declared at global scope (outside C++ namespace)
//------------------------------------------------------------------------
@interface ACWebViewHandler : NSObject <WKScriptMessageHandler, WKNavigationDelegate>
@property (nonatomic, assign) AestheticComputer::ACNotepatController* controller;
@end

@implementation ACWebViewHandler

- (void)userContentController:(WKUserContentController*)userContentController 
      didReceiveScriptMessage:(WKScriptMessage*)message
{
    if ([message.name isEqualToString:@"acBridge"])
    {
        NSDictionary* body = message.body;
        NSString* type = body[@"type"];
        
        if ([type isEqualToString:@"audioSamples"])
        {
            // Receive audio samples from WebView
            // TODO: Route to processor's ring buffer
            NSArray* left = body[@"left"];
            NSArray* right = body[@"right"];
            // Process samples...
        }
        else if ([type isEqualToString:@"ready"])
        {
            NSLog(@"AC Notepat WebView ready");
        }
    }
}

- (void)webView:(WKWebView*)webView didFinishNavigation:(WKNavigation*)navigation
{
    NSLog(@"AC Notepat: Page loaded");
    
    // Inject VST bridge JavaScript
    NSString* bridgeScript = @"window.AC = window.AC || {};"
        "window.AC.isVSTMode = function() { return true; };"
        "window.AC.sendAudioSamples = function(left, right) {"
        "  window.webkit.messageHandlers.acBridge.postMessage({"
        "    type: 'audioSamples', left: Array.from(left), right: Array.from(right)"
        "  });"
        "};"
        "window.AC.sendMessage = function(msg) {"
        "  window.webkit.messageHandlers.acBridge.postMessage(msg);"
        "};"
        "console.log('AC VST Bridge injected');";
    
    [webView evaluateJavaScript:bridgeScript completionHandler:nil];
}

- (void)webView:(WKWebView*)webView didFailNavigation:(WKNavigation*)navigation withError:(NSError*)error
{
    NSLog(@"AC Notepat navigation failed: %@", error.localizedDescription);
}

@end

namespace AestheticComputer {

//------------------------------------------------------------------------
// ACNotepatController implementation
//------------------------------------------------------------------------
tresult PLUGIN_API ACNotepatController::initialize(FUnknown* context)
{
    tresult result = EditController::initialize(context);
    if (result != kResultOk)
        return result;

    // Register parameters
    parameters.addParameter(STR16("Volume"), STR16("%"), 0, 1.0,
        ParameterInfo::kCanAutomate, kParamVolume);
    
    parameters.addParameter(STR16("Room"), STR16("%"), 0, 0.5,
        ParameterInfo::kCanAutomate, kParamRoom);

    return kResultOk;
}

//------------------------------------------------------------------------
tresult PLUGIN_API ACNotepatController::terminate()
{
    return EditController::terminate();
}

//------------------------------------------------------------------------
tresult PLUGIN_API ACNotepatController::setComponentState(IBStream* state)
{
    if (!state)
        return kResultFalse;

    IBStreamer streamer(state, kLittleEndian);
    
    float savedVolume = 0.f;
    if (streamer.readFloat(savedVolume))
        setParamNormalized(kParamVolume, savedVolume);

    float savedRoom = 0.f;
    if (streamer.readFloat(savedRoom))
        setParamNormalized(kParamRoom, savedRoom);

    return kResultOk;
}

//------------------------------------------------------------------------
IPlugView* PLUGIN_API ACNotepatController::createView(FIDString name)
{
    if (FIDStringsEqual(name, ViewType::kEditor))
    {
        return new ACNotepatView(this);
    }
    return nullptr;
}

//------------------------------------------------------------------------
// ACNotepatView implementation
//------------------------------------------------------------------------
ACNotepatView::ACNotepatView(ACNotepatController* ctrl)
    : CPluginView(nullptr)
    , controller(ctrl)
{
    rect.left = 0;
    rect.top = 0;
    rect.right = kDefaultWidth;
    rect.bottom = kDefaultHeight;
}

//------------------------------------------------------------------------
ACNotepatView::~ACNotepatView()
{
    // Cleanup handled in removed()
}

//------------------------------------------------------------------------
tresult PLUGIN_API ACNotepatView::isPlatformTypeSupported(FIDString type)
{
    // Support macOS NSView
    if (FIDStringsEqual(type, kPlatformTypeNSView))
        return kResultTrue;
    
    return kResultFalse;
}

//------------------------------------------------------------------------
tresult PLUGIN_API ACNotepatView::attached(void* parent, FIDString type)
{
    if (!FIDStringsEqual(type, kPlatformTypeNSView))
        return kResultFalse;

    NSView* parentView = (__bridge NSView*)parent;
    
    // Create WKWebView configuration
    WKWebViewConfiguration* config = [[WKWebViewConfiguration alloc] init];
    WKUserContentController* userContent = [[WKUserContentController alloc] init];
    
    // Create message handler
    ACWebViewHandler* handler = [[ACWebViewHandler alloc] init];
    handler.controller = controller;
    messageHandler = (__bridge_retained void*)handler;
    
    [userContent addScriptMessageHandler:handler name:@"acBridge"];
    config.userContentController = userContent;
    
    // Allow file access and local storage
    [config.preferences setValue:@YES forKey:@"allowFileAccessFromFileURLs"];
    [config setValue:@YES forKey:@"allowUniversalAccessFromFileURLs"];
    
    // Create WebView
    NSRect frame = NSMakeRect(0, 0, kDefaultWidth, kDefaultHeight);
    WKWebView* wkWebView = [[WKWebView alloc] initWithFrame:frame configuration:config];
    wkWebView.navigationDelegate = handler;
    wkWebView.autoresizingMask = NSViewWidthSizable | NSViewHeightSizable;
    
    // Store reference
    webView = (__bridge_retained void*)wkWebView;
    
    // Add to parent
    [parentView addSubview:wkWebView];
    wkWebView.frame = parentView.bounds;
    
    // Load aesthetic.computer notepat
#ifdef DEBUG
    NSString* urlString = @"http://localhost:8888/notepat";
#else
    NSString* urlString = @"https://aesthetic.computer/notepat";
#endif
    
    NSURL* url = [NSURL URLWithString:urlString];
    NSURLRequest* request = [NSURLRequest requestWithURL:url];
    [wkWebView loadRequest:request];
    
    NSLog(@"AC Notepat: WebView attached, loading %@", urlString);
    
    return kResultOk;
}

//------------------------------------------------------------------------
tresult PLUGIN_API ACNotepatView::removed()
{
    if (webView)
    {
        WKWebView* wkWebView = (__bridge_transfer WKWebView*)webView;
        [wkWebView removeFromSuperview];
        webView = nullptr;
    }
    
    if (messageHandler)
    {
        ACWebViewHandler* handler = (__bridge_transfer ACWebViewHandler*)messageHandler;
        handler = nil;
        messageHandler = nullptr;
    }
    
    return kResultOk;
}

//------------------------------------------------------------------------
tresult PLUGIN_API ACNotepatView::getSize(ViewRect* size)
{
    if (!size)
        return kInvalidArgument;
    
    *size = rect;
    return kResultOk;
}

//------------------------------------------------------------------------
tresult PLUGIN_API ACNotepatView::onSize(ViewRect* newSize)
{
    if (!newSize)
        return kInvalidArgument;
    
    rect = *newSize;
    
    if (webView)
    {
        WKWebView* wkWebView = (__bridge WKWebView*)webView;
        wkWebView.frame = NSMakeRect(0, 0, newSize->getWidth(), newSize->getHeight());
    }
    
    return kResultOk;
}

} // namespace AestheticComputer
