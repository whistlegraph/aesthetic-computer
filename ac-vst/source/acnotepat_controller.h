#pragma once

#include "public.sdk/source/vst/vsteditcontroller.h"

namespace AestheticComputer {

//------------------------------------------------------------------------
// ACNotepatController - UI Controller Component
//------------------------------------------------------------------------
class ACNotepatController : public Steinberg::Vst::EditController
{
public:
    ACNotepatController() = default;
    ~ACNotepatController() override = default;

    // Create instance
    static Steinberg::FUnknown* createInstance(void*) 
    { 
        return static_cast<Steinberg::Vst::IEditController*>(new ACNotepatController()); 
    }

    // EditController overrides
    Steinberg::tresult PLUGIN_API initialize(Steinberg::FUnknown* context) override;
    Steinberg::tresult PLUGIN_API terminate() override;
    Steinberg::tresult PLUGIN_API setComponentState(Steinberg::IBStream* state) override;

    // Editor (UI) creation
    Steinberg::IPlugView* PLUGIN_API createView(Steinberg::FIDString name) override;
};

//------------------------------------------------------------------------
// ACNotepatView - The actual WebView-based UI
//------------------------------------------------------------------------
class ACNotepatView : public Steinberg::CPluginView
{
public:
    ACNotepatView(ACNotepatController* controller);
    ~ACNotepatView() override;

    // IPlugView overrides
    Steinberg::tresult PLUGIN_API isPlatformTypeSupported(Steinberg::FIDString type) override;
    Steinberg::tresult PLUGIN_API attached(void* parent, Steinberg::FIDString type) override;
    Steinberg::tresult PLUGIN_API removed() override;
    Steinberg::tresult PLUGIN_API getSize(Steinberg::ViewRect* size) override;
    Steinberg::tresult PLUGIN_API onSize(Steinberg::ViewRect* newSize) override;

private:
    ACNotepatController* controller = nullptr;
    void* webView = nullptr;           // WKWebView*
    void* messageHandler = nullptr;    // ACWebViewHandler*
    
    static constexpr int kDefaultWidth = 800;
    static constexpr int kDefaultHeight = 600;
};

} // namespace AestheticComputer
