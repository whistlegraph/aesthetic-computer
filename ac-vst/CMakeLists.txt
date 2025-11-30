cmake_minimum_required(VERSION 3.22)
project(ACNotepat VERSION 0.1.0)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# JUCE is added as a subdirectory - will be cloned separately
add_subdirectory(JUCE)

# Plugin configuration
juce_add_plugin(ACNotepat
    # Plugin identity
    PLUGIN_MANUFACTURER_CODE Acmp
    PLUGIN_CODE Anpt
    
    # Plugin formats - VST3 only for modern DAWs like Ableton 12
    FORMATS VST3 Standalone
    
    # Plugin name
    PRODUCT_NAME "AC Notepat"
    COMPANY_NAME "Aesthetic Computer"
    
    # Plugin characteristics
    IS_SYNTH TRUE
    NEEDS_MIDI_INPUT TRUE
    NEEDS_MIDI_OUTPUT FALSE
    IS_MIDI_EFFECT FALSE
    EDITOR_WANTS_KEYBOARD_FOCUS TRUE
    
    # Bundle ID for macOS
    BUNDLE_ID "computer.aesthetic.notepat"
    
    # Plugin description
    DESCRIPTION "Aesthetic Computer Notepat Synthesizer"
)

# Source files
target_sources(ACNotepat
    PRIVATE
        Source/PluginProcessor.cpp
        Source/PluginEditor.cpp
)

# JUCE modules we need
target_compile_definitions(ACNotepat
    PUBLIC
        JUCE_WEB_BROWSER=1
        JUCE_USE_CURL=0
        JUCE_VST3_CAN_REPLACE_VST2=0
        JUCE_DISPLAY_SPLASH_SCREEN=0
)

# Link against JUCE modules
target_link_libraries(ACNotepat
    PRIVATE
        juce::juce_audio_basics
        juce::juce_audio_devices
        juce::juce_audio_formats
        juce::juce_audio_plugin_client
        juce::juce_audio_processors
        juce::juce_audio_utils
        juce::juce_core
        juce::juce_data_structures
        juce::juce_events
        juce::juce_graphics
        juce::juce_gui_basics
        juce::juce_gui_extra
    PUBLIC
        juce::juce_recommended_config_flags
        juce::juce_recommended_lto_flags
        juce::juce_recommended_warning_flags
)
