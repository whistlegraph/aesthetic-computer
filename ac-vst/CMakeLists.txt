cmake_minimum_required(VERSION 3.22)
project(ACNotepat VERSION 0.1.0 LANGUAGES CXX C OBJCXX)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_OSX_DEPLOYMENT_TARGET "10.15" CACHE STRING "Minimum macOS version")

# VST3 SDK - will be cloned as submodule
set(vst3sdk_SOURCE_DIR "${CMAKE_CURRENT_SOURCE_DIR}/vst3sdk")
if(NOT EXISTS "${vst3sdk_SOURCE_DIR}/CMakeLists.txt")
    message(FATAL_ERROR "VST3 SDK not found. Run: git clone --recursive https://github.com/steinbergmedia/vst3sdk.git")
endif()

# Add VST3 SDK
set(SMTG_ADD_VSTGUI OFF CACHE BOOL "" FORCE)  # We use WKWebView, not VSTGUI
set(SMTG_ADD_VST3_PLUGINS_SAMPLES OFF CACHE BOOL "" FORCE)
set(SMTG_ADD_VST3_HOSTING_SAMPLES OFF CACHE BOOL "" FORCE)
add_subdirectory(${vst3sdk_SOURCE_DIR})

# Our plugin sources
set(PLUGIN_SOURCES
    source/acnotepat.h
    source/acnotepat.cpp
    source/acnotepat_controller.h
    source/acnotepat_controller.mm
    source/acnotepat_cids.h
    source/acnotepat_entry.cpp
    source/version.h
)

# Create the VST3 plugin
smtg_add_vst3plugin(ACNotepat ${PLUGIN_SOURCES})

# Link VST3 SDK libraries
target_link_libraries(ACNotepat
    PRIVATE
        sdk
        sdk_common
)

# macOS: Link WebKit and Cocoa for WKWebView
if(APPLE)
    target_link_libraries(ACNotepat
        PRIVATE
            "-framework WebKit"
            "-framework Cocoa"
    )
    
    # Enable Objective-C++ for .mm files
    set_source_files_properties(
        source/acnotepat_controller.mm
        PROPERTIES
        COMPILE_FLAGS "-x objective-c++"
    )
endif()

# Plugin info
smtg_target_configure_version_file(ACNotepat)

# Set bundle identifier for macOS
if(APPLE)
    set_target_properties(ACNotepat PROPERTIES
        BUNDLE_IDENTIFIER "computer.aesthetic.notepat"
        MACOSX_BUNDLE_GUI_IDENTIFIER "computer.aesthetic.notepat"
    )
endif()
