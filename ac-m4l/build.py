#!/usr/bin/env python3
"""
Build script for Aesthetic Computer Max for Live device suite.

This script builds all AC M4L devices from a single configuration file.
Each device is a jweb~ instance pointing to a different AC piece URL,
with Ableton tempo/transport sync built in.

Usage:
    python3 build.py              # Build all devices (dev/localhost)
    python3 build.py --production # Build all devices (production URLs)
    python3 build.py notepat      # Build specific device
    python3 build.py --install    # Build all and install to Ableton
    python3 build.py --list       # List available devices
    
Examples:
    python3 build.py
    python3 build.py --production
    python3 build.py metronome --install
"""

import json
import struct
import sys
import os
from pathlib import Path

# M4L binary header - required for Ableton to recognize the file
M4L_HEADER = b"ampf\x04\x00\x00\x00iiiimeta\x04\x00\x00\x00\x00\x00\x00\x00ptch"

def generate_patcher(device: dict, defaults: dict, production: bool = False) -> dict:
    """Generate a complete M4L patcher for a device."""
    
    # Check if this is an effect device (has audio input)
    is_effect = device.get("type") == "effect"
    
    if is_effect:
        return generate_effect_patcher(device, defaults, production)
    else:
        return generate_instrument_patcher(device, defaults, production)

def generate_effect_patcher(device: dict, defaults: dict, production: bool = False) -> dict:
    """Generate a M4L Audio Effect patcher with audio input."""
    
    piece = device["piece"]
    width = device.get("width", 400)
    height = device.get("height", 250)
    description = device.get("description", f"Aesthetic Computer {piece} Effect")
    density = defaults.get("density", 1.5)
    latency = defaults.get("latency", 32.0)
    
    # Build URL
    if production:
        base_url = "https://aesthetic.computer"
    else:
        base_url = defaults.get("baseUrl", "https://localhost:8888")
    
    url = f"{base_url}/{piece}?daw=1&density={density}&nogap&width={width}&height={height}"
    
    return {
        "patcher": {
            "fileversion": 1,
            "appversion": {
                "major": 9,
                "minor": 0,
                "revision": 7,
                "architecture": "x64",
                "modernui": 1
            },
            "classnamespace": "box",
            "rect": [134.0, 174.0, 640.0, 480.0],
            "openrect": [0.0, 0.0, float(width), float(height)],
            "openinpresentation": 1,
            "gridsize": [15.0, 15.0],
            "enablehscroll": 0,
            "enablevscroll": 0,
            "devicewidth": float(width),
            "description": description,
            "boxes": [
                # plugin~ 2 - receive stereo audio from Ableton
                {
                    "box": {
                        "id": "obj-plugin",
                        "maxclass": "newobj",
                        "numinlets": 1,
                        "numoutlets": 2,
                        "outlettype": ["signal", "signal"],
                        "patching_rect": [10.0, 50.0, 65.0, 22.0],
                        "text": "plugin~ 2"
                    }
                },
                # Mix L+R to mono for simpler analysis
                {
                    "box": {
                        "id": "obj-mono-mix",
                        "maxclass": "newobj",
                        "numinlets": 2,
                        "numoutlets": 1,
                        "outlettype": ["signal"],
                        "patching_rect": [10.0, 80.0, 35.0, 22.0],
                        "text": "+~"
                    }
                },
                # Scale mono mix by 0.5
                {
                    "box": {
                        "id": "obj-mono-scale",
                        "maxclass": "newobj",
                        "numinlets": 2,
                        "numoutlets": 1,
                        "outlettype": ["signal"],
                        "patching_rect": [10.0, 105.0, 45.0, 22.0],
                        "text": "*~ 0.5"
                    }
                },
                # peakamp~ mono - amplitude envelope (100ms window)
                {
                    "box": {
                        "id": "obj-peak",
                        "maxclass": "newobj",
                        "numinlets": 2,
                        "numoutlets": 1,
                        "outlettype": ["float"],
                        "patching_rect": [10.0, 130.0, 85.0, 22.0],
                        "text": "peakamp~ 100"
                    }
                },
                # Throttle peak messages to 30fps (33ms)
                {
                    "box": {
                        "id": "obj-peak-throttle",
                        "maxclass": "newobj",
                        "numinlets": 2,
                        "numoutlets": 1,
                        "outlettype": [""],
                        "patching_rect": [10.0, 155.0, 70.0, 22.0],
                        "text": "speedlim 33"
                    }
                },
                # Format peak as simple JS call (single value, no commas)
                {
                    "box": {
                        "id": "obj-peak-sprintf",
                        "maxclass": "newobj",
                        "numinlets": 1,
                        "numoutlets": 1,
                        "outlettype": [""],
                        "patching_rect": [10.0, 180.0, 280.0, 22.0],
                        "text": "sprintf executejavascript window.acPedalPeak(%f)"
                    }
                },
                # Dry signal gain (left)
                {
                    "box": {
                        "id": "obj-dry-gainL",
                        "maxclass": "newobj",
                        "numinlets": 2,
                        "numoutlets": 1,
                        "outlettype": ["signal"],
                        "patching_rect": [10.0, 180.0, 45.0, 22.0],
                        "text": "*~ 1."
                    }
                },
                # Dry signal gain (right)
                {
                    "box": {
                        "id": "obj-dry-gainR",
                        "maxclass": "newobj",
                        "numinlets": 2,
                        "numoutlets": 1,
                        "outlettype": ["signal"],
                        "patching_rect": [70.0, 180.0, 45.0, 22.0],
                        "text": "*~ 1."
                    }
                },
                # jweb~ - the main web view with audio output
                {
                    "box": {
                        "disablefind": 0,
                        "id": "obj-jweb",
                        "latency": latency,
                        "maxclass": "jweb~",
                        "numinlets": 1,
                        "numoutlets": 3,
                        "outlettype": ["signal", "signal", ""],
                        "patching_rect": [200.0, 50.0, 320.0, 240.0],
                        "presentation": 1,
                        "presentation_rect": [0.0, 0.0, float(width + 1), float(height + 1)],
                        "rendermode": 1,
                        "url": url
                    }
                },
                # Wet signal gain (left)
                {
                    "box": {
                        "id": "obj-wet-gainL",
                        "maxclass": "newobj",
                        "numinlets": 2,
                        "numoutlets": 1,
                        "outlettype": ["signal"],
                        "patching_rect": [200.0, 300.0, 50.0, 22.0],
                        "text": "*~ 0.5"
                    }
                },
                # Wet signal gain (right)
                {
                    "box": {
                        "id": "obj-wet-gainR",
                        "maxclass": "newobj",
                        "numinlets": 2,
                        "numoutlets": 1,
                        "outlettype": ["signal"],
                        "patching_rect": [270.0, 300.0, 50.0, 22.0],
                        "text": "*~ 0.5"
                    }
                },
                # Mix dry + wet (left)
                {
                    "box": {
                        "id": "obj-mixL",
                        "maxclass": "newobj",
                        "numinlets": 2,
                        "numoutlets": 1,
                        "outlettype": ["signal"],
                        "patching_rect": [10.0, 350.0, 35.0, 22.0],
                        "text": "+~"
                    }
                },
                # Mix dry + wet (right)
                {
                    "box": {
                        "id": "obj-mixR",
                        "maxclass": "newobj",
                        "numinlets": 2,
                        "numoutlets": 1,
                        "outlettype": ["signal"],
                        "patching_rect": [70.0, 350.0, 35.0, 22.0],
                        "text": "+~"
                    }
                },
                # plugout~ - send stereo audio back to Ableton
                {
                    "box": {
                        "id": "obj-out",
                        "maxclass": "newobj",
                        "numinlets": 2,
                        "numoutlets": 2,
                        "outlettype": ["signal", "signal"],
                        "patching_rect": [10.0, 400.0, 85.0, 22.0],
                        "text": "plugout~"
                    }
                },
                # live.thisdevice - triggers on device load
                {
                    "box": {
                        "id": "obj-thisdevice",
                        "maxclass": "newobj",
                        "numinlets": 1,
                        "numoutlets": 3,
                        "outlettype": ["bang", "int", "int"],
                        "patching_rect": [350.0, 300.0, 85.0, 22.0],
                        "text": "live.thisdevice"
                    }
                },
                # Debug: print when device loads
                {
                    "box": {
                        "id": "obj-load-print",
                        "maxclass": "newobj",
                        "numinlets": 1,
                        "numoutlets": 0,
                        "patching_rect": [350.0, 330.0, 100.0, 22.0],
                        "text": "print [AC-EFFECT-LOADED]"
                    }
                },
                # Route 'ready' messages from jweb~ to trigger Live API sync
                {
                    "box": {
                        "id": "obj-ready-route",
                        "maxclass": "newobj",
                        "numinlets": 1,
                        "numoutlets": 2,
                        "outlettype": ["", ""],
                        "patching_rect": [530.0, 80.0, 60.0, 22.0],
                        "text": "route ready"
                    }
                },
                # Debug: print when page is ready
                {
                    "box": {
                        "id": "obj-ready-print",
                        "maxclass": "newobj",
                        "numinlets": 1,
                        "numoutlets": 0,
                        "patching_rect": [530.0, 110.0, 90.0, 22.0],
                        "text": "print [AC-READY]"
                    }
                },
                # Message to send getid to live.path
                {
                    "box": {
                        "id": "obj-getid-msg",
                        "maxclass": "message",
                        "numinlets": 2,
                        "numoutlets": 1,
                        "outlettype": [""],
                        "patching_rect": [530.0, 140.0, 40.0, 22.0],
                        "text": "getid"
                    }
                },
                # Tempo: live.path to get live_set id
                {
                    "box": {
                        "id": "obj-tempo-path",
                        "maxclass": "newobj",
                        "numinlets": 1,
                        "numoutlets": 3,
                        "outlettype": ["", "", ""],
                        "patching_rect": [530.0, 170.0, 100.0, 22.0],
                        "text": "live.path live_set"
                    }
                },
                # Delay + bang to trigger initial value output from observers
                {
                    "box": {
                        "id": "obj-init-delay",
                        "maxclass": "newobj",
                        "numinlets": 2,
                        "numoutlets": 1,
                        "outlettype": ["bang"],
                        "patching_rect": [530.0, 200.0, 60.0, 22.0],
                        "text": "delay 100"
                    }
                },
                # Tempo: observer
                {
                    "box": {
                        "id": "obj-tempo-observer",
                        "maxclass": "newobj",
                        "numinlets": 2,
                        "numoutlets": 3,
                        "outlettype": ["", "", ""],
                        "patching_rect": [530.0, 230.0, 130.0, 22.0],
                        "text": "live.observer tempo"
                    }
                },
                # Tempo: sprintf to format the JS command
                {
                    "box": {
                        "id": "obj-tempo-sprintf",
                        "maxclass": "newobj",
                        "numinlets": 1,
                        "numoutlets": 1,
                        "outlettype": [""],
                        "patching_rect": [530.0, 260.0, 280.0, 22.0],
                        "text": "sprintf executejavascript window.acDawTempo(%f)"
                    }
                },
                # Transport: observer
                {
                    "box": {
                        "id": "obj-transport-observer",
                        "maxclass": "newobj",
                        "numinlets": 2,
                        "numoutlets": 3,
                        "outlettype": ["", "", ""],
                        "patching_rect": [530.0, 290.0, 150.0, 22.0],
                        "text": "live.observer is_playing"
                    }
                },
                # Transport: sprintf
                {
                    "box": {
                        "id": "obj-transport-sprintf",
                        "maxclass": "newobj",
                        "numinlets": 1,
                        "numoutlets": 1,
                        "outlettype": [""],
                        "patching_rect": [530.0, 320.0, 290.0, 22.0],
                        "text": "sprintf executejavascript window.acDawTransport(%d)"
                    }
                },
                # Sample rate: adstatus sr
                {
                    "box": {
                        "id": "obj-samplerate-adstatus",
                        "maxclass": "newobj",
                        "numinlets": 1,
                        "numoutlets": 1,
                        "outlettype": [""],
                        "patching_rect": [530.0, 350.0, 65.0, 22.0],
                        "text": "adstatus sr"
                    }
                },
                # Filter out "clear" messages
                {
                    "box": {
                        "id": "obj-samplerate-filter",
                        "maxclass": "newobj",
                        "numinlets": 2,
                        "numoutlets": 2,
                        "outlettype": ["", ""],
                        "patching_rect": [530.0, 380.0, 55.0, 22.0],
                        "text": "sel clear"
                    }
                },
                # Sample rate: sprintf
                {
                    "box": {
                        "id": "obj-samplerate-sprintf",
                        "maxclass": "newobj",
                        "numinlets": 1,
                        "numoutlets": 1,
                        "outlettype": [""],
                        "patching_rect": [530.0, 410.0, 300.0, 22.0],
                        "text": "sprintf executejavascript window.acDawSamplerate(%d)"
                    }
                },
                # Activate message to auto-resume AudioContext
                {
                    "box": {
                        "id": "obj-activate-msg",
                        "maxclass": "message",
                        "numinlets": 2,
                        "numoutlets": 1,
                        "outlettype": [""],
                        "patching_rect": [600.0, 140.0, 60.0, 22.0],
                        "text": "activate 1"
                    }
                },
                # Debug: print jweb messages
                {
                    "box": {
                        "id": "obj-jweb-print",
                        "maxclass": "newobj",
                        "numinlets": 1,
                        "numoutlets": 0,
                        "patching_rect": [530.0, 50.0, 100.0, 22.0],
                        "text": "print [AC-JWEB]"
                    }
                }
            ],
            "lines": [
                # Audio input: plugin~ -> mono mix (for analysis)
                {"patchline": {"destination": ["obj-mono-mix", 0], "source": ["obj-plugin", 0]}},
                {"patchline": {"destination": ["obj-mono-mix", 1], "source": ["obj-plugin", 1]}},
                
                # Mono mix -> scale -> peakamp -> throttle -> sprintf -> jweb
                {"patchline": {"destination": ["obj-mono-scale", 0], "source": ["obj-mono-mix", 0]}},
                {"patchline": {"destination": ["obj-peak", 0], "source": ["obj-mono-scale", 0]}},
                {"patchline": {"destination": ["obj-peak-throttle", 0], "source": ["obj-peak", 0]}},
                {"patchline": {"destination": ["obj-peak-sprintf", 0], "source": ["obj-peak-throttle", 0]}},
                {"patchline": {"destination": ["obj-jweb", 0], "source": ["obj-peak-sprintf", 0]}},
                
                # Audio input: plugin~ -> dry gain (pass-through)
                {"patchline": {"destination": ["obj-dry-gainL", 0], "source": ["obj-plugin", 0]}},
                {"patchline": {"destination": ["obj-dry-gainR", 0], "source": ["obj-plugin", 1]}},
                
                # jweb~ signal outputs -> wet gain
                {"patchline": {"destination": ["obj-wet-gainL", 0], "source": ["obj-jweb", 0]}},
                {"patchline": {"destination": ["obj-wet-gainR", 0], "source": ["obj-jweb", 1]}},
                
                # Dry + Wet mix
                {"patchline": {"destination": ["obj-mixL", 0], "source": ["obj-dry-gainL", 0]}},
                {"patchline": {"destination": ["obj-mixL", 1], "source": ["obj-wet-gainL", 0]}},
                {"patchline": {"destination": ["obj-mixR", 0], "source": ["obj-dry-gainR", 0]}},
                {"patchline": {"destination": ["obj-mixR", 1], "source": ["obj-wet-gainR", 0]}},
                
                # Mix -> plugout~
                {"patchline": {"destination": ["obj-out", 0], "source": ["obj-mixL", 0]}},
                {"patchline": {"destination": ["obj-out", 1], "source": ["obj-mixR", 0]}},
                
                # jweb messages routing
                {"patchline": {"destination": ["obj-ready-route", 0], "source": ["obj-jweb", 2]}},
                {"patchline": {"destination": ["obj-jweb-print", 0], "source": ["obj-jweb", 2]}},
                
                # Ready -> getid + activate
                {"patchline": {"destination": ["obj-ready-print", 0], "source": ["obj-ready-route", 0]}},
                {"patchline": {"destination": ["obj-getid-msg", 0], "source": ["obj-ready-route", 0]}},
                {"patchline": {"destination": ["obj-activate-msg", 0], "source": ["obj-ready-route", 0]}},
                
                # Activate -> jweb
                {"patchline": {"destination": ["obj-jweb", 0], "source": ["obj-activate-msg", 0]}},
                
                # Device load print
                {"patchline": {"destination": ["obj-load-print", 0], "source": ["obj-thisdevice", 0]}},
                
                # getid -> live.path
                {"patchline": {"destination": ["obj-tempo-path", 0], "source": ["obj-getid-msg", 0]}},
                
                # live.path -> observers
                {"patchline": {"destination": ["obj-tempo-observer", 1], "source": ["obj-tempo-path", 0]}},
                {"patchline": {"destination": ["obj-transport-observer", 1], "source": ["obj-tempo-path", 0]}},
                {"patchline": {"destination": ["obj-init-delay", 0], "source": ["obj-tempo-path", 0]}},
                
                # Delay -> bang observers
                {"patchline": {"destination": ["obj-tempo-observer", 0], "source": ["obj-init-delay", 0]}},
                {"patchline": {"destination": ["obj-transport-observer", 0], "source": ["obj-init-delay", 0]}},
                {"patchline": {"destination": ["obj-samplerate-adstatus", 0], "source": ["obj-init-delay", 0]}},
                
                # Tempo observer -> sprintf -> jweb
                {"patchline": {"destination": ["obj-tempo-sprintf", 0], "source": ["obj-tempo-observer", 0]}},
                {"patchline": {"destination": ["obj-jweb", 0], "source": ["obj-tempo-sprintf", 0]}},
                
                # Transport observer -> sprintf -> jweb
                {"patchline": {"destination": ["obj-transport-sprintf", 0], "source": ["obj-transport-observer", 0]}},
                {"patchline": {"destination": ["obj-jweb", 0], "source": ["obj-transport-sprintf", 0]}},
                
                # Sample rate -> filter -> sprintf -> jweb
                {"patchline": {"destination": ["obj-samplerate-filter", 0], "source": ["obj-samplerate-adstatus", 0]}},
                {"patchline": {"destination": ["obj-samplerate-sprintf", 0], "source": ["obj-samplerate-filter", 1]}},
                {"patchline": {"destination": ["obj-jweb", 0], "source": ["obj-samplerate-sprintf", 0]}}
            ],
            "dependency_cache": [],
            "latency": 0,
            "is_mpe": 0,
            "external_mpe_tuning_enabled": 0,
            "minimum_live_version": "",
            "minimum_max_version": "",
            "platform_compatibility": 0,
            "autosave": 0
        }
    }

def generate_instrument_patcher(device: dict, defaults: dict, production: bool = False) -> dict:
    """Generate a complete M4L patcher for a device."""
    
    piece = device["piece"]
    width = device.get("width", 400)
    height = device.get("height", 169)
    description = device.get("description", f"Aesthetic Computer {piece}")
    density = defaults.get("density", 1.5)
    latency = defaults.get("latency", 32.0)
    
    # Check for custom URL (e.g., kidlisp.com)
    # Support both devUrl/prodUrl (preferred) and legacy url field
    dev_url = device.get("devUrl")
    prod_url = device.get("prodUrl")
    legacy_url = device.get("url")
    
    if dev_url or prod_url:
        # Use environment-specific URL
        custom_url = prod_url if production else dev_url
        if custom_url:
            url = f"{custom_url}?daw=1&density={density}&width={width}&height={height}"
        else:
            # Fall back to the other URL if only one is specified
            custom_url = dev_url or prod_url
            url = f"{custom_url}?daw=1&density={density}&width={width}&height={height}"
    elif legacy_url:
        # Legacy: Use custom URL directly (with daw params)
        url = f"{legacy_url}?daw=1&density={density}&width={width}&height={height}"
    else:
        # Use production URL or localhost
        if production:
            base_url = "https://aesthetic.computer"
        else:
            base_url = defaults.get("baseUrl", "https://localhost:8888")
        
        # Include width/height in URL for zoom compensation
        url = f"{base_url}/{piece}?daw=1&density={density}&nogap&width={width}&height={height}"
    
    return {
        "patcher": {
            "fileversion": 1,
            "appversion": {
                "major": 9,
                "minor": 0,
                "revision": 7,
                "architecture": "x64",
                "modernui": 1
            },
            "classnamespace": "box",
            "rect": [134.0, 174.0, 500.0, 300.0],
            "openrect": [0.0, 0.0, float(width), float(height)],
            "openinpresentation": 1,
            "gridsize": [15.0, 15.0],
            "enablehscroll": 0,
            "enablevscroll": 0,
            "devicewidth": float(width),
            "description": description,
            "boxes": [
                # jweb~ - the main web view with audio
                {
                    "box": {
                        "disablefind": 0,
                        "id": "obj-jweb",
                        "latency": latency,
                        "maxclass": "jweb~",
                        "numinlets": 1,
                        "numoutlets": 3,
                        "outlettype": ["signal", "signal", ""],
                        "patching_rect": [0.0, 0.0, 320.0, 240.0],
                        "presentation": 1,
                        "presentation_rect": [0.0, 0.0, float(width + 1), float(height + 1)],
                        "rendermode": 1,
                        "url": url
                    }
                },
                # plugout~ - routes audio to Ableton mixer
                {
                    "box": {
                        "id": "obj-out",
                        "maxclass": "newobj",
                        "numinlets": 2,
                        "numoutlets": 2,
                        "outlettype": ["signal", "signal"],
                        "patching_rect": [10.0, 200.0, 85.0, 22.0],
                        "text": "plugout~"
                    }
                },
                # live.thisdevice - triggers on device load
                {
                    "box": {
                        "id": "obj-thisdevice",
                        "maxclass": "newobj",
                        "numinlets": 1,
                        "numoutlets": 3,
                        "outlettype": ["bang", "int", "int"],
                        "patching_rect": [150.0, 200.0, 85.0, 22.0],
                        "text": "live.thisdevice"
                    }
                },
                # Debug: print when device loads
                {
                    "box": {
                        "id": "obj-load-print",
                        "maxclass": "newobj",
                        "numinlets": 1,
                        "numoutlets": 0,
                        "patching_rect": [150.0, 230.0, 100.0, 22.0],
                        "text": "print [AC-LOADED]"
                    }
                },
                # Route 'ready' messages from jweb~ to trigger Live API sync
                {
                    "box": {
                        "id": "obj-ready-route",
                        "maxclass": "newobj",
                        "numinlets": 1,
                        "numoutlets": 2,
                        "outlettype": ["", ""],
                        "patching_rect": [200.0, 80.0, 60.0, 22.0],
                        "text": "route ready"
                    }
                },
                # Debug: print when page is ready
                {
                    "box": {
                        "id": "obj-ready-print",
                        "maxclass": "newobj",
                        "numinlets": 1,
                        "numoutlets": 0,
                        "patching_rect": [200.0, 110.0, 90.0, 22.0],
                        "text": "print [AC-READY]"
                    }
                },
                # Debug: print what getid message outputs
                {
                    "box": {
                        "id": "obj-getid-print",
                        "maxclass": "newobj",
                        "numinlets": 1,
                        "numoutlets": 0,
                        "patching_rect": [320.0, 170.0, 80.0, 22.0],
                        "text": "print [AC-GETID]"
                    }
                },
                # Message to send getid to live.path
                {
                    "box": {
                        "id": "obj-getid-msg",
                        "maxclass": "message",
                        "numinlets": 2,
                        "numoutlets": 1,
                        "outlettype": [""],
                        "patching_rect": [320.0, 200.0, 40.0, 22.0],
                        "text": "getid"
                    }
                },
                # Tempo: live.path to get live_set id
                {
                    "box": {
                        "id": "obj-tempo-path",
                        "maxclass": "newobj",
                        "numinlets": 1,
                        "numoutlets": 3,
                        "outlettype": ["", "", ""],
                        "patching_rect": [320.0, 260.0, 100.0, 22.0],
                        "text": "live.path live_set"
                    }
                },
                # Debug: print the id from live.path (left outlet)
                {
                    "box": {
                        "id": "obj-path-print",
                        "maxclass": "newobj",
                        "numinlets": 1,
                        "numoutlets": 0,
                        "patching_rect": [320.0, 290.0, 80.0, 22.0],
                        "text": "print [AC-PATH-L]"
                    }
                },
                # Debug: print the id from live.path (middle outlet)
                {
                    "box": {
                        "id": "obj-path-print-m",
                        "maxclass": "newobj",
                        "numinlets": 1,
                        "numoutlets": 0,
                        "patching_rect": [430.0, 290.0, 85.0, 22.0],
                        "text": "print [AC-PATH-M]"
                    }
                },
                # Debug: print the id from live.path (right outlet - errors)
                {
                    "box": {
                        "id": "obj-path-print-r",
                        "maxclass": "newobj",
                        "numinlets": 1,
                        "numoutlets": 0,
                        "patching_rect": [520.0, 290.0, 85.0, 22.0],
                        "text": "print [AC-PATH-R]"
                    }
                },
                # Delay + bang to trigger initial value output from observers
                # The delay ensures the ID has been set before we request the value
                {
                    "box": {
                        "id": "obj-init-delay",
                        "maxclass": "newobj",
                        "numinlets": 2,
                        "numoutlets": 1,
                        "outlettype": ["bang"],
                        "patching_rect": [450.0, 260.0, 60.0, 22.0],
                        "text": "delay 100"
                    }
                },
                # Tempo: observer with property argument
                {
                    "box": {
                        "id": "obj-tempo-observer",
                        "maxclass": "newobj",
                        "numinlets": 2,
                        "numoutlets": 3,
                        "outlettype": ["", "", ""],
                        "patching_rect": [260.0, 290.0, 130.0, 22.0],
                        "text": "live.observer tempo"
                    }
                },
                # Tempo: sprintf to format the JS command with actual value
                {
                    "box": {
                        "id": "obj-tempo-sprintf",
                        "maxclass": "newobj",
                        "numinlets": 1,
                        "numoutlets": 1,
                        "outlettype": [""],
                        "patching_rect": [260.0, 320.0, 280.0, 22.0],
                        "text": "sprintf executejavascript window.acDawTempo(%f)"
                    }
                },
                # Debug: Print tempo to Max console
                {
                    "box": {
                        "id": "obj-tempo-print",
                        "maxclass": "newobj",
                        "numinlets": 1,
                        "numoutlets": 0,
                        "patching_rect": [400.0, 320.0, 80.0, 22.0],
                        "text": "print [AC-TEMPO]"
                    }
                },
                # Debug: Print tempo script command to Max console
                {
                    "box": {
                        "id": "obj-tempo-script-print",
                        "maxclass": "newobj",
                        "numinlets": 1,
                        "numoutlets": 0,
                        "patching_rect": [400.0, 380.0, 120.0, 22.0],
                        "text": "print [AC-TEMPO-SCRIPT]"
                    }
                },
                # Transport: observer with property argument
                {
                    "box": {
                        "id": "obj-transport-observer",
                        "maxclass": "newobj",
                        "numinlets": 2,
                        "numoutlets": 3,
                        "outlettype": ["", "", ""],
                        "patching_rect": [260.0, 350.0, 150.0, 22.0],
                        "text": "live.observer is_playing"
                    }
                },
                # Transport: sprintf to format the JS command with actual value
                {
                    "box": {
                        "id": "obj-transport-sprintf",
                        "maxclass": "newobj",
                        "numinlets": 1,
                        "numoutlets": 1,
                        "outlettype": [""],
                        "patching_rect": [260.0, 390.0, 290.0, 22.0],
                        "text": "sprintf executejavascript window.acDawTransport(%d)"
                    }
                },
                # Debug: Print transport to Max console
                {
                    "box": {
                        "id": "obj-transport-print",
                        "maxclass": "newobj",
                        "numinlets": 1,
                        "numoutlets": 0,
                        "patching_rect": [420.0, 380.0, 100.0, 22.0],
                        "text": "print [AC-TRANSPORT]"
                    }
                },
                # Debug: Print transport script command to Max console
                {
                    "box": {
                        "id": "obj-transport-script-print",
                        "maxclass": "newobj",
                        "numinlets": 1,
                        "numoutlets": 0,
                        "patching_rect": [420.0, 420.0, 140.0, 22.0],
                        "text": "print [AC-TRANSPORT-SCRIPT]"
                    }
                },
                # Beat phase: observer for current_song_time (beat position for phase sync)
                {
                    "box": {
                        "id": "obj-phase-observer",
                        "maxclass": "newobj",
                        "numinlets": 2,
                        "numoutlets": 3,
                        "outlettype": ["", "", ""],
                        "patching_rect": [260.0, 450.0, 180.0, 22.0],
                        "text": "live.observer current_song_time"
                    }
                },
                # Beat phase: sprintf to format the JS command with actual value
                {
                    "box": {
                        "id": "obj-phase-sprintf",
                        "maxclass": "newobj",
                        "numinlets": 1,
                        "numoutlets": 1,
                        "outlettype": [""],
                        "patching_rect": [260.0, 490.0, 280.0, 22.0],
                        "text": "sprintf executejavascript window.acDawPhase(%f)"
                    }
                },
                # Sample rate: Use adstatus~ to get Max's audio sample rate (matches Ableton)
                # adstatus sr outputs the sample rate directly when banged
                # NOTE: adstatus outputs "clear" initially, so we filter with [sel clear]
                {
                    "box": {
                        "id": "obj-samplerate-adstatus",
                        "maxclass": "newobj",
                        "numinlets": 1,
                        "numoutlets": 1,
                        "outlettype": [""],
                        "patching_rect": [460.0, 350.0, 65.0, 22.0],
                        "text": "adstatus sr"
                    }
                },
                # Filter out "clear" messages from adstatus, only pass numeric values
                {
                    "box": {
                        "id": "obj-samplerate-filter",
                        "maxclass": "newobj",
                        "numinlets": 2,
                        "numoutlets": 2,
                        "outlettype": ["", ""],
                        "patching_rect": [460.0, 370.0, 55.0, 22.0],
                        "text": "sel clear"
                    }
                },
                # Sample rate: sprintf to format the JS command with actual value
                {
                    "box": {
                        "id": "obj-samplerate-sprintf",
                        "maxclass": "newobj",
                        "numinlets": 1,
                        "numoutlets": 1,
                        "outlettype": [""],
                        "patching_rect": [460.0, 390.0, 300.0, 22.0],
                        "text": "sprintf executejavascript window.acDawSamplerate(%d)"
                    }
                },
                # Debug: Print sample rate to Max console
                {
                    "box": {
                        "id": "obj-samplerate-print",
                        "maxclass": "newobj",
                        "numinlets": 1,
                        "numoutlets": 0,
                        "patching_rect": [620.0, 380.0, 110.0, 22.0],
                        "text": "print [AC-SAMPLERATE]"
                    }
                },
                # Debug: Print sample rate script command to Max console
                {
                    "box": {
                        "id": "obj-samplerate-script-print",
                        "maxclass": "newobj",
                        "numinlets": 1,
                        "numoutlets": 0,
                        "patching_rect": [620.0, 420.0, 140.0, 22.0],
                        "text": "print [AC-SAMPLERATE-SCRIPT]"
                    }
                },
                # Debug: Print messages from jweb~ (outlet 2 = third outlet)
                {
                    "box": {
                        "id": "obj-jweb-print",
                        "maxclass": "newobj",
                        "numinlets": 1,
                        "numoutlets": 0,
                        "patching_rect": [200.0, 50.0, 100.0, 22.0],
                        "text": "print [AC-JWEB]"
                    }
                },
                # Activate message to auto-resume AudioContext in DAW mode
                {
                    "box": {
                        "id": "obj-activate-msg",
                        "maxclass": "message",
                        "numinlets": 2,
                        "numoutlets": 1,
                        "outlettype": [""],
                        "patching_rect": [200.0, 140.0, 60.0, 22.0],
                        "text": "activate 1"
                    }
                },
                # Debug: print activate
                {
                    "box": {
                        "id": "obj-activate-print",
                        "maxclass": "newobj",
                        "numinlets": 1,
                        "numoutlets": 0,
                        "patching_rect": [270.0, 140.0, 95.0, 22.0],
                        "text": "print [AC-ACTIVATE]"
                    }
                },
                # MIDI input - receives MIDI from Ableton track
                {
                    "box": {
                        "id": "obj-midiin",
                        "maxclass": "newobj",
                        "numinlets": 1,
                        "numoutlets": 1,
                        "outlettype": ["int"],
                        "patching_rect": [10.0, 260.0, 50.0, 22.0],
                        "text": "midiin"
                    }
                },
                # Parse MIDI messages into status, data1, data2
                {
                    "box": {
                        "id": "obj-midiparse",
                        "maxclass": "newobj",
                        "numinlets": 1,
                        "numoutlets": 8,
                        "outlettype": ["", "", "", "int", "int", "", "int", ""],
                        "patching_rect": [10.0, 290.0, 120.0, 22.0],
                        "text": "midiparse"
                    }
                },
                # Pack note-on: note, velocity -> midi message for jweb
                {
                    "box": {
                        "id": "obj-noteon-pack",
                        "maxclass": "newobj",
                        "numinlets": 2,
                        "numoutlets": 1,
                        "outlettype": [""],
                        "patching_rect": [10.0, 320.0, 50.0, 22.0],
                        "text": "pack i i"
                    }
                },
                # Format note-on as midi message for jweb: midi <status> <note> <velocity>
                {
                    "box": {
                        "id": "obj-noteon-fmt",
                        "maxclass": "message",
                        "numinlets": 2,
                        "numoutlets": 1,
                        "outlettype": [""],
                        "patching_rect": [10.0, 350.0, 80.0, 22.0],
                        "text": "midi 144 $1 $2"
                    }
                },
                # Pack poly aftertouch (note off): note, pressure -> also used for note-off with velocity 0
                {
                    "box": {
                        "id": "obj-noteoff-pack",
                        "maxclass": "newobj",
                        "numinlets": 2,
                        "numoutlets": 1,
                        "outlettype": [""],
                        "patching_rect": [70.0, 320.0, 50.0, 22.0],
                        "text": "pack i i"
                    }
                },
                # Format note-off as midi message for jweb: midi <status> <note> <velocity>
                {
                    "box": {
                        "id": "obj-noteoff-fmt",
                        "maxclass": "message",
                        "numinlets": 2,
                        "numoutlets": 1,
                        "outlettype": [""],
                        "patching_rect": [70.0, 350.0, 80.0, 22.0],
                        "text": "midi 128 $1 $2"
                    }
                },
                # Pack pitch bend: LSB, MSB -> midi message for jweb
                {
                    "box": {
                        "id": "obj-pitchbend-pack",
                        "maxclass": "newobj",
                        "numinlets": 2,
                        "numoutlets": 1,
                        "outlettype": [""],
                        "patching_rect": [140.0, 320.0, 50.0, 22.0],
                        "text": "pack i i"
                    }
                },
                # Format pitch bend as midi message for jweb
                {
                    "box": {
                        "id": "obj-pitchbend-fmt",
                        "maxclass": "message",
                        "numinlets": 2,
                        "numoutlets": 1,
                        "outlettype": [""],
                        "patching_rect": [140.0, 350.0, 80.0, 22.0],
                        "text": "midi 224 $1 $2"
                    }
                },
                # Debug: print MIDI messages
                {
                    "box": {
                        "id": "obj-midi-print",
                        "maxclass": "newobj",
                        "numinlets": 1,
                        "numoutlets": 0,
                        "patching_rect": [10.0, 380.0, 80.0, 22.0],
                        "text": "print [AC-MIDI]"
                    }
                }
            ],
            "lines": [
                # Audio routing: jweb~ -> plugout~
                {"patchline": {"destination": ["obj-out", 1], "source": ["obj-jweb", 1]}},
                {"patchline": {"destination": ["obj-out", 0], "source": ["obj-jweb", 0]}},
                
                # Route messages from jweb~ (outlet 2) through ready router
                {"patchline": {"destination": ["obj-ready-route", 0], "source": ["obj-jweb", 2]}},
                {"patchline": {"destination": ["obj-jweb-print", 0], "source": ["obj-jweb", 2]}},
                
                # When page signals 'ready', trigger getid to start Live API sync
                {"patchline": {"destination": ["obj-ready-print", 0], "source": ["obj-ready-route", 0]}},
                {"patchline": {"destination": ["obj-getid-msg", 0], "source": ["obj-ready-route", 0]}},
                {"patchline": {"destination": ["obj-activate-msg", 0], "source": ["obj-ready-route", 0]}},
                
                # Activate message -> jweb + print (to auto-resume AudioContext)
                {"patchline": {"destination": ["obj-jweb", 0], "source": ["obj-activate-msg", 0]}},
                {"patchline": {"destination": ["obj-activate-print", 0], "source": ["obj-activate-msg", 0]}},
                
                # Debug: print when device loads
                {"patchline": {"destination": ["obj-load-print", 0], "source": ["obj-thisdevice", 0]}},
                
                # getid message -> live.path (now triggered by ready signal, not device load)
                {"patchline": {"destination": ["obj-getid-print", 0], "source": ["obj-getid-msg", 0]}},
                {"patchline": {"destination": ["obj-tempo-path", 0], "source": ["obj-getid-msg", 0]}},
                
                # Debug: print path id from all three outlets
                {"patchline": {"destination": ["obj-path-print", 0], "source": ["obj-tempo-path", 0]}},
                {"patchline": {"destination": ["obj-path-print-m", 0], "source": ["obj-tempo-path", 1]}},
                {"patchline": {"destination": ["obj-path-print-r", 0], "source": ["obj-tempo-path", 2]}},
                
                # live.path left outlet (id from getid) -> live.observer right inlet
                {"patchline": {"destination": ["obj-tempo-observer", 1], "source": ["obj-tempo-path", 0]}},
                {"patchline": {"destination": ["obj-transport-observer", 1], "source": ["obj-tempo-path", 0]}},
                {"patchline": {"destination": ["obj-phase-observer", 1], "source": ["obj-tempo-path", 0]}},
                
                # Delay trigger: live.path output also triggers delay for initial value fetch
                {"patchline": {"destination": ["obj-init-delay", 0], "source": ["obj-tempo-path", 0]}},
                
                # After delay, bang the LEFT inlet of observers to get initial values
                {"patchline": {"destination": ["obj-tempo-observer", 0], "source": ["obj-init-delay", 0]}},
                {"patchline": {"destination": ["obj-transport-observer", 0], "source": ["obj-init-delay", 0]}},
                {"patchline": {"destination": ["obj-phase-observer", 0], "source": ["obj-init-delay", 0]}},
                
                # Also trigger sample rate query after delay (audio engine should be ready by then)
                {"patchline": {"destination": ["obj-samplerate-adstatus", 0], "source": ["obj-init-delay", 0]}},
                
                # Tempo observer -> sprintf -> jweb + print (also print the formatted script command)
                {"patchline": {"destination": ["obj-tempo-sprintf", 0], "source": ["obj-tempo-observer", 0]}},
                {"patchline": {"destination": ["obj-jweb", 0], "source": ["obj-tempo-sprintf", 0]}},
                {"patchline": {"destination": ["obj-tempo-print", 0], "source": ["obj-tempo-observer", 0]}},
                {"patchline": {"destination": ["obj-tempo-script-print", 0], "source": ["obj-tempo-sprintf", 0]}},
                
                # Transport observer -> sprintf -> jweb + print
                {"patchline": {"destination": ["obj-transport-sprintf", 0], "source": ["obj-transport-observer", 0]}},
                {"patchline": {"destination": ["obj-jweb", 0], "source": ["obj-transport-sprintf", 0]}},
                {"patchline": {"destination": ["obj-transport-script-print", 0], "source": ["obj-transport-sprintf", 0]}},
                {"patchline": {"destination": ["obj-transport-print", 0], "source": ["obj-transport-observer", 0]}},
                
                # Phase observer -> sprintf -> jweb (for beat position sync)
                {"patchline": {"destination": ["obj-phase-sprintf", 0], "source": ["obj-phase-observer", 0]}},
                {"patchline": {"destination": ["obj-jweb", 0], "source": ["obj-phase-sprintf", 0]}},
                
                # Sample rate: adstatus sr -> filter "clear" -> sprintf -> jweb + print
                # (triggered directly by ready signal, not init-delay, to ensure it arrives first)
                # sel clear: left outlet = matched "clear" (ignored), right outlet = non-matching (sample rate)
                {"patchline": {"destination": ["obj-samplerate-filter", 0], "source": ["obj-samplerate-adstatus", 0]}},
                {"patchline": {"destination": ["obj-samplerate-sprintf", 0], "source": ["obj-samplerate-filter", 1]}},
                {"patchline": {"destination": ["obj-jweb", 0], "source": ["obj-samplerate-sprintf", 0]}},
                {"patchline": {"destination": ["obj-samplerate-print", 0], "source": ["obj-samplerate-filter", 1]}},
                {"patchline": {"destination": ["obj-samplerate-script-print", 0], "source": ["obj-samplerate-sprintf", 0]}},
                
                # MIDI routing: midiin -> midiparse
                {"patchline": {"destination": ["obj-midiparse", 0], "source": ["obj-midiin", 0]}},
                
                # Note-on (outlet 0): note, velocity pair -> pack -> format -> jweb
                {"patchline": {"destination": ["obj-noteon-pack", 0], "source": ["obj-midiparse", 0]}},
                {"patchline": {"destination": ["obj-noteon-fmt", 0], "source": ["obj-noteon-pack", 0]}},
                {"patchline": {"destination": ["obj-jweb", 0], "source": ["obj-noteon-fmt", 0]}},
                {"patchline": {"destination": ["obj-midi-print", 0], "source": ["obj-noteon-fmt", 0]}},
                
                # Poly aftertouch / note-off (outlet 1): note, pressure -> pack -> format -> jweb
                {"patchline": {"destination": ["obj-noteoff-pack", 0], "source": ["obj-midiparse", 1]}},
                {"patchline": {"destination": ["obj-noteoff-fmt", 0], "source": ["obj-noteoff-pack", 0]}},
                {"patchline": {"destination": ["obj-jweb", 0], "source": ["obj-noteoff-fmt", 0]}},
                {"patchline": {"destination": ["obj-midi-print", 0], "source": ["obj-noteoff-fmt", 0]}},
                
                # Pitch bend (outlet 6): bend value -> format -> jweb
                {"patchline": {"destination": ["obj-pitchbend-pack", 0], "source": ["obj-midiparse", 6]}},
                {"patchline": {"destination": ["obj-pitchbend-fmt", 0], "source": ["obj-pitchbend-pack", 0]}},
                {"patchline": {"destination": ["obj-jweb", 0], "source": ["obj-pitchbend-fmt", 0]}},
                {"patchline": {"destination": ["obj-midi-print", 0], "source": ["obj-pitchbend-fmt", 0]}}
            ],
            "dependency_cache": [],
            "latency": 0,
            "is_mpe": 0,
            "external_mpe_tuning_enabled": 0,
            "minimum_live_version": "",
            "minimum_max_version": "",
            "platform_compatibility": 0,
            "autosave": 0
        }
    }

def build_amxd(patcher: dict, output_amxd: Path) -> int:
    """Convert patcher dict to .amxd binary format."""
    
    # Serialize to JSON string (compact)
    json_data = json.dumps(patcher)
    json_bytes = json_data.encode('utf-8')
    
    # Build the .amxd file
    # Format: 32-byte header + 4-byte length (little-endian) + JSON data
    length_bytes = struct.pack('<I', len(json_bytes))
    
    with open(output_amxd, 'wb') as f:
        f.write(M4L_HEADER)
        f.write(length_bytes)
        f.write(json_bytes)
    
    return os.path.getsize(output_amxd)

def install_to_ableton(amxd_path: Path, remote_host: str = None, is_effect: bool = False) -> None:
    """Copy .amxd to Ableton User Library."""
    
    # Use correct folder based on device type
    folder_type = "Audio Effects/Max Audio Effect" if is_effect else "Instruments/Max Instrument"
    
    if remote_host:
        # Remote install via SSH - use single quotes around the whole remote path
        dest = f"/Users/jas/Music/Ableton/User Library/Presets/{folder_type}/{amxd_path.name}"
        # Escape single quotes in dest path and wrap in single quotes for shell
        escaped_dest = dest.replace("'", "'\\''")
        cmd = f"scp '{amxd_path}' '{remote_host}:{escaped_dest}'"
        result = os.system(cmd)
        if result == 0:
            print(f"   📦 Installed to: {remote_host}")
        else:
            print(f"   ❌ Failed to install (exit code {result})")
    else:
        # Local install
        user_library = Path.home() / "Music" / "Ableton" / "User Library" / "Presets" / folder_type.replace("/", os.sep)
        
        if not user_library.exists():
            print(f"   ⚠️  Ableton User Library not found at: {user_library}")
            return
        
        dest = user_library / amxd_path.name
        import shutil
        shutil.copy2(amxd_path, dest)
        print(f"   📦 Installed to: {dest}")

def main():
    script_dir = Path(__file__).parent
    config_path = script_dir / "devices.json"
    
    if not config_path.exists():
        print(f"❌ Config file not found: {config_path}")
        sys.exit(1)
    
    with open(config_path, 'r') as f:
        config = json.load(f)
    
    devices = config.get("devices", [])
    defaults = config.get("defaults", {})
    
    # Parse arguments
    install = "--install" in sys.argv
    list_only = "--list" in sys.argv
    production = "--production" in sys.argv or "--prod" in sys.argv
    remote = None
    
    for arg in sys.argv[1:]:
        if arg.startswith("--remote="):
            remote = arg.split("=", 1)[1]
    
    # Auto-detect remote host from machines.json when in devcontainer
    if install and not remote:
        # Check if we're in devcontainer (Docker)
        in_devcontainer = os.path.exists("/.dockerenv") or os.environ.get("REMOTE_CONTAINERS")
        if in_devcontainer:
            # Default to MacBook via Docker host gateway
            remote = "jas@host.docker.internal"
            print(f"📡 Auto-detected devcontainer, using remote: {remote}")
    
    # Get specific device filter
    device_filter = None
    for arg in sys.argv[1:]:
        if not arg.startswith("--"):
            device_filter = arg.lower()
            break
    
    if list_only:
        print("📦 Available AC M4L Devices:")
        for d in devices:
            print(f"   • {d['name']} ({d['piece']})")
        return
    
    # Get base URL for display in device names
    base_url = defaults.get("baseUrl", "https://localhost:8888")
    
    mode = "PRODUCTION" if production else f"DEV → {base_url}"
    print(f"🎹 Building AC M4L Device Suite [{mode}]")
    print("=" * 40)
    
    built = []
    for device in devices:
        original_name = device["name"]
        piece = device["piece"]
        has_custom_url = device.get("url") or device.get("devUrl") or device.get("prodUrl")
        is_effect = device.get("type") == "effect"
        
        # Filter if specified
        if device_filter and device_filter not in piece.lower() and device_filter not in original_name.lower():
            continue
        
        # Use different emoji for effect vs instrument devices
        device_emoji = "🎸" if is_effect else "🟪"
        
        # Handle custom URL devices (like kidlisp.com) vs piece-based devices
        if has_custom_url:
            # Custom URL device - use original name directly
            display_name = original_name
            # Sanitize piece for filename (replace slashes with dashes)
            safe_piece = piece.replace("/", "-")
            filename = f"{original_name}.amxd"
        elif production:
            display_name = f"AC {device_emoji} {piece} (aesthetic.computer)"
            filename = f"AC {device_emoji} {piece} (aesthetic.computer).amxd"
        else:
            # Extract host from URL for cleaner display
            url_host = base_url.replace("https://", "").replace("http://", "")
            display_name = f"AC {device_emoji} {piece} ({url_host})"
            filename = f"AC {device_emoji} {piece} ({url_host}).amxd"
        
        device_type_str = "Effect" if is_effect else "Instrument"
        print(f"\n🔧 {display_name} [{device_type_str}]")
        
        # Generate patcher with updated name
        device_copy = device.copy()
        device_copy["name"] = display_name
        patcher = generate_patcher(device_copy, defaults, production=production)
        
        output_path = script_dir / filename
        size = build_amxd(patcher, output_path)
        print(f"   ✅ Built: {output_path.name} ({size} bytes)")
        
        built.append(output_path)
        
        # Install if requested
        if install:
            install_to_ableton(output_path, remote, is_effect=is_effect)
    
    print(f"\n{'=' * 40}")
    print(f"✨ Built {len(built)} device(s)")

if __name__ == "__main__":
    main()
