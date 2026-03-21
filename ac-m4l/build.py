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

# M4L binary headers - the 4-byte marker after 'ampf' determines device type
# 'iiii' = Instrument, 'aaaa' = Audio Effect, 'mmmm' = MIDI Effect
M4L_HEADER_INSTRUMENT = b"ampf\x04\x00\x00\x00iiiimeta\x04\x00\x00\x00\x00\x00\x00\x00ptch"
M4L_HEADER_AUDIO_EFFECT = b"ampf\x04\x00\x00\x00aaaameta\x04\x00\x00\x00\x00\x00\x00\x00ptch"
M4L_HEADER_MIDI_EFFECT = b"ampf\x04\x00\x00\x00mmmmmeta\x04\x00\x00\x00\x00\x00\x00\x00ptch"

def generate_patcher(device: dict, defaults: dict, production: bool = False) -> dict:
    """Generate a complete M4L patcher for a device."""
    
    # Check if this is an effect device (has audio input)
    is_effect = device.get("type") == "effect"
    
    if is_effect:
        return generate_effect_patcher(device, defaults, production)
    else:
        return generate_instrument_patcher(device, defaults, production)

def generate_effect_patcher(device: dict, defaults: dict, production: bool = False) -> dict:
    """Generate a M4L Audio Effect patcher that streams audio to AC Web Audio.
    
    Architecture:
    - plugin~ receives stereo audio from Ableton  
    - record~ writes to circular buffer~ at full 44.1kHz
    - metro triggers js to read chunks from buffer~
    - js sends sample arrays to jweb~ via executejavascript
    - AC's Web Audio engine processes with effects
    - jweb~ audio output goes to plugout~
    """
    
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
    
    url = f"{base_url}/{piece}?daw=1&density={density}&nogap&width={width}&height={height}&effect=1"
    
    # Buffer size in samples (4096 samples = ~93ms at 44.1kHz)
    BUFFER_SIZE = 4096
    # Chunk read interval in ms (11.6ms = 512 samples at 44.1kHz)
    READ_INTERVAL = 12
    
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
            "rect": [134.0, 174.0, 900.0, 600.0],
            "openrect": [0.0, 0.0, float(width), float(height)],
            "openinpresentation": 1,
            "gridsize": [15.0, 15.0],
            "enablehscroll": 0,
            "enablevscroll": 0,
            "devicewidth": float(width),
            "description": description,
            "boxes": [
                # === AUDIO INPUT ===
                {
                    "box": {
                        "id": "obj-plugin",
                        "maxclass": "newobj",
                        "numinlets": 1,
                        "numoutlets": 2,
                        "outlettype": ["signal", "signal"],
                        "patching_rect": [10.0, 10.0, 65.0, 22.0],
                        "text": "plugin~ 2"
                    }
                },
                
                # === CIRCULAR BUFFERS ===
                {
                    "box": {
                        "id": "obj-buf-L",
                        "maxclass": "newobj",
                        "numinlets": 1,
                        "numoutlets": 2,
                        "outlettype": ["float", "bang"],
                        "patching_rect": [10.0, 40.0, 120.0, 22.0],
                        "text": f"buffer~ ac-audio-L {BUFFER_SIZE}"
                    }
                },
                {
                    "box": {
                        "id": "obj-buf-R",
                        "maxclass": "newobj",
                        "numinlets": 1,
                        "numoutlets": 2,
                        "outlettype": ["float", "bang"],
                        "patching_rect": [140.0, 40.0, 120.0, 22.0],
                        "text": f"buffer~ ac-audio-R {BUFFER_SIZE}"
                    }
                },
                
                # === RECORD TO BUFFERS (circular/looping) ===
                {
                    "box": {
                        "id": "obj-rec-L",
                        "maxclass": "newobj",
                        "numinlets": 3,
                        "numoutlets": 1,
                        "outlettype": ["signal"],
                        "patching_rect": [10.0, 70.0, 110.0, 22.0],
                        "text": "record~ ac-audio-L 1"
                    }
                },
                {
                    "box": {
                        "id": "obj-rec-R",
                        "maxclass": "newobj",
                        "numinlets": 3,
                        "numoutlets": 1,
                        "outlettype": ["signal"],
                        "patching_rect": [140.0, 70.0, 110.0, 22.0],
                        "text": "record~ ac-audio-R 1"
                    }
                },
                
                # === START RECORDING ON LOAD ===
                {
                    "box": {
                        "id": "obj-loadbang",
                        "maxclass": "newobj",
                        "numinlets": 1,
                        "numoutlets": 1,
                        "outlettype": ["bang"],
                        "patching_rect": [280.0, 10.0, 60.0, 22.0],
                        "text": "loadbang"
                    }
                },
                {
                    "box": {
                        "id": "obj-rec-on",
                        "maxclass": "message",
                        "numinlets": 2,
                        "numoutlets": 1,
                        "outlettype": [""],
                        "patching_rect": [280.0, 40.0, 55.0, 22.0],
                        "text": "loop 1, 1"
                    }
                },
                
                # === METRO FOR READING CHUNKS ===
                {
                    "box": {
                        "id": "obj-metro",
                        "maxclass": "newobj",
                        "numinlets": 2,
                        "numoutlets": 1,
                        "outlettype": ["bang"],
                        "patching_rect": [350.0, 40.0, 65.0, 22.0],
                        "text": f"metro {READ_INTERVAL}"
                    }
                },
                {
                    "box": {
                        "id": "obj-start-msg",
                        "maxclass": "message",
                        "numinlets": 2,
                        "numoutlets": 1,
                        "outlettype": [""],
                        "patching_rect": [350.0, 70.0, 35.0, 22.0],
                        "text": "start"
                    }
                },
                
                # === JS BUFFER READER ===
                {
                    "box": {
                        "id": "obj-js",
                        "maxclass": "newobj",
                        "numinlets": 1,
                        "numoutlets": 1,
                        "outlettype": [""],
                        "patching_rect": [350.0, 100.0, 110.0, 22.0],
                        "text": "js buffer-reader.js"
                    }
                },
                
                # === PEAK VISUALIZATION ===
                {
                    "box": {
                        "id": "obj-mono-mix",
                        "maxclass": "newobj",
                        "numinlets": 2,
                        "numoutlets": 1,
                        "outlettype": ["signal"],
                        "patching_rect": [280.0, 50.0, 35.0, 22.0],
                        "text": "+~"
                    }
                },
                {
                    "box": {
                        "id": "obj-mono-scale",
                        "maxclass": "newobj",
                        "numinlets": 2,
                        "numoutlets": 1,
                        "outlettype": ["signal"],
                        "patching_rect": [280.0, 80.0, 45.0, 22.0],
                        "text": "*~ 0.5"
                    }
                },
                {
                    "box": {
                        "id": "obj-peak",
                        "maxclass": "newobj",
                        "numinlets": 2,
                        "numoutlets": 1,
                        "outlettype": ["float"],
                        "patching_rect": [280.0, 110.0, 85.0, 22.0],
                        "text": "peakamp~ 50"
                    }
                },
                {
                    "box": {
                        "id": "obj-throttle",
                        "maxclass": "newobj",
                        "numinlets": 2,
                        "numoutlets": 1,
                        "outlettype": [""],
                        "patching_rect": [280.0, 140.0, 70.0, 22.0],
                        "text": "speedlim 33"
                    }
                },
                {
                    "box": {
                        "id": "obj-sprintf-peak",
                        "maxclass": "newobj",
                        "numinlets": 1,
                        "numoutlets": 1,
                        "outlettype": [""],
                        "patching_rect": [280.0, 170.0, 320.0, 22.0],
                        "text": "sprintf executejavascript \\\"window.acPedalPeak&&window.acPedalPeak(%f)\\\""
                    }
                },
                
                # === OUTPUT ===
                {
                    "box": {
                        "id": "obj-out",
                        "maxclass": "newobj",
                        "numinlets": 2,
                        "numoutlets": 0,
                        "patching_rect": [10.0, 200.0, 75.0, 22.0],
                        "text": "plugout~ 1 2"
                    }
                },
                
                # === JWEB~ ===
                {
                    "box": {
                        "disablefind": 0,
                        "id": "obj-jweb",
                        "latency": latency,
                        "maxclass": "jweb~",
                        "numinlets": 1,
                        "numoutlets": 3,
                        "outlettype": ["signal", "signal", ""],
                        "patching_rect": [450.0, 10.0, 320.0, 240.0],
                        "presentation": 1,
                        "presentation_rect": [0.0, 0.0, float(width + 1), float(height + 1)],
                        "rendermode": 1,
                        "url": url
                    }
                },
                {
                    "box": {
                        "id": "obj-thisdevice",
                        "maxclass": "newobj",
                        "numinlets": 1,
                        "numoutlets": 3,
                        "outlettype": ["bang", "int", "int"],
                        "patching_rect": [650.0, 280.0, 85.0, 22.0],
                        "text": "live.thisdevice"
                    }
                },
                {
                    "box": {
                        "id": "obj-print",
                        "maxclass": "newobj",
                        "numinlets": 1,
                        "numoutlets": 0,
                        "patching_rect": [650.0, 310.0, 100.0, 22.0],
                        "text": "print [AC-PEDAL]"
                    }
                },
                {
                    "box": {
                        "id": "obj-route",
                        "maxclass": "newobj",
                        "numinlets": 1,
                        "numoutlets": 2,
                        "outlettype": ["", ""],
                        "patching_rect": [780.0, 100.0, 60.0, 22.0],
                        "text": "route ready"
                    }
                },
                {
                    "box": {
                        "id": "obj-activate",
                        "maxclass": "message",
                        "numinlets": 2,
                        "numoutlets": 1,
                        "outlettype": [""],
                        "patching_rect": [780.0, 130.0, 60.0, 22.0],
                        "text": "activate 1"
                    }
                },
                {
                    "box": {
                        "id": "obj-jweb-print",
                        "maxclass": "newobj",
                        "numinlets": 1,
                        "numoutlets": 0,
                        "patching_rect": [780.0, 70.0, 90.0, 22.0],
                        "text": "print [AC-JWEB]"
                    }
                },
                # Console log forwarding
                {
                    "box": {
                        "id": "obj-route-logs",
                        "maxclass": "newobj",
                        "numinlets": 1,
                        "numoutlets": 4,
                        "outlettype": ["", "", "", ""],
                        "patching_rect": [870.0, 100.0, 120.0, 22.0],
                        "text": "route log error warn"
                    }
                },
                {
                    "box": {
                        "id": "obj-udpsend",
                        "maxclass": "newobj",
                        "numinlets": 1,
                        "numoutlets": 0,
                        "patching_rect": [870.0, 170.0, 160.0, 22.0],
                        "text": "udpsend 127.0.0.1 7777"
                    }
                },
                {
                    "box": {
                        "id": "obj-prepend-log",
                        "maxclass": "newobj",
                        "numinlets": 1,
                        "numoutlets": 1,
                        "outlettype": [""],
                        "patching_rect": [870.0, 130.0, 55.0, 22.0],
                        "text": "prepend log"
                    }
                },
                {
                    "box": {
                        "id": "obj-prepend-error",
                        "maxclass": "newobj",
                        "numinlets": 1,
                        "numoutlets": 1,
                        "outlettype": [""],
                        "patching_rect": [930.0, 130.0, 65.0, 22.0],
                        "text": "prepend error"
                    }
                },
                {
                    "box": {
                        "id": "obj-prepend-warn",
                        "maxclass": "newobj",
                        "numinlets": 1,
                        "numoutlets": 1,
                        "outlettype": [""],
                        "patching_rect": [1000.0, 130.0, 60.0, 22.0],
                        "text": "prepend warn"
                    }
                }
            ],
            "lines": [
                # === AUDIO TO CIRCULAR BUFFERS ===
                # plugin~ -> record~ (writes at full sample rate)
                {"patchline": {"destination": ["obj-rec-L", 0], "source": ["obj-plugin", 0]}},
                {"patchline": {"destination": ["obj-rec-R", 0], "source": ["obj-plugin", 1]}},
                
                # === START RECORDING ON LOAD ===
                # loadbang -> "loop 1, 1" -> record~ (enable looping and start)
                {"patchline": {"destination": ["obj-rec-on", 0], "source": ["obj-loadbang", 0]}},
                {"patchline": {"destination": ["obj-rec-L", 0], "source": ["obj-rec-on", 0]}},
                {"patchline": {"destination": ["obj-rec-R", 0], "source": ["obj-rec-on", 0]}},
                
                # === START METRO AND JS ON LOAD ===
                {"patchline": {"destination": ["obj-metro", 0], "source": ["obj-loadbang", 0]}},
                {"patchline": {"destination": ["obj-start-msg", 0], "source": ["obj-loadbang", 0]}},
                {"patchline": {"destination": ["obj-js", 0], "source": ["obj-start-msg", 0]}},
                
                # === METRO -> JS -> JWEB~ ===
                {"patchline": {"destination": ["obj-js", 0], "source": ["obj-metro", 0]}},
                {"patchline": {"destination": ["obj-jweb", 0], "source": ["obj-js", 0]}},
                
                # === PEAK VISUALIZATION ===
                {"patchline": {"destination": ["obj-mono-mix", 0], "source": ["obj-plugin", 0]}},
                {"patchline": {"destination": ["obj-mono-mix", 1], "source": ["obj-plugin", 1]}},
                {"patchline": {"destination": ["obj-mono-scale", 0], "source": ["obj-mono-mix", 0]}},
                {"patchline": {"destination": ["obj-peak", 0], "source": ["obj-mono-scale", 0]}},
                {"patchline": {"destination": ["obj-throttle", 0], "source": ["obj-peak", 0]}},
                {"patchline": {"destination": ["obj-sprintf-peak", 0], "source": ["obj-throttle", 0]}},
                {"patchline": {"destination": ["obj-jweb", 0], "source": ["obj-sprintf-peak", 0]}},
                
                # === AUDIO OUTPUT FROM JWEB~ ===
                {"patchline": {"destination": ["obj-out", 0], "source": ["obj-jweb", 0]}},
                {"patchline": {"destination": ["obj-out", 1], "source": ["obj-jweb", 1]}},
                
                # === JWEB~ MESSAGE ROUTING ===
                {"patchline": {"destination": ["obj-jweb-print", 0], "source": ["obj-jweb", 2]}},
                {"patchline": {"destination": ["obj-route", 0], "source": ["obj-jweb", 2]}},
                {"patchline": {"destination": ["obj-activate", 0], "source": ["obj-route", 0]}},
                {"patchline": {"destination": ["obj-jweb", 0], "source": ["obj-activate", 0]}},
                
                # === CONSOLE LOGS ===
                {"patchline": {"destination": ["obj-route-logs", 0], "source": ["obj-jweb", 2]}},
                {"patchline": {"destination": ["obj-prepend-log", 0], "source": ["obj-route-logs", 0]}},
                {"patchline": {"destination": ["obj-prepend-error", 0], "source": ["obj-route-logs", 1]}},
                {"patchline": {"destination": ["obj-prepend-warn", 0], "source": ["obj-route-logs", 2]}},
                {"patchline": {"destination": ["obj-udpsend", 0], "source": ["obj-prepend-log", 0]}},
                {"patchline": {"destination": ["obj-udpsend", 0], "source": ["obj-prepend-error", 0]}},
                {"patchline": {"destination": ["obj-udpsend", 0], "source": ["obj-prepend-warn", 0]}},
                
                # Device load
                {"patchline": {"destination": ["obj-print", 0], "source": ["obj-thisdevice", 0]}}
            ],
            "dependency_cache": [
                {"name": "buffer-reader.js", "bootpath": "~/Documents/Max 8/Packages/aesthetic-computer/javascript"}
            ],
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
        # Standard: Build URL from piece name
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
            "rect": [134.0, 174.0, 800.0, 600.0],
            "openrect": [0.0, 0.0, float(width), float(height)],
            "openinpresentation": 1,
            "gridsize": [15.0, 15.0],
            "enablehscroll": 0,
            "enablevscroll": 0,
            "devicewidth": float(width),
            "description": description,
            "boxes": [
                {
                    "box": {
                        "disablefind": 0,
                        "id": "obj-jweb",
                        "latency": latency,
                        "maxclass": "jweb~",
                        "numinlets": 1,
                        "numoutlets": 3,
                        "outlettype": ["signal", "signal", ""],
                        "patching_rect": [10.0, 50.0, float(width), float(height)],
                        "presentation": 1,
                        "presentation_rect": [0.0, 0.0, float(width + 1), float(height + 1)],
                        "rendermode": 1,
                        "url": url
                    }
                },
                {
                    "box": {
                        "id": "obj-plugout",
                        "maxclass": "newobj",
                        "numinlets": 2,
                        "numoutlets": 0,
                        "patching_rect": [10.0, 280.0, 75.0, 22.0],
                        "text": "plugout~ 1 2"
                    }
                },
                {
                    "box": {
                        "id": "obj-thisdevice",
                        "maxclass": "newobj",
                        "numinlets": 1,
                        "numoutlets": 3,
                        "outlettype": ["bang", "int", "int"],
                        "patching_rect": [350.0, 50.0, 85.0, 22.0],
                        "text": "live.thisdevice"
                    }
                },
                {
                    "box": {
                        "id": "obj-print",
                        "maxclass": "newobj",
                        "numinlets": 1,
                        "numoutlets": 0,
                        "patching_rect": [350.0, 80.0, 150.0, 22.0],
                        "text": f"print [AC-{piece.upper()}]"
                    }
                },
                {
                    "box": {
                        "id": "obj-route",
                        "maxclass": "newobj",
                        "numinlets": 1,
                        "numoutlets": 2,
                        "outlettype": ["", ""],
                        "patching_rect": [350.0, 140.0, 60.0, 22.0],
                        "text": "route ready"
                    }
                },
                {
                    "box": {
                        "id": "obj-activate",
                        "maxclass": "message",
                        "numinlets": 2,
                        "numoutlets": 1,
                        "outlettype": [""],
                        "patching_rect": [350.0, 170.0, 60.0, 22.0],
                        "text": "activate 1"
                    }
                },
                {
                    "box": {
                        "id": "obj-jweb-print",
                        "maxclass": "newobj",
                        "numinlets": 1,
                        "numoutlets": 0,
                        "patching_rect": [350.0, 110.0, 90.0, 22.0],
                        "text": "print [AC-JWEB]"
                    }
                },
                {
                    "box": {
                        "id": "obj-route-logs",
                        "maxclass": "newobj",
                        "numinlets": 1,
                        "numoutlets": 4,
                        "outlettype": ["", "", "", ""],
                        "patching_rect": [470.0, 140.0, 120.0, 22.0],
                        "text": "route log error warn"
                    }
                },
                {
                    "box": {
                        "id": "obj-udpsend",
                        "maxclass": "newobj",
                        "numinlets": 1,
                        "numoutlets": 0,
                        "patching_rect": [470.0, 210.0, 160.0, 22.0],
                        "text": "udpsend 127.0.0.1 7777"
                    }
                },
                {
                    "box": {
                        "id": "obj-prepend-log",
                        "maxclass": "newobj",
                        "numinlets": 1,
                        "numoutlets": 1,
                        "outlettype": [""],
                        "patching_rect": [470.0, 170.0, 55.0, 22.0],
                        "text": "prepend log"
                    }
                },
                {
                    "box": {
                        "id": "obj-prepend-error",
                        "maxclass": "newobj",
                        "numinlets": 1,
                        "numoutlets": 1,
                        "outlettype": [""],
                        "patching_rect": [530.0, 170.0, 65.0, 22.0],
                        "text": "prepend error"
                    }
                },
                {
                    "box": {
                        "id": "obj-prepend-warn",
                        "maxclass": "newobj",
                        "numinlets": 1,
                        "numoutlets": 1,
                        "outlettype": [""],
                        "patching_rect": [600.0, 170.0, 60.0, 22.0],
                        "text": "prepend warn"
                    }
                }
            ],
            "lines": [
                {"patchline": {"destination": ["obj-plugout", 0], "source": ["obj-jweb", 0]}},
                {"patchline": {"destination": ["obj-plugout", 1], "source": ["obj-jweb", 1]}},
                {"patchline": {"destination": ["obj-print", 0], "source": ["obj-thisdevice", 0]}},
                {"patchline": {"destination": ["obj-jweb-print", 0], "source": ["obj-jweb", 2]}},
                {"patchline": {"destination": ["obj-route", 0], "source": ["obj-jweb", 2]}},
                {"patchline": {"destination": ["obj-activate", 0], "source": ["obj-route", 0]}},
                {"patchline": {"destination": ["obj-jweb", 0], "source": ["obj-activate", 0]}},
                {"patchline": {"destination": ["obj-route-logs", 0], "source": ["obj-jweb", 2]}},
                {"patchline": {"destination": ["obj-prepend-log", 0], "source": ["obj-route-logs", 0]}},
                {"patchline": {"destination": ["obj-prepend-error", 0], "source": ["obj-route-logs", 1]}},
                {"patchline": {"destination": ["obj-prepend-warn", 0], "source": ["obj-route-logs", 2]}},
                {"patchline": {"destination": ["obj-udpsend", 0], "source": ["obj-prepend-log", 0]}},
                {"patchline": {"destination": ["obj-udpsend", 0], "source": ["obj-prepend-error", 0]}},
                {"patchline": {"destination": ["obj-udpsend", 0], "source": ["obj-prepend-warn", 0]}}
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

def build_device(device: dict, defaults: dict, production: bool = False) -> bytes:
    """Build a complete .amxd file for a device."""
    
    patcher = generate_patcher(device, defaults, production)
    
    # Serialize patcher to JSON
    patcher_json = json.dumps(patcher, separators=(',', ':')).encode('utf-8')
    
    # Choose header based on device type
    device_type = device.get("type", "instrument")
    if device_type == "effect":
        header = M4L_HEADER_AUDIO_EFFECT
    elif device_type == "midi":
        header = M4L_HEADER_MIDI_EFFECT
    else:
        header = M4L_HEADER_INSTRUMENT
    
    # Pack length as 4-byte little-endian
    length_bytes = struct.pack('<I', len(patcher_json))
    
    # Combine header + length + JSON
    return header + length_bytes + patcher_json

def load_config() -> dict:
    """Load device configuration from devices.json."""
    config_path = Path(__file__).parent / "devices.json"
    with open(config_path) as f:
        return json.load(f)

def build_all(production: bool = False, device_filter: str = None, install: bool = False):
    """Build all devices (or a specific one)."""
    
    config = load_config()
    defaults = config.get("defaults", {})
    devices = config.get("devices", [])
    
    # Filter devices if specified
    if device_filter:
        devices = [d for d in devices if d["piece"] == device_filter]
        if not devices:
            print(f"❌ Device '{device_filter}' not found")
            return
    
    # Build output directory
    output_dir = Path(__file__).parent
    
    # Print build mode
    mode = "PROD → https://aesthetic.computer" if production else f"DEV → {defaults.get('baseUrl', 'https://localhost:8888')}"
    print(f"🎹 Building AC M4L Device Suite [{mode}]")
    print("=" * 40)
    
    built = []
    for device in devices:
        piece = device["piece"]
        device_type = device.get("type", "instrument")
        type_label = device_type.capitalize()
        
        # Build filename
        if production:
            filename = f"AC 🎸 {piece} (aesthetic.computer).amxd" if device_type == "effect" else f"AC 🟪 {piece} (aesthetic.computer).amxd"
        else:
            filename = f"AC 🎸 {piece} (localhost:8888).amxd" if device_type == "effect" else f"AC 🟪 {piece} (localhost:8888).amxd"
        
        # Build device
        data = build_device(device, defaults, production)
        
        # Write file
        output_path = output_dir / filename
        with open(output_path, 'wb') as f:
            f.write(data)
        
        print(f"\n🔧 {filename} [{type_label}]")
        print(f"   ✅ Built: {filename} ({len(data)} bytes)")
        built.append((filename, output_path))
        
        # Install if requested
        if install:
            install_path = Path.home() / "Music" / "Ableton" / "User Library" / "Presets" / "Audio Effects" / "Max Audio Effect"
            if install_path.exists():
                import shutil
                dest = install_path / filename
                shutil.copy(output_path, dest)
                print(f"   📦 Installed to: {dest}")
    
    print("\n" + "=" * 40)
    print(f"✨ Built {len(built)} device(s)")

def list_devices():
    """List available devices."""
    config = load_config()
    devices = config.get("devices", [])
    
    print("📋 Available AC M4L Devices:")
    print("=" * 40)
    for device in devices:
        piece = device["piece"]
        device_type = device.get("type", "instrument")
        description = device.get("description", "")
        print(f"  • {piece} ({device_type})")
        if description:
            print(f"    {description}")

def main():
    args = sys.argv[1:]
    
    production = "--production" in args or "--prod" in args
    install = "--install" in args
    list_only = "--list" in args
    
    # Remove flags from args
    args = [a for a in args if not a.startswith("--")]
    
    if list_only:
        list_devices()
    elif args:
        # Build specific device
        build_all(production=production, device_filter=args[0], install=install)
    else:
        # Build all devices
        build_all(production=production, install=install)

if __name__ == "__main__":
    main()
