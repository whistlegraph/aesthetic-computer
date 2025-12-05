#!/usr/bin/env python3
"""
Parse UE5 uncooked .uasset files to extract widget/blueprint references.
Works outside of Unreal Editor.
"""
import struct
import sys
import json
from pathlib import Path

def parse_uasset(filepath):
    """Parse uncooked .uasset file and extract useful references"""
    with open(filepath, 'rb') as f:
        data = f.read()
    
    result = {
        'file': str(filepath),
        'size': len(data),
        'game_paths': [],
        'script_refs': [],
        'widget_refs': [],
        'button_bindings': [],
        'fonts': [],
        'textures': []
    }
    
    # Find /Game/ paths (widget, blueprint, asset references)
    game_pattern = b'/Game/'
    idx = 0
    while True:
        idx = data.find(game_pattern, idx)
        if idx == -1:
            break
        end = idx
        while end < len(data) and data[end] != 0 and 32 <= data[end] < 127:
            end += 1
        path = data[idx:end].decode('utf-8', errors='replace')
        if path and path not in result['game_paths']:
            result['game_paths'].append(path)
            # Categorize
            if 'WBP_' in path or 'Widget' in path:
                result['widget_refs'].append(path)
            if 'Font' in path:
                result['fonts'].append(path)
            if 'Texture' in path or '.uasset' in path.lower():
                result['textures'].append(path)
        idx += 1
    
    # Find /Script/ references (engine classes)
    script_pattern = b'/Script/'
    idx = 0
    while True:
        idx = data.find(script_pattern, idx)
        if idx == -1:
            break
        end = idx
        while end < len(data) and data[end] != 0 and 32 <= data[end] < 127:
            end += 1
        path = data[idx:end].decode('utf-8', errors='replace')
        if path and path not in result['script_refs']:
            result['script_refs'].append(path)
        idx += 1
    
    # Find button event bindings (BndEvt__ pattern)
    evt_pattern = b'BndEvt__'
    idx = 0
    while True:
        idx = data.find(evt_pattern, idx)
        if idx == -1:
            break
        end = idx
        while end < len(data) and data[end] != 0 and 32 <= data[end] < 127:
            end += 1
        binding = data[idx:end].decode('utf-8', errors='replace')
        if binding and binding not in result['button_bindings']:
            result['button_bindings'].append(binding)
        idx += 1
    
    return result

def compare_widgets(file1, file2):
    """Compare two widget assets"""
    r1 = parse_uasset(file1)
    r2 = parse_uasset(file2)
    
    comparison = {
        'file1': str(file1),
        'file2': str(file2),
        'widget_refs': {
            'only_in_file1': [x for x in r1['widget_refs'] if x not in r2['widget_refs']],
            'only_in_file2': [x for x in r2['widget_refs'] if x not in r1['widget_refs']],
            'common': [x for x in r1['widget_refs'] if x in r2['widget_refs']]
        },
        'button_bindings': {
            'only_in_file1': [x for x in r1['button_bindings'] if x not in r2['button_bindings']],
            'only_in_file2': [x for x in r2['button_bindings'] if x not in r1['button_bindings']],
            'common': [x for x in r1['button_bindings'] if x in r2['button_bindings']]
        },
        'game_paths': {
            'only_in_file1': [x for x in r1['game_paths'] if x not in r2['game_paths']],
            'only_in_file2': [x for x in r2['game_paths'] if x not in r1['game_paths']]
        }
    }
    return comparison

if __name__ == '__main__':
    if len(sys.argv) < 2:
        print("Usage:")
        print("  python parse_uasset.py <file.uasset>           # Parse single file")
        print("  python parse_uasset.py <file1> <file2>         # Compare two files")
        sys.exit(1)
    
    if len(sys.argv) == 2:
        result = parse_uasset(sys.argv[1])
        print(json.dumps(result, indent=2))
    else:
        comparison = compare_widgets(sys.argv[1], sys.argv[2])
        print(json.dumps(comparison, indent=2))
