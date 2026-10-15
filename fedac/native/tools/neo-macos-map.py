#!/usr/bin/env python3
"""Read hardware identifiers on a macOS reference machine; no device writes.

Use --output to save a local JSON reference for J700 Linux bring-up. Properties
are allowlisted: no network names, credentials, serials, or user input events.
PCI IDs come from live IOPCIDevice properties, not firmware compatible strings.
"""
import argparse
import datetime
import json
from pathlib import Path
import plistlib
import re
import subprocess


def registry(*args):
    data = subprocess.check_output(['ioreg', *args, '-a'])
    return plistlib.loads(data) if data.strip() else []


def walk(value, ancestors=()):
    if isinstance(value, list):
        for node in value:
            yield from walk(node, ancestors)
    elif isinstance(value, dict):
        chain = ancestors + ((value.get('IORegistryEntryName', '?'),
                              value.get('IOObjectClass', '?')),)
        yield value, chain
        yield from walk(value.get('IORegistryEntryChildren', []), chain)


def encoded(value):
    if isinstance(value, bytes):
        return {'hex': value.hex()}
    if isinstance(value, (str, int, bool)):
        return value
    return None


def selected(node, keys):
    return {k: encoded(node[k]) for k in keys if k in node}


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--output', type=Path)
    args = parser.parse_args()
    report = {
        'captured_at': datetime.datetime.now(datetime.timezone.utc).isoformat(),
        'source': 'local macOS IORegistry; reference machine, not Linux target',
        'model': subprocess.check_output(['sysctl', '-n', 'hw.model'], text=True).strip(),
        'pci': [], 'hid': [], 'firmware_nodes': [], 'driver_chains': [],
    }
    for node, _ in walk(registry('-r', '-c', 'IOPCIDevice', '-l')):
        if node.get('IOObjectClass') != 'IOPCIDevice':
            continue
        item = selected(node, ['IORegistryEntryName', 'IOName', 'compatible'])
        for key in ['vendor-id', 'device-id', 'class-code']:
            raw = node.get(key)
            if isinstance(raw, bytes):
                item[key] = hex(int.from_bytes(raw, 'little'))
        report['pci'].append(item)
    for node, _ in walk(registry('-r', '-c', 'IOHIDDevice', '-l')):
        if node.get('IOObjectClass') != 'AppleHIDTransportHIDDevice' or node.get('Transport') != 'FIFO':
            continue
        report['hid'].append(selected(node, ['Product', 'Transport', 'Built-In',
            'VendorID', 'ProductID', 'PrimaryUsagePage', 'PrimaryUsage', 'ReportDescriptor']))
    hardware = re.compile(r'mtp|hid|keyboard|audio|mca|codec|speaker|wlan|wifi|dcp|backlight|pwm|i2c|spmi|apcie|gpio|admac|smc', re.I)
    props = ['compatible', 'reg', 'interrupts', 'interrupt-parent', 'clock-gates',
             'power-gates', 'AAPL,phandle', 'device_type', 'dart-options']
    for node, chain in walk(registry('-p', 'IODeviceTree', '-l')):
        path = '/'.join(name for name, _ in chain)
        if hardware.search(path):
            report['firmware_nodes'].append({'path': path, **selected(node,
                props + [k for k in node if k.startswith('function-')])})
    driver = re.compile(r'backlight|sunrise|HIDTransport|HIDKeyboard|admac-leap|AudioController|speaker|AppleDCP', re.I)
    for node, chain in walk(registry('-p', 'IOService')):
        if driver.search(node.get('IORegistryEntryName', '')):
            report['driver_chains'].append([{'name': n, 'class': c} for n, c in chain[-6:]])
    text = json.dumps(report, indent=2) + '\n'
    if args.output:
        args.output.write_text(text)
        print(args.output.resolve())
    else:
        print(text, end='')


if __name__ == '__main__':
    main()
