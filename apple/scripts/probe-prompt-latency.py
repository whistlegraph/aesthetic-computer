#!/usr/bin/env python3
"""Exercise the installed macOS app with real AppKit key events and save timings."""
import argparse
import json
from pathlib import Path
import subprocess

parser = argparse.ArgumentParser()
parser.add_argument('--interval', type=int, default=80, help='Milliseconds between keys; 16 tests held-key pressure')
parser.add_argument('--output', type=Path, default=Path('/tmp/ac-input-latency.json'))
parser.add_argument('--app', type=Path, default=Path('/Applications/Aesthetic Computer.app'))
args = parser.parse_args()
args.output = args.output.resolve()
script = Path(__file__).with_name('prompt-latency.js').resolve()
executable = args.app / 'Contents/MacOS/Aesthetic Computer'
args.output.parent.mkdir(parents=True, exist_ok=True)
args.output.unlink(missing_ok=True)
with args.output.with_suffix('.log').open('w') as log:
    subprocess.run([str(executable), '--ac-input-probe', str(script),
                    '--ac-input-result', str(args.output),
                    '--ac-input-interval-ms', str(args.interval)],
                   stdout=log, stderr=log, timeout=120, check=True)
result = json.loads(args.output.read_text())
print(json.dumps({k: v for k, v in result.items() if k != 'samples'}, indent=2))
if result.get('keys') != 60 or result.get('inputLength') != 60:
    raise SystemExit('FAIL: native keys did not arrive intact')
if not result.get('keyToRendered'):
    raise SystemExit('FAIL: no rendered input samples; confirm deployed runtime has probe markers')
print(f'Results: {args.output}\nSnapshot: {args.output}.png')
