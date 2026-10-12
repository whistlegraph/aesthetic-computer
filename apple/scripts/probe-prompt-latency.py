#!/usr/bin/env python3
"""Exercise the installed macOS app with real AppKit key events and save timings."""
import argparse
import json
from pathlib import Path
import subprocess
import shutil
import tempfile

parser = argparse.ArgumentParser()
parser.add_argument('--interval', type=int, default=80, help='Milliseconds between keys; 16 tests held-key pressure')
parser.add_argument('--baseline', action='store_true', help='Use the prior animation-frame-only input scheduling for an A/B comparison')
parser.add_argument('--output', type=Path, default=Path('/tmp/ac-input-latency.json'))
parser.add_argument('--app', type=Path, default=Path('/Applications/Aesthetic Computer.app'))
args = parser.parse_args()
args.output = args.output.resolve()
script = Path(__file__).with_name('prompt-latency.js').resolve()
executable = args.app / 'Contents/MacOS/Aesthetic Computer'
args.output.parent.mkdir(parents=True, exist_ok=True)
args.output.unlink(missing_ok=True)
# Keep App Sandbox enabled. Stage this explicit test's script/results inside
# the app's own container instead of granting the app arbitrary file access.
container = Path.home() / 'Library/Containers/aesthetic.computer/Data/Library/Application Support/InputLatency'
container.mkdir(parents=True, exist_ok=True)
with tempfile.TemporaryDirectory(prefix='probe-', dir=container) as directory:
    staged = Path(directory)
    shutil.copyfile(script, staged / 'probe.js')
    native_result = staged / 'result.json'
    with args.output.with_suffix('.log').open('w') as log:
        subprocess.run([str(executable), '--ac-input-probe', str(staged / 'probe.js'),
                        '--ac-input-result', str(native_result),
                        '--ac-input-interval-ms', str(args.interval)] + (['--ac-input-baseline'] if args.baseline else []),
                       stdout=log, stderr=log, timeout=120, check=True)
    if not native_result.exists():
        raise SystemExit(f'Probe produced no result; see {args.output.with_suffix(".log")}')
    shutil.copyfile(native_result, args.output)
    snapshot = Path(str(native_result) + '.png')
    if snapshot.exists():
        shutil.copyfile(snapshot, str(args.output) + '.png')
result = json.loads(args.output.read_text())
print(json.dumps({k: v for k, v in result.items() if k not in ('samples', 'nativeToKeydown')}, indent=2))
if native := sorted(result.get('nativeToKeydown', [])):
    print(f'nativeToKeydown: median {native[len(native)//2]:.2f} ms, max {native[-1]:.2f} ms')
if result.get('keys') != 60 or result.get('inputLength') != 60:
    raise SystemExit('FAIL: native keys did not arrive intact')
if not result.get('keyToRendered'):
    raise SystemExit('FAIL: no rendered input samples; confirm deployed runtime has probe markers')
if result.get('editingCheck') is not True:
    raise SystemExit('FAIL: native backspace / arrow / insertion check failed (or installed app needs rebuilding)')
print(f'Results: {args.output}\nSnapshot: {args.output}.png')
