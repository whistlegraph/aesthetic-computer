#!/usr/bin/env python3
"""Run paired baseline/fast native-app measurements, then aggregate all keys."""
import argparse
import json
from pathlib import Path
import subprocess
import sys

parser = argparse.ArgumentParser()
parser.add_argument('--runs', type=int, default=3)
parser.add_argument('--output', type=Path, default=Path('/tmp/ac-prompt-performance'))
args = parser.parse_args()
if args.runs < 1:
    parser.error('--runs must be positive')
args.output.mkdir(parents=True, exist_ok=True)
probe = Path(__file__).with_name('probe-prompt-latency.py')
groups = {}
viewport = None
for interval in (80, 16):
    for run in range(args.runs):
        for baseline in ((True, False) if run % 2 == 0 else (False, True)):
            mode = 'baseline' if baseline else 'fast'
            label = f'{mode}-{interval}-{run + 1}'
            path = args.output / f'{label}.json'
            print(f'Running {label}', flush=True)
            command = [sys.executable, str(probe), '--interval', str(interval), '--output', str(path)]
            if baseline:
                command.append('--baseline')
            with (args.output / f'{label}.txt').open('w') as output:
                subprocess.run(command, stdout=output, stderr=subprocess.STDOUT, check=True)
            result = json.loads(path.read_text())
            # SwiftUI's title bar consumes part of NSWindow's content size;
            # require equal actual web viewports across all paired runs.
            viewport = viewport or result['viewport']
            if result['viewport'] != viewport:
                raise SystemExit(f'Window resized during {label}: {result["viewport"]}; rerun for a comparable sample')
            if result['keysVisible'] != 60:
                raise SystemExit(f'Incomplete rendered input in {label}')
            groups.setdefault(f'{mode}-{interval}', []).append(result)

def stats(values):
    values = sorted(values)
    return {'count': len(values), 'median': values[len(values)//2],
            'p95': values[min(len(values)-1, int(len(values)*.95))], 'max': values[-1]}

summary = {}
for name, results in groups.items():
    samples = [s for result in results for s in result['samples']]
    summary[name] = {
        'keys': sum(r['keys'] for r in results),
        'keysVisible': sum(r['keysVisible'] for r in results),
        'keyToCanvasMs': stats([s['visible']-s['keydown'] for s in samples]),
        'keyToNextRAFMs': stats([s['visibleRAF']-s['keydown'] for s in samples]),
        'firstKeyMs': [r['firstKey'] for r in results],
    }
    native = [d for r in results for d in r.get('nativeToKeydown', [])]
    if native:  # Older app builds do not report the native send time.
        summary[name]['nativeToKeydownMs'] = stats(native)
(args.output / 'summary.json').write_text(json.dumps(summary, indent=2) + '\n')
print(json.dumps(summary, indent=2))
