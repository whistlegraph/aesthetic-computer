#!/usr/bin/env python3
"""Aggregate repeated compare-terminal-latency.swift runs (never mix canvas timings)."""
import argparse
import json
import math
from pathlib import Path
import statistics


def distribution(values):
    values = sorted(values)
    if not values:
        raise ValueError('No timing samples')
    return {'n': len(values), 'meanMs': round(statistics.fmean(values), 2),
            'medianMs': round(statistics.median(values), 2),
            'p95Ms': round(values[math.ceil(len(values) * .95) - 1], 2),
            'minMs': round(values[0], 2), 'maxMs': round(values[-1], 2)}


def summarize(paths):
    groups = {}
    sizes = set()
    for path in paths:
        report = json.loads(path.read_text())
        assert report['timeouts'] == 0, f'{path}: missing frames'
        assert len(report['samples']) == report['trials'], f'{path}: missing samples'
        sizes.add(tuple(report['windowSize']))
        groups.setdefault(report['app'], []).append(report)
    assert len(sizes) == 1, f'Mismatched window sizes: {sizes}'
    assert set(groups) == {'Aesthetic Computer', 'Terminal'}, 'Need both apps'
    result = {'windowSize': list(sizes.pop()), 'apps': {}}
    for app, reports in groups.items():
        samples = [sample for report in reports for sample in report['samples']]
        result['apps'][app] = {
            'runs': len(reports),
            'all': distribution([s['keyToWindowServerMs'] for s in samples]),
            **{action: distribution([s['keyToWindowServerMs'] for s in samples if s['action'] == action])
               for action in ('insert', 'delete')},
            # Coarse capture: the true latency lies in (lowerBound, match].
            # Newer harness runs record the bound; midpoints de-bias the mean.
            **({'censoredMidpoint': distribution([(s['lowerBoundMs'] + s['keyToWindowServerMs']) / 2 for s in samples])}
               if all('lowerBoundMs' in s for s in samples) else {}),
            'captureFrameIntervals': distribution([v for r in reports for v in r['captureFrameIntervalsMs']]),
            'perRunMedianMs': [round(statistics.median(s['keyToWindowServerMs'] for s in r['samples']), 2)
                               for r in reports],
        }
    result['measurement'] = reports[0]['measurement']
    return result


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('results', type=Path, nargs='+')
    parser.add_argument('--output', type=Path)
    args = parser.parse_args()
    encoded = json.dumps(summarize(args.results), indent=2) + '\n'
    if args.output:
        args.output.write_text(encoded)
    print(encoded, end='')
