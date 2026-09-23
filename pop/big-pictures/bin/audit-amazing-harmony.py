#!/usr/bin/env python3
"""Validate pitch, duration, deliverable format, and static-gain limiter demand."""
import json
import os
from pathlib import Path
import re
import subprocess
import sys
import numpy as np
import soundfile as sf
p = Path(sys.argv[1])
r = json.loads((p / 'arrangement-receipt.json').read_text())
reports = [w['verification'] for w in r['verification']] + [r['leadRepair']['verification']]
errors = [int(e) for report in reports for e in re.findall(r'([+-]\d+)¢', report)]
expected = sum(len(w['notes'].split(',')) for w in r['verification']) + 1
assert len(errors) == expected and max(abs(e) for e in errors) <= 30, errors
assert all('voiced 0%' not in report for report in reports)
y, sr = sf.read(p / 'premaster.wav')
z, sz = sf.read(p / 'amazing-grace-master.wav')
assert sr == sz == 48000 and z.shape[1] == 2 and abs(len(z)/sz - r['seconds']) < .001
assert np.isfinite(z).all() and np.max(np.abs(z)) < 1
log = subprocess.run(['ffmpeg', '-hide_banner', '-nostats', '-i', str(p/'premaster.wav'),
    '-af', 'loudnorm=I=-11.5:TP=-2:LRA=9:print_format=json', '-f', 'null', '-'],
    check=True, capture_output=True, text=True).stderr
loudness = json.JSONDecoder().raw_decode(log[log.rfind('{'):])[0]
gain = round(float(os.getenv('TARGET', '-11.5')) - float(loudness['input_i']), 2)
limit = float(os.getenv('LIMIT', '.74'))
demand = np.maximum(0, 20*np.log10(np.maximum(np.max(np.abs(y),axis=1)*10**(gain/20)/limit, 1e-12)))
checks = {'seconds': len(z)/sz, 'sampleRate': sz, 'channels': 2,
    'subtype': sf.info(p/'amazing-grace-master.wav').subtype,
    'finite': True, 'pitchMeasuredNotes': len(errors),
    'pitchMedianAbsoluteCents': float(np.median(np.abs(errors))),
    'pitchMaxAbsoluteCents': max(abs(e) for e in errors),
    'staticGainDB': gain,
    'limiterCeilingDemandSamplePeakDB': float(demand.max()),
    'limiterCeilingDemand99thPercentileDB': float(np.percentile(demand, 99)),
    'limiterNote': 'Static-gain sample peak demand estimate, not measured gain-reduction trace',
    'listeningApproval': 'Pending user listening review'}
assert checks['subtype'] == 'PCM_24'
assert demand.max() < 4, checks
(p/'technical-checks.json').write_text(json.dumps(checks, indent=2)+'\n')
print(json.dumps(checks, indent=2))
