#!/usr/bin/env python3
"""Reproducible two-pass review arrangement from smooth lead's archived sources.
Usage: pop/.venv/bin/python pop/big-pictures/bin/harmonize-amazing.py SOURCE OUT
Then OUT=OUT bash pop/big-pictures/bin/cut-amazing.sh
"""
import hashlib
import json
import math
import os
from pathlib import Path
import subprocess
import sys
import numpy as np
import soundfile as sf

REPO = Path(__file__).resolve().parents[3]
SOURCE, OUT = [Path(p).resolve() for p in sys.argv[1:3]]
assert SOURCE != OUT, 'Preserve the source review'
OUT.mkdir(parents=True, exist_ok=True)
SCORE = json.loads((REPO / 'pop/big-pictures/amazing-grace.harmony.json').read_text())
RECEIPT = json.loads((SOURCE / 'vox-receipt.json').read_text())
SR = 48000
SPB = 60 / SCORE['bpm']
OFFSET = SCORE['repeatOffsetBeats'] * SPB
INTRO = SCORE['introSeconds']

def run(args):
    return subprocess.run([str(a) for a in args], check=True, capture_output=True, text=True).stdout.strip()

def read(path):
    y, sr = sf.read(path, dtype='float64', always_2d=True)
    assert sr == SR, (path, sr)
    assert np.isfinite(y).all()
    return y

def write(name, y):
    sf.write(OUT / name, y, SR, subtype='FLOAT')

def process(y, name, filters):
    write(name + '-raw.wav', y)
    run(['ffmpeg', '-y', '-v', 'error', '-i', OUT / (name + '-raw.wav'), '-af', filters,
         '-c:a', 'pcm_f32le', OUT / (name + '.wav')])
    return read(OUT / (name + '.wav'))

bed = read(SOURCE / 'bed.wav')
# The earlier 'but' source is entirely unvoiced; reuse the later, voiced
# 'but' from the same archived performance and resynthesize its scored B3.
replacement = OUT / 'lead-but-repaired.wav'
but_source = list((SOURCE / 'words').glob('22-*.wav'))
assert len(but_source) == 1
lead_repair_report = run([sys.executable, '-W', 'ignore', REPO / 'pop/cult/bin/sing.py',
    but_source[0], replacement, '--notes', f'B3:{SPB + .035:.3f}',
    '--f0-floor', '65', '--f0-ceil', '520', '--vibrato-hz', '5.2',
    '--vibrato-cents', '9', '--overshoot-cents', '12', '--formant-db', '2.4',
    '--attack-ms', '12', '--release-ms', '55', '--xfade-ms', '90',
    '--shimmer-frames', '.35', '--deess', '.05', '--gain', '.9', '--verify'])
lead = np.zeros(len(read(SOURCE / 'vox.wav')))
for w in RECEIPT['words']:
    files = list((SOURCE / 'sung').glob(f"{w['i']:02d}-*.wav"))
    assert len(files) == 1
    y = read(replacement if w['i'] == 16 else files[0])[:, 0]
    at = round(w['start'] * SR)
    lead[at:at + len(y)] += y
lead *= 10 ** (-3/20) / np.max(np.abs(lead))
stamp = read(SOURCE / 'stamp.wav')
n = len(bed) + round(OFFSET * SR)

def place(target, audio, seconds, gain=1):
    at = round(seconds * SR)
    end = min(len(target), at + len(audio))
    target[at:end] += audio[:end-at] * gain

# Keep the original instrumental performance and its tail, then crossfade the
# return's pickup across the plagal turnaround. No abrupt concatenation.
bedmix = np.zeros((n, 2))
t = np.arange(len(bed)) / SR
first_env = np.clip((INTRO + OFFSET + 0.4 - t) / 3.0, 0, 1)
second_env = np.clip(t / 4.5, 0, 1)
place(bedmix, bed * first_env[:, None], 0)
place(bedmix, bed * second_env[:, None], OFFSET)
bedmix = process(bedmix, 'bed-arranged',
    'highpass=f=30,equalizer=f=2200:t=q:w=0.8:g=-1.2')

leadbus = np.zeros(n)
place(leadbus, lead, INTRO)
place(leadbus, lead, INTRO + OFFSET, 0.98)
leadbus = process(leadbus, 'lead-centered',
    'highpass=f=85,equalizer=f=280:t=q:w=0.8:g=-1.3,'
    'equalizer=f=2900:t=q:w=1.1:g=-1.0,'
    'acompressor=threshold=0.14:ratio=2:attack=18:release=180:knee=3:makeup=1.12')[:, 0]

verification = []
for part in ['lower', 'upper']:
    sung = OUT / part
    sung.mkdir(exist_ok=True)
    bus = np.zeros(n)
    note_index = 0
    for w in RECEIPT['words']:
        pitches = SCORE[part][note_index:note_index + len(w['notes'])]
        note_index += len(w['notes'])
        if w['i'] < SCORE[part + 'EntryWord']:
            continue
        source_index = 22 if w['i'] == 16 else w['i']
        source = list((SOURCE / 'words').glob(f"{source_index:02d}-*.wav"))
        assert len(source) == 1, source
        phrase_end = w['text'] in ['sound', 'me', 'found', 'see']
        spec = ','.join(f"{pitch}:{note['beats'] * SPB - (0.12 if phrase_end else -0.04) if i == len(pitches)-1 else note['beats'] * SPB:.3f}"
                        for i, (pitch, note) in enumerate(zip(pitches, w['notes'])))
        dest = sung / f"{w['i']:02d}.wav"
        args = [sys.executable, '-W', 'ignore', REPO / 'pop/cult/bin/sing.py', source[0], dest,
                '--notes', spec, '--f0-floor', '65', '--f0-ceil', '600',
                '--vibrato-hz', '4.8' if part == 'lower' else '5.35',
                '--vibrato-cents', '13', '--vibrato-onset-ms', '500',
                '--overshoot-cents', '8', '--formant-db', '1.3',
                '--attack-ms', '20', '--release-ms', '110' if phrase_end else '65',
                '--xfade-ms', '100', '--shimmer-frames', '0.25', '--deess', '0.045',
                '--gain', '0.707', '--verify']
        key = hashlib.sha256(source[0].read_bytes() +
             (REPO / 'pop/cult/bin/sing.py').read_bytes() + str(args).encode()).hexdigest()
        cache = dest.with_suffix('.json')
        if dest.exists() and cache.exists() and json.loads(cache.read_text())['key'] == key:
            report = json.loads(cache.read_text())['report']
        else:
            report = run(args)
            cache.write_text(json.dumps({'key': key, 'report': report}))
        verification.append({'part': part, 'word': w['text'], 'notes': spec, 'verification': report})
        print(part, w['text'], report, flush=True)
        y = read(dest)[:, 0]
        # Natural 14/23 ms onset offsets, distinct contours, no phase widening.
        at = INTRO + OFFSET + w['start'] + (0.014 if part == 'lower' else 0.023)
        # Upper part blooms for the latter half of the returning verse.
        gain = 0.75 if part == 'upper' and w['i'] < 12 else 1.0
        place(bus, y, at, gain)
    assert note_index == 28
    bus = process(bus, part + '-voice',
        'highpass=f=155,lowpass=f=8500,equalizer=f=3000:t=q:w=0.9:g=-2,'
        'acompressor=threshold=0.12:ratio=2:attack=22:release=210:knee=3')[:, 0]
    angle = (SCORE[part + 'Pan'] + 1) * math.pi / 4
    stereo = bus[:, None] * np.array([math.cos(angle), math.sin(angle)])
    write(part + '-panned.wav', stereo * SCORE[part + 'Gain'])

# Positive-polarity early reflections keep the center intelligible and collapse
# safely to mono. Filtered tails are quiet, avoiding comb-filter widening.
voice = np.repeat(leadbus[:, None], 2, axis=1) * SCORE['leadGain']
voice += read(OUT / 'lower-panned.wav') + read(OUT / 'upper-panned.wav')
wet_source = process(voice, 'room-send', 'highpass=f=260,lowpass=f=5800')
room = np.zeros_like(voice)
for delay, left, right in [(0.071, .055, .025), (.109, .025, .055), (.173, .035, .03),
                           (.257, .025, .03), (.389, .020, .017), (.541, .012, .014)]:
    place(room, wet_source * np.array([left, right]), delay)
write('vocal-spatial.wav', voice + room)
mix = bedmix * SCORE['bedGain'] + voice + room
place(mix, stamp, 65.8 + OFFSET, .72)
fade = round(2.5 * SR)
mix[-fade:] *= np.linspace(1, 0, fade)[:, None]
assert np.isfinite(mix).all()
write('pre.wav', mix)
receipt = {'artist': SCORE['artist'], 'status': 'listening review, not released',
           'seconds': n / SR, 'secondPassStart': INTRO + OFFSET,
           'source': str(SOURCE), 'score': SCORE, 'verification': verification,
           'leadRepair': {'word': 'but', 'wordIndex': 16, 'borrowedFromWordIndex': 22, 'verification': lead_repair_report},
           'voiceSource': 'Existing Jeffrey PVC spoken word stems, WORLD resynthesis; no new recorded take',
           'mix': 'center lead; two scored panned harmonies; filtered positive-polarity room reflections; return crossfade'}
(OUT / 'arrangement-receipt.json').write_text(json.dumps(receipt, indent=2) + '\n')
print('Premix:', OUT / 'pre.wav', n / SR, 'seconds', flush=True)
