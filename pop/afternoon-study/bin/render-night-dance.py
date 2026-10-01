#!/usr/bin/env python3
"""Nighttime Study, dance pass: rolling Jeffrey vocals, warm sixths, 108 BPM.

Keeps the nine-minute span but rebuilds the rhythm and harmony. Chamber
platter gradual substitution/return and rhythm platter interlocking are
compositional constraints; all sung sounds use the three original takes.
"""
import argparse
import importlib.util
import json
import re
from pathlib import Path
import numpy as np

spec = importlib.util.spec_from_file_location('night', Path(__file__).with_name('render-night.py'))
n = importlib.util.module_from_spec(spec)
spec.loader.exec_module(n)
ac = n.ac
BPM, BARS = 108, 240
BEAT = 60 / BPM
BAR = BEAT * 4
OUT = n.ROOT / 'out/nighttime-dance'
CHORDS = {
    'C6add9': (36, [60, 64, 67, 69, 74]),
    'F6add9': (41, [60, 65, 67, 69, 74]),
    'G6add9': (43, [59, 62, 67, 69, 76]),
    'C6overE': (40, [60, 64, 67, 69, 74]),
}
SECTIONS = [
    (0, 24, 'lights on', .66),
    (24, 36, 'rolling floor', .84),
    (60, 24, 'first lift', 1.0),
    (84, 12, 'open air', .64),
    (96, 48, 'night dancing', .94),
    (144, 36, 'high windows', 1.06),
    (180, 12, 'floating pulse', .66),
    (192, 24, 'last dance', 1.08),
    (216, 24, 'walk home', .76),
]
# Eight-bar lines. Shorter notes articulate; held tones earn their space.
MELODIES = [
    [(0, 1, 67), (1.5, .75, 69), (3, 1.5, 72), (5.5, .75, 69),
     (7, 2, 67), (11, 1, 64), (13, 2.5, 67), (17, 1, 69),
     (18.5, 1, 67), (21, 2, 65), (24, 1, 67), (26, 1, 69), (28, 3, 72)],
    [(0, 1, 72), (1.5, 1, 74), (4, 2, 76), (7, 1, 74),
     (9, 2, 72), (13, 2, 69), (17, 1, 72), (18.5, 1, 69),
     (21, 2, 67), (24, 1, 69), (26, 1.5, 67), (29, 2, 72)],
]


def meter(path):
    log = ac.ffmpeg('-i', path, '-af', 'ebur128=peak=true:framelog=quiet', '-f', 'null', '-').stderr
    summary = log[log.rfind('Integrated loudness'):]
    def number(pattern):
        return float(re.search(pattern, summary)[1])
    return dict(input_i=number(r'I:\s*(-?[\d.]+) LUFS'),
                input_tp=number(r'Peak:\s*(-?[\d.]+) dBFS'),
                input_lra=number(r'LRA:\s*(-?[\d.]+) LU'))


def arrange():
    rng = np.random.default_rng(n.SEED)
    piano = ac.Piano(rng)
    mix = n.Mixer(n.DURATION)
    voices = [n.Voice(ac.Bank(name, OUT / 'cache')) for name in ac.NAMES]
    lemon, indigo, saffron = voices
    harmony = []
    kick = ac.soft_kick()
    # Synthesized brush and wood taps, all project-owned.
    closed = ac.brush(.075, .004, 3600, 10500, 31)
    opened = ac.brush(.17, .012, 2600, 9200, 54)
    clap = ac.brush(.14, .012, 900, 5900, 92)
    for start, bars, name, level in SECTIONS:
        print(f'arrange {start:3d}: {name}', flush=True)
        for b in range(bars):
            bar = start + b
            at = bar * BAR
            airy = start in (84, 180)
            ending = start == 216
            gain = level * (1 - .55 * b / bars if ending else 1)
            route = ['C6add9', 'F6add9', 'C6overE', 'G6add9']
            chordname = route[(bar // 8) % 4]
            root, tones = CHORDS[chordname]
            harmony.append(dict(bar=bar, chord=chordname))
            # Four to the floor from the opening. Breaks keep a light pulse;
            # the last eight bars let it drift out instead of a hard stop.
            kick_beats = [0, 2] if airy or bar < 4 else range(4)
            if bar >= 232:
                kick_beats = []
            for beat in kick_beats:
                mix.place('kick', kick, at + beat * BEAT, .22 * gain)
            for beat in range(4):
                if bar >= 236:
                    continue
                # Eighth-note lift plus a late, quiet sixteenth: light swing.
                mix.place('shuffle', opened if beat == 3 else closed,
                          at + (beat + .54) * BEAT, .039 * gain,
                          -.18 if beat % 2 else .18)
                if bar >= 12 and not airy and beat in (0, 2):
                    mix.place('shuffle', closed, at + (beat + .80) * BEAT,
                              .012 * gain, .25)
            if bar >= 8 and not airy and bar < 228:
                for beat in (1, 3):
                    mix.place('clap', clap, at + (beat + .025) * BEAT, .043 * gain)
            # Bass answers the kick; upper harmonics make the line audible.
            if bar >= 8 and not airy and bar < 228:
                for beat, midi, length in [(0.55, root, .68), (2.55, root, .65),
                                           (3.55, root + (7 if bar % 2 else 12), .32)]:
                    bass = ac.bass_tone(midi, length * BEAT, .7)
                    mix.place('bass', bass, at + beat * BEAT, .075 * gain)
            # Short vocal rolls, staggered with mallets on the empty slots.
            if not airy and bar < 232:
                pattern = [tones[0], tones[2], tones[3], tones[2], tones[4], tones[2]]
                count = 2 if bar < 8 else 3 if bar < 24 else 4
                for k in range(8):
                    beat = bar * 4 + k * .5
                    if k % 2 == 0 and k // 2 < count:
                        saffron.sing(mix, 'rolling', pattern[(bar * 4 + k // 2) % 6],
                                     beat, .85, .46 * gain,
                                     .3 * np.sin(beat / 25), False)
                    elif k in (3, 7) and bar >= 12:
                        mix.place('mallets', ac.marimba(tones[(bar + k) % 4], 'rosewood', 1.4),
                                  beat * BEAT, .032 * gain, -.3 if k == 3 else .3)
            # Lift comes from sixths/ninths above a stable major root. A single
            # breathing upper voice replaces the continuous low two-note bed.
            if b % 8 == 0 and bar >= 16:
                indigo.sing(mix, 'hums', tones[2], bar * 4 + 2, 6,
                            .25 * gain, -.1, True)
            if b % 2 == 0:
                for k, midi in enumerate([tones[1] + 12, tones[3] + 12, tones[2] + 12]):
                    piano.key(mix, midi, at + (1.5 + k * .55) * BEAT,
                              1.5 * BEAT, .34 * gain, vel=.48, pan=.1)
            if airy:
                if b % 4 == 0:
                    saffron.sing(mix, 'answer', tones[2], bar * 4 + 1, 3,
                                 .4 * gain, .12, True)
                continue
            # Returns develop by register and rhythmic answer, with breathing
            # gaps between eight-bar statements. The voice arrives at 0:36.
            if bar >= 16 and bar < 224 and bar % 16 == 0:
                line = MELODIES[1 if 144 <= bar < 180 or bar >= 192 else 0]
                for beat, length, midi in line:
                    lemon.sing(mix, 'lead', midi, bar * 4 + beat + .25,
                               length, .93 * gain)
            # Pitched Indigo replies, only in the gap after Lemon's phrase.
            if bar >= 40 and bar < 216 and bar % 16 == 10:
                for beat, midi in [(0, tones[2]), (1.5, tones[3]), (3, tones[2])]:
                    indigo.sing(mix, 'answer', midi, bar * 4 + beat,
                                .9, .40 * gain, -.15)
        if ending:
            piano.roll(mix, [48, 64, 67, 74], 236 * BAR, BAR * 4, .2, vel=.35)
    return mix, voices, harmony


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--study', type=Path, default=ac.STUDY)
    args = parser.parse_args()
    ac.STUDY = args.study
    for sub in ('cache', 'stems'):
        (OUT / sub).mkdir(parents=True, exist_ok=True)
    n.OUT, n.BPM, n.BEAT, n.BAR, n.BARS = OUT, BPM, BEAT, BAR, BARS
    n.DURATION = BARS * BAR + 12
    n.PHRASES, n.MOTIFS = SECTIONS, MELODIES
    ac.BEAT, ac.BAR = BEAT, BAR
    mix, voices, harmony = arrange()
    settings = {'piano': (.17, 2.8, 8000), 'lead': (.12, 2.3, 10000),
                'hums': (.28, 4.2, 6000), 'rolling': (.19, 3.0, 7800),
                'answer': (.23, 3.2, 8500), 'mallets': (.19, 2.8, 7200),
                'bass': (0, 0, 2100), 'kick': (0, 0, 8000),
                'shuffle': (.06, .8, 12000), 'clap': (.09, 1.0, 10000)}
    n.finish(mix, voices, harmony, title='Nighttime Study — Dance',
             slug='nighttime-study-dance', settings=settings,
             bus_gains={'piano': 2.8, 'hums': .8}, target_lufs=-15, measure=meter)


if __name__ == '__main__':
    main()
