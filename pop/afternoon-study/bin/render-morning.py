#!/usr/bin/env python3
"""Morning Study: a gentle wake-up companion using the original three voices.

80 BPM, 80 bars, four minutes. Soft grand piano carries a
rounded version of Afternoon Study's E-G-A-G gesture. Wordless voices enter
gradually; wooden mallets supply a little movement without a drum build.
"""
import argparse
import importlib.util
import json
from pathlib import Path

import numpy as np

spec = importlib.util.spec_from_file_location('night', Path(__file__).with_name('render-night.py'))
n = importlib.util.module_from_spec(spec)
spec.loader.exec_module(n)
ac = n.ac
BPM, BARS, SEED = 80, 80, 10042026
BEAT = 60 / BPM
BAR = BEAT * 4
OUT = n.ROOT / 'out/morning'
CHORDS = {
    'C6add9': (48, [55, 64, 69, 74]),
    'Am7': (45, [55, 60, 64, 67]),
    'Fmaj9': (41, [57, 60, 64, 67]),
    'C6overE': (40, [55, 60, 64, 69]),
    'Dm9': (50, [57, 60, 64, 65]),
    'G6add9': (43, [55, 59, 64, 69]),
}
SECTIONS = [
    (0, 8, 'first light', ['C6add9', 'Fmaj9']),
    (8, 16, 'waking', ['C6add9', 'Am7', 'Fmaj9', 'G6add9']),
    (24, 16, 'open window', ['C6overE', 'Fmaj9', 'Dm9', 'G6add9']),
    (40, 8, 'a breath', ['Fmaj9', 'C6overE']),
    (48, 16, 'daylight', ['C6add9', 'Am7', 'Fmaj9', 'G6add9']),
    (64, 8, 'feet on the floor', ['Fmaj9', 'G6add9']),
    (72, 8, 'morning', ['C6add9', 'C6add9']),
]
# Beat offset, duration, MIDI within a four-bar harmony. Related to the
# afternoon tune, with more breathing room and no high-register climax.
LINES = {
    'C6add9': [(1, 1.5, 64), (4, 2, 67), (7.5, 2.5, 69), (12, 2, 67)],
    'Am7': [(1, 2, 64), (5, 2, 60), (10, 3, 64)],
    'Fmaj9': [(1, 2, 65), (5, 2, 69), (9, 2.5, 67), (13, 2, 64)],
    'C6overE': [(1, 2, 64), (5, 2.5, 67), (10, 3, 64)],
    'Dm9': [(1, 2, 65), (5, 2, 69), (10, 3, 65)],
    'G6add9': [(1, 2, 62), (5, 2, 64), (10, 3, 67)],
}


class SoftPiano(ac.Piano):
    def note(self, midi, dur, vel=.7, pan=0):
        y = super().note(midi, dur, vel, pan)
        y *= ac.fades(len(y), .018, .08)[:, None]
        return y


def arrange():
    rng = np.random.default_rng(SEED)
    piano = SoftPiano(rng)
    mix = n.Mixer(n.DURATION)
    voices = [n.Voice(ac.Bank(name, OUT / 'cache')) for name in ac.NAMES]
    lemon, indigo, saffron = voices
    harmony = []
    for start, bars, label, route in SECTIONS:
        print(f'arrange {start:02d}: {label}', flush=True)
        for b in range(bars):
            bar = start + b
            at = bar * BAR
            chord = route[b // 4]
            root, tones = CHORDS[chord]
            harmony.append(dict(bar=bar, chord=chord))
            # Rise over the first minute, hold a modest ceiling, then leave
            # the same morning motif in a less crowded room.
            lift = .48 + .30 * min(1, bar / 24)
            if start == 40:
                lift *= .76
            if start == 72:
                lift *= .85 - .025 * b

            if b % 2 == 0:
                piano.roll(mix, [root, tones[0], tones[1]], at,
                           BEAT * 5.2, .16 * lift, vel=.28)
            # Keep the first minute connected: quiet answering notes bridge
            # the odd bars while the voice is still gradually arriving.
            if bar % 2 == 0:
                keys = [tones[1] + 12, tones[2] + 12]
                for beat, midi in zip([.7, 2.65], keys):
                    piano.key(mix, midi, at + beat * BEAT,
                              BEAT * 2.4, .18 * lift, vel=.27)
            elif bar < 20:
                for beat, midi in [(.55, tones[0] + 12), (2.4, tones[1] + 12)]:
                    piano.key(mix, midi, at + beat * BEAT,
                              BEAT * 2.8, .14 * lift, vel=.25)
            elif 20 <= bar < 72 and start != 40:
                piano.key(mix, tones[1] + 12, at + 2.1 * BEAT,
                          BEAT * 2.2, .11 * lift, vel=.25)

            # Soft wooden notes only after the opening has had time to wake.
            # No kick, hats, risers, sub-bass, or sudden drop.
            if 24 <= bar < 72 and start != 40 and b % 2 == 1:
                for beat, midi, pan in [(1.4, tones[1], -.22), (3.15, tones[2], .22)]:
                    mallet = ac.marimba(midi, 'rosewood', 1.3)
                    mallet *= ac.fades(len(mallet), .025, .15)
                    mix.place('mallets', mallet, at + beat * BEAT, .026 * lift, pan)

            # One quiet hum per harmony, rather than a continuous low choir.
            if b % 4 == 0 and 16 <= bar < 72 and start != 40:
                indigo.sing(mix, 'hums', tones[1], bar * 4 + 1.5,
                            6, .20 * lift, -.12, True)

            if b % 4 == 0:
                line = LINES[chord]
                if bar < 4:
                    # Familiar melodic contour appears first on piano.
                    for beat, length, midi in line[:3]:
                        piano.key(mix, midi + 12, at + beat * BEAT,
                                  length * BEAT, .10 * lift, vel=.25)
                elif start == 40:
                    saffron.sing(mix, 'answer', tones[1], bar * 4 + 3,
                                 3, .33 * lift, .12, True)
                elif bar < 76:
                    singer = saffron if bar in (28, 36, 60, 68) else lemon
                    voice_lift = .35 + .65 * min(1, max(0, (bar - 4) / 16))
                    for beat, length, midi in line:
                        singer.sing(mix, 'lead', midi, bar * 4 + beat,
                                    length, .58 * lift * voice_lift, 0, False)
                    # A brief, higher answer appears only in the daylight.
                    if 48 <= bar < 64:
                        saffron.sing(mix, 'answer', tones[2], bar * 4 + 14,
                                     1.5, .22 * lift, .18, True)
                else:
                    piano.roll(mix, [48, 55, 64, 69], at, BAR * 4,
                               .16, vel=.25)
    return mix, voices, harmony


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--study', type=Path, default=ac.STUDY)
    args = parser.parse_args()
    ac.STUDY = args.study
    n.OUT, n.BPM, n.BEAT, n.BAR, n.BARS = OUT, BPM, BEAT, BAR, BARS
    n.DURATION, n.SEED = BARS * BAR, SEED
    n.PHRASES, n.MOTIFS = SECTIONS, LINES
    n.SOURCES = ['pop/afternoon-study/bin/render.py',
                 'pop/afternoon-study/bin/render-night.py']
    ac.BEAT, ac.BAR = BEAT, BAR
    for sub in ('cache', 'stems'):
        (OUT / sub).mkdir(parents=True, exist_ok=True)
    mix, voices, harmony = arrange()
    n.finish(mix, voices, harmony, title='Morning Study', slug='morning-study',
             settings={'piano': (.18, 3.1, 4800), 'lead': (.17, 2.6, 6100),
                       'answer': (.24, 3.6, 5200), 'hums': (.27, 4.5, 3600),
                       'mallets': (.20, 2.8, 4200)},
             bus_gains={'piano': 2.1, 'lead': 1.0, 'answer': .85,
                        'hums': .8, 'mallets': 1.2},
             target_lufs=-20, fade_seconds=12)
    receipt_path = OUT / 'receipts.json'
    receipt = json.loads(receipt_path.read_text())
    receipt['brief'] = 'gentle, soft wake-up version of Afternoon Study'
    receipt['revision'] = 'v2: first-minute piano gaps filled; voice enters at 12.75 seconds'
    receipt['relationship'] = 'Same three project-owned vocal banks and Salamander piano; slower, spacious E-G-A-G motif, gradual morning lift.'
    receipt_path.write_text(json.dumps(receipt, indent=2))


if __name__ == '__main__':
    main()
