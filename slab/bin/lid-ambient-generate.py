#!/usr/bin/env python3
"""Generate the ambient WAV used as the base layer of the lid-closed soundscape.

Tweak knobs at top; re-run to regenerate.
Usage:  lid-ambient-generate.py [seed]
"""
import math
import wave
import random
import array
import os
import sys
import time

# --- knobs ---
TOTAL_SECONDS   = 300.0
NOTE_GAP_RANGE  = (4.0, 10.0)
NOTE_DUR_RANGE  = (18.0, 45.0)
FADE_IN_RANGE   = (3.5, 8.0)
FADE_OUT_RANGE  = (8.0, 18.0)
AMP_RANGE       = (0.07, 0.13)
DETUNE_CENTS    = 7.0
DRONE_EVERY     = (30.0, 60.0)

# C major pentatonic, C3..E5
SCALE_MIDI = [48, 50, 52, 55, 57, 60, 62, 64, 67, 69, 72, 74, 76]

SLAB_HOME = os.environ.get('SLAB_HOME', os.path.expanduser('~/.local/share/slab'))
OUT = os.path.join(SLAB_HOME, 'sounds', 'ambient.wav')
SR  = 44100


def main():
    seed = int(sys.argv[1]) if len(sys.argv) > 1 else random.randint(0, 99999)
    random.seed(seed)
    t0 = time.time()

    N = int(SR * TOTAL_SECONDS)
    pitches = [440.0 * 2**((m - 69) / 12) for m in SCALE_MIDI]
    buf = array.array('d', [0.0]) * N

    def add_note(freq, start_t, dur, amp, fin, fout, det_c=0.0):
        si = int(SR * start_t)
        ns = int(SR * dur)
        w1 = 2 * math.pi * freq
        w2 = 2 * math.pi * freq * 2**(det_c / 1200)
        for i in range(ns):
            idx = si + i
            if idx >= N: break
            lt = i / SR
            if lt < fin:
                env = lt / fin
            elif lt > dur - fout:
                env = max(0.0, (dur - lt) / fout)
            else:
                env = 1.0
            buf[idx] += amp * env * (0.6 * math.sin(w1 * lt) + 0.4 * math.sin(w2 * lt))

    count = 0
    t = 0.0
    while t < TOTAL_SECONDS - 10:
        add_note(
            random.choice(pitches), t,
            random.uniform(*NOTE_DUR_RANGE),
            random.uniform(*AMP_RANGE),
            random.uniform(*FADE_IN_RANGE),
            random.uniform(*FADE_OUT_RANGE),
            random.uniform(-DETUNE_CENTS, DETUNE_CENTS),
        )
        count += 1
        t += random.uniform(*NOTE_GAP_RANGE)

    td = 0.0
    while td < TOTAL_SECONDS - 30:
        add_note(
            random.choice(pitches[:4]) / 2.0, td,
            random.uniform(40, 80),
            random.uniform(0.05, 0.09),
            random.uniform(6, 12),
            random.uniform(15, 25),
            random.uniform(-5, 5),
        )
        td += random.uniform(*DRONE_EVERY)

    peak = max(abs(s) for s in buf) or 1.0
    scale = (0.85 / peak) * 32767
    out = array.array('h', [0]) * N
    for i in range(N):
        v = int(buf[i] * scale)
        if v > 32767: v = 32767
        elif v < -32768: v = -32768
        out[i] = v

    os.makedirs(os.path.dirname(OUT), exist_ok=True)
    with wave.open(OUT, 'wb') as w:
        w.setnchannels(1); w.setsampwidth(2); w.setframerate(SR)
        w.writeframes(out.tobytes())

    print(f'wrote {OUT}')
    print(f'seed={seed} notes={count} peak={peak:.3f} elapsed={time.time()-t0:.1f}s')


if __name__ == '__main__':
    main()
