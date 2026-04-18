#!/usr/bin/env python3
"""Generate lid-return.wav — a slow, smooth descending arp for lid-open.

Tweak knobs at top; re-run to regenerate.
"""
import math
import wave
import array
import os

SR = 44100
DUR = 2.8

# C major pentatonic descending: E6 D6 C6 A5 G5 (MIDI 76 74 72 69 67)
NOTES         = [76, 74, 72, 69, 67]
NOTE_OFFSETS  = [0.00, 0.18, 0.36, 0.58, 0.80]
NOTE_ATTACK   = 0.08
NOTE_RELEASE  = 1.7
NOTE_AMP      = 0.42
SUB_AMP       = 0.22
PEAK_SCALE    = 0.62
GLOBAL_FADE_OUT = 0.15

SLAB_HOME = os.environ.get('SLAB_HOME', os.path.expanduser('~/.local/share/slab'))
OUT = os.path.join(SLAB_HOME, 'sounds', 'lid-return.wav')


def main():
    N = int(SR * DUR)
    buf = array.array('d', [0.0]) * N

    for midi, off in zip(NOTES, NOTE_OFFSETS):
        freq = 440.0 * 2 ** ((midi - 69) / 12)
        sub = freq * 0.5
        start = int(off * SR)
        for i in range(N - start):
            t = i / SR
            if t < NOTE_ATTACK:
                env = t / NOTE_ATTACK
            else:
                env = math.exp(-(t - NOTE_ATTACK) / NOTE_RELEASE)
            buf[start + i] += env * (
                NOTE_AMP * math.sin(2 * math.pi * freq * t)
                + SUB_AMP * math.sin(2 * math.pi * sub * t)
            )

    peak = max(abs(s) for s in buf) or 1.0
    scale = (PEAK_SCALE / peak) * 32767
    fin  = int(0.01 * SR)
    fout = int(GLOBAL_FADE_OUT * SR)

    out = array.array('h', [0]) * N
    for i in range(N):
        env_global = 1.0
        if i < fin:
            env_global = i / fin
        elif i > N - fout:
            env_global = max(0.0, (N - i) / fout)
        v = int(buf[i] * scale * env_global)
        if v > 32767: v = 32767
        elif v < -32768: v = -32768
        out[i] = v

    os.makedirs(os.path.dirname(OUT), exist_ok=True)
    with wave.open(OUT, 'wb') as w:
        w.setnchannels(1); w.setsampwidth(2); w.setframerate(SR)
        w.writeframes(out.tobytes())

    print(f'wrote {OUT} ({DUR:.2f}s, peak {peak:.3f})')


if __name__ == '__main__':
    main()
