#!/usr/bin/env python3
"""gen-instruments.py — rebuild the v10.2 session's synthesized samples.

Everything samples/-side that the engines score but git doesn't carry
(the lane rule: media is derived, scripts are source). Run with
pop/.venv/bin/python. Produces, into pop/cult/samples/:

  cathedral-ir.wav   the castle: RT60 3.8 s stereo IR, HF damping, 25 ms
                     predelay — convolved by cut-v10/cut-radio/cut-final
  violin-secret.wav  bowed D5-C#5-B4 with per-note vibrato, one crescendo
  guitar-chug.wav    palm-muted B-pedal 8ths, dark tanh drive, decay flange
  guitar-wide.wav    open B-D-G-Em walls, octave shimmer, slower flange
  boing-{b,d,g,e}.wav  the act-VII springs, pitched to the guitar roots

Not generated here, but part of the same bank:
  waterhole.wav      ffmpeg cut of alt/raw/7071087615948148010.mp4
                     (atrim=1.64:2.09, fade 0.41/0.04, volume=13dB) — the
                     metallic ding after "guys"
  dotorg.wav         neo's ~/.cache/ac/v4pid/stamp-jeffrey.wav through
                     pop/bin/pitchsnap_world.py --notes "B2,F#2,B1"
  accordion-*.wav    pop/accordion/c/accordion (see that README):
                     secret: --chord "B3,D4,F#4" --dur 13 --vel 0.4
                             --voices 3 --detune 8 --bellows swell --tremor 0.15
                     b/d/g/e: --chord <triad> --dur 4.2 --vel 0.6 --voices 2
                             --detune 10 --bellows push|pull --tremor 0.4
                     (triads: B3,D4,F#4 · D4,F#4,A4 · G3,B3,D4 · E3,G3,B3)
"""
import os
import wave

import numpy as np

SR = 48000
HERE = os.path.dirname(os.path.abspath(__file__))
S = os.path.join(HERE, "..", "samples")


def save(name, x, channels=1):
    x = np.asarray(x)
    peak = np.abs(x).max()
    if name != "cathedral-ir.wav" and peak > 0:
        x = x / peak * (0.7 if name.startswith("boing") else 0.75)
    w = wave.open(os.path.join(S, name), "w")
    w.setnchannels(channels)
    w.setsampwidth(2)
    w.setframerate(SR)
    w.writeframes((x * 32767).astype(np.int16).tobytes())
    w.close()
    print(f"  {name}")


def onepole(sig, fc):
    k = 1 - np.exp(-2 * np.pi * fc / SR)
    acc = 0.0
    y = np.empty_like(sig)
    for i in range(len(sig)):
        acc += k * (sig[i] - acc)
        y[i] = acc
    return y


# ── the cathedral ──────────────────────────────────────────────────────
def cathedral(rt60=3.8, dur=4.4, pre=0.025):
    n = int(dur * SR)
    rng = np.random.default_rng(1204)
    t = np.arange(n) / SR
    chans = []
    for ch in range(2):
        x = rng.standard_normal(n) * np.exp(-6.908 * t / rt60)
        bright, dark = onepole(x, 8500), onepole(x, 1800)
        w = np.minimum(1, t / (rt60 * 0.7))
        x = bright * (1 - w) + dark * w
        for tap, g in [(0.011, 0.5), (0.023, 0.38), (0.041, 0.3), (0.067, 0.22)]:
            x[int(tap * SR)] += g * (1 if ch == 0 else -1)
        chans.append(x)
    ir = np.stack(chans, 1)
    ir /= np.abs(ir).max()
    ir = np.concatenate([np.zeros((int(pre * SR), 2)), ir]) * 0.5
    save("cathedral-ir.wav", ir, channels=2)


# ── the violin ─────────────────────────────────────────────────────────
def violin():
    notes = [(587.33, 2.0), (554.37, 2.0), (493.88, 3.2)]
    total = sum(d for _, d in notes)
    n = int(total * SR)
    t = np.arange(n) / SR
    f = np.zeros(n)
    vibenv = np.zeros(n)
    pos = 0.0
    for i, (hz, d) in enumerate(notes):
        a, b = int(pos * SR), int((pos + d) * SR)
        f[a:b] = hz
        if i:
            g = int(0.08 * SR)
            f[a:a + g] = np.linspace(notes[i - 1][0], hz, g)
        m = b - a
        vibenv[a:b] = np.minimum(1, np.arange(m) / (0.5 * SR))
        pos += d
    f = f * (1 + 0.007 * vibenv * np.sin(2 * np.pi * 5.3 * t))
    ph = 2 * np.pi * np.cumsum(f) / SR
    x = np.zeros(n)
    for h in range(1, 31):
        x += np.sin(h * ph) / h * np.exp(-h * f.mean() / 5200)
    x += onepole(np.random.default_rng(7).standard_normal(n) * 0.015, 2500)
    env = np.minimum(1, t / 0.15) * (0.3 + 0.7 * (t / total) ** 1.4)
    env *= np.minimum(1, (total - t) / 0.35)
    save("violin-secret.wav", x * env)


# ── the guitars ────────────────────────────────────────────────────────
RNG = np.random.default_rng(66)


def ks(freq, dur, damp=0.996, bright=0.5):
    n = int(dur * SR)
    p = int(SR / freq)
    buf = RNG.standard_normal(p) * bright + (1 - bright) * np.sign(RNG.standard_normal(p))
    out = np.empty(n)
    prev = 0.0
    for i in range(n):
        v = buf[i % p]
        buf[i % p] = damp * 0.5 * (v + prev)
        prev = v
        out[i] = v
    return out


def power(freq, dur, strum=0.012, **kw):
    x = np.zeros(int(dur * SR) + int(2 * strum * SR) + 1)
    for k, ratio in enumerate([1, 1.5, 2]):
        s = ks(freq * ratio, dur, **kw)
        off = int(k * strum * SR)
        x[off:off + len(s)] += s * (1.0 if k == 0 else 0.8)
    return x[:int(dur * SR)]


def distort(x, drive):
    return np.tanh(x * drive) / np.tanh(drive)


def flange(x, rate, base_ms=1.6, depth_ms=2.2, mix=0.65, env=None):
    n = len(x)
    t = np.arange(n) / SR
    d = (base_ms + depth_ms * 0.5 * (1 + np.sin(2 * np.pi * rate * t))) * SR / 1000
    idx = np.arange(n) - d
    lo = np.clip(np.floor(idx).astype(int), 0, n - 1)
    hi = np.clip(lo + 1, 0, n - 1)
    fr = idx - np.floor(idx)
    wet = x[lo] * (1 - fr) + x[hi] * fr
    return x + mix * (np.ones(n) if env is None else env) * wet


def mixin(dst, a, src):
    n = min(len(src), len(dst) - a)
    if n > 0:
        dst[a:a + n] += src[:n]


def guitars():
    B2 = 123.47
    chug = np.zeros(int(8.0 * SR))
    fenv = np.zeros(int(8.0 * SR))
    for e in range(32):
        hit = power(B2, 0.42, damp=0.9905, bright=0.3)
        env = np.exp(-np.arange(len(hit)) / (0.085 * SR))
        g = 1.0 if e % 8 == 0 else (0.82 if e % 2 == 0 else 0.62)
        a = int(e * 0.25 * SR)
        mixin(chug, a, hit * env * g)
        fe = np.minimum(1, np.arange(len(hit)) / (0.06 * SR))
        n = min(len(fe), len(fenv) - a)
        fenv[a:a + n] = np.maximum(fenv[a:a + n], fe[:n])
    save("guitar-chug.wav", flange(distort(onepole(chug, 1250), 7.5), rate=0.9, env=fenv))

    prog = [123.47, 146.83, 98.0, 82.41]
    wide = np.zeros(int(16.0 * SR))
    fenv = np.zeros(int(16.0 * SR))
    for i, f in enumerate(prog):
        ch = power(f, 4.1, damp=0.9992, bright=0.6)
        ch *= np.minimum(1, np.arange(len(ch)) / (0.01 * SR))
        a = int(i * 4.0 * SR)
        mixin(wide, a, ch)
        mixin(wide, a, power(f * 2, 3.8, damp=0.999, bright=0.5) * 0.4)
        fe = np.minimum(1, np.arange(int(4.1 * SR)) / (0.9 * SR))
        n = min(len(fe), len(fenv) - a)
        fenv[a:a + n] = np.maximum(fenv[a:a + n], fe[:n])
    save("guitar-wide.wav",
         flange(distort(onepole(wide, 1650), 5.0), rate=0.5, depth_ms=2.8, env=fenv))


# ── the boings ─────────────────────────────────────────────────────────
def boing(f0, dur=1.1):
    n = int(dur * SR)
    t = np.arange(n) / SR
    wob = np.exp(-t * 4.0) * np.cos(2 * np.pi * (7.5 - 3.0 * t) * t)
    f = f0 * (1 + 1.15 * np.exp(-t * 9.0) + 0.28 * wob)
    ph = 2 * np.pi * np.cumsum(f) / SR
    x = np.sin(ph) + 0.35 * np.sin(2 * ph + 0.7) + 0.12 * np.sin(3 * ph)
    x *= np.exp(-t * 3.2) * np.minimum(1, t / 0.004)
    x += np.exp(-t * 40) * 0.4 * np.random.default_rng(int(f0)).standard_normal(n)
    return x


if __name__ == "__main__":
    print("gen-instruments:")
    cathedral()
    violin()
    guitars()
    for name, hz in [("boing-b", 246.94), ("boing-d", 293.66),
                     ("boing-g", 196.00), ("boing-e", 164.81)]:
        save(f"{name}.wav", boing(hz))
