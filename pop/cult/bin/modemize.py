#!/usr/bin/env python3
"""
modemize.py — the Matrix throat: a voice that turns into a modem.

@jeffrey: "how did they do the matrix movie throat scene where the voice
turns into the modem? can we add a feature like that to aesthetivox?"
The technique (Dane Davis's Matrix sound design, and the Kyma-era spectral
morph in general) is cross-synthesis: keep the VOICE's formant envelope —
the part that makes it a mouth — and swap the EXCITATION underneath it
from vocal cords to data, interpolating between the two over time. The ear
tracks the formants, so it hears one continuous thing becoming another.

The aesthetivox chain already speaks WORLD, and WORLD hands us exactly the
split we need:

  in.wav → harvest/stonemask (f0) · cheaptrick (spectral envelope = the
           mouth) · d4c (aperiodicity)
    → y_voice: normal WORLD synthesis (the human end of the morph)
    → y_data:  a modem carrier — V.21-flavour FSK (980/1180 Hz, 300 baud,
           random bits), answer-tone bursts at 2100 Hz with the real 15 Hz
           phase reversals, scraps of DTMF — pushed through the voice's own
           spectral envelope frame by frame (STFT × envelope), so the data
           is being said BY THE SAME MOUTH
    → morph: equal-power crossfade voice→data along a cosine ramp
           (--morph start:end, seconds; f0 also quantizes to semitone
           steps as the morph proceeds — pitch becomes protocol)
    → raised-cosine top and tail.

  modemize.py in.wav out.wav [--morph 0.4:2.2] [--baud 300] [--mix 1.0]
  modemize.py in.wav out.wav --morph 0:0        # fully modem, still mouthed

Runs on pop/.venv (pyworld/numpy/soundfile), same as sing.py.
"""
import argparse

import numpy as np
import soundfile as sf
import pyworld as pw

FRAME_MS = 5.0


def data_carrier(n, fs, baud, seed):
    """FSK + answer-tone + DTMF scraps, mono, unit-ish level."""
    rng = np.random.default_rng(seed)
    t = np.arange(n) / fs
    # 300-baud FSK: random bits, 980/1180 Hz (V.21 originate space/mark)
    bits = rng.integers(0, 2, int(np.ceil(n / fs * baud)) + 1)
    bit_at = (t * baud).astype(int)
    f_inst = np.where(bits[bit_at] > 0, 1180.0, 980.0)
    phase = 2 * np.pi * np.cumsum(f_inst) / fs
    y = 0.55 * np.sin(phase)
    # answer tone: 2100 Hz bursts with 15 Hz phase reversals (the classic
    # "aaaannnn" under a handshake), gated on for ~0.45 s every ~1.4 s
    gate = ((t % 1.4) < 0.45).astype(float)
    rev = np.sign(np.sin(2 * np.pi * 15 * t / 2))  # flips phase at 15 Hz
    y += 0.30 * gate * rev * np.sin(2 * np.pi * 2100 * t)
    # DTMF scraps: a fresh digit every ~0.23 s, quiet, behind the FSK
    ROWS, COLS = [697, 770, 852, 941], [1209, 1336, 1477, 1633]
    seg = (t / 0.23).astype(int)
    r = rng.integers(0, 4, seg.max() + 1)
    c = rng.integers(0, 4, seg.max() + 1)
    y += 0.16 * (np.sin(2 * np.pi * np.take(np.array(ROWS)[r], seg) * t)
                 + np.sin(2 * np.pi * np.take(np.array(COLS)[c], seg) * t)) * 0.5
    return y


def mouth_filter(carrier, sp, fs, hop):
    """Impose the voice's spectral envelope (WORLD sp frames) on the carrier
    via STFT multiply — the data spoken by the same mouth."""
    nfft = 2 * (sp.shape[1] - 1)
    win = np.hanning(nfft)
    out = np.zeros(len(carrier) + nfft)
    norm = np.zeros_like(out)
    n_frames = sp.shape[0]
    env = np.sqrt(np.maximum(sp, 1e-12))
    env = env / (np.max(env) + 1e-9)
    for i in range(0, len(carrier) - nfft, hop):
        fr = min(n_frames - 1, int(round(i / fs * 1000.0 / FRAME_MS)))
        seg = carrier[i:i + nfft] * win
        spec = np.fft.rfft(seg) * env[fr]
        rec = np.fft.irfft(spec) * win
        out[i:i + nfft] += rec
        norm[i:i + nfft] += win * win
    out = out[: len(carrier)] / np.maximum(norm[: len(carrier)], 1e-6)
    return out


def main():
    p = argparse.ArgumentParser()
    p.add_argument("in_wav")
    p.add_argument("out_wav")
    p.add_argument("--morph", default="0.3:1.8",
                   help="seconds start:end of the voice→modem ramp; 0:0 = all modem")
    p.add_argument("--baud", type=float, default=300.0)
    p.add_argument("--mix", type=float, default=1.0, help="depth of the modem end (0..1)")
    p.add_argument("--f0-floor", type=float, default=90.0)
    p.add_argument("--f0-ceil", type=float, default=700.0)
    p.add_argument("--gain", type=float, default=0.92)
    p.add_argument("--seed", type=int, default=20220120)
    a = p.parse_args()

    x, fs = sf.read(a.in_wav)
    if x.ndim > 1:
        x = x.mean(axis=1)
    x = np.ascontiguousarray(x, dtype=np.float64)

    f0, tpos = pw.harvest(x, fs, f0_floor=a.f0_floor, f0_ceil=a.f0_ceil,
                          frame_period=FRAME_MS)
    f0 = pw.stonemask(x, f0, tpos, fs)
    sp = pw.cheaptrick(x, f0, tpos, fs)
    ap = pw.d4c(x, f0, tpos, fs)

    m0, m1 = (float(v) for v in a.morph.split(":"))
    n = len(x)
    t = np.arange(n) / fs
    if m1 <= m0:
        morph = np.ones(n)
    else:
        u = np.clip((t - m0) / (m1 - m0), 0, 1)
        morph = 0.5 - 0.5 * np.cos(np.pi * u)
    morph *= a.mix

    # the human end — with f0 quantizing to semitones as the morph comes in
    # (measured against the frame-rate morph curve): pitch becomes protocol
    frame_morph = morph[np.minimum((tpos * fs).astype(int), n - 1)]
    voiced = f0 > 0
    midi = np.zeros_like(f0)
    midi[voiced] = 69 + 12 * np.log2(f0[voiced] / 440.0)
    quant = np.round(midi)
    f0q = f0.copy()
    f0q[voiced] = 440.0 * 2 ** (((1 - frame_morph[voiced]) * midi[voiced]
                                 + frame_morph[voiced] * quant[voiced] - 69) / 12)
    y_voice = pw.synthesize(f0q, sp, ap, fs, frame_period=FRAME_MS)[:n]

    # the machine end — data through the same mouth
    carrier = data_carrier(n, fs, a.baud, a.seed)
    y_data = mouth_filter(carrier, sp, fs, hop=(2 * (sp.shape[1] - 1)) // 4)[:n]
    y_data *= 0.9 / (np.max(np.abs(y_data)) + 1e-9)

    y = np.sqrt(1 - morph) * y_voice[: len(morph)] + np.sqrt(morph) * y_data[: len(morph)]

    # raised-cosine top and tail
    edge = int(0.012 * fs)
    ramp = 0.5 - 0.5 * np.cos(np.pi * np.arange(edge) / edge)
    y[:edge] *= ramp
    y[-edge:] *= ramp[::-1]
    y *= a.gain / (np.max(np.abs(y)) + 1e-9)

    sf.write(a.out_wav, y, fs)
    dur = len(y) / fs
    print(f"  modemize · {dur:.2f}s · morph {m0:.2f}→{m1:.2f}s · {a.baud:.0f} baud")


if __name__ == "__main__":
    main()
