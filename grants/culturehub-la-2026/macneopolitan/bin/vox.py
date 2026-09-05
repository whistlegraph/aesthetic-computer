#!/usr/bin/env python
# vox.py — render a sing-voice from an .mbscore as an actually-SUNG line:
# each syllable is spoken by the member's cast macOS voice, then WORLD-lifted
# to the exact note pitch and time-stretched so the vowel nucleus fills the
# note while the consonant onset stays natural and lands as a pickup ahead
# of the beat (spinging's p-center rule, approximated). Frames are
# interpolated, never tiled. Output: one wav on the score's beat grid.
#
#   pop/.venv/bin/python bin/vox.py <score.mbscore> <voice-index> <out.wav>
#
# Crude next to spinging's full engine (no goalposts, no choir, no legato
# bridging) — this is the machine-voice vox proof for the MacNeoPolitan lane.
import json
import subprocess
import sys
import tempfile
import os
import numpy as np
import pyworld
import soundfile as sf

score_path, voice_idx, out_path = sys.argv[1], int(sys.argv[2]), sys.argv[3]
score = json.load(open(score_path))
voice = score["voices"][voice_idx]
bpm = float(score.get("bpm", 120))
beat = 60.0 / bpm
say_voice = sys.argv[4] if len(sys.argv) > 4 else voice.get("singVoice", "Fred")
vib_hz = float(voice.get("singVibratoHz", 6.4))
fs = 22050

notes = []
for token in str(voice["notes"]).split(","):
    tok, _, dur = token.partition(":")
    notes.append((tok, float(dur or 1)))
syllables = str(voice["lyrics"]).split()

def speak(text):
    # Engine switch: singVoice "espeak" (or "espeak:<variant>") routes through
    # the open-source formant engine instead of macOS say — the candidate for
    # the fleet's own shippable TTS organ (runs headless anywhere, incl.
    # AC Native OS). Everything downstream (WORLD lift) is identical.
    with tempfile.TemporaryDirectory() as d:
        wav = os.path.join(d, "s.wav")
        if say_voice.startswith("espeak"):
            variant = say_voice.partition(":")[2] or "en-us"
            subprocess.run(["espeak", "-v", variant, "-s", "140", "-w", wav, text],
                           check=True)
        else:
            aiff = os.path.join(d, "s.aiff")
            subprocess.run(["say", "-v", say_voice, "-o", aiff, text], check=True)
            subprocess.run(["afconvert", "-f", "WAVE", "-d", f"LEI16@{fs}", aiff, wav],
                           check=True)
        x, wfs = sf.read(wav)
        x = np.ascontiguousarray(x if x.ndim == 1 else x.mean(axis=1),
                                 dtype=np.float64)
        if wfs != fs:
            n = int(round(len(x) * fs / wfs))
            x = np.interp(np.linspace(0, len(x) - 1, n), np.arange(len(x)), x)
        return x

def sing_syllable(text, midi, dur_s):
    x = speak(text)
    f0, t = pyworld.harvest(x, fs, f0_floor=60.0, f0_ceil=300.0)
    sp = pyworld.cheaptrick(x, f0, t, fs)
    ap = pyworld.d4c(x, f0, t, fs)
    frame_s = t[1] - t[0] if len(t) > 1 else 0.005
    voiced = np.where(f0 > 0)[0]
    if not len(voiced):
        return x[: int(dur_s * fs)], 0.0  # unvoiced syllable: leave as spoken
    v0, v1 = voiced[0], voiced[-1] + 1
    onset_s = v0 * frame_s  # consonant pickup length (kept natural)
    # Stretch the voiced region so onset + voiced fills the note; the
    # spoken consonant tail past the vowel is folded into the stretch.
    target_frames = max(2, int(round((dur_s) / frame_s)) - v0)
    src = np.arange(v0, len(f0))
    dst = np.linspace(v0, len(f0) - 1, target_frames)
    hz = 440.0 * 2 ** ((midi - 69) / 12.0)
    n_total = v0 + target_frames
    new_f0 = np.zeros(n_total)
    vib = 2 ** (18 / 1200 * np.sin(2 * np.pi * vib_hz * np.arange(n_total) * frame_s))
    # voiced mask travels with the interpolation; consonant head stays 0
    voiced_mask = np.interp(dst, src, (f0[v0:] > 0).astype(float)) > 0.5
    new_f0[v0:] = np.where(voiced_mask, hz * vib[v0:], 0.0)
    def warp(m):
        head = m[:v0]
        body = np.empty((target_frames, m.shape[1]))
        for k in range(m.shape[1]):
            body[:, k] = np.interp(dst, src, m[v0:, k])
        return np.ascontiguousarray(np.vstack([head, body]))
    y = pyworld.synthesize(new_f0, warp(sp), warp(ap), fs)
    return y, onset_s

# ---- assemble on the beat grid: nucleus on the beat, consonant as pickup --
total_beats = sum(d for _, d in notes)
master = np.zeros(int((total_beats * beat + 2.0) * fs))
pos_beats = 0.0
s = 0
for tok, dur in notes:
    if tok != "r":
        midi = float(tok)
        syl = syllables[s] if s < len(syllables) else "la"
        s += 1
        y, onset_s = sing_syllable(syl, midi, dur * beat)
        start = int(max(0.0, pos_beats * beat - onset_s) * fs)
        end = min(len(master), start + len(y))
        master[start:end] += y[: end - start]
        print(f"  {syl:>6} → midi {midi:.0f}, {dur:g} beat(s)", file=sys.stderr)
    pos_beats += dur

master = master / max(1e-9, np.abs(master).max()) * 0.89
sf.write(out_path, master, fs)
print(out_path)
