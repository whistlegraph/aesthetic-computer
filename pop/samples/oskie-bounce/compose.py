#!/usr/bin/env python3
# compose.py — analyze + chop + harmonize oskie's 4-bar bounce into an
# 84-second composition with classic song form (intro / V1 / CH1 / bridge
# / CH2 / outro). All instrumentation is bottom-up: drums from the stem,
# bass / pad / bells / hat are sine-additive voices tuned from chroma-
# derived per-beat root notes.
#
# Writes composition.wav and a heartbeat to ~/.ac-pop-renders/ so the
# Slab menubar shows progress while it renders.

from pathlib import Path
import json, os, random, signal, sys, time, warnings, atexit

import numpy as np
import librosa
import soundfile as sf

warnings.filterwarnings("ignore")

# ── paths + constants ────────────────────────────────────────────────
HERE = Path(__file__).parent
SRC_PATH = HERE / "source.wav"
BASS_PATH = HERE / "stems/htdemucs/bass.wav"
DRUMS_PATH = HERE / "stems/htdemucs/drums.wav"
OUT_PATH = HERE / "composition.wav"

SR = 44100
BPM = 101.33
BEAT_S = 60 / BPM
BAR_S = BEAT_S * 4
LOOP_START_S = 0.813
LOOP_BARS = 4
LOOP_S = LOOP_BARS * BAR_S
TARGET_S = 84.0
SEED = 73

random.seed(SEED)
rng = np.random.default_rng(SEED)

# ── heartbeat (Slab menubar) ─────────────────────────────────────────
RENDERS_DIR = Path(os.environ.get("HOME", "/tmp")) / ".ac-pop-renders"
RENDERS_DIR.mkdir(exist_ok=True)
_PID = os.getpid()
_STARTED = int(time.time() * 1000)
def _b36(n):
    d = "0123456789abcdefghijklmnopqrstuvwxyz"; s = ""
    while n: s = d[n % 36] + s; n //= 36
    return s or "0"
_HB_ID = f"audio-{_PID}-{_b36(_STARTED)}"
_HB_FILE = RENDERS_DIR / f"{_HB_ID}.json"
_HB_LABEL = "oskie-bounce composition"

def hb(pct=None, done=None, total=None):
    rec = {
        "id": _HB_ID, "type": "audio", "label": _HB_LABEL,
        "pct": None if pct is None else int(max(0, min(100, pct))),
        "done": done, "total": total,
        "pid": _PID, "startedAt": _STARTED, "updatedAt": int(time.time()*1000),
    }
    _HB_FILE.write_text(json.dumps(rec))

def hb_end():
    try: _HB_FILE.unlink()
    except FileNotFoundError: pass

atexit.register(hb_end)
signal.signal(signal.SIGTERM, lambda *_: (hb_end(), sys.exit(143)))
signal.signal(signal.SIGINT,  lambda *_: (hb_end(), sys.exit(130)))
hb(0)

# ── load + 4-bar loops ───────────────────────────────────────────────
def load_stereo(p):
    y, _ = librosa.load(str(p), sr=SR, mono=False)
    if y.ndim == 1: y = np.stack([y, y])
    return y.astype(np.float32)

source = load_stereo(SRC_PATH)
bass = load_stereo(BASS_PATH)
drums = load_stereo(DRUMS_PATH)
hb(8)

def cut_loop(stereo, start_s, dur_s):
    a = int(start_s * SR); b = a + int(dur_s * SR)
    return stereo[:, a:b]

src_loop = cut_loop(source, LOOP_START_S, LOOP_S)
bass_loop = cut_loop(bass, LOOP_START_S, LOOP_S)
drums_loop = cut_loop(drums, LOOP_START_S, LOOP_S)
LOOP_N = src_loop.shape[1]

# ── onset chops on the original recording ────────────────────────────
mono_src = source.mean(axis=0)
onsets = librosa.onset.onset_detect(
    y=mono_src, sr=SR, backtrack=True, hop_length=512, units="samples"
)
onsets = np.append(onsets, mono_src.shape[0])
chops = [source[:, a:b] for a, b in zip(onsets[:-1], onsets[1:])
         if b - a >= int(0.04 * SR)]
hb(15)

# ── tonal analysis on bass stem ──────────────────────────────────────
mono_bass = bass.mean(axis=0)
chroma = librosa.feature.chroma_cqt(y=mono_bass, sr=SR, hop_length=512)
HOP = 512
beat_roots = []
for i in range(LOOP_BARS * 4):
    bs = LOOP_START_S + i * BEAT_S
    f0 = int(bs * SR / HOP); f1 = int((bs + BEAT_S) * SR / HOP)
    f1 = min(f1, chroma.shape[1])
    if f1 <= f0:
        beat_roots.append(beat_roots[-1] if beat_roots else 0); continue
    beat_roots.append(int(np.argmax(chroma[:, f0:f1].mean(axis=1))))

maj = np.array([6.35,2.23,3.48,2.33,4.38,4.09,2.52,5.19,2.39,3.66,2.29,2.88])
mnr = np.array([6.33,2.68,3.52,5.38,2.60,3.53,2.54,4.75,3.98,2.69,3.34,3.17])
cmean = chroma.mean(axis=1)
best = max(
    [(float(np.corrcoef(np.roll(p, r), cmean)[0,1]), r, m)
     for m, p in [("maj", maj), ("min", mnr)] for r in range(12)]
)
KEY_ROOT, KEY_MODE = best[1], best[2]
THIRD = 3 if KEY_MODE == "min" else 4
NOTES = ["C","C#","D","D#","E","F","F#","G","G#","A","A#","B"]

# ── spectral envelope of the SOURCE (where the actual content lives) ─
src_loop_mono = source.mean(axis=0)[int(LOOP_START_S*SR):int((LOOP_START_S+LOOP_S)*SR)]
S = np.abs(librosa.stft(src_loop_mono, n_fft=4096, hop_length=512))
mag = S.mean(axis=1)
freqs = librosa.fft_frequencies(sr=SR, n_fft=4096)
csum = np.cumsum(mag); _tot = csum[-1] + 1e-12
def _at_pct(p):
    return float(freqs[min(int(np.searchsorted(csum, p*_tot)), len(freqs)-1)])
SRC_CENTROID = float((freqs*mag).sum() / mag.sum())
SRC_MEDIAN_HZ = _at_pct(0.50)
SRC_ROLLOFF_85 = _at_pct(0.85)

# Bass fundamental via YIN on the bass stem — anchors our octave choices
bass_loop = bass.mean(axis=0)[int(LOOP_START_S*SR):int((LOOP_START_S+LOOP_S)*SR)]
f0_track = librosa.yin(bass_loop.astype(np.float32), fmin=35, fmax=400,
                       sr=SR, frame_length=4096)
f0_clean = f0_track[(f0_track > 35) & (f0_track < 400)]
BASS_F0 = float(np.median(f0_clean)) if f0_clean.size else 82.0
# MIDI of the bass fundamental
BASS_MIDI = int(round(69 + 12 * np.log2(BASS_F0 / 440.0)))
BASS_OCTAVE = (BASS_MIDI // 12) - 1   # C-1 = 0 → octave = midi//12 - 1
print(f"key: {NOTES[KEY_ROOT]} {KEY_MODE}   beat roots: {[NOTES[r] for r in beat_roots]}")
print(f"src centroid={SRC_CENTROID:.0f}Hz  median={SRC_MEDIAN_HZ:.0f}Hz  rolloff85={SRC_ROLLOFF_85:.0f}Hz")
print(f"bass f0={BASS_F0:.1f}Hz  →  MIDI {BASS_MIDI}  →  octave {BASS_OCTAVE}")
hb(22)

# ── synth voices ─────────────────────────────────────────────────────
def midi_to_hz(m): return 440.0 * 2 ** ((m - 69) / 12)
def pc_to_midi(pc, octave): return pc + 12 * (octave + 1)

def env_ar(n, attack_s, release_s):
    e = np.ones(n, dtype=np.float32)
    a = min(int(attack_s * SR), n); r = min(int(release_s * SR), n)
    if a: e[:a] = np.linspace(0, 1, a)
    if r: e[-r:] *= np.linspace(1, 0, r)
    return e

def sub_voice(freq, dur_s, gain=0.45):
    n = int(dur_s * SR); t = np.arange(n) / SR
    y = (np.sin(2*np.pi*freq*t)*0.85 + np.sin(2*np.pi*freq*2*t)*0.15).astype(np.float32)
    y *= env_ar(n, 0.02, 0.08) * gain
    return np.stack([y*0.92, y])

def pad_voice(midis, dur_s, gain=0.20):
    n = int(dur_s * SR); t = np.arange(n) / SR
    y = np.zeros(n, dtype=np.float32)
    for m in midis:
        f = midi_to_hz(m)
        y += np.sin(2*np.pi*f*t).astype(np.float32) * 0.5
        y += np.sin(2*np.pi*f*1.005*t).astype(np.float32) * 0.25
    peak = np.abs(y).max()
    if peak > 0: y *= gain / max(peak, 1e-6)
    y *= env_ar(n, 0.25, 0.6)
    return np.stack([y, y])

def bell_voice(freq, dur_s, gain=0.30):
    n = int(dur_s * SR); t = np.arange(n) / SR
    parts = [(1.0, 1.0, 0.8), (2.01, 0.5, 0.45),
             (3.02, 0.25, 0.30), (4.5, 0.12, 0.18)]
    y = np.zeros(n, dtype=np.float32)
    for mult, amp, dec in parts:
        y += amp * np.sin(2*np.pi*freq*mult*t).astype(np.float32) * np.exp(-t / dec)
    peak = np.abs(y).max()
    if peak > 0: y *= gain / peak
    haas = min(int(0.003 * SR), n - 1)
    yR = np.concatenate([np.zeros(haas, dtype=np.float32), y[:n-haas]])
    return np.stack([y, yR])

def hat_voice(dur_s=0.05, gain=0.18):
    n = int(dur_s * SR)
    y = rng.standard_normal(n).astype(np.float32) * gain
    y *= np.exp(-np.arange(n) / (0.015 * SR))
    return np.stack([y*0.9, y])

# ── per-loop synth layers (octaves anchored to BASS_OCTAVE) ──────────
# All synth voices sit inside the measured spectral envelope of the
# source — nothing above SRC_ROLLOFF_85, sub at the bass fundamental,
# pad one octave up, bells two octaves up (still well below rolloff).
SUB_OCTAVE = BASS_OCTAVE                # match bass fundamental octave
PAD_OCTAVE = BASS_OCTAVE + 1            # pad sits a 5th–octave above sub
BELL_OCTAVE = BASS_OCTAVE + 2           # bells two octaves up, clamped
BELL_MAX_HZ = min(SRC_ROLLOFF_85 * 0.55, 700.0)

def _clamp_hz(hz, ceiling):
    while hz > ceiling: hz *= 0.5
    return hz

sub_layer = np.zeros((2, LOOP_N), dtype=np.float32)
for i, pc in enumerate(beat_roots):
    freq = midi_to_hz(pc_to_midi(pc, SUB_OCTAVE))
    seg = sub_voice(freq, BEAT_S * 0.95, gain=0.45)
    a = int(i * BEAT_S * SR); b = min(a + seg.shape[1], LOOP_N)
    sub_layer[:, a:b] += seg[:, :b-a]

pad_layer = np.zeros((2, LOOP_N), dtype=np.float32)
for bar in range(LOOP_BARS):
    quad = beat_roots[bar*4:(bar+1)*4]
    root = int(np.bincount(quad, minlength=12).argmax())
    root_midi = pc_to_midi(root, PAD_OCTAVE)
    midis = [root_midi, root_midi + THIRD, root_midi + 7, root_midi + 12]
    seg = pad_voice(midis, BAR_S, gain=0.22)
    a = int(bar * BAR_S * SR); b = min(a + seg.shape[1], LOOP_N)
    pad_layer[:, a:b] += seg[:, :b-a]

bell_layer = np.zeros((2, LOOP_N), dtype=np.float32)
arp = [0, 7, 12, THIRD]
sixteenth_n = int(BEAT_S * SR / 4)
for step in range(LOOP_BARS * 16):
    pc = beat_roots[step // 4]
    freq = midi_to_hz(pc_to_midi(pc, BELL_OCTAVE) + arp[step % 4])
    freq = _clamp_hz(freq, BELL_MAX_HZ)
    seg = bell_voice(freq, 0.5, gain=0.18)
    a = step * sixteenth_n
    if a >= LOOP_N: break
    b = min(a + seg.shape[1], LOOP_N)
    bell_layer[:, a:b] += seg[:, :b-a]

hat_layer = np.zeros((2, LOOP_N), dtype=np.float32)
eighth_n = int(BEAT_S * SR / 2)
for step in range(LOOP_BARS * 8):
    seg = hat_voice(0.05, gain=0.16 if step % 2 == 0 else 0.10)
    a = step * eighth_n
    if a >= LOOP_N: break
    b = min(a + seg.shape[1], LOOP_N)
    hat_layer[:, a:b] += seg[:, :b-a]
hb(45)

# ── arrangement: 36 bars (~85s, trimmed to exactly 84.0s) ────────────
sections = [("intro",4), ("verse1",8), ("chorus1",8),
            ("bridge",4), ("chorus2",8), ("outro",4)]
TOTAL_BARS = sum(n for _, n in sections)
out_n = int(TOTAL_BARS * BAR_S * SR) + int(2.0 * SR)
mix = np.zeros((2, out_n), dtype=np.float32)

def tile(layer, n_bars, gain=1.0):
    reps = (n_bars + LOOP_BARS - 1) // LOOP_BARS
    tiled = np.tile(layer, (1, reps))
    return tiled[:, :int(n_bars * BAR_S * SR)] * gain

def place(layer, bar_offset, gain=1.0):
    a = int(bar_offset * BAR_S * SR)
    b = min(a + layer.shape[1], mix.shape[1])
    mix[:, a:b] += layer[:, :b-a] * gain

cursor = 0
for name, nb in sections:
    if name == "intro":
        # sparse chops on 16ths + whisper pad to telegraph key
        slots = nb * 16; sixteenth_s = BEAT_S / 4
        buf_n = int(nb * BAR_S * SR) + int(0.3 * SR)
        buf = np.zeros((2, buf_n), dtype=np.float32)
        slot = 0
        while slot < slots:
            ch = random.choice(chops)
            if random.random() < 0.25: ch = ch[:, ::-1]
            a = int(slot * sixteenth_s * SR)
            b = min(a + ch.shape[1], buf.shape[1])
            buf[:, a:b] += ch[:, :b-a] * 0.7
            slot += max(2, round(ch.shape[1] / (sixteenth_s * SR)))
        place(buf, cursor)
        place(tile(pad_layer, nb), cursor, gain=0.35)
    elif name == "verse1":
        place(tile(src_loop, nb), cursor, gain=0.95)
        place(tile(sub_layer, nb), cursor, gain=0.85)
        place(tile(hat_layer, nb), cursor, gain=0.5)
    elif name == "chorus1":
        place(tile(src_loop, nb), cursor, gain=1.0)
        place(tile(sub_layer, nb), cursor, gain=0.9)
        place(tile(pad_layer, nb), cursor, gain=0.85)
        place(tile(bell_layer, nb), cursor, gain=0.75)
        place(tile(hat_layer, nb), cursor, gain=0.7)
    elif name == "bridge":
        place(tile(drums_loop, nb), cursor, gain=0.7)
        root = beat_roots[0]
        root_midi = pc_to_midi(root, PAD_OCTAVE)
        big = pad_voice(
            [root_midi, root_midi + THIRD, root_midi + 7, root_midi + 12],
            nb * BAR_S, gain=0.40,
        )
        place(big, cursor)
        for step in range(nb * 16):
            if random.random() < 0.30:
                pc = beat_roots[step % len(beat_roots)]
                freq = midi_to_hz(pc_to_midi(pc, BELL_OCTAVE) + arp[step % 4])
                freq = _clamp_hz(freq, BELL_MAX_HZ)
                seg = bell_voice(freq, 0.55, gain=0.20)
                a = int(cursor * BAR_S * SR + step * (BEAT_S/4) * SR)
                b = min(a + seg.shape[1], mix.shape[1])
                mix[:, a:b] += seg[:, :b-a]
            if step % 4 == 0:  # quarter-note chops as anchors
                ch = random.choice(chops)
                if ch.shape[1] > sixteenth_n * 2:
                    ch = ch[:, :sixteenth_n * 3]
                a = int(cursor * BAR_S * SR + step * (BEAT_S/4) * SR)
                b = min(a + ch.shape[1], mix.shape[1])
                mix[:, a:b] += ch[:, :b-a] * 0.45
    elif name == "chorus2":
        place(tile(src_loop, nb), cursor, gain=1.0)
        place(tile(sub_layer, nb), cursor, gain=1.0)
        place(tile(pad_layer, nb), cursor, gain=0.95)
        place(tile(bell_layer, nb), cursor, gain=0.85)
        place(tile(hat_layer, nb), cursor, gain=0.8)
    elif name == "outro":
        tail = tile(src_loop, nb, 1.0)
        tail *= np.linspace(1, 0, tail.shape[1]) ** 1.5
        place(tail, cursor)
        ptail = tile(pad_layer, nb, 0.5)
        ptail *= np.linspace(1, 0, ptail.shape[1]) ** 1.2
        place(ptail, cursor)
        last_freq = midi_to_hz(pc_to_midi(beat_roots[0], BELL_OCTAVE))
        last_freq = _clamp_hz(last_freq, BELL_MAX_HZ)
        last = bell_voice(last_freq, 3.0, gain=0.22)
        place(last, cursor)
    cursor += nb
    hb(45 + int((cursor / TOTAL_BARS) * 45))

# trim to exactly 84.0s
final_n = int(TARGET_S * SR)
if mix.shape[1] < final_n:
    mix = np.pad(mix, ((0,0),(0, final_n - mix.shape[1])))
else:
    mix = mix[:, :final_n]

# soft limit + master fade
peak = float(np.abs(mix).max())
if peak > 0.95: mix *= 0.95 / peak
fi = int(0.3 * SR); fo = int(2.0 * SR)
mix[:, :fi] *= np.linspace(0, 1, fi)
mix[:, -fo:] *= np.linspace(1, 0, fo) ** 1.4
hb(95)

sf.write(str(OUT_PATH), mix.T, SR)
print(f"wrote {OUT_PATH.name}, {mix.shape[1]/SR:.2f}s")
hb(100)
