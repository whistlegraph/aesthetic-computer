#!/usr/bin/env python3
"""Slab reactive listener. Detects high-frequency transients (claps/snaps/
shushes/kisses/hums/sings) and responds with short pentatonic pluck-arpeggios
that MIRROR the input's pitch contour (rising → asc arp, falling → desc).

Writes per-session artifacts to $SLAB_HOME/sessions/:
  - <timestamp>.wav   (Python-generated output mix, int16 mono 44.1 kHz)
  - <timestamp>.jsonl (trigger events: peak, base, contour, notes, energy)

Launched by lid-ambient.sh on lid-close; terminated on lid-open.
"""

import sys
import os
import time
import json
import wave
import signal
import threading
import math
from datetime import datetime

import numpy as np
import sounddevice as sd

SLAB_HOME = os.environ.get('SLAB_HOME', os.path.expanduser('~/.local/share/slab'))
LOG_PATH = os.path.join(SLAB_HOME, 'logs', 'reactive.log')
SESSION_DIR = os.path.join(SLAB_HOME, 'sessions')
os.makedirs(os.path.dirname(LOG_PATH), exist_ok=True)
os.makedirs(SESSION_DIR, exist_ok=True)

# -------- config (tuneable) --------
SR = 44100
BLOCK = 1024
HIGH_BAND = (2000.0, 8000.0)
TRIGGER_RATIO = 3.5
MIN_ABS_ENERGY = 0.02
MIN_GAP = 0.3
FLOOR_DECAY = 0.98
WARMUP = 1.5

DIV_FACTOR = 5.0
NOTE_DUR = 0.09
PLUCK_TAIL = 0.22
ARP_NOTES = 4
ARP_AMP = 0.22

# C major pentatonic, C3..C6
PENT_MIDI = [48, 50, 52, 55, 57, 60, 62, 64, 67, 69, 72, 74, 76, 79, 81, 84]
PENT_HZ = [440.0 * 2 ** ((m - 69) / 12) for m in PENT_MIDI]

RECENT_SEC = 0.20
RECENT_N = int(SR * RECENT_SEC)

_stamp = datetime.now().strftime('%Y%m%d-%H%M%S')
WAV_PATH = os.path.join(SESSION_DIR, f'{_stamp}.wav')
JSONL_PATH = os.path.join(SESSION_DIR, f'{_stamp}.jsonl')


def log(msg):
    with open(LOG_PATH, 'a') as f:
        f.write(f"{time.strftime('%Y-%m-%d %H:%M:%S')} {msg}\n")


# -------- session artifacts --------
_wav = wave.open(WAV_PATH, 'wb')
_wav.setnchannels(1)
_wav.setsampwidth(2)
_wav.setframerate(SR)
_jsonl = open(JSONL_PATH, 'w', buffering=1)
_wav_lock = threading.Lock()


def session_event(kind, **kw):
    kw['event'] = kind
    kw['ts'] = time.time()
    try:
        _jsonl.write(json.dumps(kw) + '\n')
    except Exception:
        pass


def session_write_frames(float_buf):
    pcm = np.clip(float_buf * 32767.0, -32768, 32767).astype(np.int16).tobytes()
    with _wav_lock:
        try:
            _wav.writeframes(pcm)
        except Exception:
            pass


# -------- synthesis --------
def make_pluck(freq, tail=PLUCK_TAIL, amp=ARP_AMP):
    n = int(SR * tail)
    t = np.arange(n) / SR
    sig = np.sin(2 * np.pi * freq * t) + 0.25 * np.sin(2 * np.pi * freq * 2 * t)
    env = np.exp(-9 * t)
    atk = 0.003
    m_atk = t < atk
    env[m_atk] *= t[m_atk] / atk
    return (amp * env * sig).astype(np.float32)


def make_arp(base_freq, contour, n=ARP_NOTES):
    log_b = math.log(base_freq)
    idx0 = min(range(len(PENT_HZ)),
               key=lambda i: abs(math.log(PENT_HZ[i]) - log_b))
    if contour == 'asc':
        offsets = [0, 2, 4, 6]
    elif contour == 'desc':
        offsets = [6, 4, 2, 0]
    else:
        offsets = [0, 2, 4, 2]
    notes = []
    for off in offsets[:n]:
        i = max(0, min(len(PENT_HZ) - 1, idx0 + off))
        notes.append(PENT_HZ[i])
    total = int(SR * (NOTE_DUR * (n - 1) + PLUCK_TAIL))
    buf = np.zeros(total, dtype=np.float32)
    for i, f in enumerate(notes):
        pluck = make_pluck(f)
        start = int(i * NOTE_DUR * SR)
        end = min(start + len(pluck), total)
        buf[start:end] += pluck[: end - start]
    return buf, notes


def detect_contour(window):
    if len(window) < 256:
        return 'flat'
    thirds = np.array_split(window, 3)
    peaks = []
    for chunk in thirds:
        if len(chunk) < 64:
            continue
        w = chunk * np.hanning(len(chunk))
        spec = np.abs(np.fft.rfft(w))
        freqs = np.fft.rfftfreq(len(chunk), 1.0 / SR)
        mask = (freqs >= HIGH_BAND[0]) & (freqs <= HIGH_BAND[1])
        if not mask.any():
            continue
        band = spec.copy()
        band[~mask] = 0
        peaks.append(float(freqs[int(np.argmax(band))]))
    if len(peaks) < 2:
        return 'flat'
    if peaks[-1] > peaks[0] * 1.12:
        return 'asc'
    if peaks[-1] < peaks[0] * 0.88:
        return 'desc'
    return 'flat'


# -------- state --------
active_voices = []
voices_lock = threading.Lock()
floor = 1e-3
last_trigger = 0.0
start_time = time.time()
recent_audio = np.zeros(RECENT_N, dtype=np.float32)


def input_callback(indata, frames, time_info, status):
    global floor, last_trigger, recent_audio
    audio = indata[:, 0].astype(np.float32)
    if frames >= RECENT_N:
        recent_audio = audio[-RECENT_N:].copy()
    else:
        recent_audio = np.concatenate([recent_audio[frames:], audio])

    win = audio * np.hanning(len(audio))
    spec = np.abs(np.fft.rfft(win))
    freqs = np.fft.rfftfreq(len(audio), 1.0 / SR)
    mask = (freqs >= HIGH_BAND[0]) & (freqs <= HIGH_BAND[1])
    high_energy = float(np.sum(spec[mask] ** 2))
    floor = FLOOR_DECAY * floor + (1 - FLOOR_DECAY) * high_energy

    now = time.time()
    if now - start_time < WARMUP:
        return
    if (high_energy > TRIGGER_RATIO * floor
            and high_energy > MIN_ABS_ENERGY
            and now - last_trigger > MIN_GAP):
        last_trigger = now
        band_spec = spec.copy()
        band_spec[~mask] = 0
        peak_idx = int(np.argmax(band_spec))
        peak_freq = float(freqs[peak_idx])
        base = peak_freq / DIV_FACTOR
        contour = detect_contour(recent_audio)
        arp, notes = make_arp(base, contour)
        with voices_lock:
            active_voices.append([arp, 0])
        log(f"trig peak={peak_freq:7.1f}Hz base={base:6.1f}Hz "
            f"contour={contour} arp=[{' '.join(f'{f:.0f}' for f in notes)}]")
        session_event('arp',
                      peak_hz=round(peak_freq, 1),
                      base_hz=round(base, 1),
                      contour=contour,
                      notes=[round(f, 1) for f in notes],
                      energy=round(high_energy, 4),
                      floor=round(floor, 4))
        floor = max(floor, high_energy * 0.5)


def output_callback(outdata, frames, time_info, status):
    outdata.fill(0)
    with voices_lock:
        remaining = []
        for v in active_voices:
            buf, pos = v
            take = min(len(buf) - pos, frames)
            if take > 0:
                outdata[:take, 0] += buf[pos:pos + take]
                v[1] = pos + take
                if v[1] < len(buf):
                    remaining.append(v)
        active_voices[:] = remaining
    np.clip(outdata, -0.95, 0.95, out=outdata)
    session_write_frames(outdata[:, 0])


def shutdown(signum, frame):
    log(f"shutdown signal {signum}")
    session_event('shutdown', signal=int(signum))
    try:
        _wav.close()
        _jsonl.close()
    except Exception:
        pass
    sys.exit(0)


signal.signal(signal.SIGTERM, shutdown)
signal.signal(signal.SIGINT, shutdown)


def main():
    log(f"listener starting (arp mode) session={_stamp}")
    session_event('listener_start', wav=WAV_PATH, jsonl=JSONL_PATH)
    try:
        out_stream = sd.OutputStream(
            samplerate=SR, channels=1, blocksize=BLOCK,
            callback=output_callback, dtype='float32')
        in_stream = sd.InputStream(
            samplerate=SR, channels=1, blocksize=BLOCK,
            callback=input_callback, dtype='float32')
        with in_stream, out_stream:
            while True:
                time.sleep(1)
    except Exception as e:
        log(f"error: {e!r}")
        session_event('error', message=repr(e))
        raise


if __name__ == '__main__':
    main()
