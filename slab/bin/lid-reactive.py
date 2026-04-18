#!/usr/bin/env python3
"""Slab reactive listener and ambient engine.

Owns all lid-closed audio:
  - AMBIENT  — ambient.wav looped at AMBIENT_GAIN
  - NOISE    — continuous soft low-pass-filtered noise whose amplitude
               tracks the smoothed mic RMS (asymmetric EMA: quick rise,
               slow fall). Gives a gentle, always-on signal of what the
               mic is hearing, separate from the discrete pluck triggers.
  - PLUCKS   — high-band transients spawn short pentatonic arpeggios
               whose direction mirrors the input's pitch contour.

On SIGTERM/SIGINT the master gain fades from 1.0 → 0.0 over FADE_DUR
seconds, then the process exits — giving the lid-open return stinger
a soft bed to land on.

Session artifacts in $SLAB_HOME/sessions/:
  - <timestamp>.wav    int16 mono mix of everything this listener emits
  - <timestamp>.jsonl  event log:
        listener_start, arp (per trigger), energy_sample (every
        SAMPLE_LOG_INTERVAL s), fade_start, shutdown (w/ summary stats)
"""

import sys
import os
import time
import json
import wave
import shutil
import signal
import subprocess
import threading
import math
from datetime import datetime

import numpy as np
import sounddevice as sd

SLAB_HOME = os.environ.get('SLAB_HOME', os.path.expanduser('~/.local/share/slab'))
LOG_PATH = os.path.join(SLAB_HOME, 'logs', 'reactive.log')
SESSION_DIR = os.path.join(SLAB_HOME, 'sessions')
CONFIG_DIR = os.path.join(SLAB_HOME, 'config')
ZONES_PATH = os.path.join(CONFIG_DIR, 'zones.json')
LAST_LOC_PATH = os.path.join(SLAB_HOME, 'state', 'last-location.json')
AMBIENT_PATH = os.path.join(SLAB_HOME, 'sounds', 'ambient.wav')
os.makedirs(os.path.dirname(LOG_PATH), exist_ok=True)
os.makedirs(SESSION_DIR, exist_ok=True)
os.makedirs(os.path.dirname(LAST_LOC_PATH), exist_ok=True)

# -------- defaults (overridden per-zone at startup) --------
SR = 44100
BLOCK = 1024
HIGH_BAND = (2000.0, 8000.0)
TRIGGER_RATIO = 3.0
MIN_ABS_ENERGY = 0.015
MIN_GAP = 0.18
FLOOR_DECAY = 0.98
WARMUP = 1.5

DIV_FACTOR = 5.0
NOTE_DUR = 0.09
PLUCK_TAIL = 0.22
ARP_NOTES = 4
ARP_AMP = 0.22

# ambient + noise bed + fade
AMBIENT_GAIN = 0.85
NOISE_GAIN_MAX = 0.10           # cap on noise voice amplitude
NOISE_MIC_SCALE = 2.2           # multiplier on smoothed mic RMS → gain target
NOISE_RISE_ALPHA = 0.30         # EMA alpha when rising (fast)
NOISE_FALL_ALPHA = 0.01         # EMA alpha when falling (slow)
NOISE_LP_TAPS = 12              # moving-average length (lower freqs dominate)
FADE_DUR = 2.0

SAMPLE_LOG_INTERVAL = 0.5
RECENT_SEC = 0.20
RECENT_N = int(SR * RECENT_SEC)

# -------- scales --------
SCALE_INTERVALS = {
    'major_pentatonic': [0, 2, 4, 7, 9],
    'minor_pentatonic': [0, 3, 5, 7, 10],
    'blues':            [0, 3, 5, 6, 7, 10],
    'dorian':           [0, 2, 3, 5, 7, 9, 10],
    'phrygian':         [0, 1, 3, 5, 7, 8, 10],
    'lydian':           [0, 2, 4, 6, 7, 9, 11],
    'whole_tone':       [0, 2, 4, 6, 8, 10],
    'chromatic':        list(range(12)),
}


def build_scale(name, tonic_midi=48, octaves=3):
    intervals = SCALE_INTERVALS.get(name, SCALE_INTERVALS['major_pentatonic'])
    notes = []
    for o in range(octaves + 1):
        for iv in intervals:
            m = tonic_midi + o * 12 + iv
            if m <= tonic_midi + octaves * 12:
                notes.append(m)
    return [440.0 * 2 ** ((m - 69) / 12) for m in notes]


# -------- location + zone resolution --------
def read_location(timeout_s=3.0):
    cli = shutil.which('CoreLocationCLI')
    if not cli:
        return None
    try:
        out = subprocess.run([cli], capture_output=True, text=True,
                             timeout=timeout_s).stdout.strip()
        parts = out.split()
        if len(parts) >= 2:
            lat, lon = float(parts[0]), float(parts[1])
            with open(LAST_LOC_PATH, 'w') as f:
                json.dump({'lat': lat, 'lon': lon, 'ts': time.time()}, f)
            return lat, lon
    except Exception:
        pass
    try:
        with open(LAST_LOC_PATH) as f:
            d = json.load(f)
        return d['lat'], d['lon']
    except Exception:
        return None


def haversine_m(lat1, lon1, lat2, lon2):
    R = 6371000.0
    p1, p2 = math.radians(lat1), math.radians(lat2)
    dp = math.radians(lat2 - lat1)
    dl = math.radians(lon2 - lon1)
    a = math.sin(dp/2)**2 + math.cos(p1) * math.cos(p2) * math.sin(dl/2)**2
    return 2 * R * math.asin(math.sqrt(a))


def resolve_zone(coords):
    try:
        with open(ZONES_PATH) as f:
            cfg = json.load(f)
    except Exception:
        cfg = {'default': {}, 'zones': []}
    defaults = {
        'name': 'default',
        'scale': 'major_pentatonic',
        'div_factor': 5.0,
        'arp_amp': 0.22,
    }
    defaults.update(cfg.get('default', {}))
    if not coords:
        return defaults, None
    lat, lon = coords
    matches = []
    for z in cfg.get('zones', []):
        d = haversine_m(lat, lon, z['lat'], z['lon'])
        if d <= z.get('radius_m', 100):
            matches.append((d, z))
    if not matches:
        return defaults, None
    d, z = min(matches, key=lambda x: x[0])
    out = dict(defaults); out.update(z)
    return out, d


_coords = read_location()
_zone, _zone_dist = resolve_zone(_coords)
DIV_FACTOR = float(_zone.get('div_factor', DIV_FACTOR))
ARP_AMP    = float(_zone.get('arp_amp', ARP_AMP))
PENT_HZ    = build_scale(_zone.get('scale', 'major_pentatonic'))

_stamp = datetime.now().strftime('%Y%m%d-%H%M%S')
_tag = f"-{_zone['name']}" if _zone.get('name') else ''
WAV_PATH = os.path.join(SESSION_DIR, f'{_stamp}{_tag}.wav')
JSONL_PATH = os.path.join(SESSION_DIR, f'{_stamp}{_tag}.jsonl')


def log(msg):
    with open(LOG_PATH, 'a') as f:
        f.write(f"{time.strftime('%Y-%m-%d %H:%M:%S')} {msg}\n")


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


# -------- ambient load --------
def load_wav_f32(path):
    with wave.open(path, 'rb') as w:
        sr = w.getframerate()
        n = w.getnframes()
        data = w.readframes(n)
    arr = np.frombuffer(data, dtype=np.int16).astype(np.float32) / 32767.0
    return arr, sr


try:
    AMBIENT, _amb_sr = load_wav_f32(AMBIENT_PATH)
    if _amb_sr != SR:
        log(f"warning: ambient sr={_amb_sr} != {SR}")
except Exception as e:
    log(f"ambient load failed: {e!r} — silent ambient")
    AMBIENT = np.zeros(SR, dtype=np.float32)


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
session_start_ts = time.time()
recent_audio = np.zeros(RECENT_N, dtype=np.float32)

mic_rms = 0.0
mic_rms_smooth = 0.0
noise_gain_cur = 0.0
ambient_pos = 0

fading = False
fade_start_ts = 0.0

n_triggers = 0
max_mic_rms = 0.0
total_energy_samples = 0
sum_mic_rms = 0.0

_rng = np.random.default_rng()
_NOISE_LP_KERNEL = (np.ones(NOISE_LP_TAPS) / NOISE_LP_TAPS).astype(np.float32)


def current_fade():
    if not fading:
        return 1.0
    t = time.time() - fade_start_ts
    return max(0.0, 1.0 - t / FADE_DUR)


def input_callback(indata, frames, time_info, status):
    global floor, last_trigger, recent_audio, mic_rms, mic_rms_smooth, n_triggers, max_mic_rms
    audio = indata[:, 0].astype(np.float32)
    if frames >= RECENT_N:
        recent_audio = audio[-RECENT_N:].copy()
    else:
        recent_audio = np.concatenate([recent_audio[frames:], audio])

    r = float(np.sqrt(np.mean(audio ** 2))) if frames > 0 else 0.0
    mic_rms = r
    alpha = NOISE_RISE_ALPHA if r > mic_rms_smooth else NOISE_FALL_ALPHA
    mic_rms_smooth = alpha * r + (1.0 - alpha) * mic_rms_smooth
    if r > max_mic_rms:
        max_mic_rms = r

    win = audio * np.hanning(len(audio))
    spec = np.abs(np.fft.rfft(win))
    freqs = np.fft.rfftfreq(len(audio), 1.0 / SR)
    mask = (freqs >= HIGH_BAND[0]) & (freqs <= HIGH_BAND[1])
    high_energy = float(np.sum(spec[mask] ** 2))
    floor = FLOOR_DECAY * floor + (1 - FLOOR_DECAY) * high_energy

    now = time.time()
    if now - session_start_ts < WARMUP or fading:
        return
    if (high_energy > TRIGGER_RATIO * floor
            and high_energy > MIN_ABS_ENERGY
            and now - last_trigger > MIN_GAP):
        last_trigger = now
        n_triggers += 1
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
                      floor=round(floor, 4),
                      mic_rms=round(r, 5))
        floor = max(floor, high_energy * 0.5)


def output_callback(outdata, frames, time_info, status):
    global ambient_pos, noise_gain_cur
    outdata.fill(0)
    fade = current_fade()

    # ambient bed (looped)
    if AMBIENT.size > 0:
        end = ambient_pos + frames
        if end <= AMBIENT.size:
            chunk = AMBIENT[ambient_pos:end]
            ambient_pos = end
        else:
            first = AMBIENT.size - ambient_pos
            chunk = np.empty(frames, dtype=np.float32)
            chunk[:first] = AMBIENT[ambient_pos:]
            chunk[first:] = AMBIENT[:frames - first]
            ambient_pos = frames - first
        outdata[:, 0] += chunk * AMBIENT_GAIN * fade

    # noise voice (soft low-passed, amp tracks smoothed mic rms)
    target = min(NOISE_GAIN_MAX, mic_rms_smooth * NOISE_MIC_SCALE)
    noise_gain_cur = 0.85 * noise_gain_cur + 0.15 * target
    if noise_gain_cur > 1e-4:
        k = _NOISE_LP_KERNEL.size
        white = _rng.standard_normal(frames + k - 1).astype(np.float32)
        filt = np.convolve(white, _NOISE_LP_KERNEL, mode='valid')[:frames]
        outdata[:, 0] += filt * noise_gain_cur * fade

    # pluck arps
    with voices_lock:
        remaining = []
        for v in active_voices:
            buf, pos = v
            take = min(len(buf) - pos, frames)
            if take > 0:
                outdata[:take, 0] += buf[pos:pos + take] * fade
                v[1] = pos + take
                if v[1] < len(buf):
                    remaining.append(v)
        active_voices[:] = remaining

    np.clip(outdata, -0.95, 0.95, out=outdata)
    session_write_frames(outdata[:, 0])


def periodic_sampler():
    """Emit a continuous energy_sample trace every SAMPLE_LOG_INTERVAL s."""
    global total_energy_samples, sum_mic_rms
    while not fading:
        time.sleep(SAMPLE_LOG_INTERVAL)
        if fading:
            break
        total_energy_samples += 1
        sum_mic_rms += mic_rms
        session_event('energy_sample',
                      mic_rms=round(mic_rms, 5),
                      mic_rms_smooth=round(mic_rms_smooth, 5),
                      high_floor=round(floor, 5),
                      noise_gain=round(noise_gain_cur, 4),
                      voices=len(active_voices))


def emit_shutdown(signum, forced=False):
    dur = round(time.time() - session_start_ts, 2)
    avg = round(sum_mic_rms / total_energy_samples, 5) if total_energy_samples else 0.0
    session_event('shutdown',
                  signal=int(signum),
                  forced=forced,
                  triggers=n_triggers,
                  max_mic_rms=round(max_mic_rms, 5),
                  avg_mic_rms=avg,
                  session_duration_s=dur)


def shutdown(signum, frame):
    global fading, fade_start_ts
    if fading:
        log(f"second signal {signum} — exit now")
        emit_shutdown(signum, forced=True)
        try:
            _wav.close(); _jsonl.close()
        except Exception:
            pass
        os._exit(0)
    log(f"signal {signum} — fading out over {FADE_DUR}s "
        f"(triggers={n_triggers} max_rms={max_mic_rms:.4f})")
    session_event('fade_start', duration_s=FADE_DUR)
    fading = True
    fade_start_ts = time.time()

    def exit_after_fade():
        time.sleep(FADE_DUR + 0.2)
        log("fade complete, exiting")
        emit_shutdown(signum)
        try:
            _wav.close(); _jsonl.close()
        except Exception:
            pass
        os._exit(0)
    threading.Thread(target=exit_after_fade, daemon=True).start()


signal.signal(signal.SIGTERM, shutdown)
signal.signal(signal.SIGINT, shutdown)


def main():
    log(f"listener starting session={_stamp} zone={_zone.get('name')} "
        f"coords={_coords} dist={_zone_dist} "
        f"ambient_len={AMBIENT.size / SR:.1f}s")
    session_event('listener_start',
                  wav=WAV_PATH, jsonl=JSONL_PATH,
                  zone=_zone.get('name'),
                  zone_scale=_zone.get('scale'),
                  zone_div_factor=DIV_FACTOR,
                  zone_arp_amp=ARP_AMP,
                  coords=_coords,
                  zone_distance_m=_zone_dist,
                  ambient_seconds=round(AMBIENT.size / SR, 2),
                  trigger_ratio=TRIGGER_RATIO,
                  min_gap=MIN_GAP,
                  fade_dur=FADE_DUR)

    threading.Thread(target=periodic_sampler, daemon=True).start()

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
