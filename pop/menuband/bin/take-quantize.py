#!/usr/bin/env python3
"""take-quantize.py — cut a Menu Band take to its good part and beat-align it.

The chord player leads: every chord onset in notes.mid becomes a grid point a
whole number of beats from the last one. Percussion onsets are snapped to the
grid's nearest beat (feel between beats kept; --snap 4 also pins every hit
to a 16th); tones follow the chord anchors; the mic (the
room, the ambient bed) is warped by the same beat map so it stays with the band
but keeps its own detail. rubberband --timemap does the warping per stem.

  pop/.venv/bin/python pop/menuband/bin/take-quantize.py \
      --stems ~/Desktop/cream-comets-nov-1-stems --from 29.45 --bpm 156 --desktop
"""
import argparse, json, subprocess, pathlib, importlib.util
import numpy as np, soundfile as sf, librosa

HERE = pathlib.Path(__file__).resolve().parent
ROOT = HERE.parent.parent.parent
spec = importlib.util.spec_from_file_location('tr', HERE / 'take-remix.py')
tr = importlib.util.module_from_spec(spec); spec.loader.exec_module(tr)


def chord_onsets(notes, start, tol=0.03):
    ts = sorted({round(n['t'], 3) for n in notes if n['t'] >= start - tol})
    out = []
    for t in ts:
        if not out or t - out[-1] > 0.25: out.append(t)   # a chord's notes land within ms
    return out


def beat_grid(anchors, end, beat):
    """Source times for every target beat: anchors spaced by whole beats."""
    src, beats = [anchors[0]], [0]
    for a, b in zip(anchors, anchors[1:]):
        n = max(2, 2 * round((b - a) / beat / 2))  # chords move on half-bars
        src.append(b); beats.append(beats[-1] + n)
    local = (anchors[-1] - anchors[-2]) / (beats[-1] - beats[-2]) if len(anchors) > 1 else beat
    n = int((end - anchors[-1]) / local)
    src.append(anchors[-1] + n * local); beats.append(beats[-1] + n)
    return np.array(src), np.array(beats, float)


def warp(src_t, tgt_t, x, sr, crisp, out):
    tmp = out.with_suffix('.in.wav'); sf.write(tmp, x, sr)
    tm = out.with_suffix('.map')
    pairs = sorted(set((int(round(s * sr)), int(round(t * sr))) for s, t in zip(src_t, tgt_t)))
    keep = [pairs[0]]
    for p in pairs[1:]:
        if p[0] > keep[-1][0] + 64 and p[1] > keep[-1][1] + 64: keep.append(p)
    tm.write_text('\n'.join(f'{s} {t}' for s, t in keep) + '\n')
    dur = len(x) / sr
    target_len = keep[-1][1] / sr + (dur - keep[-1][0] / sr)
    subprocess.run(['rubberband', '-q', '--fine', '-D', f'{target_len:.6f}', '--timemap', str(tm),
                    *(['--crisp', '6'] if crisp else ['--crisp', '1']), str(tmp), str(out)], check=True)
    y, _ = sf.read(out); tmp.unlink(); return y


def slice_place(x, sr, on_src, on_tgt, length, fade=0.006):
    """Cut a stem at its onsets and lay every hit on its grid slot. Hits that
    snap to the same slot ride inside the first one's slice (flams stay flams)."""
    out = np.zeros((int(length * sr) + sr, x.shape[1]))
    f = int(fade * sr)
    slots = []
    for s, t in zip(on_src, on_tgt):
        if slots and abs(t - slots[-1][1]) < 1e-6: continue
        slots.append((s, t))
    for k, (s, t) in enumerate(slots):
        a = int(s * sr) - int(0.004 * sr)                    # a breath before the transient
        b = int(slots[k + 1][0] * sr) - int(0.004 * sr) if k + 1 < len(slots) else len(x)
        room = int(slots[k + 1][1] * sr) - int(t * sr) if k + 1 < len(slots) else b - a
        n = max(0, min(b - a, room, len(x) - a))
        seg = x[a:a + n].copy()
        ramp = min(f, n // 2)
        if ramp:
            seg[:ramp] *= np.linspace(0, 1, ramp)[:, None] ** 0.5
            seg[-ramp:] *= np.linspace(1, 0, ramp)[:, None]
        d = max(0, int(t * sr) - int(0.004 * sr))
        out[d:d + len(seg)] += seg[: len(out) - d]
    return out[: int(length * sr)], len(slots)


def deepen(x, sr, lane):
    """The room, warmer: top end eased off, low shelf up, an octave-down shadow under it."""
    from scipy.signal import butter, sosfilt
    lo = sosfilt(butter(2, 220, 'low', fs=sr, output='sos'), x, axis=0)
    hi = sosfilt(butter(2, 2400, 'high', fs=sr, output='sos'), x, axis=0)
    y = x + lo * 0.8 - hi * 0.6                              # +~5 dB lows, −8 dB above 2.4 kHz
    y = sosfilt(butter(4, 6000, 'low', fs=sr, output='sos'), y, axis=0)
    tmp = lane / 'room-oct.in.wav'; sf.write(tmp, x, sr)
    subprocess.run(['rubberband', '-q', '--fine', '-p', '-12', '-F', str(tmp), str(lane / 'room-oct.wav')], check=True)
    o, _ = sf.read(lane / 'room-oct.wav'); tmp.unlink()
    o = sosfilt(butter(4, 1500, 'low', fs=sr, output='sos'), o[: len(y)], axis=0)
    return y + o * 0.45


def eq_room(x, sr, beat, bars=4):
    """The room shaped: rumble off below 45 Hz, the box (350–700 Hz) scooped, and
    a parallel band-pass whose center drifts 300 Hz–1.6 kHz and back every `bars`
    bars — the moving dub-techno filter (bass-platter 01-dub-bass R7)."""
    from scipy.signal import butter, sosfilt
    y = sosfilt(butter(2, 45, 'high', fs=sr, output='sos'), x, axis=0)
    box = sosfilt(butter(2, [350, 700], 'band', fs=sr, output='sos'), y, axis=0)
    y = y - box * 0.45
    period = bars * 4 * beat * sr; blk = 512; bp = np.zeros_like(y); zi = None
    for i in range(0, len(y), blk):
        ph = 0.5 - 0.5 * np.cos(2 * np.pi * i / period)
        fc = 300 * (1600 / 300) ** ph
        sos = butter(2, [fc / 1.5, fc * 1.5], 'band', fs=sr, output='sos')
        if zi is None: zi = np.zeros((sos.shape[0], 2, 2))
        for c in (0, 1):
            bp[i:i + blk, c], zi[:, :, c] = sosfilt(sos, y[i:i + blk, c], zi=zi[:, :, c])
    return y + bp * 0.9


def hall(sr, secs=2.6, seed=7):
    r = np.random.default_rng(seed); n = int(secs * sr); t = np.arange(n) / sr
    ir = r.standard_normal((n, 2)) * np.exp(-6.9 * t / secs)[:, None]
    ir[: int(0.012 * sr)] = 0                                # predelay
    from scipy.signal import butter, sosfilt
    ir = sosfilt(butter(2, [180, 7000], 'band', fs=sr, output='sos'), ir, axis=0)
    return ir / np.sqrt((ir ** 2).sum(0))


def level(x, db):
    a = np.abs(x).mean(1) if x.ndim > 1 else np.abs(x)
    act = x[a > a.max() * 0.02]
    return x * 10 ** ((db - 20 * np.log10(np.sqrt((act ** 2).mean()) + 1e-12)) / 20)


def kick_times(perc, sr):
    """Kick hits: onsets in the percussion's low band."""
    from scipy.signal import butter, sosfilt
    lo = sosfilt(butter(4, 150, 'low', fs=sr, output='sos'), perc.mean(1))
    return librosa.onset.onset_detect(y=lo, sr=sr, hop_length=256, units='time', backtrack=False)


def duck(n, sr, hits, depth_db=-3, attack=0.002, release=0.12):
    """Sidechain gain: dips depth_db on every hit, eases back over `release`.
    2–3 dB with the fastest attack (bass-platter 01-dub-bass R8); deeper pumps."""
    k = np.arange(int((attack + release * 4) * sr)) / sr
    shape = np.where(k < attack, k / attack, np.exp(-(k - attack) / release))
    trig = np.zeros(n); trig[np.clip((hits * sr).astype(int), 0, n - 1)] = 1
    from scipy.signal import fftconvolve
    env = np.clip(fftconvolve(trig, shape)[:n], 0, 1)
    return (1 - env * (1 - 10 ** (depth_db / 20)))[:, None]


def spread(x, sr, seed=3, ms=(9, 23)):
    """Mono → stereo: decorrelated early reflections (two short noise IRs) on the sides."""
    from scipy.signal import fftconvolve
    m = x.mean(1); r = np.random.default_rng(seed); n = int(0.06 * sr); t = np.arange(n) / sr
    side = []
    for c, d in enumerate(ms):
        ir = r.standard_normal(n) * np.exp(-t / 0.018); ir[: int(d / 1000 * sr)] = 0
        side.append(fftconvolve(m, ir / np.sqrt((ir ** 2).sum()))[: len(m)])
    s = (side[0] - side[1]) * 0.5
    return np.stack([m + s, m - s], 1)


def ms(x, width=1.0, mono_below=120, sr=44100):
    """Mid/side width, with everything under mono_below Hz folded to the center."""
    from scipy.signal import butter, sosfilt
    m, s = (x[:, 0] + x[:, 1]) / 2, (x[:, 0] - x[:, 1]) / 2
    s = sosfilt(butter(2, mono_below, 'high', fs=sr, output='sos'), s) * width
    return np.stack([m + s, m - s], 1)


def glue(x, sr, thresh_db=-18, ratio=2.0, attack=0.02, release=0.25):
    """Slow bus compressor on the summed level (stereo-linked)."""
    from scipy.signal import lfilter
    p = (x ** 2).mean(1)
    a = np.exp(-1 / (release * sr)); env = lfilter([1 - a], [1, -a], p)
    db = 10 * np.log10(env + 1e-12)
    gr = np.minimum(0, (thresh_db - db) * (1 - 1 / ratio))
    return x * 10 ** (gr / 20)[:, None]


def mix(W, sr, beat, pre, lane, title, desktop, room_db, name='aligned', stamp=None):
    from scipy.signal import fftconvolve, butter, sosfilt
    import pyloudnorm
    tail = int(2.5 * sr)
    pad = lambda x: np.vstack([x, np.zeros((tail, 2))])
    perc, tones, room = (pad(level(W[k], d)) for k, d in
                         (('percussion', -21), ('tones', -18), ('voice', room_db)))
    n = len(perc)
    # the room, spread wide and pumping under the kick
    room = ms(spread(eq_room(room, sr, beat), sr), 1.6, sr=sr) * duck(n, sr, kick_times(perc, sr))
    room[: int(pre * sr)] *= np.linspace(0, 1, int(pre * sr))[:, None] ** 2  # the room fades in over `pre`
    tones = ms(tones, 1.25, sr=sr)
    perc = ms(perc, 1.1, 150, sr)
    # dotted-8th echo on the tones, ping-ponged
    echo = np.zeros_like(tones); d = int(0.75 * beat * sr)
    for k in range(1, 4):
        e = tones[:-k * d].mean(1) * 0.32 ** k
        echo[k * d:, k % 2] += e
    hall_ir, er_ir = hall(sr), hall(sr, 0.35, seed=11)
    conv = lambda x, ir: np.stack([fftconvolve(x[:, c], ir[:, c])[: len(x)] for c in (0, 1)], 1)
    verb = conv(tones * 0.5 + echo + room * 0.35 + perc * 0.08, hall_ir) * 0.5
    early = conv(perc * 0.3 + tones * 0.3, er_ir) * 0.35       # a room around the band, close
    out = perc + tones + echo * 0.6 + room + verb + early
    end = len(out); f0 = end - tail - int(2 * beat * sr)   # last chord rings, then into the hall
    out[f0:] *= np.linspace(1, 0, end - f0)[:, None] ** 1.5
    if stamp is not None:  # (audio, at seconds): a sung ident inside the band — echo + hall like the tones
        v, at = stamp; v = level(v, -20); i = int(at * sr)
        vv = np.vstack([v, np.zeros((int(2.5 * sr), 2))])
        ve = np.zeros_like(vv)
        for k in range(1, 4): ve[k * d:, k % 2] += vv[:-k * d].mean(1) * 0.3 ** k
        vv = vv + ve * 0.7
        vv = vv + conv(vv, hall_ir) * 0.35
        out[i:i + len(vv)] += vv[: len(out) - i]
    # master: low end centered, a little air, a little body, glue, loudness
    out = ms(out, 1.0, 110, sr)
    air = sosfilt(butter(2, 9000, 'high', fs=sr, output='sos'), out, axis=0)
    mud = sosfilt(butter(2, [220, 450], 'band', fs=sr, output='sos'), out, axis=0)
    out = glue(out + air * 0.25 - mud * 0.2, sr)
    # no dead air: trim the tail where it falls 50 dB under the peak, plus a breath
    env = np.abs(out).max(1); live = np.nonzero(env > env.max() * 10 ** (-50 / 20))[0]
    out = out[: min(len(out), live[-1] + int(0.25 * sr))]
    meter = pyloudnorm.Meter(sr)
    out *= 10 ** ((-14 - meter.integrated_loudness(out)) / 20)
    wav = lane / f'{title}-{name}.wav'; sf.write(wav, out, sr, subtype='FLOAT')
    mp3 = (pathlib.Path('~/Desktop').expanduser() if desktop else lane) / f'{title}-{name}.mp3'
    subprocess.run(['ffmpeg', '-v', 'error', '-y', '-i', str(wav), '-af',
                    'alimiter=limit=0.89:attack=2:release=80:level=disabled',
                    '-b:a', '320k', str(mp3)], check=True)
    print(f'  mix → {mp3}  ({len(out)/sr:.1f}s)')


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('--stems', required=True)
    ap.add_argument('--from', dest='start', type=float, required=True, help='first chord (s)')
    ap.add_argument('--bpm', type=float, default=156)
    ap.add_argument('--preroll-beats', type=int, default=8, help='room-only lead-in')
    ap.add_argument('--room-db', type=float, default=-19, help='ambient mic level (dBFS active RMS)')
    ap.add_argument('--desktop', action='store_true')
    ap.add_argument('--snap', type=int, default=0,
                    help='also snap every hit to 1/N beat (4 = 16ths); 0 = beat-aligned, feel kept')
    a = ap.parse_args()

    stems = pathlib.Path(a.stems).expanduser()
    title = stems.name.removesuffix('-stems')
    lane = ROOT / 'pop/menuband/takes' / title / 'quantize'; lane.mkdir(parents=True, exist_ok=True)
    beat = 60 / a.bpm
    notes = tr.read_midi(stems / 'notes.mid')

    perc, sr = sf.read(stems / 'percussion.wav')
    tones, _ = sf.read(stems / 'tones.wav')
    voice, _ = sf.read(stems / 'voice.wav')
    if voice.ndim == 1: voice = np.stack([voice, voice], 1)
    end = len(perc) / sr

    anchors = chord_onsets(notes, a.start)
    src, beats = beat_grid(anchors, end, beat)
    pre = a.preroll_beats * beat
    # source window: room lead-in before the first chord, through the last grid beat
    s0 = anchors[0] - pre
    tgt = pre + beats * beat
    print(f'{title}: {len(anchors)} chords, {int(beats[-1])} beats at {a.bpm:g} bpm')
    for s, b in zip(src, beats): print(f'  {s:7.3f}s → beat {int(b):3d}')

    def to_beats(t):  # source time → fractional target beat (piecewise linear over the anchors)
        return np.interp(t, src, beats)

    y = perc.mean(1)
    i0, i1 = int(s0 * sr), int(src[-1] * sr)
    if a.snap:  # every hit onto its 1/N-beat slot
        on = librosa.onset.onset_detect(y=y, sr=sr, units='time', hop_length=128, backtrack=True)
        on = on[(on > anchors[0] - 0.05) & (on < src[-1])]
        snapped = np.maximum(0, np.round(to_beats(on) * a.snap) / a.snap)
    else:       # the drummer's own beats onto the grid's beats; what happens between them stays
        _, bf = librosa.beat.beat_track(y=y, sr=sr, hop_length=128, start_bpm=a.bpm, tightness=200)
        on = librosa.frames_to_time(bf, sr=sr, hop_length=128)
        on = on[(on > anchors[0] - beat / 2) & (on < src[-1] - beat / 2)]
        snapped = np.round(to_beats(on))
        keep = np.concatenate([[True], np.diff(snapped) > 0]); on, snapped = on[keep], snapped[keep]
    moved = np.abs(snapped - to_beats(on)) * beat
    print(f'  percussion: {len(on)} anchors, median nudge {1000*np.median(moved):.0f} ms, max {1000*moved.max():.0f} ms')

    t_src = np.concatenate([[s0], src]) - s0
    t_tgt = np.concatenate([[0], tgt])
    W = {}
    W['tones'] = warp(t_src, t_tgt, tones[i0:i1], sr, False, lane / 'tones.wav')
    if a.snap:
        W['percussion'], slots = slice_place(perc, sr, on, pre + snapped * beat, len(W['tones']) / sr)
        sf.write(lane / 'percussion.wav', W['percussion'], sr)
    else:
        p_src = np.concatenate([[0, anchors[0] - s0], on - s0, [src[-1] - s0]])
        p_tgt = np.concatenate([[0, pre], pre + snapped * beat, [tgt[-1]]])
        o = np.argsort(p_src); p_src, p_tgt = p_src[o], p_tgt[o]
        ok = np.concatenate([[True], (np.diff(p_src) > 0.05) & (np.diff(p_tgt) > 0.05)])
        W['percussion'] = warp(p_src[ok], p_tgt[ok], perc[i0:i1], sr, True, lane / 'percussion.wav')
        W['percussion'][: int(pre * sr)] = 0
    W['voice'] = deepen(warp(t_src, t_tgt, voice[i0:i1], sr, False, lane / 'voice.wav'), sr, lane)
    W['tones'][: int(pre * sr)] = 0           # the lead-in is the room alone

    json.dump({'title': title, 'bpm': a.bpm, 'from': a.start, 'preroll': pre,
               'anchors': anchors, 'grid': {'src': src.tolist(), 'beats': beats.tolist()},
               'snap': a.snap, 'percussion_anchors': len(on)}, open(lane / 'quantize.json', 'w'), indent=1)
    print(f'  stems → {lane}')
    mix(W, sr, beat, pre, lane, title, a.desktop, a.room_db)


if __name__ == '__main__':
    main()
