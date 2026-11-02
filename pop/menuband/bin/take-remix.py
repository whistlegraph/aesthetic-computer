#!/usr/bin/env python3
"""take-remix.py — a Menu Band take's voice and composition, in three commands.

  analyze   stems → composition.json (grid, meter, key, arp, keyed melody, sung
            line) + an Aesthetivox note bank (one WORLD-retuned WAV per sung
            note, the manifest shape pop/afternoon-study reads). Dropout holes
            (runs of zeros a skipped IO cycle leaves) are spliced out first.
  sing      lines.json → vocal stems: every line is sung from the bank
            (nearest note, WORLD shift ≤ 5 st + octaves, stretched legato),
            placed at absolute seconds. take-orchestrate.mjs writes lines.json.
  render    the faithful one-shot track (intro · take · take over I–vi–IV–V ·
            outro) with the afternoon-study renderer — the pre-platter path.

  pop/.venv/bin/python pop/menuband/bin/take-remix.py analyze --stems ~/Desktop/<take>-stems
  pop/.venv/bin/python pop/menuband/bin/take-remix.py sing --take <title> --lines <lane>/lines.json
  pop/.venv/bin/python pop/menuband/bin/take-remix.py render --stems ~/Desktop/<take>-stems --desktop

The lane for a take is pop/menuband/takes/<title>/remix/ (gitignored).
"""
import argparse
import json
import math
import shutil
import struct
import sys
from pathlib import Path

import numpy as np
import soundfile as sf
import librosa
import pyworld as pw
from scipy.signal import resample_poly

HERE = Path(__file__).resolve().parent
POP = HERE.parent.parent
sys.path.insert(0, str(POP / 'afternoon-study' / 'bin'))
import render as study  # noqa: E402  — Piano, Mixer, kit, fades, ffmpeg, loudness

SR = study.SR
NAMES = 'C C# D D# E F F# G G# A A# B'.split()


def name(m):
    return f'{NAMES[int(m) % 12]}{int(m) // 12 - 1}'


def lane_for(title):
    return POP / 'menuband' / 'takes' / title / 'remix'


# ------------------------------------------------------------------ stems
def heal(x, min_run=8, xfade=24):
    """Splice out runs of exact zeros inside the signal (skipped IO cycles).

    The zeros are inserted time, not lost samples: the stream resumes where
    it paused, so cutting the run and crossfading the seam restores it.
    """
    mono = x if x.ndim == 1 else x[:, 0]
    nz = np.flatnonzero(np.abs(mono) > 0)
    if len(nz) == 0:
        return x, []
    z = (np.abs(mono) == 0).astype(np.int8)
    dz = np.diff(np.concatenate([[0], z, [0]]))
    starts, ends = np.flatnonzero(dz == 1), np.flatnonzero(dz == -1)
    runs = [(s, e) for s, e in zip(starts, ends) if e - s >= min_run and nz[0] < s and e < nz[-1]]
    if not runs:
        return x, []
    keep, pos = [], 0
    for s, e in runs:
        keep.append((pos, s))
        pos = e
    keep.append((pos, len(mono)))
    pieces = [x[a:b] for a, b in keep if b > a]
    out = pieces[0]
    for p in pieces[1:]:
        n = min(xfade, len(out), len(p))
        ramp = np.linspace(0, 1, n)
        if x.ndim > 1:
            ramp = ramp[:, None]
        seam = out[-n:] * (1 - ramp) + p[:n] * ramp
        out = np.concatenate([out[:-n], seam, p[n:]])
    return out, [(round(float(s) / 44100, 3), int(e - s)) for s, e in runs]


# ------------------------------------------------------------------- MIDI
def read_midi(path):
    b = path.read_bytes()
    p = 14
    div = struct.unpack('>H', b[12:14])[0]
    ntr = struct.unpack('>H', b[10:12])[0]
    notes = []
    for _ in range(ntr):
        length = struct.unpack('>I', b[p + 4:p + 8])[0]
        p += 8
        end = p + length
        tick, run, tempo = 0, 0, 500000
        open_ = {}
        while p < end:
            d = 0
            while True:
                c = b[p]; p += 1
                d = (d << 7) | (c & 0x7f)
                if not c & 0x80:
                    break
            tick += d
            st = b[p]
            if st < 0x80:
                st = run
            else:
                p += 1; run = st
            if st == 0xff:
                ty = b[p]; p += 1
                l = 0
                while True:
                    c = b[p]; p += 1
                    l = (l << 7) | (c & 0x7f)
                    if not c & 0x80:
                        break
                if ty == 0x51:
                    tempo = int.from_bytes(b[p:p + 3], 'big')
                p += l
            elif st in (0xf0, 0xf7):
                l = 0
                while True:
                    c = b[p]; p += 1
                    l = (l << 7) | (c & 0x7f)
                    if not c & 0x80:
                        break
                p += l
            else:
                hi = st >> 4
                d1 = b[p]; p += 1
                d2 = 0 if hi in (0xc, 0xd) else b[p]
                if hi not in (0xc, 0xd):
                    p += 1
                t = tick * tempo / div / 1e6
                if hi == 9 and d2 > 0:
                    open_[d1] = (t, d2)
                elif hi == 8 or (hi == 9 and d2 == 0):
                    if d1 in open_:
                        t0, vel = open_.pop(d1)
                        notes.append(dict(t=t0, dur=t - t0, m=d1, vel=vel))
    return sorted(notes, key=lambda n: n['t'])


def composition(notes):
    """Grid, meter and key from the take's own arpeggio."""
    split = 67  # the Menu Band arp lives in octave 4; the keyed melody above
    arp = [n for n in notes if n['m'] <= split]
    mel = [n for n in notes if n['m'] > split]
    onsets = np.array([n['t'] for n in arp])
    d = np.diff(onsets)
    d = d[(d > 0.08) & (d < 1.5)]
    step = float(np.median(d))
    low = min(n['m'] for n in arp)
    lows = np.array([n['t'] for n in arp if n['m'] == low])
    cycle = float(np.median(np.diff(lows))) / step if len(lows) > 2 else 3
    beats_per_bar = 3 if abs(cycle - 3) < 0.4 else 4
    root = low % 12
    major = {(root + i) % 12 for i in (0, 2, 4, 5, 7, 9, 11)}
    return dict(step=step, bpm=60 / step, beats_per_bar=beats_per_bar, root=root,
                key=f'{NAMES[root]} major', scale=sorted(major), arp=arp, melody=mel,
                steps=int(round(max(n['t'] + n['dur'] for n in notes) / step)))


def quantize(notes, step, min_dur=1):
    return [(int(round(n['t'] / step)), max(min_dur, int(round(n['dur'] / step))), n['m']) for n in notes]


def snap(m, scale):
    cands = [(abs(m - (12 * o + pc)), 12 * o + pc) for o in range(2, 8) for pc in scale]
    return min(cands)[1]


# ------------------------------------------------------------------ voice
def pitch_track(x, sr):
    hop = int(sr * 0.005)
    f0, voiced, _ = librosa.pyin(x, fmin=80, fmax=1000, sr=sr, frame_length=2048, hop_length=hop)
    midi = np.where(voiced & np.isfinite(f0), 12 * np.log2(np.maximum(f0, 1) / 440) + 69, np.nan)
    return midi, hop / sr


def sung_line(midi, dt, step, scale, steps):
    """The sung melody on the grid: one note per beat, snapped to the key, merged."""
    per = []
    for b in range(steps):
        a, z = int(b * step / dt), int((b + 1) * step / dt)
        seg = midi[a:z]
        ok = np.isfinite(seg)
        per.append(float(np.median(seg[ok])) if ok.mean() > 0.5 and ok.sum() > 3 else None)
    line = []
    for b, m in enumerate(per):
        if m is None:
            continue
        s = snap(m, scale)
        if line and line[-1][2] == s and line[-1][0] + line[-1][1] == b:
            line[-1] = (line[-1][0], line[-1][1] + 1, s)
        else:
            line.append((b, 1, s))
    return line


def build_bank(x, sr, midi, dt, out, scale):
    """Aesthetivox note bank: segment the sung voice by pitch, retune each note."""
    out.mkdir(parents=True, exist_ok=True)
    idx, notes, i, n = 0, [], 0, len(midi)
    while i < n:
        if not np.isfinite(midi[i]):
            i += 1; continue
        j, ref = i, []
        while j < n and np.isfinite(midi[j]):
            ref.append(midi[j])
            med = np.median(ref[-20:])
            if len(ref) > 20 and abs(midi[j] - med) > 0.7:
                break
            j += 1
        if (j - i) * dt >= 0.11:
            t0, t1 = i * dt, j * dt
            seg = x[int(t0 * sr):int(t1 * sr)].astype(np.float64)
            target = snap(float(np.median(midi[i:j])), scale)
            f0, t = pw.harvest(seg, sr, f0_floor=70, f0_ceil=1000, frame_period=5)
            f0 = pw.stonemask(seg, f0, t, sr)
            fft = pw.get_cheaptrick_fft_size(sr, f0_floor=70)
            sp = pw.cheaptrick(seg, f0, t, sr, fft_size=fft, f0_floor=70)
            ap = pw.d4c(seg, f0, t, sr, fft_size=fft)
            voiced = f0 > 0
            if voiced.sum() < 5:
                i = j; continue
            hz = 440 * 2 ** ((target - 69) / 12)
            med = np.median(f0[voiced])
            f0n = np.where(voiced, hz * (f0 / med) ** 0.04, 0.0)   # 4 % of the glide survives
            y = pw.synthesize(np.ascontiguousarray(f0n), np.ascontiguousarray(sp),
                              np.ascontiguousarray(ap), sr, 5)
            y *= study.fades(len(y), 0.01, 0.03)
            y = y / max(1e-6, np.abs(y).max()) * 0.5
            path = out / f'{idx:03d}.wav'
            sf.write(path, y, sr, subtype='FLOAT')
            notes.append(dict(index=idx, start=round(t0, 3), end=round(t1, 3), pitchable=True,
                              sungMidi=round(float(np.median(midi[i:j])), 2),
                              samples=dict(lead=dict(file=path.name, targetMidi=int(target)))))
            idx += 1
        i = j
    manifest = dict(sampleRate=sr, source='voice.wav', retune='WORLD f0 → nearest scale tone, 4 % contour kept', notes=notes)
    (out / 'manifest.json').write_text(json.dumps(manifest, indent=1))
    return manifest


class Bank(study.Bank):
    def __init__(self, root, cache):
        self.name = root.name
        self.root = root
        self.manifest = json.loads((root / 'manifest.json').read_text())
        self.cache = cache
        self.used = set()
        self._audio = {}


class Singer:
    def __init__(self, bank, max_shift=5):
        self.bank, self.max_shift, self.turn = bank, max_shift, 0
        self.pool = [n for n in bank.manifest['notes'] if n['pitchable']]

    def pick(self, midi, dur):
        best = []
        for n in self.pool:
            target = n['samples']['lead']['targetMidi']
            natural = n['end'] - n['start']
            shift = midi - target
            octaves = round(shift / 12)
            residue = shift - 12 * octaves
            if abs(residue) > self.max_shift:
                continue
            cost = abs(residue) + .8 * abs(octaves) + (dur / natural if natural < dur * .7 else 0)
            best.append((cost, n['index'], shift, natural))
        best.sort()
        top = [b for b in best if b[0] <= best[0][0] + .6] or best[:1]
        self.turn += 1
        return top[self.turn % len(top)]

    def note(self, midi, dur):
        """One sung note at 48 kHz, legato length, as a mono array."""
        cost, idx, shift, natural = self.pick(midi, dur)
        stretch = min(3.5, max(1.0, dur * 1.15 / natural))
        return self.bank.chop(idx, dur * 1.12, shift=shift, skip=.01, stretch=round(stretch, 2),
                              attack=.015, release=min(.25, dur * .35))


# ---------------------------------------------------------------- analyze
def analyze(stems, out):
    for d in ('cache', 'bank'):
        (out / d).mkdir(parents=True, exist_ok=True)
    meta = json.loads((stems / 'mix.json').read_text())
    title = meta.get('title') or stems.name.replace('-stems', '')
    voice, vsr = sf.read(stems / 'voice.wav', dtype='float32')
    if voice.ndim > 1:
        voice = voice.mean(1)
    voice, vholes = heal(voice)
    print(f'voice: {len(voice) / vsr:.1f}s, healed {len(vholes)} hole(s) {vholes[:6]}')
    sf.write(out / 'voice-healed.wav', voice, vsr)
    v24 = resample_poly(voice.astype(np.float64), 24000, vsr) if vsr != 24000 else voice.astype(np.float64)
    comp = composition(read_midi(stems / 'notes.mid'))
    print(f"grid: step {comp['step']:.4f}s → {comp['bpm']:.1f} BPM, {comp['beats_per_bar']}/4, {comp['key']}, "
          f"{comp['steps']} steps, arp {len(comp['arp'])} notes, keyed melody {len(comp['melody'])} notes")
    midi, dt = pitch_track(v24, 24000)
    line = sung_line(midi, dt, comp['step'], comp['scale'], comp['steps'])
    print('sung line:', ' '.join(f'{name(m)}×{d}' for _, d, m in line))
    manifest = build_bank(v24, 24000, midi, dt, out / 'bank', comp['scale'])
    print(f"bank: {len(manifest['notes'])} notes")
    doc = dict(title=title, stems=str(stems), bpm=comp['bpm'], step=comp['step'], beats_per_bar=comp['beats_per_bar'],
               key=comp['key'], root=comp['root'], scale=comp['scale'], steps=comp['steps'],
               duration=float(meta.get('duration') or len(voice) / vsr),
               arp=quantize(comp['arp'], comp['step']), melody=quantize(comp['melody'], comp['step']), sung=line,
               arpSeconds=[dict(t=round(n['t'], 3), dur=round(n['dur'], 3), midi=n['m'], vel=n['vel']) for n in comp['arp']],
               melodySeconds=[dict(t=round(n['t'], 3), dur=round(n['dur'], 3), midi=n['m'], vel=n['vel']) for n in comp['melody']],
               bank=len(manifest['notes']), holes=dict(voice=vholes))
    (out / 'composition.json').write_text(json.dumps(doc, indent=1))
    print('→', out / 'composition.json')
    return doc, line, manifest


# ------------------------------------------------------------------- sing
class RawVoice:
    """The uncut take itself, WORLD-analysed once, re-sung by the frame: tempo
    stretch, and harmonies that follow the sung line's scale degrees while
    keeping every glide (f0 scaled by the interval; envelope untouched)."""
    def __init__(self, out, comp):
        x, sr = sf.read(out / 'voice-healed.wav', dtype='float64')
        if x.ndim > 1:
            x = x.mean(1)
        self.sr = 24000
        self.x = resample_poly(x, self.sr, sr) if sr != self.sr else x
        self.f0, self.t = pw.harvest(self.x, self.sr, f0_floor=70, f0_ceil=1000, frame_period=5)
        self.f0 = pw.stonemask(self.x, self.f0, self.t, self.sr)
        fft = pw.get_cheaptrick_fft_size(self.sr, f0_floor=70)
        self.sp = pw.cheaptrick(self.x, self.f0, self.t, self.sr, fft_size=fft, f0_floor=70)
        self.ap = pw.d4c(self.x, self.f0, self.t, self.sr, fft_size=fft)
        self.comp = comp
        self.cache = {}

    def ratio_track(self, mode):
        """Per-frame pitch ratio for a harmony mode, following the snapped sung line."""
        n = len(self.f0)
        if mode == 'lead':
            return np.ones(n)
        if mode == 'octave-':
            return np.full(n, 0.5)
        if mode == 'octave+':
            return np.full(n, 2.0)
        step, scale = self.comp['step'], self.comp['scale']
        by_beat = {}
        for b, d, m in self.comp['sung']:
            for k in range(b, b + d):
                by_beat[k] = m
        r = np.ones(n)
        last = 1.0
        for i in range(n):
            m = by_beat.get(int(self.t[i] // step))
            if m is not None:
                if mode == 'third':
                    tgt = third_below(m, scale)
                elif mode == 'third+':
                    k = scale.index(m % 12) if m % 12 in scale else 0
                    tgt = m + ((scale[(k + 2) % 7] - m % 12) % 12)
                elif mode == 'fifth':
                    tgt = m + 7 if (m + 7) % 12 in scale else m + 8
                elif mode == 'sixth':
                    tgt = third_below(third_below(m, scale), scale) + 12
                else:
                    tgt = m
                last = 2 ** ((tgt - m) / 12)
            r[i] = last
        # smooth the interval changes so harmonies glide with the voice
        k = 9
        return np.convolve(np.pad(r, (k // 2, k // 2), mode='edge'), np.ones(k) / k, mode='valid')

    def render(self, mode, tempo=1.0, pitch=0.0):
        key = (mode, round(tempo, 3), round(pitch, 2))
        if key in self.cache:
            return self.cache[key]
        f0 = self.f0 * self.ratio_track(mode) * 2 ** (pitch / 12)
        sp, ap = self.sp, self.ap
        if abs(tempo - 1.0) > 1e-3:
            src = np.arange(len(f0))
            dst = np.linspace(0, len(f0) - 1, int(round(len(f0) / tempo)))
            voiced = np.interp(dst, src, (self.f0 > 0).astype(float)) > 0.5
            f0 = np.where(voiced, np.interp(dst, src, np.where(self.f0 > 0, f0, np.nan)), 0.0)
            f0 = np.nan_to_num(f0)
            sp = np.stack([np.interp(dst, src, sp[:, k]) for k in range(sp.shape[1])], axis=1)
            ap = np.stack([np.interp(dst, src, ap[:, k]) for k in range(ap.shape[1])], axis=1)
        y = pw.synthesize(np.ascontiguousarray(f0), np.ascontiguousarray(sp), np.ascontiguousarray(ap), self.sr, 5)
        y = resample_poly(y, SR, self.sr)
        y = y / max(1e-6, np.abs(y).max()) * 0.6
        self.cache[key] = y
        return y


def sing(out, lines_path):
    """lines.json: {duration, tempo, stems: {name: [{t, dur, midi, gain, pan}]}, raw: [{t, mode, gain, pan}]}
    → <out>/vocals/<name>.wav and raw-<mode>.wav (48 kHz stereo)."""
    spec = json.loads(Path(lines_path).read_text())
    bank = Bank(out / 'bank', out / 'cache')
    singer = Singer(bank)
    vdir = out / 'vocals'
    vdir.mkdir(exist_ok=True)
    total = int(math.ceil(spec['duration'] * SR)) + SR
    report = {}
    if spec.get('raw'):
        comp = json.loads((out / 'composition.json').read_text())
        raw = RawVoice(out, comp)
        tempo = float(spec.get('tempo', 1.0))
        pitch = float(spec.get('pitch', 0.0))
        by_mode = {}
        for pl in spec['raw']:
            by_mode.setdefault(pl['mode'], []).append(pl)
        for mode, places in by_mode.items():
            y = raw.render(mode, tempo, pitch)
            buf = np.zeros((total, 2))
            for pl in places:
                pan = float(pl.get('pan', 0))
                angle = (np.clip(pan, -1, 1) + 1) * np.pi / 4
                g = float(pl.get('gain', 1.0))
                start = int(round(pl['t'] * SR))
                k = min(len(y), total - start, int(float(pl.get('dur', 1e9)) * SR))
                if k <= 0:
                    continue
                seg = y[:k].copy()
                rel = min(k, int(0.4 * SR))
                seg[-rel:] *= np.linspace(1, 0, rel)
                buf[start:start + k, 0] += seg * g * np.cos(angle)
                buf[start:start + k, 1] += seg * g * np.sin(angle)
            stem = f'raw-{mode}'
            sf.write(vdir / f'{stem}.wav', buf, SR, subtype='FLOAT')
            report[stem] = dict(places=len(places), peak=round(float(np.abs(buf).max()), 3))
            print(f'raw {mode}: {len(places)} placement(s) → {vdir / (stem + ".wav")}')
    for stem, notes in spec['stems'].items():
        buf = np.zeros((total, 2))
        for n in notes:
            y = singer.note(int(n['midi']), float(n['dur']))
            pan = float(n.get('pan', 0))
            angle = (np.clip(pan, -1, 1) + 1) * np.pi / 4
            g = float(n.get('gain', 1.0))
            start = int(round((n['t'] - 0.02) * SR))
            if start < 0:
                y, start = y[-start:], 0
            k = min(len(y), total - start)
            if k <= 0:
                continue
            buf[start:start + k, 0] += y[:k] * g * np.cos(angle)
            buf[start:start + k, 1] += y[:k] * g * np.sin(angle)
        peak = np.abs(buf).max()
        if peak > 0.95:
            buf *= 0.95 / peak
        sf.write(vdir / f'{stem}.wav', buf, SR, subtype='FLOAT')
        report[stem] = dict(notes=len(notes), peak=round(float(np.abs(buf).max()), 3))
        print(f'sung {stem}: {len(notes)} notes → {vdir / (stem + ".wav")}')
    (vdir / 'report.json').write_text(json.dumps(report, indent=1))


# ------------------------------------------------- the faithful one-shot
def third_below(m, scale):
    k = scale.index(m % 12) if m % 12 in scale else 0
    pc = scale[(k - 2) % 7]
    return m - ((m % 12 - pc) % 12)


def render_faithful(comp, line, out, lufs, desktop):
    step, bpb = comp['step'], comp['beats_per_bar']
    beat, bar = step, step * bpb
    study.BEAT, study.BAR = beat, bar
    root, scale = comp['root'], comp['scale']
    arp, mel = comp['arp'], comp['melody']
    take_bars = math.ceil(comp['steps'] / bpb)
    form = [('intro', 4), ('A', take_bars), ('B', take_bars), ('outro', 6)]
    total = sum(b for _, b in form) * bar + 6
    mix = study.Mixer(total)
    bank = Bank(out / 'bank', out / 'cache')
    singer = Singer(bank)
    piano = study.Piano(np.random.default_rng(515))
    cycle = [0, 9, 5, 7]
    (out / 'stems').mkdir(exist_ok=True)

    def sing_line(bus, notes, at, gain, pan=0.0):
        for b, d, m in notes:
            if m < 52:
                continue
            y = singer.note(m, d * beat)
            mix.place(bus, y, at + b * beat - .02, gain, pan)

    t = 0.0
    for sect, bars in form:
        for bi in range(bars):
            at = t + bi * bar
            chord_root = root + (cycle[(bi // 2) % 4] if sect == 'B' else 0)
            offset = chord_root - root
            minor = offset % 12 == 9
            tone = lambda m: m + offset - (1 if minor and (m - root) % 12 == 4 else 0)
            if sect in ('intro', 'outro'):
                src, base = [n for n in arp if n[0] < bpb], 0
            else:
                src, base = [n for n in arp if bi * bpb <= n[0] < (bi + 1) * bpb], bi * bpb
            for b, d, m in src:
                g = {'intro': .55, 'A': .7, 'B': .78, 'outro': .5}[sect] * (1 - 0.08 * (bi % 2))
                piano.key(mix, tone(m) - 12 if sect == 'B' else tone(m), at + (b - base) * beat, d * beat * 1.1, g, vel=.62)
            if sect == 'B':
                for b, d, m in [n for n in mel if bi * bpb <= n[0] < (bi + 1) * bpb]:
                    piano.key(mix, m, at + (b - bi * bpb) * beat, d * beat, .42, vel=.55, pan=.3)
            if sect in ('A', 'B'):
                mix.place('bass', study.bass_tone(chord_root - 12, bar * .9), at, .5)
                if bi % 2 == 1:
                    mix.place('bass', study.bass_tone(chord_root - 12 + (7 if sect == 'B' else 0), beat * .5), at + bar - beat, .35)
                mix.place('kit', study.soft_kick(1.0 if sect == 'B' else .8), at, .9)
                mix.place('kit', study.brush(seed=21 + bi), at + bar - beat, .35 if sect == 'B' else .25, pan=.3)
                if sect == 'B':
                    mix.place('kit', study.soft_kick(.6), at + beat, .5)
                    for k in range(bpb):
                        if k % 2 == 1:
                            mix.place('kit', study.bubble(seed=bi * 7 + k), at + k * beat + beat / 2, .2, pan=-.4 + .8 * (k % 2))
                mix.place('mallets', study.marimba(tone(root + 12 + [0, 4, 7][bi % 3]), 'rosewood'), at + beat, .35 if sect == 'B' else .25, pan=-.35)
        if sect in ('A', 'B'):
            sing_line('vox', line, t, .95 if sect == 'A' else 1.0)
            if sect == 'B':
                sing_line('harmony', [(b, d, third_below(m, scale)) for b, d, m in line], t, .45, pan=-.45)
                sing_line('harmony', [(b, d, m + 12) for b, d, m in line if d >= 2], t, .3, pan=.45)
        if sect == 'outro':
            piano.roll(mix, [root + 36, root + 40, root + 43, root + 48], t + 2 * bar, bar * 3, .6)
        t += bars * bar

    levels = dict(piano=.9, vox=1.0, harmony=1.0, bass=1.7, kit=.9, mallets=.8)
    total_buf = np.zeros((mix.total, 2))
    for bus, data in mix.buses.items():
        g = levels.get(bus, 1.0)
        sf.write(out / 'stems' / f'{bus}.wav', np.clip(data * g, -1, 1), SR, subtype='FLOAT')
        total_buf += data * g
    fade = int(3 * SR)
    total_buf[-fade:] *= np.linspace(1, 0, fade)[:, None]
    title = comp['title']
    premaster = out / f'{title}-premaster.wav'
    sf.write(premaster, total_buf / max(1.0, np.abs(total_buf).max() * 1.05), SR, subtype='FLOAT')
    return master(premaster, out / f'{title}-remix.wav', out / f'{title}-remix.mp3', title, lufs, desktop)


def master(premaster, wav, mp3, title, lufs, desktop):
    measured = study.loudness(premaster)
    gain = lufs - float(measured['input_i'])
    study.ffmpeg('-i', str(premaster), '-af', f'volume={gain:.3f}dB,alimiter=limit=0.841:attack=5:release=80:level=false',
                 '-ar', '44100', '-c:a', 'pcm_s16le', str(wav))
    study.ffmpeg('-i', str(wav), '-c:a', 'libmp3lame', '-b:a', '320k', '-id3v2_version', '3',
                 '-metadata', f'title={title}', '-metadata', 'artist=Whistlegraph Dot Org', str(mp3))
    final = study.loudness(wav)
    r = dict(premasterLUFS=float(measured['input_i']), gainDb=round(gain, 2), LUFS=float(final['input_i']),
             truePeak=float(final['input_tp']), LRA=float(final['input_lra']), mp3=str(mp3))
    print(json.dumps(r))
    if desktop:
        shutil.copy(mp3, Path.home() / 'Desktop' / mp3.name)
        print('→', Path.home() / 'Desktop' / mp3.name)
    return r


def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    sub = ap.add_subparsers(dest='cmd', required=True)
    a = sub.add_parser('analyze'); a.add_argument('--stems', type=Path, required=True); a.add_argument('--out', type=Path)
    s = sub.add_parser('sing'); s.add_argument('--take', required=True); s.add_argument('--lines', type=Path, required=True)
    r = sub.add_parser('render'); r.add_argument('--stems', type=Path, required=True); r.add_argument('--out', type=Path)
    r.add_argument('--lufs', type=float, default=-14.0); r.add_argument('--desktop', action='store_true')
    args = ap.parse_args()
    if args.cmd == 'sing':
        sing(lane_for(args.take), args.lines)
        return
    stems = args.stems.expanduser()
    meta = json.loads((stems / 'mix.json').read_text())
    title = meta.get('title') or stems.name.replace('-stems', '')
    out = args.out or lane_for(title)
    comp, line, manifest = analyze(stems, out)
    if args.cmd == 'render':
        render_faithful(comp, line, out, args.lufs, args.desktop)


if __name__ == '__main__':
    main()
