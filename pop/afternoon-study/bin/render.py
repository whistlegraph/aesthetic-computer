#!/usr/bin/env python3
"""Afternoon Study: one dance track from the three Sep 26 Menu Band takes.

Saffron Swallows supplies the arpeggio (its C-D-E-F-E-D-C arch, chopped to
sixteenths) and the slow topline of the breakdown. Indigo Shadows supplies
the rhythm (its recorded percussion, sliced into hits and re-sequenced on
its own folded groove) and the hymn (its long hums, WORLD-shifted into
chord tones). Lemon Kittens supplies the hook (its C-D-E-D figure on
eighths) and the climax (its rising answer). Kick, bass and keys are
authored. Every vocal sound is @jeffrey's own pitch-corrected note sample.
"""
import argparse
import hashlib
import json
import math
import re
import subprocess
from pathlib import Path

import numpy as np
import soundfile as sf
import librosa
import pyworld as pw
from scipy.signal import resample_poly, butter, sosfiltfilt

SR = 48000
BPM = 118
BEAT = 60 / BPM
BAR = 4 * BEAT
STEP = BEAT / 4                       # a sixteenth
STUDY = Path.home() / 'Documents/Shelf/Menu Band pop sketches 2026-09-26'
NAMES = ['lemon-kittens', 'indigo-shadows', 'saffron-swallows']


# ---------------------------------------------------------------- utilities
def ffmpeg(*args):
    result = subprocess.run(['ffmpeg', '-hide_banner', '-nostats', '-y', *map(str, args)],
                            text=True, capture_output=True)
    if result.returncode:
        raise RuntimeError(result.stderr[-4000:])
    return result


def loudness(path):
    r = ffmpeg('-i', path, '-af', 'loudnorm=I=-14:TP=-1.5:LRA=11:print_format=json', '-f', 'null', '-')
    return json.loads(re.search(r'\{\s*"input_i"[\s\S]*?\}', r.stderr)[0])


def normalize(source, target, level, peak=-3.0):
    measured = loudness(source)
    adjustment = min(level - float(measured['input_i']), peak - float(measured['input_tp']))
    assert math.isfinite(adjustment), f'silent source: {source}'
    ffmpeg('-i', source, '-af', f'volume={adjustment:.3f}dB', '-ar', SR, '-ac', 2, '-c:a', 'pcm_f32le', target)
    return adjustment


def load_mono(path, sr=SR):
    x, rate = sf.read(path, always_2d=True)
    x = x.mean(axis=1)
    if rate != sr:
        g = math.gcd(sr, rate)
        x = resample_poly(x, sr // g, rate // g)
    return x.astype(np.float64)


def fades(n, attack, release):
    env = np.ones(n)
    a, r = min(n, round(attack * SR)), min(n, round(release * SR))
    if a:
        env[:a] = .5 - .5 * np.cos(np.linspace(0, np.pi, a))
    if r:
        env[n - r:] *= .5 + .5 * np.cos(np.linspace(0, np.pi, r))
    return env


def midi_hz(m):
    return 440 * 2 ** ((m - 69) / 12)


NOTE_RE = re.compile(r'([A-G])(s?)(\d)')
def name_midi(name):
    letter, sharp, octave = NOTE_RE.match(name).groups()
    return 12 * (int(octave) + 1) + 'C D EF G A B'.index(letter) + (1 if sharp else 0)


# ------------------------------------------------------------- vocal banks
class Bank:
    """One take's Aesthetivox note bank (24 kHz mono, one file per note)."""
    def __init__(self, name, cache):
        self.name = name
        self.root = STUDY / 'enhanced-v2/vocal-banks' / name
        self.manifest = json.loads((self.root / 'manifest.json').read_text())
        self.cache = cache
        self.used = set()
        self._audio = {}

    def note(self, index, part='lead'):
        return self.manifest['notes'][index]['samples'][part]

    def midi(self, index, part='lead'):
        return self.note(index, part)['targetMidi']

    def raw(self, index, part='lead'):
        key = (index, part)
        if key not in self._audio:
            x = load_mono(self.root / self.note(index, part)['file'], 24000)
            rms = float(np.sqrt(np.mean(x ** 2)))
            if rms > 1e-5:
                x = x * min(.12 / rms, .9 / max(1e-6, np.abs(x).max()))
            self._audio[key] = x
        return self._audio[key]

    def sample(self, index, part='lead', shift=0, stretch=1.0):
        """The note at 48 kHz, optionally WORLD-shifted (semitones) / stretched."""
        self.used.add((index, part, shift, round(stretch, 3)))
        if shift == 0 and stretch == 1.0:
            return resample_poly(self.raw(index, part), 2, 1)
        tag = f'{self.name}-{index:03d}-{part}-{shift:+d}-x{stretch:.3f}.wav'
        path = self.cache / tag
        if path.exists():
            return load_mono(path)
        x = np.ascontiguousarray(self.raw(index, part))
        fs = 24000
        f0, t = pw.harvest(x, fs, f0_floor=65, f0_ceil=1400, frame_period=5)
        f0 = pw.stonemask(x, f0, t, fs)
        fft = pw.get_cheaptrick_fft_size(fs, f0_floor=65)
        sp = pw.cheaptrick(x, f0, t, fs, fft_size=fft, f0_floor=65)
        ap = pw.d4c(x, f0, t, fs, fft_size=fft)
        if stretch != 1.0:
            src = np.arange(len(f0))
            dst = np.linspace(0, len(f0) - 1, round(len(f0) * stretch))
            f0 = np.interp(dst, src, f0)
            sp = np.stack([np.interp(dst, src, sp[:, k]) for k in range(sp.shape[1])], axis=1)
            ap = np.stack([np.interp(dst, src, ap[:, k]) for k in range(ap.shape[1])], axis=1)
        f0 = f0 * 2 ** (shift / 12)
        y = pw.synthesize(np.ascontiguousarray(f0), np.ascontiguousarray(sp),
                          np.ascontiguousarray(ap), fs, 5)
        y = y / max(1e-6, np.abs(y).max()) * max(1e-6, np.abs(x).max())
        y = resample_poly(y, 2, 1)
        sf.write(path, y, SR, subtype='FLOAT')
        return y

    def chop(self, index, length, part='lead', shift=0, skip=0.0, attack=.004, release=.03, stretch=1.0):
        x = self.sample(index, part, shift, stretch)[round(skip * SR):]
        n = min(len(x), round(length * SR))
        return x[:n] * fades(n, attack, release)




# ------------------------------------------------ Indigo's recorded thumps
def indigo_thumps(cache):
    """Slice the loud, real hits out of Indigo Shadows' percussion stem.

    Only hits with a peak above .3 are kept: the quiet taps and ticks in this
    stem are recording floor, and normalizing them up is what sounded like
    static in the earlier cuts.
    """
    stem = STUDY / 'sources/indigo-shadows/stems/percussion.wav'
    y = load_mono(stem)
    onsets = librosa.onset.onset_detect(y=y, sr=SR, units='time', backtrack=False, delta=.05)
    shots, report = [], []
    for i, t in enumerate(onsets):
        a = round(t * SR)
        nxt = round(onsets[i + 1] * SR) if i + 1 < len(onsets) else len(y)
        seg = y[a:min(nxt, a + round(.35 * SR))]
        peak = float(np.abs(seg[:round(.06 * SR)]).max())
        if peak < .3 or len(seg) < round(.08 * SR):
            continue
        seg = seg * fades(len(seg), .002, .03)
        shots.append(seg / peak)
        report.append(dict(time=round(float(t), 3), peak=round(peak, 3)))
    shots.sort(key=lambda s: -np.abs(s).max())
    return shots[:8], dict(stem=str(stem.relative_to(STUDY)), onsets=len(onsets), thumpsKept=len(shots[:8]), hits=report[:8])


# ------------------------------------------------------------- the kit
KIT_DIR = Path(__file__).resolve().parents[2] / 'imab/samples/real'


def load_kit():
    meta = json.loads((KIT_DIR / 'kit-real.json').read_text())
    kit, credits = {}, {}
    for name in ['clap', 'snare', 'hat-closed', 'hat-open', 'shaker']:
        assert 'zero/1.0' in meta[name]['license'], name
        x = load_mono(KIT_DIR / f'{name}.wav')
        kit[name] = x / max(1e-6, np.abs(x).max())
        credits[name] = dict(name=meta[name]['name'], by=meta[name]['by'], url=meta[name]['url'], license='CC0')
    return kit, credits


# -------------------------------------------- the AC OS grand (Salamander)
PIANO_DIR = Path(__file__).resolve().parents[3] / 'fedac/native/samples/piano'
PIANO_RELEASE = .42


class Piano:
    """The same Salamander anchors AC OS plays, voiced the way pianotrax does."""
    def __init__(self, rng):
        self.anchors = {}
        for path in sorted(PIANO_DIR.glob('*.raw')):
            self.anchors[int(path.stem)] = np.fromfile(path, dtype=np.float32).astype(np.float64)
        assert 60 in self.anchors, 'no piano bank'
        self.rng = rng

    def note(self, midi, dur, vel=.7, pan=0.0):
        anchor = min(self.anchors, key=lambda a: abs(a - midi))
        src = self.anchors[anchor]
        step = 2 ** ((midi - anchor) / 12)
        n = min(int((len(src) - 1) / step), round((dur + PIANO_RELEASE) * SR))
        pos = np.arange(n) * step
        y = np.interp(pos, np.arange(len(src)), src)
        env = np.ones(n)
        att = max(1, round(.0015 * SR))
        env[:att] = np.linspace(0, 1, att)
        hold = round(dur * SR)
        if hold < n:
            rel = np.linspace(1, 0, n - hold)
            env[hold:] *= rel
        amp = (.70 + .85 * vel)
        kbpan = np.clip((midi - 60) / 40, -1, 1)
        P = pan * .5 + kbpan * .35
        y = y * env * amp
        return np.stack([y * (1 - P * .5), y * (1 + P * .5)], axis=1)

    def key(self, mix, midi, at, dur, gain, vel=.7, pan=0.0):
        """A humanized press: a hair late, jittered, velocity wobbled (pianotrax's hazy hand).

        On top of the per-note jitter the hand drifts slowly (a few ms ahead or
        behind over a couple of bars) and leans into phrases, so no two bars
        land the same way.
        """
        drift = .007 * math.sin(at * 2 * math.pi / (BAR * 2.7)) + .004 * math.sin(at * 2 * math.pi / (BAR * 1.3) + 1)
        t = max(0.0, at + .016 + drift + self.rng.normal(0, .009))
        lean = 1 + .12 * math.sin(at * 2 * math.pi / (BAR * 4) - 1.2)
        g = gain * lean * (1 + self.rng.uniform(-.22, .22))
        v = float(np.clip(vel + self.rng.normal(0, .07), .2, 1))
        d = dur * (1 + self.rng.uniform(-.2, .15))
        mix.place_stereo('piano', self.note(midi, d, v, pan), t, g)

    def roll(self, mix, midis, at, dur, gain, vel=.65):
        spread = self.rng.uniform(.012, .034)
        order = sorted(midis) if self.rng.random() < .75 else sorted(midis, reverse=True)
        for k, m in enumerate(order):
            self.key(mix, m, at + k * spread, dur, gain, vel)

    def thin(self, notes, keep=(2, 3)):
        """A lighter hand: two or three notes of the voicing, a different pick each time."""
        count = min(len(notes), int(self.rng.integers(keep[0], keep[1] + 1)))
        picks = sorted(self.rng.choice(len(notes), count, replace=False))
        return [notes[i] for i in picks]


# ------------------------------------------------------------- synthesis
def kick(vel=1.0):
    n = round(.34 * SR)
    t = np.arange(n) / SR
    hz = 43 + 105 * np.exp(-t * 26)
    phase = 2 * np.pi * np.cumsum(hz) / SR
    body = np.sin(phase) * np.exp(-t * 8.5)
    click = np.sin(2 * np.pi * np.cumsum(320 * np.exp(-t * 60) + 90) / SR) * np.exp(-t * 90) * .4
    return (np.tanh(1.5 * (body + click)) / 1.25) * vel * fades(n, .0005, .03)


def sub_stinger(midi_from=33, midi_to=45, beats=3.8, drive=3.4):
    """femrag++'s drop stinger: a driven sub that slides up under the downbeat."""
    n = round(beats * BEAT * SR)
    t = np.arange(n) / SR
    m = midi_from + (midi_to - midi_from) * np.minimum(1, t / (beats * BEAT * .8))
    phase = 2 * np.pi * np.cumsum(midi_hz(m)) / SR
    wave = np.tanh(drive * np.sin(phase)) / np.tanh(drive)
    env = np.minimum(1, t / .01) * np.exp(-t * .9) * fades(n, .01, .3)
    return wave * env


def bass_tone(midi, duration, drive=1.4):
    n = round((duration + .08) * SR)
    t = np.arange(n) / SR
    phase = 2 * np.pi * midi_hz(midi) * t
    wave = np.sin(phase) + .35 * np.sin(2 * phase) + .1 * np.sin(3 * phase)
    wave = np.tanh(wave * drive) / drive
    env = np.minimum(1, t / .006) * np.where(t < duration, 1, np.exp(-(t - duration) / .025))
    return wave * env * (.6 + .4 * np.exp(-t * 6))


from scipy.signal import iirpeak, lfilter

MALLET = {  # ported from pop/marimba/synths/marimba.mjs
    'rosewood': dict(partials=[1, 4, 9.2], amps=[1, .32, .1], decays=[1.6, .32, .09], mallet=.0025, resQ=18, resGain=.65),
    'bass': dict(partials=[1, 4], amps=[1, .18], decays=[2.4, .55], mallet=.0038, resQ=25, resGain=1.0),
    'staccato': dict(partials=[1, 4, 10, 17], amps=[1, .55, .28, .12], decays=[.55, .18, .06, .02], mallet=.001, resQ=12, resGain=0),
    'roll': dict(partials=[1, 4, 9.5], amps=[1, .3, .1], decays=[.65, .2, .07], mallet=.0022, resQ=16, resGain=.5),
}


def marimba(midi, preset='rosewood', decay_mul=1.0):
    """Modal bar: damped sines at the bar's inharmonic ratios, a half-cosine mallet, a tube at f0."""
    P = MALLET[preset]
    f0 = midi_hz(midi)
    n = round((max(P['decays']) * decay_mul * 1.2 + .05) * SR)
    t = np.arange(n) / SR
    y = np.zeros(n)
    total = sum(P['amps'])
    for ratio, amp, t60 in zip(P['partials'], P['amps'], P['decays']):
        f = f0 * ratio
        if f > SR * .45:
            continue
        # a longer mallet contact low-passes the strike: higher modes get less
        contact = P['mallet']
        lp = max(0.0, np.cos(np.pi * min(.5, f * contact / 2)))
        y += (amp / total) * lp * np.sin(2 * np.pi * f * t) * np.exp(-np.log(1000) / (t60 * decay_mul) * t)
    y *= np.minimum(1, t / P['mallet'])
    if P['resGain'] and f0 < SR * .45:
        b, a = iirpeak(f0, P['resQ'], fs=SR)
        y = y + P['resGain'] * lfilter(b, a, y)
    return y / max(1e-6, np.abs(y).max()) * fades(n, .0003, .02)


def block(midi=89):
    return marimba(midi, 'staccato')


def bubble(seed=0, hz_from=900, hz_to=260):
    """A pop: a sine that falls an octave and a half in forty milliseconds."""
    n = round(.09 * SR)
    t = np.arange(n) / SR
    hz = hz_to + (hz_from - hz_to) * np.exp(-t * 70)
    y = np.sin(2 * np.pi * np.cumsum(hz) / SR) * np.exp(-t * 55)
    return y * fades(n, .0005, .02)


def brush(seconds=.22, attack=.06, low=1800, high=7000, seed=21):
    """A brush swish: a band of noise that swells in and falls away."""
    n = round(seconds * SR)
    t = np.arange(n) / SR
    noise = np.random.default_rng(seed).normal(0, 1, n)
    sos = butter(2, [low, high], 'band', fs=SR, output='sos')
    env = np.minimum(1, t / attack) * np.exp(-np.maximum(0, t - attack) * 18)
    y = sosfiltfilt(sos, noise) * env
    return y / max(1e-6, np.abs(y).max()) * fades(n, .005, .03)


def soft_kick(vel=1.0):
    """A round thump with no click: 75 → 46 Hz, two hundred milliseconds."""
    n = round(.24 * SR)
    t = np.arange(n) / SR
    hz = 46 + 29 * np.exp(-t * 30)
    y = np.sin(2 * np.pi * np.cumsum(hz) / SR) * np.exp(-t * 11)
    return np.tanh(1.2 * y) / 1.1 * vel * fades(n, .002, .04)


def gliss(mix, piano, at, top=96, bottom=72, gain=.35):
    """A marimba glissando up into the downbeat."""
    notes = [m for m in range(bottom, top + 1) if m % 12 in (0, 2, 4, 7, 9)]
    for k, m in enumerate(notes):
        mix.place('mallet', marimba(m, 'roll'), at - (len(notes) - k) * .028, gain * (.5 + .5 * k / len(notes)), -.5 + k / len(notes))


def riser(bars_long):
    """femrag++'s riser: a noise band sweeping 500→7500 Hz under two detuned sines climbing two octaves."""
    n = round(bars_long * BAR * SR)
    t = np.arange(n) / SR
    frac = t / t[-1]
    noise = np.random.default_rng(11).normal(0, 1, n)
    # sweep by crossfading four bands
    bands = [(400, 1200), (1000, 3000), (2500, 6000), (5000, 11000)]
    swept = np.zeros(n)
    for k, (lo, hi) in enumerate(bands):
        sos = butter(2, [lo, hi], 'band', fs=SR, output='sos')
        w = np.clip(1 - np.abs(frac * (len(bands) - 1) - k), 0, 1)
        swept += sosfiltfilt(sos, noise) * w
    env = frac ** 2.4
    phase = 2 * np.pi * np.cumsum(midi_hz(57 + 24 * frac)) / SR
    tones = (np.sin(phase) + np.sin(phase * 1.011 + 2)) * .5 * frac ** 2.6
    return (swept * .7 + tones * .6) * env * fades(n, .05, .01)


def crash():
    n = round(1.8 * SR)
    t = np.arange(n) / SR
    noise = np.random.default_rng(5).normal(0, 1, n)
    sos = butter(2, [2500, 14000], 'band', fs=SR, output='sos')
    return sosfiltfilt(sos, noise) * np.exp(-t * 2.6) * fades(n, .001, .4)


# ------------------------------------------------------------- harmony
CHORDS = {
    'Cmaj7': dict(bass=36, tones=[60, 64, 67, 71], piano=[48, 55, 60, 64, 67, 71]),
    'Am7': dict(bass=33, tones=[57, 60, 64, 67], piano=[45, 52, 57, 60, 64, 67]),
    'Fmaj7': dict(bass=41, tones=[57, 60, 64, 65], piano=[41, 48, 53, 57, 60, 64]),
    'G7': dict(bass=43, tones=[59, 62, 65, 67], piano=[43, 50, 55, 59, 62, 65]),
    'Bbmaj7': dict(bass=46, tones=[58, 62, 65, 69], piano=[46, 53, 58, 62, 65, 69]),
}
HUM_E4, HUM_E4b, HUM_B3, HUM_AS3 = 16, 17, 18, 19
HYMN = {
    'Cmaj7': [(HUM_B3, +1), (HUM_E4, 0), (HUM_E4b, +3), (HUM_B3, 0)],
    'Am7': [(HUM_B3, -2), (HUM_B3, +1), (HUM_E4, 0), (HUM_E4b, +3)],
    'Fmaj7': [(HUM_AS3, -5), (HUM_B3, -2), (HUM_B3, +1), (HUM_E4, 0)],
    'G7': [(HUM_B3, 0), (HUM_E4, -2), (HUM_E4b, +1), (HUM_E4, +3)],
    'Bbmaj7': [(HUM_AS3, 0), (HUM_E4, -2), (HUM_E4b, +1), (HUM_B3, -2)],
}
MAIN_LOOP = ['Cmaj7', 'Am7', 'Fmaj7', 'G7']
HYMN_LOOP = ['Cmaj7', 'Am7', 'Bbmaj7', 'G7']

# ------------------------------------------------------------- melodies
# New lines written for the record (the takes' notes are the instrument, not
# the tune). Each entry: (beat offset within the phrase, length in beats, midi).
# Phrases are four bars long and sit on Cmaj7 | Am7 | Fmaj7 | G7.
C4, D4, E4, F4, G4, A4, B4, C5, D5, E5 = 60, 62, 64, 65, 67, 69, 71, 72, 74, 76
VERSE_A = [  # a question that leans on the sixth and falls home
    (0, .5, E4), (.5, .5, G4), (1, 1.5, A4), (2.5, .5, G4), (3, 1, E4),
    (4, 2, C4), (6.5, .5, D4), (7, 1, E4),
    (8, .5, A4), (8.5, .5, A4), (9, 1, G4), (10, .5, F4), (10.5, .5, E4), (11, 1, F4),
    (12, 1.5, D4), (13.5, .5, E4), (14, 2, D4),
]
VERSE_B = [  # the answer, up to the fifth and down through the fourth
    (0, .5, G4), (.5, .5, A4), (1, 1, G4), (2, .5, E4), (2.5, 1.5, G4),
    (4, 1, E4), (5, 1, C4), (6, 2, E4),
    (8, .5, F4), (8.5, .5, A4), (9, 1, C5), (10, .5, A4), (10.5, 1.5, F4),
    (12, 1, D4), (13, .5, E4), (13.5, .5, F4), (14, 2, G4),
]
CHORUS_A = [  # the hook: up the arpeggio, hold the ninth, settle
    (0, .5, G4), (.5, .5, A4), (1, 1.5, C5), (2.5, .5, B4), (3, 1, A4),
    (4, 1, G4), (5, 1, E4), (6, 2, G4),
    (8, .5, A4), (8.5, .5, C5), (9, 1.5, D5), (10.5, .5, C5), (11, 1, A4),
    (12, 1, G4), (13, 1, F4), (14, 2, D4),
]
CHORUS_B = [  # the second half climbs to the high E and comes home
    (0, .5, G4), (.5, .5, A4), (1, 1.5, C5), (2.5, .5, D5), (3, 1, E5),
    (4, 1.5, D5), (5.5, .5, C5), (6, 2, A4),
    (8, .5, F4), (8.5, .5, G4), (9, 1.5, A4), (10.5, .5, G4), (11, 1, F4),
    (12, 1, D4), (13, 1, E4), (14, 2, C5),
]
LONG_LINE = [  # Saffron's whole-note counter line
    (0, 4, E4), (4, 4, E4), (8, 4, F4), (12, 4, D4),
]
LONG_LINE_B = [(0, 4, G4), (4, 4, E4), (8, 4, F4), (12, 3.5, D4)]
PULSE = [  # Indigo's short notes as a rhythmic chord-tone pulse (off-beats)
    (1.5, .5, E4), (3.5, .5, E4), (5.5, .5, E4), (7.5, .5, C4), (9.5, .5, C4), (11.5, .5, A4 - 12 + 12), (13.5, .5, D4), (15.5, .5, D4),
]


class Singer:
    """Sings a written midi line from one take's note bank, picking the nearest sample."""
    def __init__(self, bank, max_shift=5):
        self.bank = bank
        self.max_shift = max_shift
        self.turn = 0
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

    def sing(self, mix, bus, line, at, gain, pan=0.0, part='lead', shift_extra=0, harmony=False, stretch_mul=1.0):
        for beat, beats, midi in line:
            if shift_extra < 0 and midi + shift_extra < 55:
                continue
            dur = beats * BEAT
            cost, idx, shift, natural = self.pick(midi, dur)
            stretch = min(3.5, max(1.0, dur * 1.15 / natural)) * stretch_mul
            length = dur * 1.12
            chop = self.bank.chop(idx, length, part=part, shift=shift + shift_extra, skip=.01,
                                  stretch=round(stretch, 2), attack=.015, release=min(.25, dur * .35))
            mix.place(bus, chop, at + beat * BEAT - .02, gain, pan)


# ------------------------------------------------------------- the form
SECTIONS = [
    ('intro', 4, dict(piano='solo', long_line=.7, hymn=.5, verse=.6)),
    ('verse', 8, dict(piano='comp', kick=.7, brush=.7, clave=.8, bubbles=.6, bass=1, verse=1, long_line=.5, fills=1)),
    ('build', 4, dict(piano='comp', kick=.8, clave=1, blocks=.8, bass=1, verse=.8, roll=1, riser=1, breath=1)),
    ('chorus', 16, dict(piano='block', kick=1, brush=1, clave=1, blocks=.7, bubbles=.8, bass=1, chorus=1, arp=.8, pulse=.5, hymn=.5, stinger=1, gliss=1, fills=1, sparkle=1)),
    ('hymn', 8, dict(piano='arp', hymn=1, arch=1, long_line=.6, bass=.4, fills=1, kick=.6, hook=.7)),
    ('verse2', 8, dict(piano='comp', kick=.8, brush=.8, clave=.8, blocks=.5, bubbles=.6, bass=1, verse=1, long_line=.5, fills=1)),
    ('build2', 4, dict(piano='comp', kick=.9, clave=1, blocks=.9, bass=1, verse=.8, roll=1, riser=1, breath=1)),
    ('chorus2', 16, dict(piano='block', kick=1, brush=1, clave=1, blocks=.7, bubbles=.8, bass=1, chorus=1, arp=.8, pulse=.6, hymn=.6, stinger=1, gliss=1, high=1, fills=1, sparkle=1)),
    ('outro', 8, dict(piano='solo', hymn=.6, long_line=.7, hook=.8, kick=.8, fade=1)),
]
BUBBLES_VERSE = [2, 6, 10, 14]                # the off-eighths, sparsely
BUBBLES_CHORUS = [1, 3, 6, 9, 11, 14]         # the "e"s and "a"s, a little pattern
PENTATONIC = [0, 2, 4, 7, 9]
CLAVE = [0, 6, 12, 20, 24]            # son clave 3-2 in sixteenths over two bars
HEMIOLA_STEP = 3                      # shaker every three sixteenths (3:4 against the kick)


def chart_text():
    rows = [('kick', [0, 8, 16, 24]), ('brush', [4, 12, 20, 28]),
            ('marimba', CLAVE), ('block', list(range(0, 48, HEMIOLA_STEP))), ('bubble', [s + 16 * k for k in range(2) for s in BUBBLES_VERSE])]
    lines = ['slot   ' + ' '.join(f'{b + 1}e&a' for b in range(8))]
    for name, slots in rows:
        cells = ''.join('x' if s in slots else '.' for s in range(32))
        lines.append(f'{name:6s} ' + ' '.join(cells[k:k + 4] for k in range(0, 32, 4))
                     + ('   (3-bar cycle)' if name == 'block' else ''))
    return '\n'.join(lines)


def chart_svg():
    rows = [('soft kick', [0, 8, 16, 24], '#c0392b'), ('brush swish', [4, 12, 20, 28], '#e67e22'),
            ('marimba · clave (chord tones)', CLAVE, '#8e44ad'), ('woodblock · 3:4 hemiola', list(range(0, 48, 3)), '#2980b9'),
            ('bubble pops · off-eighths', [s + 16 * k for k in range(3) for s in BUBBLES_VERSE], '#16a085')]
    cw, ch, left, top = 22, 30, 190, 40
    w, h = left + 48 * cw + 20, top + len(rows) * ch + 30
    out = [f'<svg xmlns="http://www.w3.org/2000/svg" width="{w}" height="{h}" font-family="Helvetica, Arial" font-size="12">',
           f'<rect width="{w}" height="{h}" fill="#fdfaf3"/>',
           f'<text x="{left}" y="20" font-size="14" font-weight="bold">Afternoon Study · percussion chart · 118 BPM · three bars of sixteenths</text>']
    for s in range(48):
        x = left + s * cw
        stroke = '#333' if s % 16 == 0 else '#999' if s % 4 == 0 else '#e5e0d5'
        out.append(f'<line x1="{x}" y1="{top}" x2="{x}" y2="{h - 20}" stroke="{stroke}" stroke-width="{2 if s % 16 == 0 else 1}"/>')
        if s % 16 == 0:
            out.append(f'<text x="{x + 3}" y="{top - 6}" fill="#333">bar {s // 16 + 1}</text>')
    for r, (name, slots, color) in enumerate(rows):
        y = top + r * ch
        out.append(f'<text x="8" y="{y + 19}">{name}</text>')
        for s in range(48):
            key = s if name.startswith('woodblock') or name.startswith('bubble') else s % 32
            if key in slots:
                out.append(f'<rect x="{left + s * cw + 3}" y="{y + 6}" width="{cw - 6}" height="{ch - 12}" rx="4" fill="{color}"/>')
    out.append('</svg>')
    return '\n'.join(out)


class Mixer:
    def __init__(self, total):
        self.total = round(total * SR)
        self.buses = {}

    def bus(self, name):
        if name not in self.buses:
            self.buses[name] = np.zeros((self.total, 2))
        return self.buses[name]

    def place(self, bus, signal, at, gain=1.0, pan=0.0):
        angle = (np.clip(pan, -1, 1) + 1) * np.pi / 4
        self.place_stereo(bus, np.stack([signal * np.cos(angle), signal * np.sin(angle)], axis=1), at, gain)

    def place_stereo(self, bus, signal, at, gain=1.0):
        data = self.bus(bus)
        start = round(at * SR)
        if start < 0:
            signal, start = signal[-start:], 0
        count = min(len(signal), self.total - start)
        if count <= 0:
            return
        data[start:start + count] += signal[:count] * gain


def arrange(banks, thumps, piano):
    lemon, indigo, saffron = (banks[n] for n in NAMES)
    singers = dict(lemon=Singer(lemon), saffron=Singer(saffron, max_shift=4), indigo=Singer(indigo, max_shift=4))
    total_bars = sum(s[1] for s in SECTIONS)
    mix = Mixer(total_bars * BAR + 4)
    events, breaths, bar0 = [], [], 0
    for si, (name, bars, layers) in enumerate(SECTIONS):
        for b in range(bars):
            bar = bar0 + b
            t0 = bar * BAR
            loop = HYMN_LOOP if name == 'hymn' else MAIN_LOOP
            chord_name = loop[bar % 4]
            chord = CHORDS[chord_name]
            events.append(dict(bar=bar, section=name, chord=chord_name))
            fade = 1 - b / bars if layers.get('fade') else 1.0
            phrase_bar = b % 4
            # ---- hymn: Indigo's hums as chord tones, overlapping at the seams
            if layers.get('hymn'):
                for k, (idx, shift) in enumerate(HYMN[chord_name][1:]):
                    stretch = BAR / (len(indigo.raw(idx)) / 24000) * 1.08
                    tone = indigo.sample(idx, 'lead', shift, stretch=stretch)
                    mix.place('hymn', tone * fades(len(tone), .09, .25), t0 - .06, .22 * layers['hymn'] * fade, -.85 + 1.7 * k / 3)
            # ---- piano
            mode = layers.get('piano')
            root, tones, voicing = chord['bass'], chord['tones'], chord['piano']
            high = [m + 12 for m in voicing[2:]]          # the right hand lives an octave up
            # v6: a lighter, looser hand. Each bar picks two or three notes of the
            # voicing and one of a few rhythms, so the piano breathes around the
            # voice instead of filling every beat.
            if mode == 'solo':
                if phrase_bar % 2 == 0:
                    piano.roll(mix, [root + 12] + piano.thin(high), t0, BAR * 1.9, .5 * fade, vel=.55)
                if phrase_bar == 3 and piano.rng.random() < .6:
                    run = [96, 93, 91, 88, 84, 81, 79, 76][:int(piano.rng.integers(4, 9))]
                    for k, m in enumerate(run):
                        piano.key(mix, m, t0 + 2 * BEAT + k * STEP, STEP * 1.5, .26 * fade, vel=.5 - .03 * k, pan=.4 - .1 * k)
            elif mode == 'comp':
                piano.key(mix, root + 12, t0, BEAT * 1.6, .45, vel=.55)
                hand = piano.thin(high)
                rhythm = [(1.5, .5), (3, .8)], [(1, .9), (2.5, .5)], [(2, 1.4)], [(.5, .4), (2.5, .9)]
                for beat, beats in rhythm[int(piano.rng.integers(len(rhythm)))]:
                    for m in hand:
                        piano.key(mix, m, t0 + beat * BEAT, BEAT * beats, .32, vel=.5, pan=.2)
            elif mode == 'block':
                hand = piano.thin(high, (2, 3))
                piano.key(mix, root + 12, t0, BEAT * 1.6, .45, vel=.7)
                for m in hand:
                    piano.key(mix, m, t0, BEAT * 1.5, .34, vel=.72, pan=.15)
                if piano.rng.random() < .5:
                    for m in piano.thin(high, (1, 2)):
                        piano.key(mix, m, t0 + 2.5 * BEAT, BEAT * .6, .26, vel=.6, pan=.25)
            elif mode == 'arp':
                piano.key(mix, root + 12, t0, BAR * .95, .45, vel=.5)
                pattern = [high[0], high[2], high[1], high[3], high[2] + 12, high[0] + 12, high[1] + 12, high[3]]
                for k, m in enumerate(pattern):
                    if k and piano.rng.random() < .3:
                        continue
                    piano.key(mix, m, t0 + k * BEAT / 2, BEAT * .7, .28, vel=.42 + .12 * (k % 2 == 0), pan=.25)
            # ---- right-hand fun, now occasional: a sparkle, a run or a trill, never all at once
            if layers.get('sparkle') and phrase_bar == 2 and piano.rng.random() < .5:
                for step in (1, 5, 9, 13):
                    piano.key(mix, tones[(step // 4) % 4] + 24, t0 + step * STEP, STEP * 1.2, .18, vel=.42, pan=.5)
            if layers.get('fills') and phrase_bar == 3 and (b // 4) % 2 == 1:
                run = [72 + o + p for o in (0, 12, 24) for p in PENTATONIC][:int(piano.rng.integers(6, 11))]
                start = t0 + BAR - len(run) * STEP / 2 - .5 * BEAT
                for k, m in enumerate(run):
                    piano.key(mix, m, start + k * STEP / 2, STEP * 1.1, .24 + .02 * k, vel=.42 + .04 * k, pan=-.4 + .08 * k)
            if layers.get('fills') and phrase_bar == 1 and name.startswith('chorus') and piano.rng.random() < .4:
                top = high[-1] + 12
                for k in range(6):   # a trill on the high tone
                    piano.key(mix, top + (2 if k % 2 else 0), t0 + 2 * BEAT + k * STEP / 2, STEP * .6, .2, vel=.48, pan=.45)
            # ---- the mallet kit
            chorus_like = name.startswith('chorus')
            if layers.get('kick'):
                # v6: four to the floor everywhere the kick plays, with a punch layer
                # (the clicked kick) under it, and a short brush tick on every off-beat
                for beat in range(4):
                    k = layers['kick'] * fade
                    mix.place('kick', soft_kick(1 if beat in (0, 2) else .9), t0 + beat * BEAT, .9 * k)
                    mix.place('kick', kick(.8 if chorus_like else .55), t0 + beat * BEAT, .45 * k)
                    mix.place('brush', brush(.07, .004, 5000, 12000, seed=bar * 8 + beat), t0 + (beat + .5) * BEAT, (.4 if chorus_like else .28) * k, .25 - .5 * (beat % 2))
            if layers.get('brush'):
                for beat in (1, 3):
                    mix.place('brush', brush(seed=bar * 4 + beat), t0 + beat * BEAT - .05, .8 * layers['brush'], .1)
                if chorus_like:
                    mix.place('brush', brush(.14, .03, 3000, 9000, seed=bar), t0 + 3.5 * BEAT - .02, .3, -.3)
            if layers.get('clave'):
                for slot in range(16):
                    if ((bar - bar0) % 2 * 16 + slot) in CLAVE:
                        midi = tones[(slot // 4) % 4] - 12
                        mix.place('mallet', marimba(midi, 'rosewood'), t0 + slot * STEP, .55 * layers['clave'], -.2)
                        mix.place('perc', thumps[(bar + slot) % len(thumps)], t0 + slot * STEP, .25 * layers['clave'], .2)
            if layers.get('blocks'):
                for slot in range(16):
                    s3 = ((bar - bar0) % 3) * 16 + slot
                    if s3 % HEMIOLA_STEP == 0:
                        mix.place('mallet', block(89 if (s3 // 3) % 2 else 84), t0 + slot * STEP, .28 * layers['blocks'], .5 if (s3 // 3) % 2 else -.5)
            if layers.get('bubbles'):
                for slot in (BUBBLES_CHORUS if chorus_like else BUBBLES_VERSE):
                    mix.place('mallet', bubble(hz_from=700 + 120 * (slot % 5), hz_to=220 + 40 * (slot % 3)), t0 + slot * STEP, .4 * layers['bubbles'], .6 - 1.2 * (slot % 2))
            # ---- the build: roll 4 → 8 → 16 → 32, riser, breath (femrag++)
            if layers.get('roll'):
                division = [4, 8, 16, 32][b]
                heat = b / 4
                for s in range(division):
                    at = t0 + s * BAR / division
                    if b == 3 and at >= t0 + 3.5 * BEAT:
                        break
                    ghost = division <= 8 and s % 2 == 1
                    g = (.25 + heat * .5 + s / division * .2) * (.5 if ghost else 1)
                    mix.place('mallet', block(84 + [0, 2, 4, 7][s % 4]), at, g * .7, .3 * (s % 2) - .15)
                    if s % 2 == 0:
                        mix.place('mallet', bubble(hz_from=600 + 400 * heat), at, g * .5, -.3)
                if b == 3:
                    mix.place('brush', brush(.3, .04, 2000, 9000, seed=bar), t0 + 3.5 * BEAT - .04, .9, 0)
                    mix.place('mallet', marimba(tones[0], 'rosewood'), t0 + 3.5 * BEAT, .6, 0)
            if layers.get('riser') and b == 0:
                mix.place('riser', riser(4 - .125), t0, .45, 0)
            if layers.get('breath') and b == bars - 1:
                breaths.append((t0 + 3.5 * BEAT + .1, t0 + BAR))
            if layers.get('stinger') and b == 0:
                mix.place('bass', sub_stinger(beats=2.2), t0, .4, 0)
                mix.place('brush', brush(1.2, .01, 2500, 12000, seed=7), t0, .35, 0)
            if layers.get('gliss') and b % 8 == 0:
                gliss(mix, piano, t0)
                # a reversed Saffron C4 swells into the downbeat: the strike that arrives where it ends
                swell = saffron.sample(0)[:round(1.6 * SR)][::-1].copy()
                swell *= np.linspace(0, 1, len(swell)) ** 2
                mix.place('shimmer', swell, t0 - 1.6, .5, 0)
            # ---- bass
            if layers.get('bass'):
                if name in ('hymn', 'intro', 'outro'):
                    mix.place('bass', bass_tone(root, BEAT * 1.5, drive=1.1), t0, .4 * layers['bass'])
                else:
                    chorus = name.startswith('chorus')
                    drive = 1.8 if chorus else 1.3
                    if not (layers.get('stinger') and b == 0):
                        mix.place('bass', bass_tone(root, BEAT * 1.2, drive=drive), t0, .55)
                    # then it rests: one answer on the "and" of two, a pickup before the next bar
                    mix.place('bass', bass_tone(root + 12, BEAT * .35, drive=drive), t0 + 1.5 * BEAT, .38)
                    if chorus or b % 2 == 1:
                        mix.place('bass', bass_tone(root + (12 if b % 2 else 7), BEAT * .3, drive=drive), t0 + 3.5 * BEAT, .34)
            # ---- vocals: the written lines, sung from the banks
            if phrase_bar == 0:
                if layers.get('verse'):
                    line = VERSE_A if (b // 4) % 2 == 0 else VERSE_B
                    singers['lemon'].sing(mix, 'lead', line, t0, .62 * layers['verse'], 0)
                    singers['lemon'].sing(mix, 'lead', line, t0, .26 * layers['verse'], .5, part='harmony')
                    singers['lemon'].sing(mix, 'lead-oct', line, t0, .14 * layers['verse'], -.5, shift_extra=-12)
                if layers.get('hook'):
                    line = CHORUS_A if (b // 4) % 2 == 0 else CHORUS_B
                    singers['lemon'].sing(mix, 'lead', line, t0, .5 * layers['hook'] * fade, 0)
                    singers['lemon'].sing(mix, 'lead', line, t0, .24 * layers['hook'] * fade, -.5, part='harmony')
                if layers.get('chorus'):
                    line = CHORUS_A if (b // 4) % 2 == 0 else CHORUS_B
                    singers['lemon'].sing(mix, 'lead', line, t0, .66, 0)
                    singers['lemon'].sing(mix, 'lead', line, t0, .3, .55, part='harmony')
                    singers['lemon'].sing(mix, 'lead-oct', line, t0, .26, -.5, shift_extra=-12)
                    if layers.get('high') and (b // 4) % 2 == 1:
                        singers['lemon'].sing(mix, 'lead-oct', line, t0, .18, .5, shift_extra=12)
                    for beat, beats, midi in line:      # the grand shadows the hook's long notes only
                        if beats >= 1.5 and piano.rng.random() < .6:
                            piano.key(mix, midi + 12, t0 + beat * BEAT, beats * BEAT * .8, .28, vel=.6, pan=.2)
                if layers.get('long_line'):
                    line = LONG_LINE if (b // 4) % 2 == 0 else LONG_LINE_B
                    singers['saffron'].sing(mix, 'long', line, t0, .4 * layers['long_line'] * fade, -.25)
                    singers['saffron'].sing(mix, 'long', line, t0, .18 * layers['long_line'] * fade, .45, part='harmony')
                if layers.get('pulse'):
                    singers['indigo'].sing(mix, 'pulse', PULSE, t0, .35 * layers['pulse'], .6, stretch_mul=.9)
                    singers['indigo'].sing(mix, 'pulse', [(bt + .25, d, m) for bt, d, m in PULSE], t0, .22 * layers['pulse'], -.6)
            # ---- Saffron's arpeggio: chord tones on sixteenths, octaves trading every beat
            if layers.get('arp'):
                for step in range(16):
                    midi = tones[step % 4] + [12, 0, -12, 0][(step // 4 + b) % 4]
                    dur = STEP
                    cost, idx, shift, natural = singers['saffron'].pick(midi, dur)
                    chop = saffron.chop(idx, STEP * 1.25, shift=shift, skip=.12 if idx in (0, 7) else .04, attack=.012, release=.06)
                    gain = .42 if step % 4 == 0 else .3
                    mix.place('arp', chop, t0 + step * STEP - .01, gain * layers['arp'], -.55 + 1.1 * step / 15)
            # ---- Saffron's actual slow arch, un-chopped, over the hymn
            if layers.get('arch') and b == 0:
                for idx in [0, 1, 2, 3, 4, 5, 6]:
                    note = saffron.manifest['notes'][idx]
                    x = saffron.sample(idx)
                    mix.place('long', x * fades(len(x), .02, .25), t0 + note['start'], .55, 0)
                    h = saffron.sample(idx, 'harmony')
                    mix.place('long', h * fades(len(h), .05, .25), t0 + note['start'], .2, .5)
        bar0 += bars
    return mix, events, total_bars, breaths


def sidechain(total_samples, bars_with_kick, depth=.62):
    t = np.arange(total_samples) / SR
    env = np.ones(total_samples)
    within = t % BEAT
    pump = 1 - depth * np.exp(-within / .1)
    for start, end in bars_with_kick:
        a, b = round(start * BAR * SR), round(end * BAR * SR)
        env[a:b] = pump[a:b]
    return env


def vocal_duck(vocal, floor=.7, threshold=.05):
    energy = np.sqrt(np.convolve(np.mean(vocal ** 2, axis=1), np.ones(round(.02 * SR)) / round(.02 * SR), 'same'))
    release = np.exp(-1 / (.12 * SR))
    held = np.empty_like(energy)
    level = 0.0
    for i, e in enumerate(energy):
        level = e if e > level else level * release
        held[i] = level
    return np.clip((threshold / np.maximum(held, 1e-4)) ** .5, floor, 1)


VOCAL_BUSES = ['lead', 'lead-oct', 'long']
DUCKED = dict(piano=.45, arp=.8, hymn=.8, pulse=.6, perc=.4, mallet=.3)


def main():
    global STUDY
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument('--study', type=Path, default=STUDY)
    ap.add_argument('--out', type=Path, default=Path(__file__).resolve().parent.parent / 'out')
    ap.add_argument('--lufs', type=float, default=-12.0, help='the mix level')
    ap.add_argument('--master-lufs', type=float, default=-9.5, help='the master level')
    args = ap.parse_args()
    STUDY = args.study
    out = args.out
    (out / 'cache').mkdir(parents=True, exist_ok=True)
    (out / 'stems').mkdir(exist_ok=True)

    banks = {n: Bank(n, out / 'cache') for n in NAMES}
    thumps, thump_report = indigo_thumps(out / 'cache')
    piano = Piano(np.random.default_rng(515))
    (out / 'chart.txt').write_text(chart_text() + '\n')
    (out / 'chart.svg').write_text(chart_svg())
    print(chart_text(), flush=True)
    mix, events, total_bars, breaths = arrange(banks, thumps, piano)
    print('arranged', total_bars, 'bars', round(total_bars * BAR, 1), 's; breaths at', [round(a, 2) for a, _ in breaths], flush=True)

    kick_bars, bar0 = [], 0
    for name, bars, layers in SECTIONS:
        if layers.get('kick'):
            kick_bars.append((bar0, bar0 + bars))
        bar0 += bars
    pump = sidechain(mix.total, kick_bars)
    for bus, depth in [('hymn', 1.0), ('arp', .7), ('piano', .3), ('long', .4), ('mallet', .35), ('bass', .55)]:
        if bus in mix.buses:
            mix.buses[bus] *= (1 - depth + depth * pump)[:, None]
    # The breath: everything but the riser's last gasp goes silent for the last half-beat before each drop.
    for a, b in breaths:
        i, j = round(a * SR), round(b * SR)
        edge = round(.004 * SR)
        for bus, data in mix.buses.items():
            data[i + edge:j - edge] = 0
            data[i:i + edge] *= np.linspace(1, 0, edge)[:, None]
            data[j - edge:j] *= np.linspace(0, 1, edge)[:, None]

    carve = 'equalizer=f=3600:t=q:w=1.2:g=-2.5'
    vocal_eq = ('highpass=f=130,equalizer=f=280:t=q:w=1.1:g=-2.5,equalizer=f=4000:t=q:w=1:g=2,highshelf=f=9000:g=1.5,'
                'deesser=i=0.2:m=0.4:f=0.55,acompressor=threshold=0.12:ratio=3:attack=6:release=100:makeup=1.3')
    treat = {
        'kick': 'highpass=f=34,equalizer=f=70:t=q:w=1:g=1.5,equalizer=f=2500:t=q:w=1:g=1',
        'brush': 'highpass=f=900,aecho=0.8:0.4:70|140:0.18|0.09',
        'mallet': f'highpass=f=90,{carve},aecho=0.85:0.5:110|230:0.2|0.1',
        'perc': f'highpass=f=80,equalizer=f=250:t=q:w=1.2:g=-2,{carve},acompressor=threshold=0.25:ratio=2.5:attack=4:release=90',
        'riser': 'highpass=f=300',
        'bass': 'highpass=f=38,lowpass=f=1400,equalizer=f=200:t=q:w=1:g=1.5,acompressor=threshold=0.25:ratio=2.5:attack=6:release=80',
        'piano': f'highpass=f=45,{carve},aecho=0.85:0.5:80|165:0.18|0.09',
        'hymn': f'highpass=f=150,lowpass=f=8000,equalizer=f=320:t=q:w=1.1:g=-2,{carve},aecho=0.8:0.6:120|260:0.16|0.08',
        'arp': f'highpass=f=160,{carve},deesser=i=0.2:m=0.4:f=0.55,aecho=0.8:0.5:190|380:0.2|0.1',
        'shimmer': 'highpass=f=200,lowpass=f=11000,aecho=0.8:0.6:250|500:0.25|0.12',
        'pulse': f'highpass=f=200,{carve},aecho=0.8:0.5:150|300:0.2|0.1',
        'lead': vocal_eq + ',aecho=0.8:0.35:95:0.22',
        'lead-oct': 'highpass=f=140,equalizer=f=280:t=q:w=1.1:g=-2,deesser=i=0.2:m=0.4:f=0.55,acompressor=threshold=0.12:ratio=3:attack=6:release=100:makeup=1.3,aecho=0.8:0.35:120:0.2',
        'long': 'highpass=f=100,lowpass=f=10500,equalizer=f=4000:t=q:w=1:g=2,deesser=i=0.18:m=0.4:f=0.55,acompressor=threshold=0.15:ratio=2.5:attack=8:release=120,aecho=0.8:0.6:220|440:0.22|0.11',
    }
    levels = {'kick': -18.5, 'brush': -27, 'mallet': -23, 'perc': -30, 'riser': -30, 'bass': -21.5, 'piano': -21,
              'hymn': -29, 'arp': -25, 'shimmer': -31, 'pulse': -28, 'lead': -15, 'lead-oct': -21.5, 'long': -21}
    stems, trims = {}, {}
    for bus, data in mix.buses.items():
        raw = out / 'stems' / f'{bus}-raw.wav'
        sf.write(raw, data, SR, subtype='FLOAT')
        treated = out / 'stems' / f'{bus}-treated.wav'
        ffmpeg('-i', raw, '-af', treat.get(bus, 'anull'), '-ar', SR, '-ac', 2, '-c:a', 'pcm_f32le', treated)
        final = out / 'stems' / f'{bus}.wav'
        trims[bus] = round(normalize(treated, final, levels[bus]), 2)
        raw.unlink(); treated.unlink()
        stems[bus] = sf.read(final, always_2d=True)[0][:mix.total]
    print('bus trims', trims, flush=True)
    n = mix.total
    vocal = np.zeros((n, 2))
    for bus in VOCAL_BUSES:
        if bus in stems:
            vocal[:len(stems[bus])] += stems[bus]
    duck = vocal_duck(vocal)
    summed = np.zeros((n, 2))
    for bus, data in stems.items():
        depth = DUCKED.get(bus, 0)
        summed[:len(data)] += data * (1 - depth + depth * duck[:len(data)])[:, None]
    sf.write(out / 'stems' / 'vocals.wav', vocal, SR, subtype='FLOAT')
    sf.write(out / 'stems' / 'instrumental.wav', summed - vocal, SR, subtype='FLOAT')
    tail = round(1.5 * SR)
    summed[-tail:] *= np.linspace(1, 0, tail)[:, None]
    assert np.isfinite(summed).all()
    premaster = out / 'premaster.wav'
    sf.write(premaster, summed, SR, subtype='FLOAT')
    glue = out / 'glue.wav'
    ffmpeg('-i', premaster, '-af', 'highpass=f=26,equalizer=f=180:t=q:w=1.2:g=-1.5,equalizer=f=4200:t=q:w=1.2:g=1,'
           'highshelf=f=8500:g=1.5,acompressor=threshold=0.35:ratio=1.3:attack=30:release=220',
           '-ar', SR, '-ac', 2, '-c:a', 'pcm_f32le', glue)
    measured = loudness(glue)
    gain = args.lufs - float(measured['input_i'])
    master = out / 'afternoon-study.wav'
    ffmpeg('-i', glue, '-af', f'volume={gain:.3f}dB,alimiter=limit=0.8:attack=6:release=80:level=false',
           '-ar', SR, '-ac', 2, '-c:a', 'pcm_s24le', master)
    checked = loudness(master)
    if float(checked['input_tp']) > -1.5:
        trim = -1.5 - float(checked['input_tp'])
        ffmpeg('-i', master, '-af', f'volume={trim:.3f}dB', '-c:a', 'pcm_s24le', out / 'trimmed.wav')
        (out / 'trimmed.wav').replace(master)
        checked = loudness(master)
    # The mix above stays as afternoon-study-mix.wav; the master is its own pass:
    # tidy the lows, a slow 2:1 bus compressor, a touch of air, then two
    # limiters (a gentle catch, then the ceiling) up to pop level, and a final
    # trim so the true peak sits at -1 dBTP.
    mix_wav = out / 'afternoon-study-mix.wav'
    master.replace(mix_wav)
    mixed = checked
    stage = out / 'master-stage.wav'
    ffmpeg('-i', mix_wav, '-af', 'highpass=f=30,equalizer=f=55:t=q:w=1:g=1.2,equalizer=f=250:t=q:w=1.3:g=-1,'
           'equalizer=f=3000:t=q:w=1.5:g=.8,highshelf=f=11000:g=1.5,'
           'acompressor=threshold=0.2:ratio=2:attack=25:release=180:knee=6:makeup=1',
           '-ar', SR, '-ac', 2, '-c:a', 'pcm_f32le', stage)
    push = args.master_lufs - float(loudness(stage)['input_i'])
    ffmpeg('-i', stage, '-af', f'volume={push:.3f}dB,alimiter=limit=0.95:attack=10:release=120:level=false,'
           'alimiter=limit=0.85:attack=2:release=60:level=false',
           '-ar', SR, '-ac', 2, '-c:a', 'pcm_f32le', out / 'master-loud.wav')
    loud = loudness(out / 'master-loud.wav')
    trim = min(0.0, -1.0 - float(loud['input_tp']))
    ffmpeg('-i', out / 'master-loud.wav', '-af', f'volume={trim:.3f}dB', '-ar', SR, '-ac', 2, '-c:a', 'pcm_s24le', master)
    stage.unlink(); (out / 'master-loud.wav').unlink()
    checked = loudness(master)
    mp3 = out / 'afternoon-study.mp3'
    ffmpeg('-i', master, '-c:a', 'libmp3lame', '-b:a', '320k', mp3)
    premaster.unlink(); glue.unlink()
    receipts = dict(
        title='Afternoon Study', artist='Aesthetic Dot Computer', bpm=BPM, bars=total_bars,
        seconds=round(total_bars * BAR, 2), study=str(STUDY),
        sections=[dict(name=s[0], bars=s[1], layers=s[2]) for s in SECTIONS],
        melodies=dict(verseA=VERSE_A, verseB=VERSE_B, chorusA=CHORUS_A, chorusB=CHORUS_B, longLine=LONG_LINE, longLineB=LONG_LINE_B, pulse=PULSE),
        percussionChart=dict(clave=CLAVE, hemiolaStep=HEMIOLA_STEP, text=chart_text()),
        indigoThumps=thump_report,
        kit='synthesized: modal marimba (pop/marimba presets), woodblock (staccato preset), bubble pops, brush swishes, soft kick',
        piano=dict(bank=str(PIANO_DIR), level='forward: -17 LUFS bus, light duck', anchors=sorted(piano.anchors), source='Salamander Grand Piano V3 (CC0, Alexander Holm), the AC OS bank', release=PIANO_RELEASE),
        hymnVoicings=HYMN, chords=dict(main=MAIN_LOOP, hymn=HYMN_LOOP),
        vocalSamples={n: sorted([dict(index=i, part=p, shift=s, stretch=x, targetMidi=banks[n].midi(i, p) + s)
                                 for i, p, s, x in banks[n].used], key=lambda d: (d['index'], d['part'], d['shift']))
                      for n in NAMES},
        breaths=[dict(fromSeconds=round(a, 3), toSeconds=round(b, 3)) for a, b in breaths],
        separation=dict(vocalBuses=VOCAL_BUSES, duckDepths=DUCKED, instrumentCarveHz=3600, floor=.7),
        busLevelsLUFS=levels, busTrimsDB=trims, masterGainDB=round(gain, 2), premasterLUFS=float(measured['input_i']),
        mixLUFS=float(mixed['input_i']), masterPushDB=round(push, 2), masterTrimDB=round(trim, 2),
        masterLUFS=float(checked['input_i']), masterTruePeakDBTP=float(checked['input_tp']), masterLRA=float(checked['input_lra']),
        sha256=dict(wav=hashlib.sha256(master.read_bytes()).hexdigest(), mp3=hashlib.sha256(mp3.read_bytes()).hexdigest()),
        barChords=events)
    (out / 'receipts.json').write_text(json.dumps(receipts, indent=2))
    print('master', checked['input_i'], 'LUFS', checked['input_tp'], 'dBTP', 'LRA', checked['input_lra'])


if __name__ == '__main__':
    main()
