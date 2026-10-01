#!/usr/bin/env python3
"""Nighttime Study — new composition for Jeffrey's three Sep 26 vocal banks.

Platter applications (creative choices, not claims about the source works):
chamber/digest/01 R1,R2,R6,R12: unequal phrases, growing/mirrored melody,
stillness, opening recalled; 03: gradual substitution and half-speed answer;
rhythm/digest/06: rolling vocal and mallet attacks occupy complementary slots.
Original Afternoon Study is left intact. Run with pop/.venv/bin/python.
"""
import argparse
import gc
import hashlib
import json
from pathlib import Path

import numpy as np
import soundfile as sf
from scipy.signal import butter, sosfilt, oaconvolve
import render as ac

SR = ac.SR
BPM = 72
BEAT = 60 / BPM
BAR = BEAT * 4
BARS = 160
DURATION = BARS * BAR + 12
ROOT = Path(__file__).resolve().parents[1]
OUT = ROOT / 'out/nighttime'
SEED = 9302026
# The same four upper voices move in small steps; bass changes their meaning.
CHORDS = {
    'Am9': (45, [57, 60, 64, 71]),
    'Fmaj9': (41, [57, 60, 64, 67]),
    'Cmaj9': (48, [55, 59, 62, 64]),
    'G6add9': (43, [57, 59, 62, 67]),
    'Dm9': (38, [57, 60, 64, 65]),
    'Em7': (40, [55, 59, 62, 67]),
}
# start, length, identity, harmonic route
PHRASES = [
    (0, 8, 'lamp', ['Am9']),
    (8, 12, 'first hum', ['Am9', 'Fmaj9', 'Am9']),
    (20, 16, 'rolling in', ['Am9', 'Fmaj9', 'Cmaj9', 'G6add9']),
    (36, 8, 'first melody', ['Dm9', 'Am9']),
    (44, 12, 'answer', ['Fmaj9', 'Cmaj9', 'Em7']),
    (56, 8, 'still room', ['Am9']),
    (64, 16, 'returning motion', ['Am9', 'Fmaj9', 'Cmaj9', 'G6add9']),
    (80, 12, 'long ascent', ['Dm9', 'Fmaj9', 'Am9']),
    (92, 16, 'open window', ['Fmaj9', 'Cmaj9', 'G6add9', 'Am9']),
    (108, 8, 'night bloom', ['Dm9', 'Am9']),
    (116, 12, 'afterglow', ['Fmaj9', 'Cmaj9', 'Am9']),
    (128, 8, 'distant answer', ['Dm9', 'Am9']),
    (136, 16, 'lamp returns', ['Am9', 'Fmaj9', 'Am9', 'Am9']),
    (152, 8, 'last vowel', ['Am9']),
]
# New melodies, not the source tunes or Afternoon Study's verse/chorus.
# Beat offset, duration, MIDI; each phrase grows then mirrors toward E4.
MOTIFS = [
    [(0, 3, 64), (5, 4, 67), (11, 4, 64)],
    [(0, 3, 64), (4, 2, 67), (7, 4, 69), (13, 3, 67), (18, 5, 64)],
    [(0, 3, 64), (4, 3, 67), (8, 3, 69), (12, 4, 71),
     (18, 3, 69), (22, 3, 67), (27, 5, 64)],
    [(0, 4, 69), (6, 3, 72), (10, 4, 74), (16, 3, 72),
     (21, 3, 69), (26, 6, 64)],
]
SOURCES = [
    'papers/chamber-platter/digest/01-tonal-forms.md',
    'papers/chamber-platter/digest/03-process-and-envelopes.md',
    'papers/rhythm-platter/digest/06-complements-canons.md',
    'papers/arxiv-loner-arrangement-critique/loner-arrangement-critique.md',
]


class Mixer(ac.Mixer):
    def bus(self, name):
        if name not in self.buses:
            self.buses[name] = np.memmap(OUT / f'{name}.buffer', dtype='float32',
                                       mode='w+', shape=(self.total, 2))
        return self.buses[name]


class Voice:
    def __init__(self, bank):
        self.bank = bank
        self.cache = {}
        self.events = []

    def tone(self, midi, beats, soft=False):
        key = (midi, beats, soft)
        if key in self.cache:
            return self.cache[key]
        dur = beats * BEAT
        pool = [n for n in self.bank.manifest['notes'] if n['pitchable']
                and n['end'] - n['start'] > .18]
        note = min(pool, key=lambda n: abs(n['samples']['lead']['targetMidi'] - midi)
                   + .3 / (n['end'] - n['start']))
        shift = midi - note['samples']['lead']['targetMidi']
        filename = OUT / 'cache' / f'{self.bank.name}-{midi}-{beats}-{soft}.wav'
        if filename.exists():
            y = ac.load_mono(filename).astype('float32')
        else:
            raw = self.bank.sample(note['index'], shift=shift).astype('float32')
            # Phase-vocoder duration change keeps the note's measured pitch.
            y = ac.librosa.effects.time_stretch(raw, rate=len(raw) / (dur * SR))
            y = y[:round(dur * SR)]
            y *= ac.fades(len(y), .6 if soft else .045, min(1.2 if soft else .25, dur * .3))
            rms = np.sqrt(np.mean(y * y))
            y *= min(.10 / max(rms, 1e-6), .75 / max(float(np.max(np.abs(y))), 1e-6))
            sf.write(filename, y, SR, subtype='FLOAT')
        self.cache[key] = y
        return y

    def sing(self, mix, bus, midi, beat, length, gain, pan=0, soft=False):
        mix.place(bus, self.tone(midi, length, soft), beat * BEAT, gain, pan)
        self.events.append(dict(bus=bus, midi=midi, beat=beat, length=length, gain=round(gain, 4)))


def arrange():
    ac.BEAT, ac.BAR = BEAT, BAR
    rng = np.random.default_rng(SEED)
    piano = ac.Piano(rng)
    mix = Mixer(DURATION)
    voices = [Voice(ac.Bank(n, OUT / 'cache')) for n in ac.NAMES]
    lemon, indigo, saffron = voices
    harmony = []
    for pi, (start, bars, name, route) in enumerate(PHRASES):
        print(f'arrange {start:3d}: {name}', flush=True)
        # 120 seconds of gradual growth leads into the late bloom.
        strength = .6 if start < 64 else min(1.1, .6 + max(0, start - 80) / 64)
        if start >= 116:
            strength = .34 if start < 128 else .5 * (160 - start) / 32
        if start == 56:
            # Two held vocal notes; no fresh attacks inside this clearing.
            indigo.sing(mix, 'hums', 57, start * 4, 32, .22, -.2, True)
            saffron.sing(mix, 'hums', 64, start * 4, 32, .16, .2, True)
            continue
        if start == 152:
            indigo.sing(mix, 'hums', 57, start * 4, 32, .20, 0, True)
            piano.roll(mix, [45, 64, 71], start * BAR, BAR * 6, .16, vel=.3)
            continue
        for b in range(bars):
            bar = start + b
            chordname = route[min(len(route) - 1, b // 4)]
            root, tones = CHORDS[chordname]
            harmony.append(dict(bar=bar, chord=chordname))
            if b % 4 == 0:
                # Slow common-tone harmony; each voice enters at breath length.
                if start >= 8 and not (116 <= start < 128):
                    for k, midi in enumerate((tones[0], tones[2])):
                        indigo.sing(mix, 'hums', midi, bar * 4 + k * .35,
                                    16, .42 * strength, (-.25, .25)[k], True)
                piano.roll(mix, [root + 12, tones[1] + 12, tones[2] + 12],
                           bar * BAR, BAR * 2.2, .18 * strength, vel=.32)
            if start < 20 or start >= 136:
                # Opening recalled at half its original attack density.
                if b % (2 if start < 20 else 4) == 0:
                    for k, m in enumerate((76, 79, 76)):
                        piano.key(mix, m, bar * BAR + k * 1.3 * BEAT,
                                  BEAT * 3, .21 * strength, vel=.35)
                continue
            if 116 <= start < 128:
                continue
            # Six-beat pitch cycle slips across four-beat bars. At first only
            # some slots are filled. Mallets inhabit the unused odd slots.
            count = 2 if start < 36 else 3 if start < 80 else 4
            pattern = [tones[1], tones[2], tones[3], tones[2], tones[0] + 12, tones[2]]
            for k in range(8):
                beat = bar * 4 + k * .5
                if k % 2 == 0 and k // 2 < count:
                    midi = pattern[(bar * 4 + k // 2) % 6]
                    saffron.sing(mix, 'rolling', midi, beat, 1.5,
                                 .34 * strength * (1 + .12 * np.sin(beat / 9)),
                                 .38 * np.sin(beat / 19), True)
                elif k in (3, 7) and start >= 44 and (bar + k) % 3 != 0:
                    midi = tones[(bar + k) % 4] - 12
                    mix.place('mallets', ac.marimba(midi, 'bass', 2.4), beat * BEAT,
                              .026 * strength, -.3 if k == 3 else .3)
            # Piano answers in the space after the figure, with whole bars off.
            if b % 4 == 2:
                for k, midi in enumerate([tones[3] + 12, tones[2] + 12]):
                    piano.key(mix, midi, (bar * 4 + 2 + k) * BEAT,
                              BEAT * 3, .19 * strength, vel=.4)
            # Rounded, mono bass only in the long ascent, with rests.
            if 80 <= start < 116 and b % 2 == 0:
                bass = ac.bass_tone(root, BEAT * 4, .8)
                bass *= ac.fades(len(bass), .5, 1.2)
                mix.place('bass', bass, bar * BAR, .024 * strength)
        if 36 <= start < 116 or start == 128:
            mi = {36: 0, 44: 1, 64: 1, 80: 2, 92: 2, 108: 3, 128: 0}[start]
            line = MOTIFS[mi]
            for beat, length, midi in line:
                lemon.sing(mix, 'lead', midi, start * 4 + beat + 1, length,
                           .95 * strength, 0)
            # A half-speed answer is reserved for one long phrase, not stacked
            # under every foreground phrase.
            if start == 92:
                for beat, length, midi in MOTIFS[0]:
                    saffron.sing(mix, 'answer', midi - 12, start * 4 + 30 + beat * 2,
                                 length * 2, .55 * strength, .18)
    return mix, voices, harmony


def room_ir(seconds, seed):
    rng = np.random.default_rng(seed)
    n = round(seconds * SR)
    t = np.arange(n) / SR
    ir = sosfilt(butter(2, [260, 4400], btype='band', fs=SR, output='sos'),
                 rng.normal(size=n)).astype('float32')
    ir *= np.exp(-6.91 * t / seconds) * np.minimum(1, t / .08)
    ir /= max(1e-9, np.sqrt(np.sum(ir * ir)))
    for delay, gain in [(.061, .24), (.109, .16), (.173, .1)]:
        ir[round(delay * SR)] += gain
    return ir


def finish(mix, voices, harmony, *, title='Nighttime Study', slug='nighttime-study',
           settings=None, bus_gains=None, target_lufs=-17, measure=ac.loudness,
           vocal_source='Jeffrey, Sep 26 2026 Menu Band Aesthetivox note banks',
           fade_seconds=26):
    n = mix.total
    summed = np.memmap(OUT / 'sum.buffer', dtype='float32', mode='w+', shape=(n, 2))
    # Wet gain, room decay, lowpass. Dry lead stays centered and identifiable.
    settings = settings or {'piano': (.20, 3.8, 6800), 'lead': (.20, 4.8, 8500),
                'hums': (.44, 7.5, 4200), 'rolling': (.31, 5.8, 5800),
                'answer': (.36, 6.8, 6500), 'mallets': (.30, 5.0, 5000),
                'bass': (0, 0, 1800)}
    stem_metrics = {}
    for bus, data in list(mix.buses.items()):
        print('space and mix:', bus, flush=True)
        wet, seconds, cutoff = settings[bus]
        shaped = np.empty((n, 2), dtype='float32')
        for ch in range(2):
            y = sosfilt(butter(2, [35 if bus in ('piano', 'bass', 'kick') else 135, cutoff],
                              btype='band', fs=SR, output='sos'), data[:, ch]).astype('float32')
            if wet:
                reverb = oaconvolve(y, room_ir(seconds, SEED + ch * 77), mode='full')[:n]
                shaped[:, ch] = y + wet * reverb
                del reverb
            else:
                shaped[:, ch] = y
        # Gentle disappearance over the final held vowel and room tail.
        shaped *= (bus_gains if bus_gains is not None else
                   {'piano': 2.8, 'mallets': 1.6, 'bass': 2.5}).get(bus, 1)
        fade = round(fade_seconds * SR)
        shaped[-fade:] *= np.cos(np.linspace(0, np.pi / 2, fade))[:, None] ** 2
        shaped[:round(2 * SR)] *= np.linspace(0, 1, round(2 * SR))[:, None]
        assert np.isfinite(shaped).all(), bus
        sf.write(OUT / 'stems' / f'{bus}.wav', shaped, SR, subtype='PCM_24')
        summed[:] += shaped
        stem_metrics[bus] = dict(peak=float(np.abs(shaped).max()), rms=float(np.sqrt(np.mean(shaped ** 2))))
        del shaped, y
        del mix.buses[bus]
        gc.collect()
    sf.write(OUT / f'{slug}-premaster.wav', summed, SR, subtype='FLOAT')
    del summed
    gc.collect()
    premaster = OUT / f'{slug}-premaster.wav'
    measured = measure(premaster)
    # Static gain preserves the entire long-form dynamic arc. Ceiling limits
    # gain; the oversampled safety limiter should barely work, if at all.
    gain = min(target_lufs - float(measured['input_i']), -2.5 - float(measured['input_tp']))
    wav = OUT / f'{slug}.wav'
    ac.ffmpeg('-i', premaster, '-af',
              f'volume={gain:.4f}dB,aresample=192000,alimiter=limit=0.75:attack=5:release=150:level=false:latency=true,aresample=48000',
              '-c:a', 'pcm_s24le', wav)
    mp3 = OUT / f'{slug}.mp3'
    ac.ffmpeg('-i', wav, '-c:a', 'libmp3lame', '-b:a', '320k',
              '-metadata', f'title={title}', '-metadata', 'artist=Aesthetic Dot Computer', mp3)
    checked, encoded = measure(wav), measure(mp3)
    assert float(checked['input_tp']) <= -2.0
    assert float(encoded['input_tp']) < 0
    receipt = dict(title=title, artist='Aesthetic Dot Computer', bpm=BPM,
                   seconds=DURATION, bars=BARS, seed=SEED, phrases=PHRASES,
                   newMelodies=MOTIFS, sources=SOURCES, barChords=harmony,
                   voices={v.bank.name: v.events for v in voices},
                   vocalSource=vocal_source,
                   piano='Salamander Grand Piano, Alexander Holm, CC0; AC OS anchors',
                   stemMetrics=stem_metrics, gainDB=gain, wav=checked, mp3=encoded,
                   sha256={p.name: hashlib.sha256(p.read_bytes()).hexdigest() for p in (wav, mp3)})
    (OUT / 'receipts.json').write_text(json.dumps(receipt, indent=2))
    print(json.dumps(dict(seconds=DURATION, wav=checked, mp3=encoded)), flush=True)
    premaster.unlink()
    for path in OUT.glob('*.buffer'):
        path.unlink()


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--study', type=Path, default=ac.STUDY)
    args = parser.parse_args()
    ac.STUDY = args.study
    for sub in ('cache', 'stems'):
        (OUT / sub).mkdir(parents=True, exist_ok=True)
    mix, voices, harmony = arrange()
    finish(mix, voices, harmony)


if __name__ == '__main__':
    main()
