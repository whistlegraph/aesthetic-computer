"""The cuckoo and the stamp as voices of gundrum and bass 2 (@jeffrey 2026-11-23: "not just at the start
… part of the bachian composition … the audio stamp i like it when we pitch that around").

The stamp ("aesthetic dot computer", pop/marimba/assets/ac-stamp.mp3, jeffrey's voice) lasts 1.44 s, a
bar at 172 BPM. It is sung as a subject entry: stretched to one bar, its seven syllables put on the
pitches the entering voice plays in that bar, at the moments that voice plays them, moved by octaves
into jeffrey's register (around D3). WORLD replaces the pitch; the voice, breath and consonants stay
(pop/bin/pitchsnap_world.py). Entries sung: the exposition's subject (D) and answer (A), the entry in
F, the closing stretto pair, the stretto pair that opens V, the soprano's inversion, and the climax
where all four voices state the subject in octaves (sung there in two octaves).

The cuckoo (assets/cuckoo, DiArchangeli, CC0) calls a falling third, cu then koo an eighth later. It
is a motif, as in Daquin's Le Coucou, and arrives only with the drop (III, bar 9): two birds, left
and right, take turns a bar apart through the rest of the canon; over the passacaglia one falls onto each note of the ground
(cu a third above it, koo on it, two octaves up); in V the birds join the head's close imitation,
one each half bar; on the last chord they call D major down from the top. Every call's pitches are
the chord sounding then: the root of the sub at that step, with its D-minor triad (A major on A).
A pipe has no voicing for WORLD, so a call is resampled onto its note, with a 7 Hz wobble.

Reads out/gundrum-and-bass-2.json (the score's steps do not change between renders) and writes
out/.voices/plan.json plus one mono float32 48 kHz file per sung stamp and per call, which
bin/gundrum-and-bass-2.mjs reads as the "stamp" and "cuckoo" layers.

  python bin/cuckoo-stamp.py
"""
import json
from pathlib import Path
import subprocess
import tempfile

import numpy as np
import soundfile as sf

HERE = Path(__file__).resolve().parent.parent
OUT = HERE / 'out'
VOX = OUT / '.voices'
SR, BPM = 48000, 172
STEP = 60 / BPM / 4
STAMP = HERE.parent / 'marimba/assets/ac-stamp.mp3'
SNAP = HERE.parent / 'bin/pitchsnap_world.py'
PY = HERE.parent / '.venv/bin/python'
CUCKOO = HERE / 'assets/cuckoo/cuckoo-clock-12-01.mp3'
CU_HZ, KOO_HZ = 972.7, 802.7
CU_AT = [1.081, 1.876, 2.651, 3.426, 4.221, 5.006, 5.786, 6.556, 7.336, 8.111, 8.881, 9.661]
KOO_AT = [1.316, 2.116, 2.886, 3.661, 4.456, 5.236, 6.021, 6.781, 7.561, 8.346, 9.116, 9.896]
NAMES = ['C', 'C#', 'D', 'Eb', 'E', 'F', 'F#', 'G', 'G#', 'A', 'Bb', 'B']
SCALE = [2, 4, 5, 7, 9, 10, 0]

# (step, voice, octaves of the stamp) — entries read from the score's entry table
ENTRIES = [(0, 'A', [0])]                      # once (@jeffrey: "the stamp should only be once")
AFTER = (608, 736)                             # from the drop (III bar 9) to the passacaglia: birds a bar apart
# (@jeffrey: "the cuckoo clocks should only come after the drop")
GROUND = (896, 1056)                           # passacaglia variations 3 and 4
CLOSE = (1344, 1408)                           # V: the head in close imitation
CADENCE = 1536


def load(path, hp=None):
    af = ['-af', f'highpass=f={hp}:poles=2,highpass=f={hp}:poles=2'] if hp else []
    raw = subprocess.run(['ffmpeg', '-v', 'error', '-i', str(path), *af, '-ac', '1', '-ar', str(SR), '-f', 'f32le', '-'],
                         capture_output=True, check=True).stdout
    return np.frombuffer(raw, np.float32).astype(float)


def name(m):
    return NAMES[m % 12] + str(m // 12 - 1)


def hz(m):
    return 440 * 2 ** ((m - 69) / 12)


def norm(y):
    return y / (np.abs(y).max() + 1e-9)


plan, n = [], [0]


def save(y, voice, step, vel, pan, **extra):
    n[0] += 1
    f = VOX / f'{voice}-{n[0]:03d}.f32'
    y.astype(np.float32).tofile(f)
    plan.append({'voice': voice, 'step': step, 'file': str(f), 'hold': len(y) / SR, 'vel': vel, 'pan': pan, **extra})


# ── the stamp ────────────────────────────────────────────────────────────────────────────────────
def voiced_start(y):
    e = np.convolve(y ** 2, np.ones(480) / 480, 'same')
    return int(np.argmax(e > e.max() * .02))


VOXUP = 5                                       # semitones the formants go up: the "voxed" sheen
LAYERS = [(12, 1.0, 0), (19, .5, .025), (24, .32, .045)]    # (over the register, gain, delay s): main, fifth, octave


def stamps(events):
    """sung high and voxed (@jeffrey: "more high pitched voxed, like other tracks", after hellsine's stamp):
    WORLD sings each layer VOXUP semitones under its note, then a resample by VOXUP lifts pitch and
    formants together, so the notes land and the voice brightens. Main an octave over jeffrey's
    register, a fifth over that at half, the octave over that at a third."""
    raw = load(STAMP)
    raw = raw[max(0, voiced_start(raw) - 240):]
    r = 2 ** (VOXUP / 12)
    bar = 16 * STEP * r                         # sung this long, so it is one bar after the resample
    with tempfile.TemporaryDirectory() as tmp:
        src = Path(tmp) / 'stamp.wav'
        fit = subprocess.run(['ffmpeg', '-v', 'error', '-f', 'f32le', '-ar', str(SR), '-ac', '1', '-i', '-',
                              '-af', f'atempo={len(raw) / SR / bar:.4f}', '-f', 'f32le', '-'],
                             input=raw.astype(np.float32).tobytes(), capture_output=True, check=True).stdout
        sf.write(src, np.frombuffer(fit, np.float32), SR)
        for step, voice, _ in ENTRIES:
            notes = sorted((e for e in events if e['voice'] == voice and e.get('midi') is not None
                            and step <= e['step'] < step + 16), key=lambda e: e['step'])[:7]
            ms = [e['midi'] for e in notes]
            shift = 12 * round((50 - np.mean(ms)) / 12)
            starts = [(e['step'] - step) * STEP * r for e in notes]
            mix = None
            for up, gain, delay in LAYERS:
                dst = Path(tmp) / f'{step}-{up}.wav'
                subprocess.run([str(PY), '-W', 'ignore', str(SNAP), str(src), str(dst),
                                '--notes', ','.join(name(m + shift + up - VOXUP) for m in ms),
                                '--note-starts', ','.join(f'{s:.4f}' for s in starts), '--retain', '1.0',
                                '--xfade-ms', '25', '--vibrato-hz', '5.4', '--vibrato-cents', '14'],
                               check=True, capture_output=True)
                y, _ = sf.read(dst)
                y = np.interp(np.arange(0, len(y) - 1, r), np.arange(len(y)), y)
                d = int(delay * SR)
                if mix is None:
                    mix = np.zeros(len(y) + int(.1 * SR))
                n = min(len(y), len(mix) - d)
                mix[d:d + n] += gain * norm(y)[:n]
            save(norm(mix), 'stamp', step, .8, .1, notes=[name(m + shift + 12) for m in ms], entry=voice)


# ── the cuckoo ───────────────────────────────────────────────────────────────────────────────────
pipes = load(CUCKOO, 650)


def cut(t0, t1):
    y = pipes[int(t0 * SR):int(t1 * SR)].copy()
    f = int(.01 * SR)
    y[:f] *= np.linspace(0, 1, f)
    y[-3 * f:] *= np.linspace(1, 0, 3 * f)
    return norm(y)


CU = [cut(t - .05, t + .17) for t in CU_AT]
KOO = [cut(t - .05, t + .32) for t in KOO_AT]


def sing(clip, ratio, phase):
    pos, out, i = 0.0, [], 0
    while pos < len(clip) - 1:
        k = int(pos)
        out.append(clip[k] + (clip[k + 1] - clip[k]) * (pos - k))
        t = i / SR
        pos += ratio * 2 ** (35 / 1200 * min(1, t / .06) * np.sin(2 * np.pi * 7 * t + phase))
        i += 1
    return np.array(out)


take = [0]


def call(step, cu, koo, vel, pan):
    k = take[0] = (take[0] + 5) % len(CU)
    y = sing(CU[k], hz(cu) / CU_HZ, k)
    z = sing(KOO[k], hz(koo) / KOO_HZ, k + 1)
    gap = int(2 * STEP * SR)
    out = np.zeros(gap + len(z))
    out[:len(y)] += y
    out[gap:] += .9 * z
    save(out, 'cuckoo', step - .05 / STEP, vel, pan, cu=name(cu), koo=name(koo))


def chord(subs, step):
    r = None
    for e in subs:
        if e['step'] > step:
            break
        if e['step'] + e.get('len', 1) > step - 8:
            r = e['midi']
    pc = (r if r is not None else 62) % 12
    if pc == 9:
        return [9, 1, 4]
    if pc not in SCALE:
        return [pc, (pc + 4) % 12, (pc + 7) % 12]
    k = SCALE.index(pc)
    return [pc, SCALE[(k + 2) % 7], SCALE[(k + 4) % 7]]


def near(pc, around):
    return min((m for m in range(around - 12, around + 13) if m % 12 == pc), key=lambda m: abs(m - around))


def cuckoos(events):
    subs = sorted((e for e in events if e['voice'] == 'sub' and e.get('midi') is not None), key=lambda e: e['step'])
    # from the drop: two birds a bar apart, falling fifth to third of the chord
    for a, b in [AFTER]:
        for k, st in enumerate(range(a, b, 16)):
            c = chord(subs, st)
            cu = near(c[2], 81)
            call(st, cu, near(c[1], cu - 3), .55 + .05 * (k % 2), (-.6, .6)[k % 2])
    # passacaglia: onto each ground note from a third above
    grounds = [e for e in subs if GROUND[0] <= e['step'] < GROUND[1] and e.get('form') == 'ground']
    for k, e in enumerate(grounds):
        koo = e['midi'] + 12 * round((79 - e['midi']) / 12)
        c = chord(subs, e['step'])
        up = [m for m in range(koo + 3, koo + 5) if m % 12 in SCALE or m % 12 in c]
        call(e['step'] + 4, up[0] if up else koo + 3, koo, .5, (.55, -.55)[k % 2])
    # V: one bird each half bar, joining the head's close imitation, rising
    for k, st in enumerate(range(CLOSE[0], CLOSE[1], 8)):
        c = chord(subs, st)
        cu = near(c[2], 77 + k)
        call(st + 3, cu, near(c[1], cu - 3), .5 + .03 * k, (-.5, .5)[k % 2])
    # the last chord, D major, from the top
    for k, (cu, koo) in enumerate([(86, 81), (81, 78), (78, 74), (74, 69)]):
        call(CADENCE + 8 + 8 * k, cu, koo, .55 - .06 * k, (.5, -.5)[k % 2])


def main():
    VOX.mkdir(parents=True, exist_ok=True)
    for f in VOX.glob('*.f32'):
        f.unlink()
    events = json.loads((OUT / 'gundrum-and-bass-2.json').read_text())['events']
    stamps(events)
    cuckoos(events)
    (VOX / 'plan.json').write_text(json.dumps(plan, indent=1))
    print(len([p for p in plan if p['voice'] == 'stamp']), 'stamps ·',
          len([p for p in plan if p['voice'] == 'cuckoo']), 'cuckoos')
    for p in plan:
        if p['voice'] == 'stamp':
            print(' stamp', p['step'], p['entry'], ' '.join(p['notes']))


main()
