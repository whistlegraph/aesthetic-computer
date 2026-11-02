#!/usr/bin/env python3
"""take-arrange.py — a song from a beat-aligned take.

Reads the aligned stems take-quantize.py leaves in <lane>/quantize/ (every beat
on a fixed grid, `preroll` seconds of room before beat 0) and lays them out as
sections cut on bar lines, joined with short equal-power crossfades. The form
lives in FORM below, in source beats; each section says which stems play.

  pop/.venv/bin/python pop/menuband/bin/take-arrange.py --take cream-comets-nov-1 \
      --stamp pop/menuband/takes/cream-comets-nov-1/stamp/stamp-c.mp3 --desktop
"""
import argparse, json, pathlib, importlib.util, subprocess
import numpy as np, soundfile as sf

HERE = pathlib.Path(__file__).resolve().parent
ROOT = HERE.parent.parent.parent
spec = importlib.util.spec_from_file_location('tq', HERE / 'take-quantize.py')
tq = importlib.util.module_from_spec(spec); spec.loader.exec_module(tq)

ALL = ('percussion', 'tones', 'voice')
# cream-comets: verse = C · Cm · C · Cm (beats 0–28), chorus = B C E G E D (28–60),
# coda = C (60–70). Negative beats are the room before the first chord.
# Rules cited from papers/bass-platter/digest/01-dub-bass.md (R#) and the seven
# lessons in papers/study-one-step (L#). About 33 s.
FORM = [  # name, from, to (source beats), stems, section gain dB (drums follow it too)
    ('intro', -16,  0, ('voice',),          -3.0),  # R4: drums and space before anything else
    ('verse',   0, 28, ('tones', 'voice'),  -3.0),
    ('chorus', 28, 60, ('tones', 'voice'),   0.0),  # L3: end loudest, the reserve spent here
    ('coda',   60, 70, ('tones', 'voice'),   0.0),
]
# The drums play as one continuous take (microtiming kept, rhythm-platter 07),
# re-entering after the stamp's drop-out (R4, L5: break by subtraction; return on
# a downbeat), which hides the one join. Song beat → source beat, for N beats.
DRUMS = [(0, 4, 36), (44, 28, 42)]   # R4: a two-bar hole
SWEEP = (0, 14, 120, 16000)   # R6 throw, reversed: low-pass opens over the intro, kicks first
# The stamp sung in time: syllable starts in the TTS take (s), then where each
# lands in the song (beats) and on what note. "-ter" hits the chorus downbeat.
STAMP = {
    'syllables': [0, 0.31, 0.60, 0.87, 1.06, 1.25],          # knee · lah · dot · com · pu · ter
    'beats':     [36, 38, 40, 42, 43, 44, 46],               # last = where "ter" ends; neee · laaa held
    'notes':     ['G4', 'E4', 'G4', 'C4', 'Eb4', 'F#4'],     # C bar, Cm bar, then B for the chorus
    'formant':   3,                                          # semitones of lift (cute)
}


def sing_stamp(path, beat, lane):
    """TTS take → syllables stretched onto the grid → WORLD pitch-locked (robot)
    a few semitones low → lifted back with the formants, which reads as cute."""
    import librosa
    raw = lane / 'stamp-raw.wav'
    subprocess.run(['ffmpeg', '-v', 'error', '-y', '-i', path, '-ac', '1', '-ar', '44100', '-af',
                    'silenceremove=start_periods=1:start_threshold=-45dB,areverse,'
                    'silenceremove=start_periods=1:start_threshold=-45dB,areverse', str(raw)], check=True)
    x, sr = sf.read(raw)
    b0 = STAMP['beats'][0]
    tgt = [(b - b0) * beat for b in STAMP['beats']]
    src = STAMP['syllables'] + [len(x) / sr]
    timed = lane / 'stamp-timed.wav'
    tq.warp(np.array(src), np.array(tgt), x, sr, False, timed)
    lift = STAMP['formant']
    low = [librosa.midi_to_note(librosa.note_to_midi(n) - lift, unicode=False) for n in STAMP['notes']]
    locked = lane / 'stamp-locked.wav'
    subprocess.run([str(ROOT / 'pop/.venv/bin/python'), str(ROOT / 'pop/bin/pitchsnap_world.py'),
                    str(timed), str(locked), '--notes', ','.join(low),
                    '--note-starts', ','.join(f'{t:.4f}' for t in tgt[:-1]),
                    '--retain', '1', '--xfade-ms', '12', '--voicing-ramp-ms', '15'],
                   check=True, capture_output=True)
    out = lane / 'stamp-sung.wav'
    subprocess.run(['rubberband', '-q', '-p', str(lift), '--crisp', '5', str(locked), str(out)],
                   check=True, capture_output=True)
    y, _ = sf.read(out)
    return np.stack([y, y], 1), b0 * beat


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('--take', required=True)
    ap.add_argument('--stamp', help='spoken ident (TTS take); sung in time per STAMP')
    ap.add_argument('--room-db', type=float, default=-19)
    ap.add_argument('--desktop', action='store_true')
    a = ap.parse_args()

    lane = ROOT / 'pop/menuband/takes' / a.take / 'quantize'
    q = json.load(open(lane / 'quantize.json'))
    beat, pre = 60 / q['bpm'], q['preroll']
    S = {}
    for k in ALL:
        x, sr = sf.read(lane / f'{k}.wav')
        S[k] = x if x.ndim > 1 else np.stack([x, x], 1)
    S['voice'] = tq.deepen(S['voice'], sr, lane)
    from scipy.signal import butter, sosfilt
    S['kick'] = sosfilt(butter(4, 170, 'low', fs=sr, output='sos'), S['percussion'], axis=0) * 1.6

    xf = int(0.02 * sr)
    fin, fout = np.sin(np.linspace(0, np.pi / 2, xf))[:, None], np.cos(np.linspace(0, np.pi / 2, xf))[:, None]
    total = sum(f[2] - f[1] for f in FORM)
    n = int(total * beat * sr) + xf
    W = {k: np.zeros((n, 2)) for k in ALL}

    def lay(k, src_beat, dst_beat, beats, gain=1.0):
        s0 = int((pre + src_beat * beat) * sr)
        s1 = min(int((pre + (src_beat + beats) * beat) * sr) + xf, len(S[k]))
        seg = S[k][s0:s1] * gain
        seg[:xf] *= fin; seg[-xf:] *= fout
        d = int(dst_beat * beat * sr)
        W[k][d:d + len(seg)] += seg[: n - d]

    at = 0
    print(f'{a.take}: {total} beats at {q["bpm"]:g} bpm = {total * beat:.1f}s')
    for name, b0, b1, stems, db in FORM:
        print(f'  {at:4d}  {name:<7} beats {b0:3d}–{b1:3d}  {" ".join(stems)}  {db:+.1f} dB')
        for k in stems: lay(k, b0, at, b1 - b0, 10 ** (db / 20))
        at += b1 - b0
    for dst, src, beats in DRUMS:
        print(f'  drums {dst:3d}–{dst + beats:3d} ← source {src}–{src + beats}')
        lay('percussion', src, dst, beats)
    # section gains on the drums too, ramped over a beat at each change
    g = np.ones(n); at = 0
    for _, b0, b1, _, db in FORM:
        g[int(at * beat * sr):int((at + b1 - b0) * beat * sr)] = 10 ** (db / 20); at += b1 - b0
    g = np.convolve(g, np.ones(int(beat * sr)) / int(beat * sr), mode='same')
    W['percussion'] *= g[:, None]

    # the intro's low-pass opens (exponential in Hz), block by block
    from scipy.signal import butter, sosfilt, sosfilt_zi
    b0, b1, f0, f1 = SWEEP
    i0, i1 = int(b0 * beat * sr), int(b1 * beat * sr); blk = 512
    zi = None
    for i in range(i0, i1, blk):
        x = (i - i0) / (i1 - i0)
        sos = butter(2, f0 * (f1 / f0) ** x, 'low', fs=sr, output='sos')
        if zi is None: zi = np.zeros((sos.shape[0], 2, 2))
        seg = W['percussion'][i:i + blk]
        for c in (0, 1):
            seg[:, c], zi[:, :, c] = sosfilt(sos, seg[:, c], zi=zi[:, :, c])

    stamp = sing_stamp(a.stamp, beat, lane) if a.stamp else None
    tq.mix(W, sr, beat, 16 * beat, lane, a.take, a.desktop, a.room_db, name='song', stamp=stamp)


if __name__ == '__main__':
    main()
