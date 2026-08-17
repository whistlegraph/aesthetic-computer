# survey.py — first look at every downloaded loner take.
#
# For each source/<id>-48k.wav: duration, RMS, an onset-based tempo guess,
# pyin in the VOICE range (65–500 Hz) and in the WHISTLE range (500–3000 Hz)
# — the cult harvest taught us to check both honestly before assuming which
# one the melody lives in.
#
#   pop/.venv/bin/python pop/loner/bin/survey.py

import glob, json, os, sys
import numpy as np
import librosa

SR = 22050  # analysis rate; the render will use the 48k decodes directly
HERE = os.path.dirname(os.path.abspath(__file__))
LANE = os.path.dirname(HERE)

report = {}
for path in sorted(glob.glob(os.path.join(LANE, "source", "*-48k.wav"))):
    vid = os.path.basename(path).replace("-48k.wav", "")
    y, sr = librosa.load(path, sr=SR, mono=True)
    dur = len(y) / sr
    rms = float(np.sqrt(np.mean(y ** 2)))

    tempo, beats = librosa.beat.beat_track(y=y, sr=sr)
    tempo = float(np.atleast_1d(tempo)[0])

    def pyin_scan(fmin, fmax):
        f0, vflag, vprob = librosa.pyin(
            y, fmin=fmin, fmax=fmax, sr=sr, frame_length=2048)
        voiced = f0[vflag & (vprob > 0.5)] if vflag is not None else np.array([])
        if voiced.size == 0:
            return None, 0.0
        return float(np.median(voiced)), float(voiced.size / len(f0))

    v_f0, v_frac = pyin_scan(65, 500)
    w_f0, w_frac = pyin_scan(500, 3000)

    report[vid] = dict(
        duration=round(dur, 2), rms_db=round(20 * np.log10(rms + 1e-12), 1),
        tempo_bpm=round(tempo, 1),
        voice_f0_hz=round(v_f0, 1) if v_f0 else None, voice_frac=round(v_frac, 2),
        whistle_f0_hz=round(w_f0, 1) if w_f0 else None, whistle_frac=round(w_frac, 2),
    )
    r = report[vid]
    print(f"{vid}  {r['duration']:6.2f}s  {r['rms_db']:6.1f}dB  "
          f"tempo~{r['tempo_bpm']:5.1f}  voice f0={r['voice_f0_hz']} ({r['voice_frac']})  "
          f"whistle f0={r['whistle_f0_hz']} ({r['whistle_frac']})")

out = os.path.join(LANE, "survey.json")
json.dump(report, open(out, "w"), indent=2)
print("WROTE", out)
