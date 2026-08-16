# analyze.py — measure the h0t whistlegraph takes before anything is scored.
#
# Six posts carry the `h0t` work (spring 2022, posts.json), and every one
# of them says the same three lines:
#
#     it's too hot / no it's not / now I'm back in season
#
# with a sung "doo doo doo" melody between the argument and the tagline.
# This probe measures, per take: duration, onset-based tempo (librosa,
# measured — never guessed), pyin f0 over every whisper word span, and a
# whistle scan at 500–3000 Hz (same honesty check the cult harvest ran —
# report what the pitch tracker finds, not what the caption implies).
#
#   pop/.venv/bin/python pop/season/bin/analyze.py   # → analysis/harvest.json
#
# Run from pop/season/.

import json, os
import numpy as np
import librosa

SR = 48_000
IDS = [
    "7080453509149134126",  # spring flower — 28.3M
    "7079639110025088298",  # all four seasons in 9 seconds — 12.4M
    "7087134943930846506",  # springy vibesies — 7.8M
    "7093848478245358894",  # nawwww waaa baaaak in seeeeeeeezn — 6.9M
    "7078095590348836139",  # summer, fall, winter, spring — 6.3M
    "7078347899049905450",  # (no caption) — 3.5M
]

def f0_of(y, t0, t1, fmin=70, fmax=600):
    seg = y[int(t0 * SR):int(t1 * SR)]
    if len(seg) < SR * 0.05:
        return None, 0.0
    f0, vflag, _ = librosa.pyin(seg, fmin=fmin, fmax=fmax, sr=SR,
                                frame_length=2048, hop_length=256)
    voiced = f0[vflag] if vflag is not None else f0[~np.isnan(f0)]
    voiced = voiced[~np.isnan(voiced)]
    if len(voiced) == 0:
        return None, 0.0
    return float(np.median(voiced)), float(len(voiced) / max(1, len(f0)))

def whistle_scan(y):
    # A whistle is a sustained near-sinusoid well above speech. Same test
    # the cult harvest used: pyin 500–3000 Hz, look for a voiced run
    # >= 0.15 s. Report the longest run and its median f0, or nothing.
    f0, vflag, _ = librosa.pyin(y, fmin=500, fmax=3000, sr=SR,
                                frame_length=2048, hop_length=256)
    hop_s = 256 / SR
    best, cur, cur_f = (0.0, None), 0, []
    for i in range(len(f0)):
        ok = vflag[i] and not np.isnan(f0[i])
        if ok:
            cur += 1; cur_f.append(f0[i])
        else:
            if cur * hop_s > best[0]:
                best = (cur * hop_s, float(np.median(cur_f)))
            cur, cur_f = 0, []
    if cur * hop_s > best[0]:
        best = (cur * hop_s, float(np.median(cur_f)))
    return best if best[0] >= 0.15 else None

def note_name(hz):
    if not hz: return None
    m = 69 + 12 * np.log2(hz / 440.0)
    names = ["C","C#","D","D#","E","F","F#","G","G#","A","A#","B"]
    n = int(round(m))
    return f"{names[n % 12]}{n // 12 - 1}"

report = {}
for id in IDS:
    y, _ = librosa.load(f"wav/{id}.wav", sr=SR, mono=True)
    dur = len(y) / SR

    # tempo, measured from onsets — not from anyone's opinion
    oenv = librosa.onset.onset_strength(y=y, sr=SR, hop_length=256)
    tempo, beats = librosa.beat.beat_track(onset_envelope=oenv, sr=SR,
                                           hop_length=256)
    tempo = float(np.atleast_1d(tempo)[0])

    # onsets, for hand-checking phrase boundaries against whisper
    onsets = librosa.onset.onset_detect(onset_envelope=oenv, sr=SR,
                                        hop_length=256, units="time",
                                        backtrack=True)

    words = json.load(open(f"txt/{id}.json"))["transcription"]
    spans = []
    for w in words:
        t = w["text"].strip()
        if not t or t in "(),.!?♪-":
            continue
        t0, t1 = w["offsets"]["from"] / 1000, w["offsets"]["to"] / 1000
        f0, vfrac = f0_of(y, t0, t1)
        spans.append(dict(t=t, start=round(t0, 3), end=round(t1, 3),
                          f0=round(f0, 1) if f0 else None,
                          note=note_name(f0), voiced=round(vfrac, 2)))

    wh = whistle_scan(y)
    report[id] = dict(
        wav=os.path.abspath(f"wav/{id}.wav"),
        duration=round(dur, 3),
        tempo_bpm=round(tempo, 1),
        onsets=[round(t, 3) for t in onsets.tolist()],
        words=spans,
        whistle=dict(longest_run_s=round(wh[0], 2), median_hz=round(wh[1], 1),
                     note=note_name(wh[1])) if wh else None,
    )
    print(f"{id}  {dur:5.2f}s  tempo={tempo:6.1f}  "
          f"whistle={'%.2fs @ %.0f Hz' % wh if wh else 'none'}")
    for s in spans:
        print(f"    {s['start']:6.2f}-{s['end']:6.2f}  {s['t']:8s} "
              f"f0={s['f0'] or 0:6.1f} {s['note'] or '':4s} v={s['voiced']:.2f}")

os.makedirs("analysis", exist_ok=True)
json.dump(report, open("analysis/harvest.json", "w"), indent=2)
print("WROTE analysis/harvest.json")
