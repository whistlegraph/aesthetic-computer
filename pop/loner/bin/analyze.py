# analyze.py — measure the loner takes that matter and write harvest.json.
#
# The lyric (whisper.cpp ggml-small, word-level):
#
#     "Sitting curled up in myself, I think of a stone,
#      just waiting very patiently for time to pass."
#
# For each take: word timestamps (from the whisper JSON the transcribe pass
# left in the scratchpad), per-word pyin f0 → nearest note, a chroma key
# estimate, and an onset-based tempo reading. harvest.json is the receipt —
# tracked, like cult's alt/harvest.json.
#
#   pop/.venv/bin/python pop/loner/bin/analyze.py <scratchpad-dir>

import json, os, sys
import numpy as np
import librosa

HERE = os.path.dirname(os.path.abspath(__file__))
LANE = os.path.dirname(HERE)
SP = sys.argv[1] if len(sys.argv) > 1 else "/tmp"
SR = 48000

TAKES = {
    "7108062006980201771": "the Ten Whistlegraphs / Feral File recording — clean solo voice, the whole lyric",
    "7021262898479549702": "the 13.8M 'not again!' take — spoken 'Camille, are you doing emo whistlegraphs again?' then the sung line",
    "6988954628167585030": "the 1.4M 'loner by @cksuperstore' take — sung, roomier",
    "6988619239657622790": "the origin take — 'Here's a whistlegraph by Camille called loner, ready?' then the line, performed with jeffrey and alex",
}

NOTE = lambda m: librosa.midi_to_note(int(round(m)))

def words_of(vid):
    p = os.path.join(SP, f"{vid}.json")
    if not os.path.exists(p):
        return None
    j = json.load(open(p))
    out = []
    for seg in j["transcription"]:
        t = seg["text"].strip()
        if not t or t in ",.?!♪":
            continue
        out.append(dict(t=t, start=seg["offsets"]["from"] / 1000,
                        end=seg["offsets"]["to"] / 1000))
    return out

report = {}
for vid, note in TAKES.items():
    path = os.path.join(LANE, "source", f"{vid}-48k.wav")
    y, sr = librosa.load(path, sr=SR, mono=True)
    dur = len(y) / sr

    # f0 once over the whole take, then read per-word medians out of it.
    f0, vflag, vprob = librosa.pyin(y, fmin=100, fmax=600, sr=sr,
                                    frame_length=4096, hop_length=512)
    times = librosa.times_like(f0, sr=sr, hop_length=512)
    good = vflag & (vprob > 0.5)

    def span_f0(a, b):
        m = good & (times >= a) & (times < b)
        v = f0[m]
        if v.size < 3:
            return None
        return float(np.median(v))

    words = words_of(vid)
    word_rows = []
    if words:
        for w in words:
            hz = span_f0(w["start"], w["end"])
            row = dict(t=w["t"], start=round(w["start"], 3), end=round(w["end"], 3))
            if hz:
                midi = librosa.hz_to_midi(hz)
                row.update(f0_hz=round(hz, 1), midi=round(float(midi), 2),
                           note=NOTE(midi))
            word_rows.append(row)

    voiced = f0[good]
    med = float(np.median(voiced)) if voiced.size else None

    # key: chroma folded over the voiced regions, matched against the 24
    # Krumhansl-Schmuckler profiles.
    chroma = librosa.feature.chroma_cqt(y=y, sr=sr).mean(axis=1)
    MAJ = np.array([6.35,2.23,3.48,2.33,4.38,4.09,2.52,5.19,2.39,3.66,2.29,2.88])
    MIN = np.array([6.33,2.68,3.52,5.38,2.60,3.53,2.54,4.75,3.98,2.69,3.34,3.17])
    best = (-2, None)
    for tonic in range(12):
        for mode, prof in (("major", MAJ), ("minor", MIN)):
            r = np.corrcoef(np.roll(prof, tonic), chroma)[0, 1]
            if r > best[0]:
                best = (r, f"{librosa.midi_to_note(tonic + 60, octave=False)} {mode}")

    tempo = float(np.atleast_1d(librosa.beat.beat_track(y=y, sr=sr)[0])[0])

    report[vid] = dict(
        source_mp4=f"https://assets.aesthetic.computer/whistlegraph/index/posts/{vid}.mp4",
        what=note, duration=round(dur, 2),
        tempo_bpm=round(tempo, 1),
        key=best[1], key_r=round(best[0], 3),
        median_f0_hz=round(med, 1) if med else None,
        median_note=NOTE(librosa.hz_to_midi(med)) if med else None,
        transcription_method="whisper.cpp (whisper-cli) ggml-small, word-level -ml 1",
        word_timestamps=word_rows,
    )
    r = report[vid]
    print(f"== {vid}  {r['duration']}s  tempo~{r['tempo_bpm']}  key={r['key']} "
          f"(r={r['key_r']})  median f0={r['median_f0_hz']} ({r['median_note']})")
    for w in word_rows:
        print(f"   {w['start']:6.2f}-{w['end']:6.2f}  {w.get('note','—'):5s} "
              f"{w.get('f0_hz','')}  {w['t']}")

out = os.path.join(LANE, "harvest.json")
json.dump(report, open(out, "w"), indent=2)
print("WROTE", out)
