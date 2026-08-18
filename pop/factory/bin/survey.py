# survey.py — navigate the other eighteen `fact` takes.
#
# v1/v2 fed on 3 of the 21 posts the whistlegraph index tags with the
# work. @jeffrey: "lets also navigate more takes" — so this downloads
# every remaining pressing from the AC mirror, word-timestamps it with
# whisper.cpp, probes f0, and writes survey.json: the receipt the
# harvest's new cuts were chosen from. The factory gets more dies.
#
#   cd pop/factory && ../.venv/bin/python bin/survey.py

import json, os, subprocess
import numpy as np, librosa

SR = 48000
MIRROR = "https://assets.aesthetic.computer/whistlegraph/index/posts"
WHISPER_MODEL = os.path.expanduser("~/.whisper-models/ggml-small.bin")
USED = {"6925546179275099397", "6928682624529485062", "7030651123325308165"}

posts = json.load(open(os.path.join(os.path.dirname(os.path.dirname(
    os.path.dirname(os.path.dirname(os.path.abspath(__file__))))),
    "system/public/whistlegraph.org/posts.json")))
arr = posts if isinstance(posts, list) else posts.get("posts", list(posts.values()))
takes = [p for p in arr if "fact" in (p.get("works") or []) and p["id"] not in USED]
takes.sort(key=lambda p: -(p.get("views") or 0))

os.makedirs("source/txt", exist_ok=True)
report = {}
for p in takes:
    id = p["id"]
    mp4, wav, txt = f"source/{id}.mp4", f"source/{id}.wav", f"source/txt/{id}.json"
    if not os.path.exists(mp4):
        subprocess.run(["curl", "-sL", "-o", mp4, f"{MIRROR}/{id}.mp4"], check=True)
    if not os.path.exists(wav):
        subprocess.run(["ffmpeg", "-y", "-v", "error", "-i", mp4,
                        "-ac", "1", "-ar", str(SR), wav], check=True)
    if not os.path.exists(txt):
        k16 = f"source/{id}-16k.wav"
        subprocess.run(["ffmpeg", "-y", "-v", "error", "-i", wav, "-ar", "16000", k16],
                       check=True)
        subprocess.run(["whisper-cli", "-m", WHISPER_MODEL, "-l", "en", "-ml", "1",
                        "-oj", "-of", f"source/txt/{id}", "-f", k16, "--no-prints"],
                       check=True, capture_output=True)
        os.unlink(k16)
    words = json.load(open(txt))["transcription"]
    transcript = " ".join(w["text"] for w in words).strip()
    y, _ = librosa.load(wav, sr=SR, mono=True)
    f0, vf, vp = librosa.pyin(y, fmin=65, fmax=600, sr=SR, frame_length=2048)
    v = f0[~np.isnan(f0)]
    report[id] = dict(
        views=p.get("views"), date=p.get("date"), kind=p.get("kind"),
        desc=p.get("desc"), duration=round(len(y) / SR, 2),
        median_f0_hz=round(float(np.median(v)), 1) if len(v) else None,
        transcript=transcript,
        word_timestamps=[dict(t=w["text"], start=w["offsets"]["from"] / 1000,
                              end=w["offsets"]["to"] / 1000)
                         for w in words if w["text"].strip()])
    print(f"== {id}  {p.get('date')}  {p.get('views'):>9} views  "
          f"{report[id]['duration']:5.1f}s  f0~{report[id]['median_f0_hz']}")
    print(f"   {transcript[:160]}")

json.dump(report, open("survey.json", "w"), indent=1)
print(f"WROTE survey.json ({len(report)} takes)")
