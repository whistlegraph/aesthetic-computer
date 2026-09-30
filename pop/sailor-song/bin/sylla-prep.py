#!/usr/bin/env python3
"""sylla-prep.py — sailor-song's words into SyllaWizard (sylla-wizard/, --spec mode).

One take per sung lyric line: a clip of her natural lead stem around the line
(take time, 0.4 s either side), and a seed file with the line's current word
bounds (src/words-aligned.json, ms into the clip). SyllaWizard draws each word as
a box over the clip's spectrogram; drag its edges, then "Save boundaries".
bin/sylla-collect.py turns the saved boxes back into src/word-bounds.json (take
seconds), which bin/word-times.py applies last.

  pop/.venv/bin/python pop/sailor-song/bin/sylla-prep.py
  swift run --package-path sylla-wizard SyllaWizard --spec pop/sailor-song/src/sylla/spec.json
A seed a take has already saved over (tool: SyllaWizard) is never overwritten.
"track" names the whole stem: SyllaWizard then shows one scrolling spectrogram of
the entire vocal with every line's boxes in place (pass --lines for one line per page).
"""
import json, os, subprocess
LANE = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
SRC = os.path.join(LANE, "src"); SY = os.path.join(SRC, "sylla"); os.makedirs(SY, exist_ok=True)
REPO = os.path.dirname(os.path.dirname(LANE))
rel = lambda p: os.path.relpath(p, REPO)
words = json.load(open(os.path.join(SRC, "words-aligned.json")))
lines = [l.strip() for l in open(os.path.join(SRC, "lyrics-sung.txt")) if l.strip() and not l.startswith("[")]
stem = os.path.join(SRC, "vox", "vocals-natural.wav")
takes, k = [], 0
for n, line in enumerate(lines):
    ws = list(range(k, k + len(line.split()))); k += len(ws)
    ws = [i for i in ws if i < len(words)]
    if not ws: continue
    a = max(0.0, words[ws[0]]["fromMs"] / 1000 - 0.4); b = words[ws[-1]]["toMs"] / 1000 + 0.4
    tid = f"{n + 1:02d}"
    clip = os.path.join(SY, f"line-{tid}.mp3"); fix = os.path.join(SY, f"words-{tid}.json")
    subprocess.run(["ffmpeg", "-v", "error", "-y", "-ss", f"{a:.3f}", "-t", f"{b - a:.3f}", "-i", stem, "-ac", "1", "-c:a", "libmp3lame", "-q:a", "2", clip], check=True)
    seed = {"take": tid, "offsetSec": round(a, 3), "wordIndex": ws,
            "words": [{"text": words[i]["text"], "fromMs": words[i]["fromMs"] - round(a * 1000), "toMs": words[i]["toMs"] - round(a * 1000)} for i in ws]}
    old = json.load(open(fix)) if os.path.exists(fix) else {}
    if old.get("source", "").startswith("SyllaWizard"):
        seed = {**old, "offsetSec": old.get("offsetSec", seed["offsetSec"]), "wordIndex": old.get("wordIndex", ws)}
    json.dump(seed, open(fix, "w"), indent=1)
    takes.append({"id": tid, "title": line, "date": f"{a // 60:.0f}:{a % 60:04.1f}", "audio": rel(clip), "fix": rel(fix),
                  "out": rel(os.path.join(SY, f"sylls-{tid}.json")), "drawn": rel(os.path.join(SY, f"drawn-{tid}.json"))})
json.dump({"name": "sailor-song", "title": "sailor song lines", "track": rel(stem), "takes": takes}, open(os.path.join(SY, "spec.json"), "w"), indent=1)
print(f"{len(takes)} lines → {rel(os.path.join(SY, 'spec.json'))}")
