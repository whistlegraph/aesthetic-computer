#!/usr/bin/env python3
"""recog.py — can the words be heard? whisper.cpp reads the sung statements back.

  takes   every phrase take (vault voice/<voice>-phonemes/phrases/) is sung alone on the
          hook and transcribed. A take that whisper cannot read whole is not cast:
          the garble is in the take, not in the stretch.
  --arr   each statement in out/vocal-<arr>-lyrics.json (written by voice.py) is cut from
          the dry lyric stem and from the finished master and transcribed. The stem
          column must be whole; the master column says how much the mix leaves standing.

usage: pop/.venv/bin/python pop/aesthetina/bin/recog.py takes
       pop/.venv/bin/python pop/aesthetina/bin/recog.py --arr=reveal [--min=0.6]
Exits 1 when a stem line is not whole or the master's mean recall is under --min.
"""
import difflib, json, os, re, subprocess, sys, tempfile
from pathlib import Path

HERE = Path(__file__).resolve().parent
OUT = HERE.parent / "out"
MODEL = os.environ.get("WHISPER_MODEL", str(Path.home() / "Models/ggml-large-v3-turbo-q5_0.bin"))
FFMPEG = "/opt/homebrew/bin/ffmpeg"  # not the shim
# what each line should say: ../lyrics.json, hyphens and punctuation dropped
TEXT = {k: re.sub(r"[-?.!]", "", v) for k, v in json.load(open(HERE.parent / "lyrics.json")).items()}
NSYL = {k: len(re.split(r"[ -]", v)) for k, v in json.load(open(HERE.parent / "lyrics.json")).items()}
# "a-mail" is AC's mail; whisper writes it "email" or "a mail"
norm = lambda s: re.sub(r"[^a-z ]", "", re.sub(r"\b(e-?mail|a[- ]mail)", "amail", s.lower())).split()
TMP = Path(tempfile.mkdtemp(prefix="recog-"))


def hear(src, start=None, dur=None, tag="x"):
    f = TMP / f"{tag}.wav"
    cut = ["-ss", f"{max(0, start):.3f}", "-t", f"{dur:.3f}"] if start is not None else []
    subprocess.run([FFMPEG, "-loglevel", "error", "-y", *cut, "-i", str(src), "-ar", "16000", "-ac", "1", str(f)], check=True)
    return subprocess.run(["whisper-cli", "-m", MODEL, "-l", "en", "-np", "-nt", "-f", str(f)], capture_output=True, text=True).stdout.strip().replace("\n", " ")


def recall(want, got):
    a, b = norm(want), norm(got)
    return sum(m.size for m in difflib.SequenceMatcher(None, a, b).get_matching_blocks()), len(a)


def takes():
    import numpy as np, soundfile as sf
    sys.argv = ["voice.py", "--arr=radio"]; sys.path.insert(0, str(HERE))
    import voice as V
    rel = [V.at(0, s) for s in V.HOOK_STEPS]
    table = {} if "--force" in sys.argv or not (OUT / "recog-takes.json").exists() else json.load(open(OUT / "recog-takes.json"))
    for d in sorted(V.LIB.parents[1].glob("*-phonemes/phrases")):
        who = d.parent.name.replace("-phonemes", ""); octv = -2 if who == "prutti" else -1
        for meta in sorted(d.glob("*.json")):
            line = meta.stem.split("-")[0]
            if f"{who}/{meta.stem}" in table: continue  # already heard (--force to hear everything again)
            st = list(rel); mid = [V.KEY[V.HOOK[i]] + 12 * octv for i in range(10)]
            st, mid = st[:NSYL[line]], mid[:NSYL[line]]  # a shorter line stops on an earlier note
            if NSYL[line] == 11: st = [V.at(0, -3)] + st; mid = [V.KEY["a"] + 12 * octv] + mid
            _, y = V.sing_phrase(who, meta.stem, [t - st[0] for t in st], mid, 8 * V.S16 + 0.45)
            f = TMP / f"{who}-{meta.stem}-solo.wav"
            sf.write(f, np.concatenate([np.zeros(16000), y * 0.5, np.zeros(16000)]), V.PFS)
            got = hear(f, tag=f"{who}-{meta.stem}"); h, n = recall(TEXT[line], got)
            table[f"{who}/{meta.stem}"] = dict(recall=h / n, heard=got)
            print(f"{'ok ' if h == n else 'NO '} {who:<8}{meta.stem:<16}{h}/{n}  {got}", flush=True)
    json.dump(table, open(OUT / "recog-takes.json", "w"), indent=1)
    print(OUT / "recog-takes.json")


def arrangement(arr, floor):
    plan = json.load(open(OUT / f"vocal-{arr}-lyrics.json"))
    stem, mast = OUT / f"vocal-{arr}-lyric.wav", OUT / f"aesthetina-{arr}-master.flac"
    bar = 240 / json.load(open(HERE.parent / "arrangements" / f"{arr}.json")).get("bpm", 128)
    bad, tot, rows = 0, 0.0, []
    for i, p in enumerate(plan):
        want = TEXT[p["slug"].split("-")[0]]
        if p["reveal"]: want = " ".join(want.split()[: p["reveal"]])  # the opening words are one syllable each
        dur = 4.3 if not p["reveal"] else 2.6
        s = hear(stem, p["t"] - 0.45, dur, f"s{i}"); hs, n = recall(want, s)
        m = hear(mast, p["t"] - 0.45, dur, f"m{i}") if mast.exists() else ""; hm, _ = recall(want, m)
        bad += hs < n; tot += hm / n
        rows.append(dict(bar=round(p["t"] / bar, 2), slug=p["slug"], voice=p["voice"], stem=hs / n, master=hm / n, heard_stem=s, heard_master=m))
        print(f"{'ok ' if hs == n else 'NO '} bar {p['t'] / bar:5.1f} {p['voice']:<8}{p['slug']:<16}stem {hs}/{n} · master {hm}/{n}  {m}", flush=True)
    mean = tot / len(plan)
    json.dump(dict(mean_master=mean, lines=rows), open(OUT / f"recog-{arr}.json", "w"), indent=1)
    print(f"stem: {len(plan) - bad}/{len(plan)} lines whole · master mean word recall {mean:.2f} (floor {floor})")
    return 1 if bad or mean < floor else 0


if __name__ == "__main__":
    arr = next((a[6:] for a in sys.argv if a.startswith("--arr=")), None)
    floor = float(next((a[6:] for a in sys.argv if a.startswith("--min=")), 0.6))
    sys.exit(takes() if arr is None else arrangement(arr, floor))
