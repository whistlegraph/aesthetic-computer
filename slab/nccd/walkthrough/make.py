#!/usr/bin/env python3
"""The NCCd walkthrough video: each scene of scenes.json is one picture of the
real app (taken off-screen with the snapshot aids), one narrated line in
jeffrey's voice, and captions timed from the voice's own word timings.

    ./walkthrough/make.py [work-dir] [out.mp4]      (run from slab/nccd, after ./build.sh)

The pictures show whatever this Mac's NCCd shows: real chats and wiki pages.
The video is for the club, not for the public. WALK_DOC names the wiki page of
scene 10. SKIP_SHOTS=1 keeps the pictures already in the work dir. The voice
is cached by its text, so only a changed line is spoken again.
Needs: the monorepo's pop/bin/say.mjs, ffmpeg, swift."""
import glob, json, os, re, subprocess, sys

HERE = os.path.dirname(os.path.abspath(__file__))
APP = os.path.join(HERE, "..", "build", "NCCd.app", "Contents", "MacOS", "NCCd")
SAY = os.path.join(HERE, "..", "..", "..", "pop", "bin", "say.mjs")
FF = "/opt/homebrew/bin/ffmpeg"          # not the shim on PATH
PROBE = "/opt/homebrew/bin/ffprobe"
work = os.path.abspath(sys.argv[1] if len(sys.argv) > 1 else os.path.join(HERE, "..", "build", "walkthrough"))
out = os.path.abspath(sys.argv[2] if len(sys.argv) > 2 else os.path.join(work, "nccd-walkthrough.mp4"))
for d in ("shots", "say", "cap", "parts"): os.makedirs(os.path.join(work, d), exist_ok=True)
scenes = json.load(open(os.path.join(HERE, "scenes.json")))
BAR = "0x0B2A38"                          # the caption bar; caps.swift fills its strips with the same colour

def tidy(words):
    """The voice is given "N C C D" and "C L I" to say; the captions spell them NCCd and cli."""
    res, i = [], 0
    bare = lambda k: [w["text"].strip(".,") for w in words[i + 1:i + 1 + k]]
    while i < len(words):
        w = words[i]["text"]
        if w == "N" and bare(3) == ["C", "C", "D"]:
            res.append((words[i]["fromMs"], "NCCd" + words[i + 3]["text"][1:])); i += 4
        elif w == "N" and bare(2) == ["C", "C"]:
            res.append((words[i]["fromMs"], "NCC" + words[i + 2]["text"][1:])); i += 3
        elif w == "C" and bare(2)[:1] == ["L"] and words[i + 2]["text"].startswith("I"):
            res.append((words[i]["fromMs"], "cli" + words[i + 2]["text"][1:])); i += 3
        else:
            res.append((words[i]["fromMs"], w)); i += 1
    return res

parts = []
for scene in scenes:
    n = scene["name"]
    shot, mp3 = f"{work}/shots/{n}.png", f"{work}/say/{n}.mp3"
    if not (os.environ.get("SKIP_SHOTS") and os.path.exists(shot)):
        env = dict(os.environ, NCC_SNAPSHOT=shot, NCC_SNAPSHOT_TOP="1", NCC_SNAPSHOT_SIZE="960x600", NCC_SNAPSHOT_AFTER="10")
        env.update({k: os.environ.get("WALK_DOC", "") if v == "$WALK_DOC" else v for k, v in scene["env"].items()})
        subprocess.run([APP, "-appearance", "dark"], env=env, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL, check=True)
    open(f"{work}/say/{n}.txt", "w").write("verse 1\n" + scene["say"] + "\n")
    subprocess.run(["node", SAY, f"{work}/say/{n}.txt", "--out", mp3, "--timestamps", "--speed", "1.05", "--stability", "0.6"],
                   stdout=subprocess.DEVNULL, check=True)
    total = float(subprocess.check_output([PROBE, "-v", "error", "-show_entries", "format=duration", "-of", "csv=p=0", mp3])) + 0.45
    # a caption ends at a comma or a stop, or after seven words
    phrases, cur = [], []
    for ms, w in tidy(json.load(open(mp3 + ".alignment.json"))["words"]):
        cur.append((ms, w))
        if w[-1:] in ".,?!" or len(cur) >= 7: phrases.append(cur); cur = []
    if cur: phrases.append(cur)
    for old in glob.glob(f"{work}/cap/{n}-*"): os.remove(old)
    for k, p in enumerate(phrases):
        open(f"{work}/cap/{n}-{k}.txt", "w").write(" ".join(w for _, w in p).rstrip(","))
    scene["starts"], scene["total"] = [p[0][0] / 1000 for p in phrases], total

subprocess.run(["swift", os.path.join(HERE, "caps.swift"), f"{work}/cap"], check=True)

for scene in scenes:
    n, starts, total = scene["name"], scene["starts"], scene["total"]
    inputs = ["-loop", "1", "-framerate", "30", "-i", f"{work}/shots/{n}.png", "-i", f"{work}/say/{n}.mp3"]
    graph = f"[0:v]scale=1920:1200,pad=1920:1320:0:0:color={BAR}[b0];"
    for k, t0 in enumerate(starts):
        inputs += ["-loop", "1", "-framerate", "30", "-i", f"{work}/cap/{n}-{k}.png"]
        t1 = starts[k + 1] if k + 1 < len(starts) else total
        graph += f"[b{k}][{k + 2}:v]overlay=0:1200:enable='between(t,{t0:.2f},{t1:.2f})'[b{k + 1}];"
    graph += f"[b{len(starts)}]format=yuv420p[v];[1:a]apad=pad_dur=0.45,aresample=48000[a]"
    part = f"{work}/parts/{n}.mp4"
    subprocess.run([FF, "-hide_banner", "-loglevel", "error", "-y"] + inputs + ["-filter_complex", graph, "-map", "[v]", "-map", "[a]",
                    "-t", f"{total:.2f}", "-r", "30", "-c:v", "libx264", "-preset", "medium", "-tune", "stillimage", "-crf", "21",
                    "-c:a", "aac", "-b:a", "160k", part], check=True)
    parts.append(f"file '{part}'")

open(f"{work}/list.txt", "w").write("\n".join(parts) + "\n")
subprocess.run([FF, "-hide_banner", "-loglevel", "error", "-y", "-f", "concat", "-safe", "0", "-i", f"{work}/list.txt",
                "-c", "copy", "-movflags", "+faststart", out], check=True)
print(out)
