"""Map each take's own singing onto the lyric — word timestamps per
take, so segments can time-track the vocal shape of the footage
against the track's wordclock.

whisper-cli (ggml-small, word-level) transcribes every cast take;
tokens fuzzy-match the twenty-word lyric in order. Takes whose audio
yields fewer than four matched words are left out (no usable voice —
they fall back to drawing-clock sync in build.py).

Writes viz/vocal-maps.json: {take_id: [{k, word, start, end}, ...]}.
"""
import json
import os
import re
import subprocess

HERE = os.path.dirname(os.path.abspath(__file__))
LONER = os.path.dirname(os.path.dirname(HERE))
WORK = os.environ.get("PASTICHE_WORK") or os.path.expanduser(
    "~/.cache/ac/pastiche"
)
MODEL = os.path.expanduser("~/.whisper-models/ggml-small.bin")
os.makedirs(f"{WORK}/vmap", exist_ok=True)

LYRIC = ("sitting curled up in myself i think of a stone "
         "just waiting very patiently for time to pass").split()

CAST = ["6988619239657622790", "6955972523087416582",
        "7108062006980201771", "7021262898479549702",
        "7173130377798716714", "7168939549962308906",
        "7168612922757877035", "7230893600219942186",
        "7226114462145695018", "7226226683349798190",
        "7233886426910330158", "7076361738786213166"]


def transcribe(vid):
    wav = f"{WORK}/vmap/{vid}.wav"
    out = f"{WORK}/vmap/{vid}"
    if not os.path.exists(f"{out}.json"):
        subprocess.run(["ffmpeg", "-y", "-v", "error",
                        "-i", f"{LONER}/source/{vid}.mp4",
                        "-ar", "16000", "-ac", "1", wav], check=True)
        subprocess.run(["whisper-cli", "-m", MODEL, "-ml", "1",
                        "-oj", "-of", out, wav],
                       capture_output=True, check=True)
    data = json.load(open(f"{out}.json"))
    words = []
    for seg in data.get("transcription", []):
        token = seg["text"].strip().lower()
        token = re.sub(r"[^a-z']", "", token)
        if not token:
            continue
        t0 = seg["offsets"]["from"] / 1000.0
        t1 = seg["offsets"]["to"] / 1000.0
        words.append((token, t0, t1))
    return words


def match(tokens):
    """Greedy in-order alignment of transcript tokens to the lyric."""
    hits = []
    k = 0
    for token, t0, t1 in tokens:
        for j in range(k, min(k + 3, len(LYRIC))):  # small skip window
            w = LYRIC[j]
            if token == w or (len(token) >= 3 and w.startswith(token)) \
               or (len(w) >= 3 and token.startswith(w)):
                hits.append({"k": j, "word": w,
                             "start": round(t0, 2), "end": round(t1, 2)})
                k = j + 1
                break
        if k >= len(LYRIC):
            break
    return hits


maps = {}
for vid in CAST:
    try:
        hits = match(transcribe(vid))
    except subprocess.CalledProcessError:
        hits = []
    if len(hits) >= 4:
        maps[vid] = hits
        span = f'{hits[0]["start"]:.1f}-{hits[-1]["end"]:.1f}s'
        print(f"{vid}  {len(hits):2d} words  {span}")
    else:
        print(f"{vid}  no usable voice ({len(hits)} matches)")

json.dump(maps, open(f"{LONER}/viz/vocal-maps.json", "w"), indent=1)
print(f"-> viz/vocal-maps.json ({len(maps)} mapped takes)")
