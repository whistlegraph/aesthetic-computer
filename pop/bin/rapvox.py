#!/usr/bin/env python3
"""
rapvox.py — aesthetivox for rap: a spoken or rapped take, sung-tuned without being slowed.

Aesthetivox (bin/pitchsnap_world.py, aesthetina/bin/voice.py, frothyscreama/bin/aesthetivox-scream.py)
stretches vowels and replaces the pitch. A rap has to keep its speed and its place on the grid, so
rapvox uses the pitch side only and guards the words:

  speed and time    NO global time-compression: the take stays at the speed it was performed. A fast rap
                    is fast because it was rapped fast; if it is not fast enough, record it again. (--fit
                    seconds exists for a take that must be LENGTHENED to a phrase, and refuses to shorten
                    one by more than 3 %.) Every word stays where it was, so a renderer can still cut the
                    result on its word timings and move whole words to the nearest sixteenth
  the chant         in every voiced frame the pitch is replaced with the note in force: a contour of
                    [seconds, midi] (--contour), or made from the word timings (--words) and three
                    notes (--notes chant,lift,cadence): the chant note, the lift on every fourth word
                    and on the word before a sentence ends, the cadence on a sentence's last word.
                    `--nat` of the speaker's own pitch movement rides on the note
  consonants        the take's own band above 4 kHz and its unvoiced frames are laid back in untreated
  the hold          only the last word may be lengthened (--hold seconds: its last vowel is held), so
                    nothing after it moves
  strength          how much of the voice is the tuned one (1 = all tuned, 0 = the take as it was)
  the guard         whisper.cpp reads the fitted take and the result back against the words; if recall
                    falls more than --drop (default 0.1) the strength is lowered (×0.7, ×0.5) and tried
                    again, and if it still falls the take is passed through untreated. The numbers go
                    in <out>.json. (--no-guard skips it.)

  pop/.venv/bin/python pop/bin/rapvox.py --take take.mp3 --words take.json --notes 47,50,52 \\
      --fit 7.2 --strength 0.8 --out rap.f32
  take.json: {"words": [{"w": "scream:", "a": 0.16, "b": 0.71}, …]} (seconds in the unfitted take)
  → rap.f32 (mono float32, 48 k), rap.f32.json {"factor", "strength", "recall_raw", "tried": […], "contour"}
"""
import argparse, difflib, json, os, re, subprocess, sys, tempfile, warnings
warnings.filterwarnings("ignore")
import numpy as np, pyworld as pw
from scipy.signal import butter, sosfilt

SR, FP, FF = 48000, 5.0, "/opt/homebrew/bin/ffmpeg"
MODEL = os.environ.get("WHISPER_MODEL", os.path.expanduser("~/Models/ggml-large-v3-turbo-q5_0.bin"))
ap_ = argparse.ArgumentParser()
for k, d in (("take", None), ("words", None), ("contour", None), ("out", None), ("notes", "47,50,52")):
    ap_.add_argument("--" + k, default=d)
for k, d in (("fit", 0.0), ("strength", 0.8), ("nat", 0.25), ("hold", 0.0), ("drop", 0.1)):
    ap_.add_argument("--" + k, type=float, default=d)
ap_.add_argument("--no-guard", action="store_true")
A = ap_.parse_args()

words = json.load(open(A.words))["words"] if A.words else []
raw0 = np.frombuffer(subprocess.run([FF, "-v", "error", "-i", A.take, "-f", "f32le", "-ac", "1", "-ar", str(SR), "-"], capture_output=True, check=True).stdout, dtype=np.float32)
span = (words[-1]["b"] - words[0]["a"]) if words else len(raw0) / SR
factor = A.fit / span if A.fit else 1.0                       # seconds of result per second of take
if factor < 0.97: sys.exit(f"rapvox: --fit {A.fit} would speed the take up ×{1 / factor:.2f}; rapvox does not compress a voice (give the rap more bars, or record it faster)")
x = raw0 if factor == 1 else np.frombuffer(subprocess.run([FF, "-v", "error", "-i", A.take, "-af", f"atempo={1 / factor:.5f}", "-f", "f32le", "-ac", "1", "-ar", str(SR), "-"], capture_output=True, check=True).stdout, dtype=np.float32)
x = x.astype(np.float64)

if A.contour: contour = json.load(open(A.contour))
else:
    chant, lift, cadence = (int(v) for v in A.notes.split(","))
    ends = lambda w: w["w"][-1] in ".!?"
    contour = [[round(w["a"] * factor, 4), cadence if ends(w) else lift if (i % 4 == 3 or (i + 1 < len(words) and ends(words[i + 1]))) else chant] for i, w in enumerate(words)] or [[0, chant]]

f0, t = pw.harvest(x, SR, f0_floor=65, f0_ceil=420, frame_period=FP)
f0 = pw.stonemask(x, f0, t, SR); sp = pw.cheaptrick(x, f0, t, SR); ap = pw.d4c(x, f0, t, SR)
starts = np.array([c[0] for c in contour]); notes = np.array([c[1] for c in contour], float)
v = f0 > 0; med = np.median(f0[v]) if v.any() else 120.0
src = np.arange(len(f0), dtype=float)
if A.hold > 0 and v.any():                                  # hold the last vowel: its middle frames are read slowly
    b = int(np.where(v)[0][-1]); a = b
    while a > 0 and v[a - 1]: a -= 1
    keep = min(6, (b - a) // 3); extra = int(A.hold * 1000 / FP)
    src = np.r_[src[: a + keep], np.linspace(a + keep, b - keep, max(2, b - a - 2 * keep + extra)), src[b - keep:]]
lo = np.floor(src).astype(int); hi = np.minimum(lo + 1, len(f0) - 1); w_ = (src - lo)[:, None]
sp2 = np.ascontiguousarray(sp[lo] * (1 - w_) + sp[hi] * w_); ap2 = np.ascontiguousarray(ap[lo] * (1 - w_) + ap[hi] * w_)
f0s, v2 = f0[np.round(src).astype(int).clip(0, len(f0) - 1)], v[np.round(src).astype(int).clip(0, len(f0) - 1)]
tt = np.round(src).clip(0, len(t) - 1).astype(int)
target = 440 * 2 ** ((notes[np.clip(np.searchsorted(starts, t[tt], side="right") - 1, 0, len(notes) - 1)] - 69) / 12)
move = np.zeros(len(src)); move[v2] = np.clip(12 * np.log2(f0s[v2] / med), -4, 4)
tuned = pw.synthesize(np.where(v2, target * 2 ** (A.nat * move / 12), 0.0), sp2, ap2, SR, FP)
n = len(tuned); rawx = np.r_[x, np.zeros(max(0, n - len(x)))][:n]          # the take, as long as the result (silent under a hold)
tuned *= np.sqrt(np.mean(x ** 2)) / (np.sqrt(np.mean(tuned ** 2)) + 1e-12)
hop = int(SR * FP / 1000); voiced_s = np.repeat(v2.astype(float), hop)[:n]; voiced_s = np.r_[voiced_s, np.zeros(n - len(voiced_s))]
voiced_s = np.convolve(voiced_s, np.hanning(2 * hop + 1) / np.hanning(2 * hop + 1).sum(), "same")
air = sosfilt(butter(2, 4000, "highpass", fs=SR, output="sos"), rawx)


def mix(s):   # tuned where there is a voice, the take where there is not, the take's own consonant band on top
    return (s * voiced_s) * tuned + (1 - s * voiced_s) * rawx + 0.5 * s * air


norm = lambda s_: re.sub(r"[^a-z0-9 ]", "", s_.lower().replace("-", " ")).split()
want = norm(" ".join(w["w"] for w in words))


def recall(y):
    with tempfile.TemporaryDirectory() as d:
        (y / (np.abs(y).max() + 1e-9) * 0.8).astype(np.float32).tofile(f"{d}/x.f32")
        subprocess.run([FF, "-v", "error", "-y", "-f", "f32le", "-ar", str(SR), "-ac", "1", "-i", f"{d}/x.f32", "-ar", "16000", f"{d}/x.wav"], check=True)
        got = subprocess.run(["whisper-cli", "-m", MODEL, "-l", "en", "-np", "-nt", "-f", f"{d}/x.wav"], capture_output=True, text=True).stdout
    return sum(b.size for b in difflib.SequenceMatcher(None, want, norm(got)).get_matching_blocks()) / max(1, len(want))


report = {"factor": round(factor, 4), "contour": contour, "nat": A.nat, "hold": A.hold, "tried": []}
if A.no_guard or not want:
    s, y = A.strength, mix(A.strength)
else:
    base = recall(rawx); report["recall_raw"] = round(base, 3); s, y = 0.0, rawx
    for cand in (A.strength, A.strength * 0.7, A.strength * 0.5):
        yy = mix(cand); r = recall(yy); report["tried"].append({"strength": round(cand, 2), "recall": round(r, 3)})
        if r >= base - A.drop: s, y = cand, yy; break
report["strength"] = round(s, 2); report["seconds"] = round(n / SR, 3)
(y / (np.abs(y).max() + 1e-9)).astype(np.float32).tofile(A.out)
json.dump(report, open(A.out + ".json", "w"), indent=1)
print(f"rapvox: ×{1 / factor:.2f} speed · {len(contour)} notes · strength {s:.2f}" + (f" · recall {report.get('recall_raw')} → {report['tried'][-1]['recall'] if report['tried'] else '-'}" if want and not A.no_guard else ""))
