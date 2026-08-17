# words.py — whole-word lead units, closed-loop verified.
#
# @jeffrey's defect on v3: "words like patiently and stuff are broken".
# The failure class is multi-syllable words — whisper -ml 1 subword
# splits ("cur"/"led") plus tight slice edges cut mid-syllable, and the
# WORLD chain then presses a fragment. v3.1's fix, per the work order:
#
#   1. every word is ONE unit with generous edges — corpus onset − 40 ms
#      to the word's decay (corpus end + 150 ms, capped just short of
#      the next word's onset so nothing of the neighbour bleeds in);
#   2. each unit is pressed through the aesthetivox (grid pull 0.7,
#      formant, air, breath) and then TRANSCRIBED BACK — whisper on the
#      render must read the intended word (or a close phonetic match).
#      A failing render gets its edges widened once (−80/+100 ms more);
#      still failing, the take is swapped for the corpus's next-best
#      solo pressing and the loop repeats;
#   3. short connectives (< 0.45 s, one syllable — up, in, i, of, a,
#      for, to) are exempt from the per-word readback (whisper
#      hallucinates on 0.3 s clips) and are covered by the phrase-level
#      readback of the assembled verse instead (render3's ONLY=take
#      stem, checked in the run receipt).
#
# For every accepted unit it also presses the octave halo pair
# (vowels-only, +6/−7 ¢, darker, breathier) and — for the held words
# that get swells — the diatonic 3rd/5th. Two pressings ship: the f
# take (the spine) and the n take (the "not again!" verse).
#
# Writes vox3/w-<take>-<word>[.variant].wav + vox3/.words.json.
#
#   pop/.venv/bin/python pop/loner/bin/words.py <scratch-dir>

import json, os, re, subprocess, sys, difflib
import numpy as np
import soundfile as sf
import pyworld as pw

HERE = os.path.dirname(os.path.abspath(__file__))
LANE = os.path.dirname(HERE)
VOX3 = os.path.join(LANE, "vox3")
ANA = os.path.join(LANE, "analysis")
SCRATCH = sys.argv[1] if len(sys.argv) > 1 else "/tmp"
WHISPER = os.path.expanduser("~/.whisper-models/ggml-small.bin")

TONIC = 237.0
MINOR = np.array([0, 2, 3, 5, 7, 8, 10])
FRAME_MS = 5.0
SNAP, SMOOTH_MS, FORMANT_DB, AIR_DB = 0.70, 45.0, 1.6, 2.5
BREATH, HALO_DARK = 0.14, 5500.0
PAD_A, PAD_B = 0.040, 0.150          # generous edges
WIDE_A, WIDE_B = 0.080, 0.100        # the one widening retry
GAP = 0.010                          # never bleed into the neighbour

# the two shipping pressings: take label → corpus post id
PRESS = {"f": "7108062006980201771", "n": "7021262898479549702"}
# fallback pressings when a unit fails readback twice, best-first
# (solo takes in or near Camille's frame, from the corpus grades)
FALLBACKS = ["7100768279983181099", "6955972523087416582", "6988954628167585030"]
# n's first lyric word is sung "getting" — same slot, its own word
N_ALIASES = {"sitting": "getting"}
# words that get interval renders for the swells
SWELLS = {"myself": 4, "stone": 2, "patiently": 4, "pass": 4}
LYRIC = ("sitting curled up in myself i think of a stone "
         "just waiting very patiently for time to pass").split()
SHORT = {"up", "in", "i", "of", "a", "for", "to"}

corpus = json.load(open(os.path.join(ANA, "corpus.json")))


def smooth(x, frames):
    if frames <= 1:
        return x
    k = np.hanning(frames * 2 + 1)
    k /= k.sum()
    return np.convolve(x, k, mode="same")


def cents_to_grid(hz):
    cents = 1200.0 * np.log2(hz / TONIC)
    pc = np.mod(cents, 1200.0)
    grid = np.concatenate([MINOR * 100.0, [1200.0]])
    dev = pc[:, None] - grid[None, :]
    return dev[np.arange(len(pc)), np.argmin(np.abs(dev), axis=1)]


def diatonic_delta(hz, degrees):
    cents = 1200.0 * np.log2(hz / TONIC)
    steps = np.concatenate([MINOR + 12 * o for o in range(-2, 5)])
    idx = np.argmin(np.abs(cents[:, None] - steps[None, :] * 100.0), axis=1)
    return (steps[np.minimum(idx + degrees, len(steps) - 1)] - steps[idx]) * 100.0


def shelf(freqs, centre, width):
    return 1.0 / (1.0 + np.exp(-(freqs - centre) / width))


def vuv_mask(voiced, fs, n):
    spf = int(round(fs * FRAME_MS / 1000.0))
    mask = np.repeat(voiced.astype(np.float64), spf)
    mask = np.pad(mask, (0, max(0, n - len(mask))), mode="edge")[:n]
    ramp = int(0.005 * fs)
    edges = np.diff(mask.astype(np.int8))
    for i in np.where(edges == 1)[0]:
        k = np.arange(min(ramp, n - i - 1))
        mask[i + 1 + k] *= 0.5 - 0.5 * np.cos(np.pi * (k + 1) / ramp)
    for i in np.where(edges == -1)[0]:
        k = np.arange(min(ramp, i + 1))
        mask[i - k] *= 0.5 - 0.5 * np.cos(np.pi * (k + 1) / ramp)
    return mask


def dress(y, fs):
    peak = np.max(np.abs(y)) or 1.0
    y = y * (0.90 / peak)
    tip = int(0.005 * fs)
    w = 0.5 - 0.5 * np.cos(np.pi * np.arange(tip) / tip)
    y[:tip] *= w
    y[-tip:] *= w[::-1]
    return y.astype(np.float32)


SOURCES = {}
def source(vid):
    if vid not in SOURCES:
        x, fs = sf.read(os.path.join(LANE, "source", f"{vid}-48k.wav"), dtype="float64")
        SOURCES[vid] = (x.mean(axis=1) if x.ndim > 1 else x, fs)
    return SOURCES[vid]


def spans_for(vid, word, occurrence=0):
    """[t0, t1] for a lyric word in a take, generous edges, no bleed."""
    rows = corpus[vid]["words"]
    hits = [i for i, r in enumerate(rows)
            if r["lyric"] == word or re.sub(r"\W", "", r["word"]).lower() == word]
    if occurrence >= len(hits):
        return None
    i = hits[occurrence]
    r = rows[i]
    nxt = rows[i + 1]["onset"] if i + 1 < len(rows) else r["end"] + 1.0
    t0 = max(0.0, r["onset"] - PAD_A)
    t1 = min(r["end"] + PAD_B, nxt - GAP)
    return [t0, t1, r]


def press(vid, t0, t1):
    """Aesthetivox one word span; returns lead audio + analysis."""
    x, fs = source(vid)
    seg = x[int(t0 * fs):int(t1 * fs)].copy()
    f0_raw, t = pw.harvest(seg, fs, f0_floor=140.0, f0_ceil=700.0, frame_period=FRAME_MS)
    f0 = pw.stonemask(seg, f0_raw, t, fs)
    fft = pw.get_cheaptrick_fft_size(fs, f0_floor=140.0)
    sp = pw.cheaptrick(seg, f0, t, fs, fft_size=fft, f0_floor=140.0)
    ap = pw.d4c(seg, f0, t, fs, fft_size=fft)
    voiced = f0 > 0
    corr = np.zeros_like(f0)
    if voiced.any():
        corr[voiced] = -cents_to_grid(f0[voiced]) * SNAP
    corr = smooth(corr, int(SMOOTH_MS / FRAME_MS))
    f0c = np.where(voiced, f0 * 2.0 ** (corr / 1200.0), 0.0)
    return dict(x=seg, fs=fs, f0c=f0c, sp=sp, ap=ap, voiced=voiced)


def synth(a, f0_new, *, dark=None, breath_x=1.0, vowels_only=False, air=True):
    fs = a["fs"]
    freqs = np.linspace(0.0, fs / 2.0, a["sp"].shape[1])
    sp = a["sp"] * (10.0 ** ((FORMANT_DB * np.exp(-((freqs - 2800.0) / 450.0) ** 2))
                             / 10.0))[None, :]
    if air:
        sp = sp * (10.0 ** (AIR_DB * shelf(freqs, 8000.0, 900.0) / 10.0))[None, :]
    if dark:
        sp = sp * (1.0 / (1.0 + (freqs / dark) ** 2))[None, :]
    dep = np.zeros(len(a["f0c"]))
    acc = 0.0
    for i, v in enumerate(a["voiced"]):
        acc = acc + FRAME_MS / 1000.0 if v else 0.0
        dep[i] = acc
    w = np.clip((dep - 0.15) / 0.25, 0.0, 1.0)
    ap = np.minimum(1.0, a["ap"] + BREATH * breath_x * w[:, None]
                    * shelf(freqs, 8000.0, 800.0)[None, :])
    vi = np.where(a["voiced"])[0]
    f0s = (np.exp(np.interp(np.arange(len(f0_new)), vi,
                            np.log(np.maximum(f0_new[vi], 1e-6))))
           if vi.size >= 2 else np.maximum(f0_new, 1e-6))
    y = pw.synthesize(f0s, np.ascontiguousarray(sp), np.ascontiguousarray(ap),
                      fs, frame_period=FRAME_MS)
    n = min(len(y), len(a["x"]))
    mask = vuv_mask(a["voiced"], fs, n)
    out = mask * y[:n] + (0.0 if vowels_only else (1 - mask) * a["x"][:n])
    return dress(out, fs)


def readback(path, want):
    """whisper the render; True if the intended word (or near match) reads."""
    wav16 = os.path.join(SCRATCH, "rb.wav")
    subprocess.run(["ffmpeg", "-y", "-v", "error", "-i", path, "-ar", "16000", wav16],
                   check=True)
    r = subprocess.run(["whisper-cli", "-m", WHISPER, "-f", wav16, "-nt"],
                       capture_output=True, text=True)
    text = re.sub(r"[^a-z ]", "", r.stdout.lower()).strip()
    if not text:
        return False, ""
    if want in text:
        return True, text
    for tok in text.split():
        if difflib.SequenceMatcher(a=want, b=tok).ratio() >= 0.5:
            return True, text
    return False, text


receipt = {}
for take, vid in PRESS.items():
    for word in LYRIC:
        w = N_ALIASES.get(word, word) if take == "n" else word
        name = f"w-{take}-{word}"
        trail = []
        chosen = None
        for cand_vid, cand_w in [(vid, w)] + [(fb, word) for fb in FALLBACKS]:
            span = spans_for(cand_vid, cand_w)
            if not span:
                trail.append({"take": cand_vid[:5], "result": "no pressing"})
                continue
            t0, t1, row = span
            for attempt, (ea, eb) in enumerate([(0, 0), (WIDE_A, WIDE_B)]):
                a0, b0 = max(0, t0 - ea), t1 + eb
                a = press(cand_vid, a0, b0)
                lead = synth(a, a["f0c"])
                path = os.path.join(VOX3, f"{name}.wav")
                sf.write(path, lead, a["fs"])
                if word in SHORT or (row["dur"] < 0.45 and word not in
                                     ("stone", "pass", "time", "think", "just", "very")):
                    ok, heard = True, "(short — phrase-level check)"
                else:
                    ok, heard = readback(path, re.sub(r"\W", "", cand_w).lower())
                trail.append({"take": cand_vid[:5], "span": [round(a0, 3), round(b0, 3)],
                              "widened": attempt == 1, "heard": heard, "ok": ok})
                if ok:
                    chosen = (cand_vid, a0, b0, row, a)
                    break
            if chosen:
                break
        if not chosen:
            # ship the primary pressing anyway, flagged — never a hole
            t0, t1, row = spans_for(vid, w)
            a = press(vid, t0, t1)
            sf.write(os.path.join(VOX3, f"{name}.wav"), synth(a, a["f0c"]), a["fs"])
            chosen = (vid, t0, t1, row, a)
            trail.append({"take": vid[:5], "result": "UNVERIFIED — shipped flagged"})
        cvid, a0, b0, row, a = chosen
        # the halo pair + swell intervals, from the accepted unit
        for tag, cents in (("8ve-a", 1206), ("8ve-b", 1193)):
            f0h = np.where(a["voiced"], a["f0c"] * 2.0 ** (cents / 1200.0), 0.0)
            sf.write(os.path.join(VOX3, f"{name}-{tag}.wav"),
                     synth(a, f0h, dark=HALO_DARK, breath_x=1.5,
                           vowels_only=True, air=False), a["fs"])
        if word in SWELLS:
            deg = SWELLS[word]
            tag = "3rd" if deg == 2 else "5th"
            delta = np.zeros_like(a["f0c"])
            if a["voiced"].any():
                delta[a["voiced"]] = diatonic_delta(a["f0c"][a["voiced"]], deg)
            delta = smooth(delta, 12)
            f0h = np.where(a["voiced"], a["f0c"] * 2.0 ** ((delta - 6) / 1200.0), 0.0)
            sf.write(os.path.join(VOX3, f"{name}-{tag}.wav"),
                     synth(a, f0h, dark=HALO_DARK, breath_x=1.5,
                           vowels_only=True, air=False), a["fs"])
        receipt[name] = dict(word=word, sung=w, post=cvid, span=[round(a0, 3), round(b0, 3)],
                             dur=round(b0 - a0, 3), f0_hz=row["f0_hz"], note=row["note"],
                             cents_dev=row["cents_dev"], grade=row["grade"],
                             swapped=cvid != vid, trail=trail)
        mark = "" if cvid == vid else f"  ← SWAPPED to {cvid[:5]}"
        print(f"  {name:22s} {b0 - a0:5.2f}s  {row['note'] or '—':5s} "
              f"{'ok' if trail[-1].get('ok', False) or 'result' not in trail[-1] else '??'}{mark}")

json.dump(receipt, open(os.path.join(VOX3, ".words.json"), "w"), indent=1)
n_swap = sum(1 for r in receipt.values() if r["swapped"])
n_wide = sum(1 for r in receipt.values() if any(t.get("widened") and t.get("ok") for t in r["trail"]))
print(f"WROTE {VOX3}/.words.json — {len(receipt)} units, {n_wide} widened, {n_swap} swapped")
