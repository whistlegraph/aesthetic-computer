#!/usr/bin/env python3
"""melodyproof.py — does the take sing the melody we wrote down?

For every take: measure the sung pitch inside each syllable's boundary,
fit the take's register to the notepat ground truth (melody.json), and
report the miss per syllable — as cents, and as a notepat line you can
read against `c g c c c · c h c c c · g f e d · e e d c c c`.

Two stages per take. The SET (~/.cache/ac/imab/takes/<take>/audio.mp3) is
vocalset's note-locked, tempo-fit output — measuring it proves the render.
The RAW stage measures the performance itself (the demucs vocal stem, or
the raw wav), with the set's boundaries warped back onto it by DTW. The
raw stage is the one that answers "does he sing what we wrote down".

Boundaries come from three places, all measured and all shown:
  drawn    processed-boundaries-<take>.json / boundaries-drawn-<take>.json
           (@jeffrey's hand, the truth where it exists)
  dtw      the LEAD take's drawn boundaries warped onto this take by a
           DTW of MFCC+RMS (same voice, same words — timbre aligns even
           when the register doesn't). Written to boundaries-dtw-<take>.json
           so syllawizard can seed from it.
  syllnote whisper word spans from toolchain/whistlegraph/downloads,
           split into syllables (the source of the old mistakes)

  pop/.venv/bin/python pop/imab/bin/melodyproof.py            # all six takes
  pop/.venv/bin/python pop/imab/bin/melodyproof.py 7427…079   # one
  → prints the tables, writes out/melodyproof.html + out/melodyproof.json
"""
import json, os, re, sys, difflib
from pathlib import Path
import numpy as np
import librosa

HERE = Path(__file__).resolve().parent
LANE = HERE.parent
REPO = LANE.parent.parent
OUT = LANE / "out"
WORK = Path(os.environ.get("HOME", "")) / ".cache/ac/imab"
DL = REPO / "toolchain/whistlegraph/downloads"

LEAD = "7311159624588070175"
TAKES_ALL = sorted(p.name for p in (WORK / "takes").iterdir() if (p / "audio.mp3").exists())

SR, HOP = 22050, 256
FMIN, FMAX = 65.0, 900.0          # G#2 registers exist (7335…) — reach down to C2
PC_TOL, STRICT_TOL = 60, 60       # cents

NAMES = ["C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B"]
LOW = ["C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B"]        # octave below tonic (our convention)
MID = ["c", "c#", "d", "d#", "e", "f", "f#", "g", "g#", "a", "a#", "b"]        # notepat first octave
HIGH = ["h", "h#", "i", "i#", "j", "k", "k#", "l", "l#", "m", "m#", "n"]       # notepat second octave


def to_midi(name):
    m = re.match(r"^([A-G]#?)(-?\d)$", name)
    return (int(m.group(2)) + 1) * 12 + NAMES.index(m.group(1))


def nname(m):
    m = int(round(m))
    return NAMES[m % 12] + str(m // 12 - 1)


def tonic_name_(m):
    """C3 +40¢ style — the register a take was really sung in."""
    r = int(round(m)); c = int(round((m - r) * 100))
    return nname(r) + (f" {c:+d}¢" if c else "")


def degree_letter(deg):
    """semitones above the fitted tonic → notepat letter (uppercase = octave below)."""
    if deg is None:
        return "?"
    if deg < -12 or deg >= 24:
        return "!"
    if deg < 0:
        return LOW[deg % 12]
    if deg < 12:
        return MID[deg]
    return HIGH[deg - 12]


MELODY = json.load(open(LANE / "melody.json"))
SYLLS = MELODY["sylls"]
TARGET = np.array([to_midi(s["note"]) for s in SYLLS], dtype=float)
TONIC = to_midi(MELODY["tonic"])
TEMPLATE_WORDS = ["i'm", "a", "butterfly", "flapping", "for", "you", "guys",
                  "just", "a", "costume", "i", "put", "on", "in", "my", "room"]
# syllable count per template word, from melody.json
SYL_PER_WORD = [sum(1 for s in SYLLS if s["wi"] == wi) for wi in range(len(TEMPLATE_WORDS))]


# ── audio + pitch ────────────────────────────────────────────────────────

def load_take(take):
    y, _ = librosa.load(WORK / "takes" / take / "audio.mp3", sr=SR, mono=True)
    return y


def pitch_track(y):
    """WORLD harvest + stonemask, 5 ms frames. pyin voices only a third of a
    demucs stem's sung frames; harvest voices ~85% (measured on the lead,
    2026-09-13) and is what the rest of the lane's WORLD chain hears."""
    import pyworld as pw
    y64 = y.astype(np.float64)
    f0, t = pw.harvest(y64, SR, f0_floor=FMIN, f0_ceil=FMAX, frame_period=5.0)
    f0 = pw.stonemask(y64, f0, t, SR)
    # breath and room: drop frames quieter than a tenth of the sung level
    rms = librosa.feature.rms(y=y, frame_length=1024, hop_length=int(SR * 0.005))[0]
    rms = np.interp(t, np.arange(len(rms)) * 0.005, rms)
    voiced = f0 > 0
    gate = rms > 0.1 * np.median(rms[voiced]) if voiced.any() else voiced
    midi = np.full(f0.shape, np.nan)
    ok = voiced & gate
    midi[ok] = 69 + 12 * np.log2(f0[ok] / 440.0)
    return t, midi


# ── boundaries ───────────────────────────────────────────────────────────

def _read_bounds(name):
    p = LANE / name
    if not p.exists():
        return None
    by = {}
    for s in json.load(open(p))["sylls"]:
        by.setdefault((s["label"], s["wi"]), s)
    return [([b["fromMs"], b["toMs"]] if (b := by.get((s["label"], s["wi"]))) else None) for s in SYLLS]


def bounds_drawn(take):
    """@jeffrey's hand in the SET timebase (vocalset's stretched, note-locked wav)."""
    name = f"processed-boundaries-{take}.json"
    b = _read_bounds(name)
    return (b, name) if b else (None, None)


def bounds_drawn_raw(take):
    """@jeffrey's hand in the RAW timebase (the wizard run on the stem itself)."""
    name = f"boundaries-drawn-{take}.json"
    b = _read_bounds(name)
    return (b, name) if b else (None, None)


def norm_word(w):
    return re.sub(r"[^a-z']", "", w.lower())


def bounds_syllnote(take):
    p = DL / f"whistlegraph-{take}.syllnote.json"
    if not p.exists():
        return None
    words = json.load(open(p))["words"]
    got = [norm_word(w["text"]) for w in words]
    sm = difflib.SequenceMatcher(a=TEMPLATE_WORDS, b=got, autojunk=False)
    wmap = {}
    for tag, i1, i2, j1, j2 in sm.get_opcodes():
        if tag == "equal":
            for k in range(i2 - i1):
                wmap[i1 + k] = j1 + k
        elif tag == "replace" and (i2 - i1) == (j2 - j1):
            for k in range(i2 - i1):
                wmap[i1 + k] = j1 + k
    out = []
    for wi, n in enumerate(SYL_PER_WORD):
        if wi not in wmap:
            out += [None] * n
            continue
        w = words[wmap[wi]]
        a, b = w["fromMs"], w["toMs"]
        nuc = w.get("nuclei") or []
        if n == 1:
            out.append([a, b])
        elif len(nuc) == n:
            edges = [a] + [int(round(x["startSec"] * 1000)) for x in nuc[1:]] + [b]
            out += [[edges[i], edges[i + 1]] for i in range(n)]
        else:
            step = (b - a) / n
            out += [[int(a + i * step), int(a + (i + 1) * step)] for i in range(n)]
    return out


def dtw_features(y):
    hop = 512
    mf = librosa.feature.mfcc(y=y, sr=SR, n_mfcc=20, hop_length=hop)
    mf = (mf - mf.mean(1, keepdims=True)) / (mf.std(1, keepdims=True) + 1e-6)
    rms = librosa.feature.rms(y=y, hop_length=hop)
    rms = np.log(rms + 1e-5)
    rms = (rms - rms.mean()) / (rms.std() + 1e-6)
    return np.vstack([mf, rms * 2.0]), hop


def bounds_dtw(lead_y, lead_bounds, y):
    """warp the lead's drawn boundaries onto this take (ms → ms)."""
    X, hop = dtw_features(lead_y)
    Y, _ = dtw_features(y)
    _, wp = librosa.sequence.dtw(X=X, Y=Y, metric="euclidean")
    wp = wp[::-1]                       # (lead_frame, take_frame), ascending
    lead_f, take_f = wp[:, 0], wp[:, 1]
    # for each lead frame, the median take frame it maps to (a monotone function)
    n = X.shape[1]
    mapped = np.zeros(n)
    for i in range(n):
        sel = take_f[lead_f == i]
        mapped[i] = np.median(sel) if len(sel) else (mapped[i - 1] if i else 0)
    mapped = np.maximum.accumulate(mapped)

    def warp_ms(ms):
        fr = ms / 1000 * SR / hop
        i0 = int(np.clip(np.floor(fr), 0, n - 1)); i1 = int(np.clip(i0 + 1, 0, n - 1))
        frac = fr - i0
        m = mapped[i0] * (1 - frac) + mapped[i1] * frac
        return int(round(m * hop / SR * 1000))

    return [[warp_ms(a), warp_ms(b)] if b else None for a, b in
            [(x[0], x[1]) if x else (None, None) for x in lead_bounds]]


def bounds_notealign(wav):
    """neo's forced alignment: DTW the written note sequence against the f0
    track; every syllable gets a contiguous span. It finds WHERE, given the
    melody — so its verdicts lean toward agreement and are shown as such."""
    import subprocess, tempfile
    notes = [{"label": s_["label"], "note": s_["note"]} for s_ in SYLLS]
    with tempfile.NamedTemporaryFile("w", suffix=".json", delete=False) as f:
        json.dump(notes, f); nf = f.name
    try:
        r = subprocess.run([sys.executable, str(HERE / "notealign.py"), str(wav), nf],
                           capture_output=True, text=True, timeout=300)
        data = json.loads(r.stdout or "{}")
        rows = data.get("syllables", data) if isinstance(data, dict) else data
    except Exception as e:                       # noqa: BLE001 — a proposal, never fatal
        print(f"   (notealign failed: {e})"); rows = []
    finally:
        os.unlink(nf)
    if len(rows) != len(SYLLS):
        return None
    return [[int(round(r_["fromMs"])), int(round(r_["toMs"]))] for r_ in rows]


def raw_spec_png(take, y):
    """log-frequency spectrogram strip of the raw stem at 260 px/s (the wizard's scale)."""
    from PIL import Image
    out = WORK / "takes" / take / "raw-spec.png"
    if out.exists():
        return str(out)
    S = np.abs(librosa.stft(y, n_fft=2048, hop_length=int(SR / 260)))
    S = librosa.amplitude_to_db(S, ref=np.max)
    # keep 60 Hz … 4 kHz on a log axis, 520 px tall like the wizard
    freqs = librosa.fft_frequencies(sr=SR, n_fft=2048)
    lo, hi = np.log2(60), np.log2(4000)
    rows = np.logspace(lo, hi, 520, base=2)
    idx = np.clip(np.searchsorted(freqs, rows), 0, len(freqs) - 1)
    img = S[idx][::-1]                         # high frequencies on top
    img = np.clip((img + 70) / 70, 0, 1)       # −70 dB … 0 dB
    # the wizard's ember palette: black → deep red → orange → white
    r = np.clip(img * 2.2, 0, 1); g = np.clip(img * 1.4 - 0.4, 0, 1); b = np.clip(img * 2.0 - 1.4, 0, 1)
    rgb = (np.stack([r, g, b], -1) * 255).astype(np.uint8)
    Image.fromarray(rgb).save(out)
    return str(out)


def raw_audio_path(take):
    """the performance itself: demucs vocal stem if we have it, else the raw wav."""
    for cand in (WORK / "sep/htdemucs" / f"whistlegraph-{take}" / "vocals.wav",
                 DL / f"whistlegraph-{take}.wav", DL / f"{take}.wav"):
        if cand.exists():
            return cand
    return None


# ── measurement ──────────────────────────────────────────────────────────

def measure(t, midi, bounds):
    rows = []
    for b in bounds:
        if not b:
            rows.append(None); continue
        t0, t1 = b[0] / 1000 + 0.02, max(b[1] / 1000, b[0] / 1000 + 0.12)
        sel = (t >= t0) & (t <= t1) & np.isfinite(midi)
        m = midi[sel]
        if len(m) < 3:
            rows.append({"n": int(len(m)), "median": None}); continue
        k = max(1, min(5, len(m) // 4))
        rows.append({
            "n": int(len(m)), "voiced": float(len(m) / max(1, ((t >= t0) & (t <= t1)).sum())),
            "median": float(np.median(m)), "p20": float(np.percentile(m, 20)),
            "p80": float(np.percentile(m, 80)),
            "start": float(np.median(m[:k])), "end": float(np.median(m[-k:])),
        })
    return rows


def fit_register(rows):
    """integer transposition k of the target that best matches the sung medians.
    pitch-class first (octave-blind), then the octave the majority actually sang."""
    meas = np.array([r["median"] if r and r["median"] is not None else np.nan for r in rows])
    w = np.array([r["n"] if r and r["median"] is not None else 0 for r in rows], dtype=float)
    ok = np.isfinite(meas)
    if ok.sum() == 0:
        return 0, {}
    best = None
    for k in range(-36, 13):
        diff = meas[ok] - (TARGET[ok] + k)
        pc = np.abs(((diff + 6) % 12) - 6)
        cost_pc = float((pc * w[ok]).sum())
        cost_strict = float((np.minimum(np.abs(diff), 6.0) * w[ok]).sum())
        cand = (round(cost_pc, 3), cost_strict, k)
        if best is None or cand < best:
            best = cand
    # among k's tied on pitch class (k, k±12 …), the strict cost picked the octave
    k = best[2]
    # refine to cents: the weighted median of the wrapped residuals is the
    # tonic the singer actually held, which is rarely a whole semitone
    diff = meas[ok] - (TARGET[ok] + k)
    resid = ((diff + 6) % 12) - 6
    order = np.argsort(resid)
    cw = np.cumsum(w[ok][order])
    shift = float(resid[order][np.searchsorted(cw, cw[-1] / 2)])
    kf = k + shift
    return kf, {"cost_pc": best[0], "cost_strict": best[1], "shift_cents": int(round(shift * 100))}


def judge(rows, k):
    out = []
    for i, r in enumerate(rows):
        tgt = TARGET[i] + k
        if not r or r["median"] is None:
            out.append({"i": i, "label": SYLLS[i]["label"], "wi": SYLLS[i]["wi"],
                        "target": float(tgt), "targetName": nname(tgt),
                        "sung": None, "cents": None, "pcCents": None,
                        "verdict": "unvoiced", "letter": "?", "truth": degree_letter(int(TARGET[i] - TONIC))})
            continue
        diff = r["median"] - tgt
        cents = diff * 100
        pc = ((diff + 6) % 12) - 6
        pc_cents = pc * 100
        strict = abs(cents) <= STRICT_TOL
        pchit = abs(pc_cents) <= PC_TOL
        if strict:
            v = "hit"
        elif pchit:
            v = "octave"          # right note, wrong octave
        else:
            v = "miss"
        deg = int(round(r["median"] - (TONIC + k)))  # k may be fractional
        out.append({"i": i, "label": SYLLS[i]["label"], "wi": SYLLS[i]["wi"],
                    "target": float(tgt), "targetName": nname(tgt),
                    "sung": r["median"], "sungName": nname(r["median"]),
                    "p20": r["p20"], "p80": r["p80"], "start": r["start"], "end": r["end"],
                    "n": r["n"], "voiced": r["voiced"],
                    "cents": int(round(cents)), "pcCents": int(round(pc_cents)),
                    "octaves": int(round((diff - pc) / 12)),
                    "verdict": v, "letter": degree_letter(deg),
                    "truth": degree_letter(int(TARGET[i] - TONIC))})
    return out


def line_of(js, key="letter"):
    parts, cur_wi = [], None
    for j in js:
        parts.append(j[key])
    # group as the notepat line: 5 · 5 · 4 · 6
    groups = [parts[0:5], parts[5:10], parts[10:14], parts[14:20]]
    return " · ".join(" ".join(g) for g in groups)


# ── main ─────────────────────────────────────────────────────────────────

def main():
    if "--html-only" in sys.argv:            # re-skin the page from the last run's measurements
        write_html(json.load(open(OUT / "melodyproof.json")))
        print(f"✓ {OUT / 'melodyproof.html'} (from melodyproof.json)"); return
    takes = [a for a in sys.argv[1:] if not a.startswith("--")] or TAKES_ALL
    print(f"melody: {MELODY['notepat']}  (tonic {MELODY['tonic']})\n")

    lead_y = load_take(LEAD)
    lead_bounds, lead_src = bounds_drawn(LEAD)
    if lead_bounds is None:
        sys.exit("the lead take has no drawn boundaries — nothing to warp from")

    report = {"melody": MELODY, "generated": __import__("datetime").datetime.now().isoformat(timespec="seconds"),
              "lead": LEAD, "takes": []}
    for take in takes:
        y = lead_y if take == LEAD else load_take(take)
        dur = len(y) / SR
        t, midi = pitch_track(y)

        sources = {}
        drawn, drawn_name = bounds_drawn(take)
        if drawn:
            sources["drawn"] = {"bounds": drawn, "file": drawn_name}
        if take != LEAD:
            dtw = bounds_dtw(lead_y, lead_bounds, y)
            sources["dtw"] = {"bounds": dtw, "file": f"boundaries-dtw-{take}.json"}
            json.dump({"take": take, "derived": report["generated"],
                       "method": f"DTW (MFCC+RMS) of the lead take's {lead_src} onto this take",
                       "sylls": [{"label": s["label"], "wi": s["wi"], "fromMs": b[0], "toMs": b[1], "fLo": 0.15, "fHi": 0.9}
                                 for s, b in zip(SYLLS, dtw) if b]},
                      open(LANE / f"boundaries-dtw-{take}.json", "w"), indent=1)
        sn = bounds_syllnote(take)
        if sn:
            sources["syllnote"] = {"bounds": sn, "file": f"whistlegraph-{take}.syllnote.json"}

        primary = "drawn" if "drawn" in sources else "dtw"
        # register is fitted ONCE per take, on the primary boundaries, then reused
        rows_primary = measure(t, midi, sources[primary]["bounds"])
        k, fit = fit_register(rows_primary)
        tonic_name = tonic_name_(TONIC + k)

        print(f"── take {take}  ({dur:.1f}s) · tonic sung ≈ {tonic_name} (k={k:+.2f})")
        for name, src in sources.items():
            rows = measure(t, midi, src["bounds"])
            js = judge(rows, k)
            src["judged"] = js
            src["hits"] = sum(1 for j in js if j["verdict"] == "hit")
            src["octave"] = sum(1 for j in js if j["verdict"] == "octave")
            src["miss"] = sum(1 for j in js if j["verdict"] == "miss")
            src["unvoiced"] = sum(1 for j in js if j["verdict"] == "unvoiced")
            flag = "★" if name == primary else " "
            print(f" {flag} {name:<9} hit {src['hits']:>2}  octave {src['octave']:>2}  miss {src['miss']:>2}  unvoiced {src['unvoiced']:>2}")
            print(f"             truth  {line_of(js, 'truth')}")
            print(f"             sung   {line_of(js, 'letter')}")
        # per-syllable table for the primary
        js = sources[primary]["judged"]
        print(f"   {'syll':<6}{'target':>7}{'sung':>7}{'cents':>7}  verdict")
        for j in js:
            if j["sung"] is None:
                print(f"   {j['label']:<6}{j['targetName']:>7}{'—':>7}{'—':>7}  unvoiced"); continue
            mark = {"hit": "✓", "octave": f"↕ {j['octaves']:+d} oct", "miss": "✗"}[j["verdict"]]
            print(f"   {j['label']:<6}{j['targetName']:>7}{j['sungName']:>7}{j['cents']:>+6}¢  {mark}")
        print()

        def curve_of(tt_, mm_, step=4):
            return [[round(float(a), 3), (round(float(b), 2) if np.isfinite(b) else None)]
                    for a, b in zip(tt_[::step], mm_[::step])]
        curve = curve_of(t, midi)

        # ── RAW stage: the performance, boundaries warped back from the set ──
        raw = None
        rp = raw_audio_path(take)
        if rp is not None:
            ry, _ = librosa.load(rp, sr=SR, mono=True)
            rt, rmidi = pitch_track(ry)
            rsources = {}
            rdrawn, rdrawn_name = bounds_drawn_raw(take)
            if rdrawn:
                rsources["drawn"] = {"bounds": rdrawn, "file": rdrawn_name}
            rsources["dtw←set"] = {"bounds": bounds_dtw(y, sources[primary]["bounds"], ry),
                                   "file": f"{primary} bounds of the set, DTW'd onto {rp.name}"}
            na = bounds_notealign(rp)
            if na:
                rsources["notealign"] = {"bounds": na, "file": "notealign.py — the WRITTEN melody forced onto the f0 track (a proposal; biased to agree)"}
            # the primary is the boundary set that lands on the most sung frames —
            # a box on silence is wrong whatever the melody says; ties keep the
            # hand first, then the timbre warp, then the forced alignment.
            order = {"drawn": 0, "dtw←set": 1, "notealign": 2}
            def coverage(name):
                rows_ = measure(rt, rmidi, rsources[name]["bounds"])
                return (sum(1 for r_ in rows_ if r_ and r_["median"] is not None),
                        sum(r_["n"] for r_ in rows_ if r_ and r_["median"] is not None), -order[name])
            rprimary = max(rsources, key=coverage)
            rk, rfit = fit_register(measure(rt, rmidi, rsources[rprimary]["bounds"]))
            for name, src in rsources.items():
                js_ = judge(measure(rt, rmidi, src["bounds"]), rk)
                src.update(judged=js_,
                           hits=sum(1 for j in js_ if j["verdict"] == "hit"),
                           octave=sum(1 for j in js_ if j["verdict"] == "octave"),
                           miss=sum(1 for j in js_ if j["verdict"] == "miss"),
                           unvoiced=sum(1 for j in js_ if j["verdict"] == "unvoiced"))
            raw = {"audio": str(rp), "spec": raw_spec_png(take, ry),
                   "duration": round(len(ry) / SR, 3), "k": rk,
                   "tonic": tonic_name_(TONIC + rk), "fit": rfit, "curve": curve_of(rt, rmidi),
                   "sources": rsources, "primary": rprimary}
            print(f"   RAW {rp.name} · tonic sung ≈ {raw['tonic']} (k={rk:+.2f})")
            for name, R in rsources.items():
                flag = "★" if name == rprimary else " "
                print(f"   {flag} {name:<9} hit {R['hits']:>2}  octave {R['octave']:>2}  miss {R['miss']:>2}  unvoiced {R['unvoiced']:>2}")
                print(f"             truth  {line_of(R['judged'], 'truth')}")
                print(f"             sung   {line_of(R['judged'], 'letter')}")
            rjs = rsources[rprimary]["judged"]
            print(f"     {'syll':<6}{'target':>7}{'sung':>7}{'cents':>7}  verdict")
            for j in rjs:
                if j["sung"] is None:
                    print(f"     {j['label']:<6}{j['targetName']:>7}{'—':>7}{'—':>7}  unvoiced"); continue
                mark = {"hit": "✓", "octave": f"↕ {j['octaves']:+d} oct", "miss": "✗"}[j["verdict"]]
                print(f"     {j['label']:<6}{j['targetName']:>7}{j['sungName']:>7}{j['cents']:>+6}¢  {mark}")
            print()
        else:
            print("   (no raw audio locally — set stage only)\n")

        report["takes"].append({
            "raw": raw,
            "take": take, "duration": round(dur, 3), "k": k, "tonic": tonic_name, "fit": fit,
            "primary": primary, "isLead": take == LEAD,
            "audio": str(WORK / "takes" / take / "audio.mp3"),
            "spec": str(WORK / "takes" / take / "spec.png"),
            "curve": curve,
            "sources": {n: {kk: vv for kk, vv in s.items()} for n, s in sources.items()},
        })

    OUT.mkdir(exist_ok=True)
    json.dump(report, open(OUT / "melodyproof.json", "w"))
    write_html(report)
    print(f"✓ {OUT / 'melodyproof.html'}")


# ── the page ─────────────────────────────────────────────────────────────

def write_html(report):
    tpl = open(HERE / "melodyproof.html.tpl").read()
    html = tpl.replace("/*DATA*/", json.dumps(report))
    open(OUT / "melodyproof.html", "w").write(html)


if __name__ == "__main__":
    main()
