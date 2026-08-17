# corpus.py — the loner word corpus: every word of every take, graded.
#
# @jeffrey: "so lets work on grabbing those samples and analyzing them
# for words and original pitch and such, so we can build harmonies and
# so on" — and, on v3: "we need better word boundaries and to know what
# the words are".
#
# This is the foundation table. All NINETEEN lonr posts, decoded and
# freshly transcribed (whisper.cpp ggml-small, word-level -ml 1), every
# word cross-checked against the known lyric —
#
#     sitting curled up in myself, i think of a stone,
#     just waiting very patiently for time to pass
#
# — onset/end flux-refined on the audio (10 ms RMS envelope, strongest
# rise within ±80 ms of the stamp), and measured: median pyin f0, the
# nearest A#-natural-minor tone in the lane's 237 Hz frame with signed
# cents, voiced fraction, and a quality grade:
#
#   clean    solo word, voiced ≥ 0.55, no clipping
#   breathy  solo word, voiced < 0.55 — air, whisper, or consonant-heavy
#   crowd    a group take — the ensemble material
#   clipped  any sample in the span ≥ 0.985 — never a lead, maybe a layer
#
# Writes:
#   analysis/corpus.json           the machine receipt (tracked)
#   analysis/CORPUS.md             the human table (tracked)
#   samples/corpus/<label>-<i>-<word>.wav   every gradeable word, dressed
#   samples/corpus/.manifest.json  slice provenance (tracked)
#
#   pop/.venv/bin/python pop/loner/bin/corpus.py <scratch-tx-dir>

import json, os, re, sys, difflib
import numpy as np
import soundfile as sf
import librosa

HERE = os.path.dirname(os.path.abspath(__file__))
LANE = os.path.dirname(HERE)
ANA = os.path.join(LANE, "analysis")
CSAMP = os.path.join(LANE, "samples", "corpus")
os.makedirs(ANA, exist_ok=True)
os.makedirs(CSAMP, exist_ok=True)
TX = sys.argv[1]

SR = 48000
TONIC = 237.0
MINOR = [0, 2, 3, 5, 7, 8, 10]
NAMES = ["A#", "C", "C#", "D#", "F", "F#", "G#"]
LYRIC = ("sitting curled up in myself i think of a stone "
         "just waiting very patiently for time to pass").split()

# take label, performer call, and what the post is — desc + earlier
# measurement + the transcripts themselves. `ensemble` marks group sound.
TAKES = {
    "7108062006980201771": ("f",  "camille",  False, "Ten Whistlegraphs / Feral File — the spine"),
    "7021262898479549702": ("n",  "camille",  False, "13.8M 'not again!' — spoken intro is jeffrey, singing is camille"),
    "6988619239657622790": ("o",  "ensemble", True,  "origin — camille with jeffrey and alex"),
    "6988954628167585030": ("s",  "camille",  False, "1.4M solo, lower register"),
    "6955972523087416582": ("em", "camille",  False, "the first post — 'new emograph'"),
    "6974224412614675718": ("cp", "camille",  False, "'composed by @cksuperstore'"),
    "6994920700746206470": ("rq", "unknown",  False, "'we got some Loner requests'"),
    "6996714516234947845": ("pc", "unknown",  False, "'a peaceful, lonely whistlegraph'"),
    "7076361738786213166": ("du", "ensemble", True,  "#duet — emo whistlegraphs again"),
    "7100768279983181099": ("hk", "unknown",  False, "好像塊石頭"),
    "7168612922757877035": ("ls", "unknown",  False, "'loner season'"),
    "7168939549962308906": ("dt", "unknown",  False, "drawing tutorial voiceover"),
    "7173130377798716714": ("un", "unknown",  False, "no caption"),
    "7226114462145695018": ("sn", "unknown",  False, "'so alone theyre in separate notebooks'"),
    "7226226683349798190": ("pf", "unknown",  False, "6.8M 'peaceful and sad'"),
    "7226527805008268586": ("lg", "unknown",  False, "'ladies and gentleman'"),
    "7230893600219942186": ("sp", "unknown",  False, "'spun ya with the sad losers'"),
    "7233760335990230315": ("sh", "unknown",  False, "'our shop is open'"),
    "7233886426910330158": ("rd", "unknown",  False, "'red loner is the best'"),
}

HOP, WIN, SEARCH, RISE_MS, MIN_DB = 0.0025, 0.010, 0.080, 0.015, 1.5


def words_from_tokens(tx):
    """Merge whisper -ml 1 tokens into words with time spans."""
    out = []
    for seg in tx["transcription"]:
        t = seg["text"]
        clean = t.strip()
        if not clean or all(c in ",.?!♪\"'-…()[]" for c in clean):
            continue
        start = seg["offsets"]["from"] / 1000.0
        end = seg["offsets"]["to"] / 1000.0
        if t.startswith(" ") or not out:
            out.append([clean, start, end])
        else:
            out[-1][0] += clean
            out[-1][2] = end
    return [dict(w=w, start=s, end=e) for w, s, e in out if re.sub(r"\W", "", w)]


def lyric_align(words):
    """Map transcript words onto the canonical lyric (fuzzy)."""
    tw = [re.sub(r"\W", "", w["w"]).lower() for w in words]
    sm = difflib.SequenceMatcher(a=LYRIC, b=tw, autojunk=False)
    tag = [None] * len(words)
    for op, a0, a1, b0, b1 in sm.get_opcodes():
        if op in ("equal", "replace"):
            for k in range(min(a1 - a0, b1 - b0)):
                # a replace only counts when it is plausibly the same word
                if op == "equal" or difflib.SequenceMatcher(
                        a=LYRIC[a0 + k], b=tw[b0 + k]).ratio() >= 0.5:
                    tag[b0 + k] = LYRIC[a0 + k]
    return tag


def grid_note(hz):
    cents = 1200.0 * np.log2(hz / TONIC)
    steps = np.array([m + 12 * o for o in range(-3, 5) for m in MINOR])
    k = int(np.argmin(np.abs(cents - steps * 100.0)))
    st = int(steps[k])
    name = NAMES[MINOR.index(((st % 12) + 12) % 12)]
    midi = 58 + st                       # A#3 = MIDI 58 (in the +30¢ frame)
    octv = midi // 12 - 1
    dev = float(cents - st * 100.0)
    return f"{name}{octv}", st, dev


corpus, mrows, manifest = {}, [], {}
for vid, (label, who, ensemble, note) in TAKES.items():
    tx_path = os.path.join(TX, f"{vid}.json")
    if not os.path.exists(tx_path):
        print(f"  ! no transcript for {vid}")
        continue
    x, fs = sf.read(os.path.join(LANE, "source", f"{vid}-48k.wav"), dtype="float64")
    if x.ndim > 1:
        x = x.mean(axis=1)

    # flux envelope for onset refinement
    hop, win = int(HOP * fs), int(WIN * fs)
    ne = (len(x) - win) // hop
    env = np.sqrt(np.array([np.mean(x[i * hop:i * hop + win] ** 2)
                            for i in range(ne)]) + 1e-12)
    db = 20 * np.log10(env)
    lag = max(1, int(RISE_MS / HOP))
    rise = np.concatenate([np.zeros(lag), np.maximum(0, db[lag:] - db[:-lag])])

    def refine(stamp):
        a = max(0, int((stamp - SEARCH) / HOP))
        b = min(ne - 1, int((stamp + SEARCH) / HOP))
        if b <= a:
            return stamp, 0.0
        k = a + int(np.argmax(rise[a:b + 1]))
        return (k * HOP, float(rise[k])) if rise[k] >= MIN_DB else (stamp, float(rise[k]))

    # one pyin pass per take, per-word medians out of it
    f0, vf, vp = librosa.pyin(x.astype(np.float32), fmin=65, fmax=600, sr=fs,
                              frame_length=4096, hop_length=512)
    times = librosa.times_like(f0, sr=fs, hop_length=512)
    good = (vf if vf is not None else np.zeros(len(f0), bool)) & (vp > 0.5)

    words = words_from_tokens(json.load(open(tx_path)))
    tags = lyric_align(words)
    rows = []
    for i, w in enumerate(words):
        onset, _ = refine(w["start"])
        nxt = words[i + 1]["start"] if i + 1 < len(words) else len(x) / fs
        end_stamp = min(w["end"], nxt)
        end, _ = refine(end_stamp)
        if end <= onset:
            end = min(onset + max(0.08, w["end"] - w["start"]), len(x) / fs)
        m = good & (times >= onset) & (times < end)
        v = f0[m]
        v = v[np.isfinite(v)]
        seg = x[int(onset * fs):int(end * fs)]
        vfrac = float(m.sum() / max(1, ((times >= onset) & (times < end)).sum()))
        clipped = bool(np.max(np.abs(seg)) >= 0.985) if seg.size else False
        hz = float(np.median(v)) if v.size >= 3 else None
        notev, st, dev = grid_note(hz) if hz else (None, None, None)
        grade = ("clipped" if clipped else
                 "crowd" if ensemble else
                 "clean" if vfrac >= 0.55 else "breathy")
        row = dict(i=i, word=w["w"], lyric=tags[i],
                   onset=round(onset, 3), end=round(end, 3),
                   dur=round(end - onset, 3),
                   f0_hz=round(hz, 1) if hz else None,
                   note=notev, grid_st=st,
                   cents_dev=round(dev, 1) if dev is not None else None,
                   voiced=round(vfrac, 2), grade=grade,
                   drift_ms=round((onset - w["start"]) * 1000.0, 1))
        rows.append(row)

        # slice the gradeable ones into the bank, with provenance
        if end - onset >= 0.12 and (tags[i] or vfrac >= 0.15):
            key = re.sub(r"\W", "", (tags[i] or w["w"]).lower())[:12] or "x"
            sname = f"{label}-{i:02d}-{key}"
            a0 = max(0, int((onset - 0.004) * fs))
            b0 = min(len(x), int((end + 0.03) * fs))
            segd = x[a0:b0].copy()
            pk = np.max(np.abs(segd)) or 1.0
            segd *= 0.90 / pk
            tip = int(0.005 * fs)
            wnd = 0.5 - 0.5 * np.cos(np.pi * np.arange(tip) / tip)
            segd[:tip] *= wnd
            segd[-tip:] *= wnd[::-1]
            sf.write(os.path.join(CSAMP, f"{sname}.wav"), segd.astype(np.float32), fs)
            manifest[sname] = dict(post=vid, take=label, who=who, ensemble=ensemble,
                                   word=w["w"], lyric=tags[i],
                                   onset=row["onset"], end=row["end"],
                                   f0_hz=row["f0_hz"], note=row["note"],
                                   cents_dev=row["cents_dev"], grade=grade)

    hit = [t for t in tags if t]
    corpus[vid] = dict(label=label, who=who, ensemble=ensemble, what=note,
                       duration=round(len(x) / fs, 2),
                       lyric_words_found=len(set(hit)), lyric_words_total=len(set(LYRIC)),
                       transcript=" ".join(w["w"] for w in words),
                       words=rows)
    print(f"  {label:3s} {vid}  {len(rows):2d} words · lyric {len(set(hit))}/{len(set(LYRIC))} · {who}{' · ENSEMBLE' if ensemble else ''}")
    mrows.append((label, vid, who, ensemble, rows))

json.dump(corpus, open(os.path.join(ANA, "corpus.json"), "w"), indent=1)
json.dump(manifest, open(os.path.join(CSAMP, ".manifest.json"), "w"), indent=1)

# ── the human table ───────────────────────────────────────────────────
md = ["# loner corpus — every word of every take\n",
      "Lyric: *sitting curled up in myself, i think of a stone, just waiting "
      "very patiently for time to pass*\n",
      "Grid: A# natural minor, tonic **237 Hz** (+30¢ over A440). "
      "`cents` is the signed offset from the nearest grid tone. "
      "Grades: clean / breathy / crowd (group take) / clipped.\n"]
for label, vid, who, ensemble, rows in mrows:
    meta = corpus[vid]
    md.append(f"\n## {label} — {vid} ({who}{', ensemble' if ensemble else ''})\n")
    md.append(f"{meta['what']} · {meta['duration']}s · lyric coverage "
              f"{meta['lyric_words_found']}/{meta['lyric_words_total']}\n")
    md.append("| # | word | lyric | onset | end | dur | f0 | note | cents | voiced | grade |")
    md.append("|--:|------|-------|------:|----:|----:|---:|------|------:|-------:|-------|")
    for r in rows:
        md.append(f"| {r['i']} | {r['word']} | {r['lyric'] or '—'} | {r['onset']:.2f} "
                  f"| {r['end']:.2f} | {r['dur']:.2f} | {r['f0_hz'] or '—'} "
                  f"| {r['note'] or '—'} | {r['cents_dev'] if r['cents_dev'] is not None else '—'} "
                  f"| {r['voiced']:.2f} | {r['grade']} |")
open(os.path.join(ANA, "CORPUS.md"), "w").write("\n".join(md) + "\n")
print(f"WROTE {ANA}/corpus.json · {ANA}/CORPUS.md · {len(manifest)} corpus slices")
