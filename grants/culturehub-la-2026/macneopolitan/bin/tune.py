#!/usr/bin/env python3
"""tune.py — is the singing in tune, and does it harmonize with the instruments?

    pop/.venv/bin/python bin/tune.py scores/play.json --sung DIR \
        --manifest hear/best-cast.json --manifest hear/ho-before.json [--tag tune]

For every sung line of every score in the setlist: the rendered WAV's f0 track
(WORLD harvest, 5 ms) is laid against the line's written notes; each note's
measured pitch is the median voiced f0 over its middle 60 %. Reports, per note,
the error in cents; per line and per member, mean |cents|, notes within 50
cents, and octave slips (|error| within 100 cents of ±1200). Then harmony:
at every moment a sung note sounds, the intervals it MAKES against the
whistle/drone notes sounding in the score at that beat, and against the other
members' measured pitch when two sing at once — compared with the interval the
score WROTE. A note is flagged when the sounding interval differs from the
written one by more than 50 cents. Output: hear/<tag>.json + a table.
"""
import argparse, json, math, sys
from pathlib import Path
import numpy as np, soundfile as sf
import warnings; warnings.filterwarnings("ignore")
import pyworld as pw

ap = argparse.ArgumentParser()
ap.add_argument("play"); ap.add_argument("--sung", required=True); ap.add_argument("--manifest", action="append", default=[])
ap.add_argument("--tag", default="tune"); ap.add_argument("--tol", type=float, default=50)
A = ap.parse_args()
LANE = Path(__file__).resolve().parent.parent
ALIAS = {"blush": "frisbee", "third": "frisbee"}
play = json.load(open(A.play)); MEMBERS = play.get("members", ["neo", "blueberry", "frisbee"])
MAN = {}
for mp in A.manifest:
    for s in json.load(open(mp))["scores"]:
        for v in s["voices"]:
            m = ALIAS.get(v["member"], v["member"])
            MAN.setdefault((s["score"], m), [{"text": l["text"], "lyrics": l.get("lyrics", ""), "notes": l["notes"], "wav": Path(A.sung) / Path(s["score"]).stem / v["member"] / Path(l["wav"]).name} for l in v["lines"] if l.get("wav")])
toks = lambda s: [t for t in str(s or "").split(",") if t]
dur = lambda t: float(t.split(":")[1])
def midi(t): k = t.split(":")[0]; return int(k) if k.isdigit() else None
hz = lambda m: 440 * 2 ** ((m - 69) / 12)
cents = lambda f, m: 1200 * math.log2(f / hz(m)) if f > 0 else float("nan")
def member_of(v, i):
    n = str(v.get("name", "")).split("·")[0].split()[0].strip(); n = ALIAS.get(n, n)
    return n if n in MEMBERS else MEMBERS[min(i, len(MEMBERS) - 1)]
IV = {0: "unison", 1: "m2", 2: "M2", 3: "m3", 4: "M3", 5: "P4", 6: "tritone", 7: "P5", 8: "m6", 9: "M6", 10: "m7", 11: "M7"}
def iv_name(semis): return IV[int(round(semis)) % 12]

def f0_track(wav):
    y, sr = sf.read(str(wav), dtype="float64")
    if y.ndim > 1: y = y.mean(1)
    f0, t = pw.harvest(y, sr, f0_floor=60, f0_ceil=1200, frame_period=5.0)
    return f0, t


def line_spans(voice):
    """(start_beat, end_beat, note_tokens, lyric) per sung line of a voice: the
    lyrics split on ' / ', one syllable per note ('-' joins a word's syllables),
    rests skipped. Independent of the hear manifest, whose per-line `notes`
    are cumulative."""
    lines = [l.strip() for l in str(voice.get("lyrics", "")).split("/") if l.strip()]
    need = [sum(len(tok.split("-")) for tok in l.split()) for l in lines]
    vt = toks(voice.get("notes")); spans = []; i = 0; b = 0.0
    for l, n in zip(lines, need):
        while i < len(vt) and midi(vt[i]) is None: b += dur(vt[i]); i += 1
        s = b; seg = []; got = 0
        while i < len(vt) and got < n:
            seg.append(vt[i]); b += dur(vt[i])
            if midi(vt[i]) is not None: got += 1
            i += 1
        spans.append((s, b, seg, l))
    return spans

def place_lines(voice_notes, lines):
    vt = toks(voice_notes); cur = 0; at = [0.0]
    for t in vt: at.append(at[-1] + dur(t))
    out = []
    for l in lines:
        lt = toks(l["notes"]); found = None
        for i in range(cur, len(vt) - len(lt) + 1):
            if vt[i:i + len(lt)] == lt: found = i; break
        if found is None: found = cur
        out.append(at[found]); cur = found + len(lt)
    return out

report = {"tag": A.tag, "tol_cents": A.tol, "scores": [], "members": {}}
rows = []
for item in play["items"]:
    if "score" not in item: continue
    score = json.load(open(LANE / "scores" / item["score"])); bpm = float(score.get("bpm", 120)); spb = 60 / bpm
    voices = score.get("voices", [])
    # every instrument note in the score, as (start_beat, end_beat, midi, label)
    inst = []
    for i, v in enumerate(voices):
        m = member_of(v, i)
        tracks = [(k, v[k], 0) for k in ("notes2", "notes3", "notes4") if v.get(k)]
        if v.get("lyrics") and v.get("double"): tracks.append(("double", v["notes"], v.get("doubleTranspose", 24)))
        if not v.get("lyrics"): tracks.append(("notes", v["notes"], 0))
        for k, notes, tr in tracks:
            b = 0.0
            for tok in toks(notes):
                d = dur(tok); mm = midi(tok)
                if mm is not None: inst.append((b, b + d, mm + tr, f"{m}/{k}"))
                b += d
    # every sung note: measured
    sung = []   # (start_beat, end_beat, member, written, measured_hz, cents, text)
    entry = {"score": item["score"], "bpm": bpm, "lines": []}
    for i, v in enumerate(voices):
        if not v.get("lyrics"): continue
        m = member_of(v, i); lines = MAN.get((item["score"], m), [])
        for k, (s0, e0, seg, lyr) in enumerate(line_spans(v)):
            l = next((x for x in lines if x.get("lyrics", "").strip() == lyr), lines[k] if k < len(lines) else None)
            if not l or not l["wav"].exists(): continue
            l = dict(l, text=lyr.replace("-", ""))
            f0, t = f0_track(l["wav"]); b = 0.0; notes_out = []
            lead = max(0.0, (t[-1] + 0.005) - (e0 - s0) * spb)   # leading silence = the rest before the line
            t = t - lead
            for tok in seg:
                d = dur(tok); mm = midi(tok)
                if mm is not None:
                    a, z = b * spb, (b + d) * spb; mid = (a + 0.2 * (z - a), z - 0.2 * (z - a))
                    sel = f0[(t >= mid[0]) & (t <= mid[1]) & (f0 > 0)]
                    meas = float(np.median(sel)) if len(sel) >= 3 else 0.0
                    c = cents(meas, mm) if meas else float("nan")
                    notes_out.append({"beat": s0 + b, "written": mm, "hz": round(meas, 1), "cents": None if math.isnan(c) else round(c, 1)})
                    sung.append((s0 + b, s0 + b + d, m, mm, meas, c, l["text"]))
                b += d
            cs = [n["cents"] for n in notes_out if n["cents"] is not None]
            pairs = [(n["written"], 69 + 12 * math.log2(n["hz"] / 440)) for n in notes_out if n["cents"] is not None and n["hz"] > 0]
            wr = [p[0] for p in pairs]; me = [p[1] for p in pairs]
            w_range = (max(wr) - min(wr)) if wr else 0; m_range = (max(me) - min(me)) if me else 0
            follow = float(np.corrcoef(wr, me)[0, 1]) if len(pairs) >= 3 and w_range > 0 and np.std(me) > 0 else None
            entry["lines"].append({"member": m, "text": l["text"], "notes": notes_out,
                                   "written_range_semis": w_range, "measured_range_semis": round(m_range, 1), "contour_follow": None if follow is None else round(follow, 2),
                                   "mean_abs_cents": round(float(np.mean(np.abs(cs))), 1) if cs else None,
                                   "within_tol": sum(1 for c in cs if abs(c) <= A.tol), "measured": len(cs), "unvoiced": len(notes_out) - len(cs),
                                   "octave_slips": sum(1 for c in cs if abs(abs(c) - 1200) <= 100)})
    # harmony: written vs sounding intervals
    flags = []
    for (a, z, m, mm, meas, c, text) in sung:
        if not meas or math.isnan(c): continue
        for (ia, iz, im, lab) in inst:
            if ia < z and iz > a and not lab.startswith(m + "/double"):
                written = (mm - im) % 12; sounding = (12 * math.log2(meas / hz(im))) % 12
                diff = min(abs(sounding - written), 12 - abs(sounding - written)) * 100
                if diff > A.tol: flags.append({"beat": round(a, 2), "member": m, "text": text, "against": lab, "written": iv_name(mm - im), "sounding": iv_name(sounding), "off_cents": round(diff)})
        for (a2, z2, m2, mm2, meas2, c2, text2) in sung:
            if m2 <= m or not meas2 or math.isnan(c2) or not (a2 < z and z2 > a): continue
            written = (mm - mm2) % 12; sounding = (12 * math.log2(meas / meas2)) % 12
            diff = min(abs(sounding - written), 12 - abs(sounding - written)) * 100
            if diff > A.tol: flags.append({"beat": round(max(a, a2), 2), "member": m, "text": text, "against": m2 + " (sung)", "written": iv_name(mm - mm2), "sounding": iv_name(sounding), "off_cents": round(diff)})
    entry["harmony_flags"] = flags; entry["harmony_pairs_checked"] = sum(1 for s in sung for i in inst if i[0] < s[1] and i[1] > s[0])
    report["scores"].append(entry)
    for ln in entry["lines"]:
        rows.append((item["score"], ln["member"], ln["text"][:34], ln["mean_abs_cents"], ln["within_tol"], ln["measured"], ln["unvoiced"], ln["octave_slips"], ln["written_range_semis"], ln["measured_range_semis"], ln["contour_follow"]))

# per member
for m in MEMBERS:
    cs = [n["cents"] for s in report["scores"] for l in s["lines"] if l["member"] == m for n in l["notes"] if n["cents"] is not None]
    if cs: report["members"][m] = {"notes": len(cs), "mean_abs_cents": round(float(np.mean(np.abs(cs))), 1), "median_cents": round(float(np.median(cs)), 1), "within_tol_pct": round(100 * sum(1 for c in cs if abs(c) <= A.tol) / len(cs)), "octave_slips": sum(1 for c in cs if abs(abs(c) - 1200) <= 100)}
out = LANE / "hear" / f"{A.tag}.json"; json.dump(report, open(out, "w"), indent=1)

print(f"{'score':26} {'member':9} {'line':34} {'|¢|':>5} {'ok':>5} {'unv':>3} {'8ve':>3}  {'range wrote/sang':>16} {'follow':>6}")
for r in rows: print(f"{r[0][:26]:26} {r[1]:9} {r[2]:34} {str(r[3]):>5} {r[4]:>2}/{r[5]:<2} {r[6]:>3} {r[7]:>3}  {r[8]:>7}/{r[9]:<8} {str(r[10]):>6}")
fl = [l["contour_follow"] for sc in report["scores"] for l in sc["lines"] if l["contour_follow"] is not None]
rr = [l["measured_range_semis"] / l["written_range_semis"] for sc in report["scores"] for l in sc["lines"] if l["written_range_semis"]]
print(f"\ncontour: {len(fl)} lines; the measured pitch follows the written melody with median r = {np.median(fl):.2f} (1 = exactly, 0 = not at all); sung range is {100*np.median(rr):.0f}% of the written range (median)")
print("\nper member (all sung notes):")
for m, d in report["members"].items(): print(f"  {m:9} {d['notes']:4} notes  mean |{d['mean_abs_cents']}¢|  median {d['median_cents']:+}¢  within ±{A.tol:.0f}¢: {d['within_tol_pct']}%  octave slips: {d['octave_slips']}")
nf = sum(len(s["harmony_flags"]) for s in report["scores"]); nc = sum(s["harmony_pairs_checked"] for s in report["scores"])
print(f"\nharmony: {nc} sung-note × instrument overlaps checked, {nf} where the sounding interval is off the written one by > {A.tol:.0f}¢")
for s in report["scores"]:
    for f in s["harmony_flags"][:4]: print(f"  {s['score'][:24]:24} beat {f['beat']:>6} {f['member']:9} vs {f['against']:20} wrote {f['written']:8} sounds {f['sounding']:8} ({f['off_cents']}¢)  “{f['text'][:30]}”")
    if len(s["harmony_flags"]) > 4: print(f"  {s['score'][:24]:24} … {len(s['harmony_flags']) - 4} more")
print(f"\n→ {out.relative_to(LANE)}")
