#!/usr/bin/env python3
"""preview.py — render the play to an mp4: three screens, the whistle, the sung lines.

    pop/.venv/bin/python bin/preview.py scores/play.json \
        --sung DIR --manifest hear/best-cast.json --manifest hear/ho-before.json \
        --out preview.mp4 [--fps 10]

Sung lines come from WAVs already rendered by Menu Band's singer (bin/hear.mjs
--keep, or the kept dirs on poorslice), located through the hear/*.json
manifests; whistle, drones and percussion are synthesized here; intros and
outros are spoken with `say` in each member's cast voice. Stage directions in
the setlist (`stage`) are drawn as cards and change a screen's lid state.
"""
import argparse, hashlib, json, math, os, re, subprocess, sys, textwrap
from pathlib import Path
import numpy as np
import soundfile as sf
from PIL import Image, ImageDraw, ImageFont

ap = argparse.ArgumentParser()
ap.add_argument("play")
ap.add_argument("--sung", required=True)
ap.add_argument("--manifest", action="append", default=[])
ap.add_argument("--out", default="preview.mp4")
ap.add_argument("--fps", type=int, default=10)
ap.add_argument("--no-video", action="store_true")
ap.add_argument("--quiet", action="store_true", help="omit spoken intros and outros")
ap.add_argument("--audit", action="store_true", help="measure rendered pitches and export isolated stems")
ap.add_argument("--seconds", type=float, default=0, help="render only the first N seconds (a test)")
A = ap.parse_args()

LANE = Path(__file__).resolve().parent.parent
SR = 44100
play = json.load(open(A.play))
PHONEME_ONLY = play.get("phonemeOnly", False)
if "voices" in play:
    play = {"members": ["neo", "blueberry", "frisbee"], "gap": 0,
            "items": [{"score": str(Path(A.play).resolve()), "bit": play["title"].split("—")[-1].strip()}]}
MEMBERS = play.get("members", ["neo", "blueberry", "frisbee"])
ALIAS = {"blush": "frisbee", "third": "frisbee"}
GAP = float(play.get("gap", 4))
CHORUS_FOCUS = len(play["items"]) == 1 and "chorus" in play["items"][0].get("score", "")
CACHE = Path(os.environ.get("TMPDIR", "/tmp")) / "mnp-preview-speech"; CACHE.mkdir(parents=True, exist_ok=True)

# ---- manifests: (score, member) -> lines[{lyrics, notes, wav}]
MAN = {}
for mp in A.manifest:
    j = json.load(open(mp))
    for s in j["scores"]:
        for v in s["voices"]:
            member = ALIAS.get(v["member"], v["member"])
            lines = []
            for l in v["lines"]:
                wav = Path(A.sung) / Path(s["score"]).stem / v["member"] / Path(l["wav"]).name if l.get("wav") else None
                lines.append({"text": l["text"], "lyrics": l["lyrics"], "notes": l["notes"], "spanOffset": l.get("spanOffset"), "wav": wav if wav and wav.exists() else None})
            MAN.setdefault((s["score"], member), lines)

def toks(s): return [t for t in str(s or "").split(",") if t]
def dur(t): return float(t.split(":")[1])
def midi(t):
    k = t.split(":")[0]
    return int(k) if k.isdigit() else None

def member_of(voice, idx):
    name = str(voice.get("name", "")).split("·")[0].split()[0].strip()
    name = ALIAS.get(name, name)
    return name if name in MEMBERS else MEMBERS[min(idx, len(MEMBERS) - 1)]

# ---- synthesis
def whistle(m, secs, vel):
    """A soft whistle: sine with a faint second partial, a gentle 5 Hz vibrato of
    about 8 cents (phase-integrated, so it does not grow with the note), quieter
    as it goes up so the two-octave doubles sit behind the voice."""
    n = int(SR * (secs + 0.08)); t = np.arange(n) / SR
    f = 440 * 2 ** ((m - 69) / 12)
    env = np.minimum(1, t / 0.03) * np.where(t < secs, 1 - 0.15 * np.minimum(1, t / max(secs, 1e-3)), np.maximum(0, 1 - (t - secs) / 0.08))
    depth = 0.0046                                   # ≈ 8 cents
    phase = 2 * np.pi * f * t + (depth * f / 5.0) * np.sin(2 * np.pi * 5.0 * t)
    y = np.sin(phase) + 0.12 * np.sin(2 * phase)
    gain = (vel / 127) * 0.30 * min(1.0, (72 / m) ** 1.6)
    return (y * env * gain).astype(np.float32)
def sine(m, secs, vel):
    t = np.arange(int(SR * (secs + .08))) / SR
    env = np.minimum(1, t / .025) * np.clip((secs + .08 - t) / .08, 0, 1)
    return (np.sin(2*np.pi*(440*2**((m-69)/12))*t) * env * (vel/127) * .3).astype(np.float32)
def kick(vel):
    t = np.arange(int(SR * 0.15)) / SR
    return (np.sin(2 * np.pi * 62 * t) * np.exp(-t * 28) * (vel / 127) * 0.6).astype(np.float32)
def hat(vel):
    t = np.arange(int(SR * 0.05)) / SR
    return (np.random.randn(len(t)) * np.exp(-t * 90) * (vel / 127) * 0.22).astype(np.float32)

def speak(voice, text):
    key = hashlib.sha1(f"{voice}|{text}".encode()).hexdigest()[:16]
    f = CACHE / f"{key}.wav"
    if not f.exists():
        r = subprocess.run(["say", "-v", voice, "-o", str(f), "--data-format=LEI16@44100", text], capture_output=True)
        if r.returncode != 0:
            subprocess.run(["say", "-o", str(f), "--data-format=LEI16@44100", text], capture_output=True)
    y, sr = sf.read(str(f), dtype="float32")
    if y.ndim > 1: y = y.mean(axis=1)
    return y

# ---- the timeline
audio = []           # (t, samples)
audit_events = []
titles = []          # (t0, t1, act, bit)
captions = {m: [] for m in MEMBERS}   # (t0, t1, text)
notes_vis = {m: [] for m in MEMBERS}  # (t0, t1, midi)
pulses = {m: [] for m in MEMBERS}     # t
stage = []           # (t, who, does)
lids = {m: [] for m in MEMBERS}       # (t, up|down)
born = {m: (0.0 if CHORUS_FOCUS else None) for m in MEMBERS}
vox = {m: [] for m in MEMBERS}       # (t, samples) the member's own voice: sung + spoken, for the jaw
syll = {m: [] for m in MEMBERS}      # (t, syllable, hold) onsets, for the mouth's form + karaoke
def put(t, y, m=None):
    audio.append((t, y))
    if m: vox[m].append((t, y))


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
    """start/end beat of each manifest line inside the voice's notes."""
    vt = toks(voice_notes); out = []; cur = 0; beat = 0.0
    beats_at = [0.0]
    for t in vt: beats_at.append(beats_at[-1] + dur(t))
    for l in lines:
        lt = toks(l["notes"]); found = None
        for i in range(cur, len(vt) - len(lt) + 1):
            if vt[i:i + len(lt)] == lt: found = i; break
        if found is None:  # fall back: next non-rest run
            i = cur
            while i < len(vt) and midi(vt[i]) is None: i += 1
            found = i
        s = beats_at[found]; e = beats_at[min(found + len(lt), len(vt))]
        out.append((s, e)); cur = found + len(lt)
    return out

t = 0.0; act = ""
for item in play["items"]:
    if "act" in item: act = item["act"]; continue
    if "card" in item:
        secs = float(item.get("seconds", 60)); titles.append((t, t + secs, act, item["card"]))
        for i, m in enumerate(MEMBERS):
            captions[m].append((t, t + secs, item.get("note", "")))
            mm = [62, 50, 74][i]; put(t + 0.5 * i, whistle(mm, secs - 2 - 0.5 * i, 26)); notes_vis[m].append((t, t + secs - 2, mm))
        t += secs + GAP; continue
    sp = LANE / "scores" / item["score"]; score = json.load(open(sp))
    bpm = float(score.get("bpm", 120)); spb = 60 / bpm
    t_item0 = t
    voices = score.get("voices", [])
    say_voice = {i: v.get("sayVoice") or v.get("singVoice") or "Fred" for i, v in enumerate(voices)}
    def rename(x): return re.sub(r"\bblush\b", "frisbee", x)
    for line in ([] if A.quiet else score.get("intro", []) or []):
        i = line["voice"]; txt = rename(line["text"]); y = speak(say_voice.get(i, "Fred"), txt); m = member_of(voices[i], i); put(t, y * 0.9, m); captions[m].append((t, t + len(y) / SR, "“" + txt + "”")); t += len(y) / SR + 0.4
    down = t
    # optional cut: `lines` N → end where the first sung voice's Nth line ends
    cut_beat = None
    if item.get("lines"):
        for i, v in enumerate(voices):
            if v.get("lyrics"):
                sp_ = line_spans(v)
                if len(sp_) >= item["lines"]: cut_beat = sp_[item["lines"] - 1][1] + 1.0
                break
    max_beat = 0.0
    line_pos = {}
    for i, v in enumerate(voices):
        m = member_of(v, i)
        tracks = []
        if v.get("lyrics"):
            lines = MAN.get((Path(item["score"]).name, m), [])
            spans = line_spans(v); line_pos[m] = [(a, z) for a, z, _, _ in spans]
            for k, (s, e, seg, lyr) in enumerate(spans):
                if cut_beat and s >= cut_beat: break
                # Repeated backing syllables can carry different notes. Match
                # by line position, never by the first identical lyric string.
                l = lines[k] if k < len(lines) else None
                role = v.get("lineRoles", ["lead"] * len(spans))[k]
                line_gain = .9 * v.get("lineGains", [1] * len(spans))[k]
                if l and l["wav"]:
                    if A.audit:
                        rendered_notes = toks(l["notes"])
                        while rendered_notes and midi(rendered_notes[0]) is None: rendered_notes.pop(0)
                        if l["lyrics"].strip() != lyr or rendered_notes != seg:
                            raise ValueError(f"Stale sung render: {m} / {lyr}; render this score again")
                    y, sr = sf.read(str(l["wav"]), dtype="float32")
                    if y.ndim > 1: y = y.mean(axis=1)
                    if sr != SR: y = np.interp(np.arange(0, len(y), sr / SR), np.arange(len(y)), y).astype(np.float32)
                    # the render carries the rest before the line as leading silence: the
                    # wav is longer than the line's span by exactly that, so start it early
                    offset = l["spanOffset"] if l["spanOffset"] is not None else s * spb - max(0.0, len(y) / SR - (e - s) * spb)
                    put(down + offset, y * line_gain, m)
                    if A.audit:
                        nb = s
                        targets = []
                        for nt in seg:
                            dd = dur(nt)
                            if midi(nt) is not None: targets.append([down + nb * spb, down + (nb + dd) * spb, midi(nt)])
                            nb += dd
                        audit_events.append(dict(kind="vocal", member=m, text=lyr.replace("-", ""),
                            start=down + offset, samples=y * line_gain, targets=targets, source=str(l["wav"]), role=role))
                elif A.audit:
                    raise ValueError(f"Missing sung audio: {item['score']} / {m} / {lyr}")
                sy = [p for tok in lyr.split() for p in tok.split("-")]
                nb = s; si = 0
                for nt in seg:
                    dd = dur(nt)
                    if midi(nt) is not None and si < len(sy): syll[m].append((down + nb * spb, sy[si], dd * spb, len(captions[m]), si)); si += 1
                    nb += dd
                captions[m].append((down + s * spb, down + e * spb, lyr.replace("-", ""), lyr, role))
                if born[m] is None: born[m] = down + s * spb
            b = 0.0
            for tok in toks(v["notes"]):
                d = dur(tok); k = tok.split(":")[0]
                if cut_beat and b >= cut_beat: break
                if k.isdigit(): notes_vis[m].append((down + b * spb, down + (b + d) * spb, int(k)))
                b += d
            max_beat = max(max_beat, min(b, cut_beat) if cut_beat else b)
            if v.get("double"):
                tracks.append((v["notes"], v.get("doubleVelocity", 40), v.get("doubleTranspose", 24), False))
        else:
            tracks.append((v["notes"], v.get("velocity", 64), 0, True))
        for k in ("notes2", "notes3", "notes4"):
            if v.get(k): tracks.append((v[k], v.get("velocity" + k[-1], 40), 0, True))
        for notes, vel, tr, vis in tracks:
            b = 0.0
            for tok in toks(notes):
                d = dur(tok); k = tok.split(":")[0]
                if cut_beat and b >= cut_beat: break
                secs = d * spb; t0 = down + b * spb
                if k.isdigit():
                    mm = int(k) + tr; instrument = v.get("previewInstrument", "whistle")
                    rendered = (sine if instrument == "sine" else whistle)(mm, secs * 0.95, vel); put(t0, rendered)
                    if A.audit: audit_events.append(dict(kind="instrument", member=m, text=instrument, start=t0,
                        samples=rendered, targets=[[t0, t0 + secs * 0.95, mm]]))
                    if vis: notes_vis[m].append((t0, t0 + secs, mm))
                    if born[m] is None and "birth" in item["score"]: born[m] = t0
                elif k == "k": put(t0, kick(vel)); pulses[m].append(t0)
                elif k == "h": put(t0, hat(vel)); pulses[m].append(t0)
                b += d
            max_beat = max(max_beat, min(b, cut_beat) if cut_beat else b)
    end = down + max_beat * spb
    # stage directions
    for sd in item.get("stage", []) or []:
        ref = sd.get("after") or sd.get("before"); mem, n = ref.split(":"); mem = ALIAS.get(mem, mem); n = int(n)
        pl = line_pos.get(mem, [])
        if n - 1 < len(pl):
            s, e = pl[n - 1]
            ts = down + e * spb + 0.3 if sd.get("after") else down + s * spb - 2.5
            stage.append((ts, sd["who"], sd["does"]))
            if sd.get("lid"): lm, st = sd["lid"].split(":"); lids[ALIAS.get(lm, lm)].append((ts, st))
    for line in ([] if A.quiet else score.get("outro", []) or []):
        i = line["voice"]; y = speak(say_voice.get(i, "Fred"), line["text"]); m = member_of(voices[i], i); put(end + 0.5, y * 0.9, m); captions[m].append((end + 0.5, end + 0.5 + len(y) / SR, "“" + line["text"] + "”")); end += 0.5 + len(y) / SR
    titles.append((t_item0, end, act, item.get("bit", score.get("title", ""))))
    t = end + GAP
TOTAL = t - GAP + 1.0

# ---- mix
mix = np.zeros(int(SR * (TOTAL + 1)), dtype=np.float32)
for t0, y in audio:
    i = int(t0 * SR); j = min(len(mix), i + len(y)); mix[i:j] += y[: j - i]
pk = np.abs(mix).max();
if pk > 0.95: mix *= 0.95 / pk
wav_out = str(Path(A.out).with_suffix(".wav")); sf.write(wav_out, mix, SR)
print(f"audio {int(TOTAL//60)}:{int(TOTAL%60):02d} → {wav_out}")
if A.audit:
    from pitch_audit import audit_render
    audit_render(audit_events, mix, SR, Path(A.out).with_suffix(""), min(1.0, 0.95 / pk) if pk else 1.0)
json.dump({"total_seconds": TOTAL, "bits": [{"t0": a, "t1": b, "act": c, "bit": d} for a, b, c, d in titles], "stage": stage}, open(str(Path(A.out).with_suffix(".json")), "w"), indent=1)
for a, b, c, d in titles: print(f"  {int(a//60)}:{int(a%60):02d}  {d}  ({b-a:.0f}s)")
for ts, who, does in stage: print(f"  {int(ts//60)}:{int(ts%60):02d}  STAGE: {who} {does}")
if A.no_video: sys.exit(0)

# ---- video: three MacBook Neo screens (2816 x 1762 → 16:10), each the member's face
import random
W, H, FPS = 1920, 720, A.fps
TW, TH, GAPX = 608, 380, 24; ROW_Y = 96; STRIP = 88
SS = 2                                   # supersample the faces, then shrink
COL = {}
for m in MEMBERS:
    accent = json.load(open(LANE / "members" / m / "voice.json"))["color"].lstrip("#")
    COL[m] = tuple(int(accent[k:k + 2], 16) for k in (0, 2, 4))
def lum(c): return (0.2126 * c[0] + 0.7152 * c[1] + 0.0722 * c[2]) / 255
def mixc(a, b, k): return tuple(int(a[i] * (1 - k) + b[i] * k) for i in range(3))
def font(path, pt):
    try: return ImageFont.truetype(path, int(pt))
    except Exception: return ImageFont.truetype("/System/Library/Fonts/Helvetica.ttc", int(pt))
ROCK = "/System/Library/Fonts/Supplemental/Comic Sans MS Bold.ttf"
HELV = "/System/Library/Fonts/Helvetica.ttc"
f_small, f_bit, f_stage = font(HELV, 20), font(HELV, 26), font(ROCK, 40)

def mouth_class(syl):
    s = syl.lower(); v = [c for c in s if c in "aeiouy"]
    if not v: return 5 if s[:1] in ("f", "v") else 6
    if s[:1] in ("f", "v") and len(s) <= 2: return 5
    if "oo" in s or "ou" in s or "ew" in s or s.endswith("u") or s.startswith("w") or s.startswith("qu"): return 3
    if "aw" in s or "au" in s or "o" in s: return 4
    if "ee" in s or "ea" in s or "i" in s or "y" in s or "e" in s: return 2
    if "a" in s: return 1
    return 0

# the member's live level, for the jaw
VOX = {}
for m in MEMBERS:
    buf = np.zeros(len(mix), dtype=np.float32)
    for t0, y in vox[m]:
        i = int(t0 * SR); j = min(len(buf), i + len(y)); buf[i:j] += y[: j - i]
    VOX[m] = buf
def rms(m, tt):
    i = int(tt * SR); w = VOX[m][i:i + int(SR * 0.04)]
    return float(np.sqrt(np.mean(w * w))) if len(w) else 0.0

def jit(boil, i, salt):   # deterministic jitter in [-1, 1]
    h = (1469598103934665603 ^ boil) & 0xFFFFFFFFFFFFFFFF
    for b in (i, salt, 0x9E37): h = ((h ^ b) * 1099511628211) & 0xFFFFFFFFFFFFFFFF
    return (h % 2001) / 1000 - 1
def blob(cx, cy, rx, ry, lump, salt, boil, n=26, T=lambda x, y: (x, y)):
    pts = []
    for i in range(n):
        a = i / n * 2 * math.pi; k = 1 + lump * jit(boil, i, salt)
        pts.append((cx + math.cos(a) * rx * k, cy + math.sin(a) * ry * k))
    out = []                                     # Chaikin smoothing ≈ the bezier-through-midpoints blob
    for _ in range(2):
        q = []
        for i in range(len(pts)):
            a, b = pts[i], pts[(i + 1) % len(pts)]
            q.append((0.75 * a[0] + 0.25 * b[0], 0.75 * a[1] + 0.25 * b[1])); q.append((0.25 * a[0] + 0.75 * b[0], 0.25 * a[1] + 0.75 * b[1]))
        pts = q
    return [T(*p) for p in pts]
def bez(p0, p1, p2, p3, n=24):
    out = []
    for i in range(n + 1):
        u = i / n; a, b, c, d = (1 - u) ** 3, 3 * u * (1 - u) ** 2, 3 * u * u * (1 - u), u ** 3
        out.append((a * p0[0] + b * p1[0] + c * p2[0] + d * p3[0], a * p0[1] + b * p1[1] + c * p2[1] + d * p3[1]))
    return out
def ink(d, pts, width, fill=None):
    if fill is not None: d.polygon(pts, fill=fill)
    d.line(pts + [pts[0]], fill=(0, 0, 0), width=int(width), joint="curve")
def stroke(d, pts, width): d.line(pts, fill=(0, 0, 0), width=int(width), joint="curve")

def active(evs, tt):
    for e in evs:
        if e[0] <= tt < e[1]: return e[2:]
    return None
mouth_open = {m: 0.0 for m in MEMBERS}
def face(m, tt, state):
    """One member's screen at SSx: the color, the eyes, the brows, the mouth, the name, the caption."""
    Wt, Ht = TW * SS, TH * SS
    acc = COL[m]; isBB = m == "blueberry"
    bg = acc if state != "unborn" else mixc(acc, (0, 0, 0), 0.75)
    im = Image.new("RGB", (Wt, Ht), bg); d = ImageDraw.Draw(im)
    line = Wt * 0.011; boil = int(tt * 10)
    # the jaw follows the member's own sound; the form follows the syllable
    cur = None; last_on = -9; onset_i = 0
    for k, (ts, sy, hold, li, si) in enumerate(syll[m]):
        if ts <= tt: cur = (ts, sy, hold, li, si); last_on = ts; onset_i = k
        else: break
    target = min(1, max(0, (rms(m, tt) - 0.012) / 0.16))
    mo = mouth_open[m]; mo += (target - mo) * (0.85 if target > mo else 0.5); mouth_open[m] = mo
    shape = mouth_class(cur[1]) if cur and tt - cur[0] < max(0.12, cur[2] * 0.8) + 0.3 else 0
    if cur and cur[1].lower() in ("hmm", "mmm", "mm") and tt < cur[0] + cur[2]: mo = 0.02
    if cur and cur[1][:1].lower() in "mbp" and tt - cur[0] < 0.07: mo = 0
    squash = 0.78 ** ((tt - last_on) * 60) if 0 <= tt - last_on < 0.6 else 0
    lean = 1 if onset_i % 2 == 0 else -1
    if state == "unborn": mo, squash = 0, 0
    # squash on the onset around the face's center, leaning left then right
    cx0, cy0 = Wt / 2, Ht * 0.5 - math.sin(tt * 1.1) * Ht * 0.006
    sx, sy_ = 1 + 0.05 * squash, 1 - 0.08 * squash; rot = squash * lean * 0.025
    def T(x, y):
        x, y = (x - cx0) * sx, (y - cy0) * sy_
        return (cx0 + x * math.cos(rot) - y * math.sin(rot), cy0 + x * math.sin(rot) + y * math.cos(rot))
    # eyes (NSView y is up: 0.64 from the bottom → 0.36 from the top)
    seed = sum(map(ord, m))
    period = 2.6 + (seed % 10) / 10; blinking = ((tt + seed) % period) < 0.13 or state == "unborn"
    gaze = (0.5 * math.sin(tt * 0.7 + seed), 0.3 * math.sin(tt * 0.45 + seed * 0.3))
    ey = Ht * 0.36; ex = Wt * (0.2 if isBB else 0.19); ew = Wt * (0.1 if isBB else 0.105); eh = Ht * (0.15 if isBB else 0.19)
    for side in (-1, 1):
        cx = Wt / 2 + side * ex + Wt * 0.003 * jit(boil, side + 10, 4); cy = ey + Ht * 0.004 * jit(boil, side + 12, 5)
        if blinking:
            stroke(d, [T(*p) for p in bez((cx - ew, cy), (cx - ew * 0.4, cy + eh * 0.35), (cx + ew * 0.4, cy + eh * 0.35), (cx + ew, cy))], line)
        else:
            eye = blob(cx, cy, ew, eh, 0.025, side + 20, boil, 18, T); ink(d, eye, line, (247, 247, 247))
            pr = ew * (0.42 if isBB else 0.4); px, py = cx + gaze[0] * ew * 0.35, cy - gaze[1] * eh * 0.3
            d.ellipse([T(px - pr, py - pr), T(px + pr, py + pr)], fill=(0, 0, 0))
            d.ellipse([T(px - pr * 0.15, py - pr * 0.3 - pr * 0.5), T(px - pr * 0.15 + pr * 0.5, py - pr * 0.3)], fill=(255, 255, 255))
            if isBB:   # heavy lids: the top of each eye in the face's own color
                mask = Image.new("L", (Wt, Ht), 0); md = ImageDraw.Draw(mask); md.polygon(eye, fill=255)
                md.rectangle([0, T(cx, cy - eh * 0.08)[1], Wt, Ht], fill=0)
                im.paste(Image.new("RGB", (Wt, Ht), bg), mask=mask); d = ImageDraw.Draw(im)
                stroke(d, [T(cx - ew, cy - eh * 0.08), T(cx + ew, cy - eh * 0.08)], line)
        by = cy - eh * (1.25 if isBB else 1.45) + Ht * 0.005 * jit(boil, side + 30, 6)
        if isBB: stroke(d, [T(cx - ew * 0.95, by), T(cx + ew * 0.95, by)], line * 1.5)
        else: stroke(d, [T(*p) for p in bez((cx - ew * 0.95, by + eh * 0.12), (cx - ew * 0.3, by - eh * 0.3), (cx + ew * 0.3, by - eh * 0.3), (cx + ew * 0.95, by + eh * 0.12))], line * 1.1)
    # mouth — above the caption line
    my = Ht * 0.63; mw = Wt * (0.13 if isBB else 0.16)
    if mo < 0.08:
        w = mw * (1.0 if isBB else 1.25)
        stroke(d, [T(*p) for p in bez((Wt / 2 - w, my - Ht * 0.03), (Wt / 2 - w * 0.4, my + Ht * (0.01 if isBB else 0.06)), (Wt / 2 + w * 0.4, my + Ht * (0.01 if isBB else 0.06)), (Wt / 2 + w, my - Ht * 0.03))], line * 1.2)
    else:
        w, h = mw, Ht * 0.14 * mo; teeth, tongue, bite = True, False, False
        if shape == 1: w *= 1.15
        elif shape == 2: w *= 1.45; h *= 0.5
        elif shape == 3: w *= 0.5; h *= 0.9; teeth = False
        elif shape == 4: w *= 0.78; h *= 1.2; teeth = False
        elif shape == 5: w *= 1.2; h *= 0.35; bite = True
        elif shape == 6: w *= 0.9; h *= 0.55; tongue = True
        else: w *= 0.85; h *= 0.7
        h = max(h, Ht * 0.015)
        mouth = blob(Wt / 2, my, w, h, 0.05, 40, boil, 16, T)
        inner = Image.new("RGB", (Wt, Ht), (26, 8, 13)); idr = ImageDraw.Draw(inner)
        if teeth: idr.rectangle([Wt / 2 - w, my - h * (1.05 if not bite else 1.05), Wt / 2 + w, my - h * (0.45 if not bite else 0.1)], fill=(245, 245, 245))
        if tongue: idr.ellipse([Wt / 2 - w * 0.45, my - h * 0.7, Wt / 2 + w * 0.45, my + h * 0.3], fill=(217, 64, 89))
        elif not bite: idr.ellipse([Wt / 2 - w * 0.5, my + h * 0.35, Wt / 2 + w * 0.5, my + h * 1.25], fill=(217, 64, 89))
        mask = Image.new("L", (Wt, Ht), 0); ImageDraw.Draw(mask).polygon(mouth, fill=255)
        im.paste(inner, mask=mask); d = ImageDraw.Draw(im); ink(d, mouth, line * 1.2)
    # the name, top-left, in the rock lettering, in the face's ink
    inkc = (15, 15, 15) if lum(acc) > 0.58 else (250, 250, 250)
    pt = max(11, round(Wt / 22))
    if not CHORUS_FOCUS: d.text((Wt * 0.035, pt * 0.5), m, font=font(ROCK, pt), fill=inkc)
    if state == "unborn": d.text((Wt * 0.035, pt * 1.9), "not born yet", font=font(ROCK, pt * 0.55), fill=inkc)
    # the caption: the sung line, syllables lit as they are sung
    cap = active(captions[m], tt)
    if cap and not CHORUS_FOCUS:
        text = cap[0]; lyr = cap[1] if len(cap) > 1 else None
        parts = [p for tok in lyr.split() for p in tok.split("-")] if lyr else None
        lit = (cur[4] if cur and cur[3] == captions[m].index(next(c for c in captions[m] if c[0] <= tt < c[1])) else -1) if parts else 10 ** 6
        cpt = Wt / 19; f = font(ROCK, cpt)
        words = lyr.split() if lyr else text.split()
        while cpt > 18 and max(d.textlength(" ".join(w.replace("-", "") for w in words), font=f), 1) > Wt * 0.92 * 2: cpt *= 0.92; f = font(ROCK, cpt)
        # wrap by words into at most two rows
        rows, row, wsum = [], [], 0
        for w in words:
            ww = d.textlength(w.replace("-", "") + " ", font=f)
            if wsum + ww > Wt * 0.92 and row: rows.append(row); row, wsum = [], 0
            row.append(w); wsum += ww
        rows.append(row)
        dim = mixc(inkc, acc, 0.62); strokec = (250, 250, 250) if lum(acc) > 0.58 else (20, 20, 20)
        y = Ht - cpt * 1.2 * len(rows) - Ht * 0.035; si = 0
        for row in rows:
            x = (Wt - d.textlength(" ".join(w.replace("-", "") for w in row), font=f)) / 2
            for w in row:
                for sy in w.split("-"):
                    col = inkc if (parts is None or si <= lit) else dim
                    d.text((x + 3 * SS, y + 3 * SS), sy, font=f, fill=(0, 0, 0))
                    d.text((x, y), sy, font=f, fill=col, stroke_width=int(1.5 * SS), stroke_fill=strokec)
                    x += d.textlength(sy, font=f); si += 1
                x += d.textlength(" ", font=f)
            y += cpt * 1.2
    return im.resize((TW, TH), Image.BILINEAR)

def lid_state(m, tt):
    st = "up"
    for ts, s in lids[m]:
        if tt >= ts: st = s
    return st
ff = subprocess.Popen(["ffmpeg", "-y", "-loglevel", "error", "-f", "rawvideo", "-pix_fmt", "rgb24", "-s", f"{W}x{H}", "-r", str(FPS), "-i", "-",
                       "-i", wav_out, "-c:v", "libx264", "-preset", "veryfast", "-crf", "21", "-pix_fmt", "yuv420p", "-c:a", "aac", "-b:a", "160k", "-shortest", A.out], stdin=subprocess.PIPE)
LIMIT = A.seconds if A.seconds else TOTAL
nframes = int(LIMIT * FPS)
for fi in range(nframes):
    tt = fi / FPS
    im = Image.new("RGB", (W, H), (10, 10, 10)); d = ImageDraw.Draw(im)
    for i, m in enumerate(MEMBERS):
        x0 = GAPX + i * (TW + GAPX); y0 = ROW_Y
        if CHORUS_FOCUS: d.text((x0, 9), m, font=font(ROCK, 68), fill=COL[m])
        d.rectangle([x0 - 8, y0 - 8, x0 + TW + 8, y0 + TH + 8], fill=(28, 28, 30))      # the bezel
        lid = lid_state(m, tt); unborn = born[m] is None or tt < born[m]
        if lid == "down":
            d.rectangle([x0, y0, x0 + TW, y0 + TH], fill=(6, 6, 6)); d.rectangle([x0 + 3, y0 + 3, x0 + TW - 3, y0 + TH - 3], outline=COL[m], width=3)
            d.text((x0 + 22, y0 + 18), m, font=font(ROCK, 28), fill=COL[m]); d.text((x0 + 22, y0 + 56), "lid closed — still singing", font=f_small, fill=COL[m])
            cap = active(captions[m], tt)
            if cap:
                for k, ln in enumerate(textwrap.wrap(cap[0], width=30)[:3]): d.text((x0 + 22, y0 + TH - 110 + k * 30), ln, font=font(ROCK, 24), fill=COL[m])
        else:
            im.paste(face(m, tt, "unborn" if unborn else "live"), (x0, y0))
        d = ImageDraw.Draw(im)
    if CHORUS_FOCUS and PHONEME_ONLY:
        for i, m in enumerate(MEMBERS):
            current = next((s for s in syll[m] if s[0] <= tt < s[0] + s[2]), None)
            if current:
                text = current[1]; f = font(ROCK, 88)
                x = GAPX + i * (TW + GAPX) + (TW - d.textlength(text, font=f)) / 2
                d.text((x, ROW_Y + TH + 27), text, font=f, fill=COL[m])
    elif CHORUS_FOCUS:
        # One readable lyric across the stage. The family line is shared,
        # so printing it three times only makes it harder to read.
        shown = set()
        for m in MEMBERS:
            cap = active(captions[m], tt)
            if not cap or cap[0] in shown or (len(cap) > 2 and cap[2] != "lead"): continue
            shown.add(cap[0])
            text = cap[0]; size = 72; f = font(ROCK, size)
            while d.textlength(text, font=f) > W - 2 * GAPX:
                size -= 2; f = font(ROCK, size)
            x = (W - d.textlength(text, font=f)) / 2
            d.text((x, ROW_Y + TH + 27), text, font=f, fill=(255, 255, 255))
    # stage direction, beneath the screens
    for ts, who, does in stage:
        if ts - 1.5 <= tt < ts + 5:
            msg = f"{who.upper()} {does}"; w = d.textlength(msg, font=f_stage) + 70
            d.rectangle([(W - w) / 2, ROW_Y + TH + 30, (W + w) / 2, ROW_Y + TH + 100], fill=(255, 230, 0)); d.text(((W - w) / 2 + 35, ROW_Y + TH + 40), msg, font=f_stage, fill=(0, 0, 0))
    # the strip
    d.rectangle([0, H - STRIP, W, H], fill=(0, 0, 0))
    ti = active(titles, tt)
    if ti: d.text((GAPX, H - STRIP + 14), ti[0], font=f_small, fill=(160, 160, 160)); d.text((GAPX, H - STRIP + 40), ti[1], font=f_bit, fill=(255, 255, 255))
    clk = f"{int(tt//60)}:{int(tt%60):02d} / {int(TOTAL//60)}:{int(TOTAL%60):02d}"; d.text((W - 230, H - STRIP + 30), clk, font=f_bit, fill=(255, 255, 255))
    d.rectangle([0, H - 6, W * tt / TOTAL, H], fill=(255, 255, 255))
    ff.stdin.write(im.tobytes())
    if fi % (FPS * 60) == 0: print(f"  frame {fi}/{nframes}", file=sys.stderr)
ff.stdin.close(); ff.wait()
print(f"video → {A.out}")
