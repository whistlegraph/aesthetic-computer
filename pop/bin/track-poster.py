#!/usr/bin/env python3
"""track-poster.py — totalizing "how this track got made" PNG.

Layout (portrait, 18×14):
  ┌──────────────── title ─────────────────┐
  │ row 1: lyrics · score · melody contour │
  │ row 2: arrangement timeline (3 lanes)  │
  │ row 3: pipeline strip (8 stations)     │
  │ footer: output stats                   │
  └────────────────────────────────────────┘

The arrangement timeline (row 2) is the key panel: it shows when every
word enters, the per-word target pitch (the ballooned vocal), the
melody-bell strikes, and the harmonic bed chord changes — all on the
same time axis so it reads as a score.

Style mirrors `pop/bin/timeline.py`: dark bg, cream type, saturated
accents per lane, monospace.
"""

import json
import sys
import re
from pathlib import Path

import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
from matplotlib.patches import FancyBboxPatch, FancyArrowPatch, Rectangle

ROOT = Path(__file__).resolve().parent.parent
SLUG = sys.argv[1] if len(sys.argv) > 1 else "amazing"
OUT  = Path(sys.argv[2]) if len(sys.argv) > 2 else ROOT / "big-pictures" / "out" / f"{SLUG}-poster.png"

LYR    = ROOT / "big-pictures" / f"{SLUG}.txt"
NP     = ROOT / "big-pictures" / f"{SLUG}.np"
WORDS  = ROOT / "big-pictures" / "out" / f"{SLUG}-7verse-vocal-words.json"
VOCAL  = ROOT / "big-pictures" / "out" / f"{SLUG}-7verse-vocal.mp3"
ALIGN  = ROOT / "big-pictures" / "out" / f"{SLUG}-7verse-pitched-alignment.json"
WALTZ  = Path("/Users/jas/aesthetic-computer/recap/out/waltz-events.json")

# ── colors ──────────────────────────────────────────────────────────────
BG       = "#0a0a14"
PANEL_BG = "#10101e"
GRID     = "#1f1f30"
INK      = "#f3f0d8"
DIM      = "#7a8870"
CITRUS   = "#c6ff4f"
ACCENT   = "#ffd166"
MELODY   = "#5fe8b8"     # mint — melody bells
VOCAL_C  = "#ff8a3d"     # orange — ballooned vocal
BED_C    = "#5588ff"     # blue — harmonic bed (chord roots)
RED      = "#ff5566"

# ── score helpers (mirror score-render.mjs) ─────────────────────────────
NOTE_BASE = {"C":0,"C#":1,"DB":1,"D":2,"D#":3,"EB":3,"E":4,"F":5,"F#":6,
             "GB":6,"G":7,"G#":8,"AB":8,"A":9,"A#":10,"BB":10,"B":11}

def note_to_midi(s):
    s = s.upper()
    return 12 * (int(s[-1]) + 1) + NOTE_BASE[s[:-1]]

def midi_to_note(m):
    name = ["C","C#","D","D#","E","F","F#","G","G#","A","A#","B"][m % 12]
    return f"{name}{m//12 - 1}"

def parse_score(text):
    """Return (verses, all_syllables_with_break_flag)."""
    verses = []
    all_syl = []
    current = None
    for raw in text.splitlines():
        line = raw.strip()
        if not line or line.startswith("#"):
            continue
        if re.match(r"^verse\s+\d+$", line, re.I):
            current = []
            verses.append(current)
            if all_syl: all_syl[-1]["verse_break"] = True
            continue
        if current is None: continue
        line_tokens = []
        for tok in line.split():
            m = re.match(r"^([A-G][#b]?\d):([^*]+)\*([\d.]+)$", tok)
            if not m: continue
            note, syl, beats = m.group(1), m.group(2), float(m.group(3))
            line_tokens.append((note, note_to_midi(note), syl, beats))
            all_syl.append({"note": note, "midi": note_to_midi(note),
                            "syl": syl, "beats": beats, "verse_break": False})
        if line_tokens: current.append(line_tokens)
    return verses, all_syl

def syllables_to_words(all_syl, bpm=70, transpose=0, inter_verse_beats=2):
    """Mirror score-render.mjs --section all: hyphenated tail merges into
    its head word; verse-break inflates the previous syllable's beat
    weight by inter_verse_beats."""
    beat_s = 60.0 / bpm
    # Apply verse-break weight inflation (matches score-render.mjs)
    syls = []
    for s in all_syl:
        w = s["beats"]
        # We add the inter-verse rest AFTER the syllable that has the flag
        syls.append({**s, "weight": w})
    for i, s in enumerate(syls):
        if s["verse_break"]:
            s["weight"] += inter_verse_beats
    words = []
    cur = None
    pos = 0.0
    for s in syls:
        cont = s["syl"].startswith("-")
        if cont and cur is not None:
            cur["weights"].append(s["weight"])
            cur["notes"].append(s["midi"])
        else:
            if cur is not None: words.append(cur)
            cur = {"text": s["syl"].lstrip("-").rstrip("-"),
                   "start_beat": pos,
                   "notes": [s["midi"]],
                   "weights": [s["weight"]]}
        pos += s["weight"]
    if cur is not None: words.append(cur)
    for w in words:
        w["start_s"] = w["start_beat"] * beat_s
        w["dur_s"]   = sum(w["weights"]) * beat_s
        w["midi"]    = w["notes"][0] + transpose
        w["text"]    = w["text"].replace("-", "").lower()
    return words, pos * beat_s

# ── load all data ───────────────────────────────────────────────────────
lyrics = LYR.read_text()
score_text = NP.read_text()
verses, all_syl = parse_score(score_text)
score_words_hymn, score_dur_hymn = syllables_to_words(
    all_syl, bpm=70, transpose=-5, inter_verse_beats=2)

words = []
if WORDS.exists():
    words = json.loads(WORDS.read_text())

# The mp3 on the Desktop is now SCORE-PACED (stretched) — every word
# starts at its score beat position with duration from beat weights.
# Use the score's own timing for the vocal lane; pull text + note from
# the alignment sidecar (or fall back to score_words_hymn).
score_words = []
if ALIGN.exists():
    aligned_json = json.loads(ALIGN.read_text())
    N = min(len(aligned_json), len(score_words_hymn))
    for i in range(N):
        entry = aligned_json[i]
        sw = score_words_hymn[i]
        score_words.append({
            "text": entry["text"].lower(),
            "start_s": sw["start_s"],   # score-pace start
            "dur_s":   sw["dur_s"],     # score-pace duration
            "midi": entry["midi"],
            "midi_score": note_to_midi(entry["note"]),
        })
else:
    for sw in score_words_hymn:
        score_words.append({
            "text": sw["text"],
            "start_s": sw["start_s"],
            "dur_s": sw["dur_s"],
            "midi": sw["notes"][0] - 5,
            "midi_score": sw["notes"][0],
        })

vocal_kb = VOCAL.stat().st_size / 1024 if VOCAL.exists() else 0
spoken_dur = (words[-1]["toMs"] / 1000.0) if words else 0
n_words = len(words)

# Hymn-pace total duration from the score (stretched output spans this)
TOTAL_DUR = max(score_words[-1]["start_s"] + score_words[-1]["dur_s"],
                score_dur_hymn) if score_words else 60

# ── figure ──────────────────────────────────────────────────────────────
plt.rcParams.update({
    "font.family": "monospace",
    "font.monospace": ["Menlo", "DejaVu Sans Mono", "Consolas", "Courier New"],
    "axes.edgecolor": INK, "text.color": INK,
    "xtick.color": INK, "ytick.color": INK,
})

fig = plt.figure(figsize=(18, 14), facecolor=BG)
fig.subplots_adjust(left=0.04, right=0.98, top=0.95, bottom=0.04,
                    hspace=0.35, wspace=0.18)

# Title
fig.text(0.04, 0.965, "amazing grace",
         color=INK, fontsize=34, fontweight="bold")
fig.text(0.04, 0.943, "· 7 verses · how this track got made",
         color=CITRUS, fontsize=14)
fig.text(0.98, 0.965, "pop/big-pictures",
         color=DIM, fontsize=11, ha="right")
fig.text(0.98, 0.948, f"jeffrey-pvc · {TOTAL_DUR:.0f}s · 2026-05-05",
         color=DIM, fontsize=11, ha="right")

# Three rows: 3-col header / full-width timeline / full-width pipeline
gs = fig.add_gridspec(3, 3, height_ratios=[1.0, 1.4, 0.85],
                      hspace=0.45)

def panel(ax, title):
    ax.set_facecolor(BG)
    for spine in ax.spines.values():
        spine.set_visible(False)
    ax.set_xticks([]); ax.set_yticks([])
    ax.text(0.0, 1.0, title, color=CITRUS, fontsize=13, fontweight="bold",
            family="monospace", transform=ax.transAxes, va="top")

# ── (0,0) lyrics ────────────────────────────────────────────────────────
ax = fig.add_subplot(gs[0, 0]); panel(ax, "1 · what i write — words")
preview = []
for line in lyrics.splitlines():
    if not line.strip(): preview.append("")
    elif re.match(r"^verse\s+\d+$", line.strip(), re.I):
        preview.append(line.strip().upper())
    else:
        preview.append("  " + line)
shown = preview[:18]
text_block = "\n".join(shown)
if len(preview) > 18:
    text_block += f"\n  …  ({len(verses)} verses · {sum(len(v) for v in verses)} lines total)"
ax.text(0.02, 0.88, text_block, color=INK, fontsize=8.5, va="top",
        family="monospace", transform=ax.transAxes)

# ── (0,1) score ─────────────────────────────────────────────────────────
ax = fig.add_subplot(gs[0, 1]); panel(ax, "2 · what i write — pitch")
score_preview = [".np  NOTE:syllable*beats", "", "verse 1"]
for line_tokens in verses[0]:
    txt = " ".join(f"{n}:{syl}*{int(b)}" for n,_,syl,b in line_tokens)
    if len(txt) > 60: txt = txt[:57] + "…"
    score_preview.append("  " + txt)
score_preview += ["",
                  "verses 2-7  →  same melody, new syllables",
                  "",
                  "key  : G major pentatonic",
                  "range: D3 → D4   (climax on 'blind')",
                  "meter: common meter 8-6-8-6",
                  "time : 3/4  ·  70 bpm  (hymn pacing)"]
ax.text(0.02, 0.88, "\n".join(score_preview), color=INK, fontsize=8.5,
        va="top", family="monospace", transform=ax.transAxes)

# ── (0,2) verse-1 melody contour ────────────────────────────────────────
ax = fig.add_subplot(gs[0, 2]); panel(ax, "3 · melody contour (verse 1)")
xs, ys, sylabs = [], [], []
t = 0.0
for line_tokens in verses[0]:
    for note, midi, syl, beats in line_tokens:
        xs.append(t); ys.append(midi); sylabs.append(syl)
        t += beats
    t += 1
inner = ax.inset_axes([0.0, 0.18, 1.0, 0.62], facecolor=BG)
inner.plot(xs, ys, color=CITRUS, linewidth=1.6, marker="o",
           markersize=4, markerfacecolor=ACCENT, markeredgecolor=ACCENT)
inner.set_facecolor(BG)
for spine in inner.spines.values():
    spine.set_color(DIM); spine.set_linewidth(0.6)
inner.set_yticks([note_to_midi(n) for n in ["D3","G3","B3","D4"]])
inner.set_yticklabels(["D3","G3","B3","D4"], color=DIM, fontsize=8)
inner.set_xticks([])
inner.set_ylim(min(ys)-1.5, max(ys)+1.5)
if "blind" in sylabs:
    ci = sylabs.index("blind")
    inner.annotate("blind", xy=(xs[ci], ys[ci]), xytext=(xs[ci]+1.5, ys[ci]+1.3),
                   color=ACCENT, fontsize=9,
                   arrowprops=dict(arrowstyle="-", color=ACCENT, lw=0.8))
ax.text(0.02, 0.10, "verse 1 contour  ·  identical for verses 2-7",
        color=DIM, fontsize=9, transform=ax.transAxes)
ax.text(0.02, 0.04, "→ same notes, new lyrics each pass = the hymn form.",
        color=INK, fontsize=9, transform=ax.transAxes)

# ── ROW 2: arrangement timeline ─────────────────────────────────────────
ax = fig.add_subplot(gs[1, :])
ax.set_facecolor(PANEL_BG)
for sp in ax.spines.values(): sp.set_color(DIM); sp.set_linewidth(0.6)
ax.text(0.005, 0.97, "4 · the arrangement in time",
        color=CITRUS, fontsize=14, fontweight="bold", transform=ax.transAxes,
        va="top")
ax.text(0.005, 0.92,
        "every layer on a shared time axis  ·  vocal entries · bell strikes · chord roots",
        color=DIM, fontsize=10, transform=ax.transAxes, va="top")

ax.set_xlim(0, TOTAL_DUR)
# Y-lanes: 80-95 melody bells / 50-75 ballooned vocal / 20-45 bed / 5-15 axis
ax.set_ylim(0, 100)
ax.set_yticks([]); ax.set_xticks([])

# Lane labels (left margin) — drawn in axes coords as separate text
def lane_label(y, name, sub, color, vol):
    ax.text(0.005, y/100 - 0.005, name, color=color, fontsize=11,
            fontweight="bold", transform=ax.transAxes, va="top")
    ax.text(0.005, y/100 - 0.04, sub, color=DIM, fontsize=8.5,
            transform=ax.transAxes, va="top")
    ax.text(0.005, y/100 - 0.07, vol, color=INK, fontsize=8.5,
            transform=ax.transAxes, va="top", family="monospace")

# Convert seconds to data x — already same since xlim==(0, TOTAL_DUR)
# Lane 1 — Melody bells (sinebells, +12st = octave above vocal)
lane_label(95, "melody bells",
           "sinebells synth · +12st (oct above)\nscore-paced strikes",
           MELODY, "vol 0.40 · 171 strikes")
bell_pitches = [w["midi_score"] + 12 for w in score_words]
midi_min, midi_max = min(bell_pitches), max(bell_pitches)
def midi_to_y(m, lo, hi, y0, y1):
    if hi == lo: return (y0 + y1)/2
    return y0 + (m - lo) / (hi - lo) * (y1 - y0)
for w in score_words:
    midi = w["midi_score"] + 12
    y = midi_to_y(midi, midi_min, midi_max, 80, 92)
    ax.plot([w["start_s"], w["start_s"]], [80, y],
            color=MELODY, linewidth=0.7, alpha=0.6)
    ax.plot(w["start_s"], y, "o", color=MELODY, markersize=3.0)

# Lane 2 — Pitched + stretched vocal (each word held to score duration)
lane_label(73, "pitched vocal",
           "WORLD f0 → rubberband stretch (formant)\nheld to score note durations",
           VOCAL_C, "vol 2.5 · 171 words · -5st")
voc_pitches = [w["midi"] for w in score_words]
v_min, v_max = min(voc_pitches), max(voc_pitches)
for i, w in enumerate(score_words):
    y = midi_to_y(w["midi"], v_min, v_max, 52, 68)
    ax.plot([w["start_s"], w["start_s"] + w["dur_s"]], [y, y],
            color=VOCAL_C, linewidth=1.6, alpha=0.9)
    # Sample word labels: every 8th word OR every climax pitch
    if w["text"] in ("amazing", "wretch", "blind", "fear", "believed",
                     "snares", "home", "endures", "fail", "peace",
                     "snow", "mine", "years", "praise", "begun"):
        ax.text(w["start_s"] + w["dur_s"]/2, y + 1.4, w["text"],
                color=INK, fontsize=7.5, ha="center", va="bottom",
                family="monospace", alpha=0.95, fontweight="bold")

# Lane 3 — Harmonic bed: thin colored ribbon (chord changes), not a wall.
lane_label(45, "harmonic bed",
           "sinebells · I-IV-I-V · 70 bpm · G major", BED_C,
           "vol 0.20 · 1237 notes")
beat_s = 60.0 / 70.0
bar_s = beat_s * 3
n_bars = int(TOTAL_DUR / bar_s) + 1
chord_seq    = [0, 3, 0, 4]
chord_color  = {0: "#3a6cd6", 3: "#5fa9ff", 4: "#a4d4ff"}
chord_letter = {0: "G", 3: "C", 4: "D"}
# Ribbon at y=27-32 with a thin tick down at each bar 1 beat
ax.plot([0, TOTAL_DUR], [29.5, 29.5], color=DIM, linewidth=0.4, alpha=0.5)
for bar in range(n_bars):
    x = bar * bar_s
    deg = chord_seq[bar % 4]
    rect = Rectangle((x, 27), bar_s * 0.97, 5,
                     facecolor=chord_color[deg], edgecolor="none", alpha=0.85)
    ax.add_patch(rect)
    if bar % 4 == 0:
        ax.text(x + bar_s*2, 24.5, chord_letter[deg], color=BED_C,
                fontsize=8.5, ha="center", va="top", fontweight="bold")

# Verse markers — derived from whisper word starts at each verse's first
# word. Index of first word per verse: verse 1 starts at word 0, verse 2
# at word 26 (after 26 words in V1), etc. Score has 26+22+26+24+24+24+25
# = 171 words across 7 verses. Use cumulative.
verse_word_idx = []
acc = 0
for v in verses:
    verse_word_idx.append(acc)
    # syllables in this verse, grouped to words (count of non-continuation):
    head_syls = sum(1 for line in v for n,_,syl,_ in line if not syl.startswith("-"))
    acc += head_syls
for vi, wi in enumerate(verse_word_idx):
    if wi >= len(score_words): break
    vs = score_words[wi]["start_s"]
    ax.axvline(vs, ymin=0.10, ymax=0.83, color=DIM, linewidth=0.5,
               linestyle="--", alpha=0.45)
    ax.text(vs + 0.1, 84, f"v{vi+1}", color=DIM, fontsize=8.5,
            va="bottom", fontweight="bold")

# Time axis at bottom — scale tick interval based on duration
tick_step = 5 if TOTAL_DUR < 90 else (10 if TOTAL_DUR < 200 else 30)
for t in range(0, int(TOTAL_DUR) + 1, tick_step):
    ax.plot([t, t], [8, 12], color=INK, linewidth=0.6)
    mm = t // 60; ss = t % 60
    ax.text(t, 5.5, f"{mm}:{ss:02d}", color=INK, fontsize=8.5,
            ha="center", va="top", family="monospace")
ax.plot([0, TOTAL_DUR], [10, 10], color=INK, linewidth=0.4, alpha=0.5)

# Total mix dB summary in the panel header.
import math
ax.text(0.78, 0.965, "MIX  vocal +8.0 dB · bells -8.0 dB · bed -14.0 dB",
        color=DIM, fontsize=10, transform=ax.transAxes, va="top",
        family="monospace")

# ── ROW 3: pipeline strip ───────────────────────────────────────────────
ax = fig.add_subplot(gs[2, :]); panel(ax, "5 · what goes down — the pipeline")
ax.set_xlim(0, 100); ax.set_ylim(0, 100)

stations = [
    ("write\nlyrics + score", "amazing.txt\namazing.np",          3.0, CITRUS),
    ("say.mjs",                 "stab 0.6 · sim 0.9",            12, INK),
    ("ElevenLabs\njeffrey-pvc", "voice clone\nneutral:0",         21, ACCENT),
    ("align.mjs\nwhisper-cli",  "+ align-words\n(twas/tis fix)", 30, INK),
    ("score-pitch\nWORLD",      "171 notes · -5st\nf0 replace",   39, ACCENT),
    ("score-stretch\nrubberband","held to beats\n8× max stretch",48, ACCENT),
    ("melody-bells",            "sinebells · +12st\nscore-paced",57, MELODY),
    ("waltz bed",               "sinebells · 70 bpm\nI-IV-I-V",  66, BED_C),
    ("ffmpeg amix",             "vox 2.5 · bells 0.40\nbed 0.20",75, INK),
    ("finalize",                "ID3 + cover",                   84, CITRUS),
]
for (top, bot, x, color) in stations:
    box = FancyBboxPatch((x-4.0, 35), 8.0, 28,
                         boxstyle="round,pad=0.25,rounding_size=0.8",
                         linewidth=1.3, edgecolor=color, facecolor=BG)
    ax.add_patch(box)
    ax.text(x, 55, top, color=color, fontsize=7.6, ha="center",
            va="center", fontweight="bold")
    ax.text(x, 41, bot, color=DIM, fontsize=6.4, ha="center", va="center")
for i in range(len(stations)-1):
    arrow = FancyArrowPatch((stations[i][2]+4.0, 49),
                            (stations[i+1][2]-4.0, 49),
                            arrowstyle="->", color=INK, lw=1.0,
                            mutation_scale=9)
    ax.add_patch(arrow)

ax.text(3.5, 75, "JEFFREY", color=CITRUS, fontsize=10,
        ha="center", fontweight="bold")
ax.text(3.5, 68, "writes by hand", color=DIM, fontsize=8.5, ha="center")
ax.text(56, 75, "AESTHETIC.COMPUTER", color=CITRUS, fontsize=10,
        ha="center", fontweight="bold")
ax.text(56, 68, "voice  ·  pitch  ·  bells  ·  bed  ·  mix  ·  packaging",
        color=DIM, fontsize=8.5, ha="center")

mm, ss = int(TOTAL_DUR // 60), int(TOTAL_DUR % 60)
ax.text(50, 18,
        f"output  ·  {mm}:{ss:02d} hymn pace  ·  3 layers  ·  vocal pitched -5st + held to score  ·  G major pentatonic",
        color=ACCENT, fontsize=11, ha="center", fontweight="bold")
ax.text(50, 10,
        "~/Desktop/amazing-grace-7verse.mp3   (pitched → stretched → bells + bed → mix)",
        color=INK, fontsize=9, ha="center", family="monospace")
ax.text(50, 4,
        "every step content-hash cached  ·  reruns cost $0 unless inputs change",
        color=DIM, fontsize=8.5, ha="center", style="italic")

OUT.parent.mkdir(parents=True, exist_ok=True)
fig.savefig(OUT, dpi=140, facecolor=BG, bbox_inches="tight", pad_inches=0.3)
print(f"✓ wrote {OUT}  ({OUT.stat().st_size/1024:.0f} KB)")
