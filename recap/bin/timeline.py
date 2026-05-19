#!/usr/bin/env python3
# timeline.py — recap visual-score PNG: words vs waltz beats vs audio.
# eye-validation: are subtitles ELONGATED to match held narration, or
# do they pop in/out as instant flashes? mirrors pop/bin/timeline.py.
#
# four panels top→bottom, sharing the time axis:
#   1. WORDS    — subs.json windows, colored by duration class.
#                 short/medium/long/held — instant-pop words show red,
#                 sustained ones show mint. text rendered INSIDE box.
#   2. WALTZ    — waltz-events.json drum/bell events on a midi y-axis,
#                 downbeats highlighted; bar-line ticks across width.
#   3. AUDIO    — waltz.mp3 waveform + word-onset ticks (cyan).
#
# usage:
#   .venv/bin/python recap/bin/timeline.py \
#     --subs   recap/out/subs.json \
#     --events recap/out/waltz-events.json \
#     --audio  recap/out/waltz.mp3 \
#     --out    ~/Desktop/recap-timing.png

import argparse, json, os, sys
import numpy as np
import matplotlib.pyplot as plt
import matplotlib.patches as mpatches
import matplotlib.patheffects as pe

BG       = "#0a0a14"
PANEL_BG = "#10101e"
GRID     = "#1f1f30"
FG       = "#f3f0d8"
DIM      = "#7f7d68"
ACCENT   = "#5fe8b8"   # mint — long-held words / downbeat
GREEN    = "#7fe070"   # sustained
ORANGE   = "#ff8a3d"   # short
RED      = "#ff5566"   # instant-pop (under-elongated)
CYAN     = "#5fd0ff"   # waveform
PURPLE   = "#c87dff"   # melody (midi >= 60)
YELLOW   = "#ffd84d"   # downbeat (midi 36)

plt.rcParams.update({
    "figure.facecolor": BG,
    "axes.facecolor": PANEL_BG,
    "axes.edgecolor": DIM,
    "axes.labelcolor": FG,
    "axes.titlecolor": FG,
    "xtick.color": FG,
    "ytick.color": FG,
    "text.color": FG,
    "font.family": "monospace",
    "font.monospace": ["Menlo", "DejaVu Sans Mono", "Consolas", "Courier New"],
    "font.weight": "bold",
    "axes.labelweight": "bold",
    "axes.titleweight": "bold",
    "axes.linewidth": 1.4,
    "xtick.major.width": 1.4,
    "ytick.major.width": 1.4,
    "xtick.major.size": 6,
    "ytick.major.size": 6,
})

def stroke(width=3, fg=BG):
    return [pe.withStroke(linewidth=width, foreground=fg)]

def duration_color(dur):
    if dur < 0.30:  return RED          # instant — bad: not elongated
    if dur < 1.00:  return ORANGE       # short
    if dur < 3.00:  return GREEN        # sustained — good
    return ACCENT                        # held / drawn-out

def duration_class(dur):
    if dur < 0.30:  return "instant"
    if dur < 1.00:  return "short"
    if dur < 3.00:  return "sustained"
    return "held"

def midi_label(m):
    if m is None: return ""
    octave = m // 12 - 1
    pitch = ["C","C#","D","D#","E","F","F#","G","G#","A","A#","B"][m % 12]
    return f"{pitch}{octave}"

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--subs",   default="recap/out/subs.json")
    ap.add_argument("--events", default="recap/out/waltz-events.json")
    ap.add_argument("--audio",  default="recap/out/waltz.mp3")
    ap.add_argument("--out",    required=True)
    ap.add_argument("--title",  default=None)
    ap.add_argument("--max-width", type=float, default=80.0,
                    help="cap on figure width in inches")
    args = ap.parse_args()

    subs = json.load(open(args.subs))
    waltz = json.load(open(args.events))
    events = waltz.get("events", waltz) if isinstance(waltz, dict) else waltz
    bpm = waltz.get("bpm", 78) if isinstance(waltz, dict) else 78
    beat_sec = waltz.get("beatSec", 60.0 / bpm) if isinstance(waltz, dict) else 60.0 / bpm
    total = float(waltz.get("totalSec", max(s["endSec"] for s in subs) + 5))

    # ── word duration stats ──────────────────────────────────────────
    durs = [s["endSec"] - s["startSec"] for s in subs]
    n = len(durs)
    counts = {"instant":0, "short":0, "sustained":0, "held":0}
    for d in durs: counts[duration_class(d)] += 1
    pct = {k: 100.0*v/max(1,n) for k,v in counts.items()}
    mean_d = sum(durs)/max(1,n)
    median_d = sorted(durs)[n//2] if n else 0

    title = args.title or (
        f"RECAP TIMING   {bpm} BPM   {total:.0f}s   {n} words   "
        f"mean {mean_d:.2f}s · median {median_d:.2f}s   "
        f"red {counts['instant']}  orange {counts['short']}  "
        f"green {counts['sustained']}  mint {counts['held']}"
    )

    # ── waveform load ────────────────────────────────────────────────
    try:
        import librosa
        y, sr = librosa.load(args.audio, sr=22050, mono=True)
        print(f"  audio: {args.audio}  ({len(y)/sr:.1f}s)")
    except Exception as e:
        print(f"  ! librosa failed: {e}")
        y, sr = None, None

    # ── figure layout ────────────────────────────────────────────────
    panels_spec = [("words", 4.0), ("waltz", 2.6), ("audio", 2.4)]
    height_ratios = [h for _, h in panels_spec]
    fig_w = min(args.max_width, max(40.0, total * 0.18))
    fig_h = sum(height_ratios) * 1.05 + 0.6
    fig, axs = plt.subplots(
        len(panels_spec), 1, figsize=(fig_w, fig_h),
        gridspec_kw={"height_ratios": height_ratios, "hspace": 0.16},
        sharex=True,
    )
    kind_to_ax = {kind: axs[i] for i, (kind, _) in enumerate(panels_spec)}
    fig.suptitle(title, fontsize=24, fontweight="bold", color=FG, y=0.992,
                 path_effects=stroke(4))

    # ── PANEL 1: WORDS ───────────────────────────────────────────────
    ax_w = kind_to_ax["words"]
    # vertical-jitter rows so adjacent boxes don't visually collide
    ROWS = 3
    for i, s in enumerate(subs):
        x0 = s["startSec"]; x1 = s["endSec"]; dur = x1 - x0
        row = i % ROWS
        y_low = 0.05 + row * 0.31
        col = duration_color(dur)
        ax_w.add_patch(mpatches.FancyBboxPatch(
            (x0, y_low), max(0.04, x1 - x0), 0.28,
            boxstyle="round,pad=0.01,rounding_size=0.04",
            facecolor=col, alpha=0.55, edgecolor=col,
            linewidth=1.4, zorder=2))
        # text — size depends on box width-in-pixels
        bar_px = (x1 - x0) / total * fig_w * 72
        size = 13 if bar_px > 28 else (10 if bar_px > 14 else 7)
        ax_w.text((x0 + x1)/2, y_low + 0.14, s["text"],
                  ha="center", va="center", fontsize=size,
                  fontweight="bold", color=FG, zorder=3,
                  path_effects=stroke(2))
        # duration tag for the longer ones
        if dur >= 1.5 and bar_px > 60:
            ax_w.text((x0+x1)/2, y_low + 0.02, f"{dur:.1f}s",
                      ha="center", va="bottom", fontsize=8,
                      color=BG, zorder=4)
    ax_w.set_yticks([])
    ax_w.set_ylim(0, 1)
    ax_w.set_ylabel("WORDS", fontsize=18, labelpad=14, color=GREEN)
    ax_w.tick_params(axis="x", labelsize=12, length=5)
    ax_w.grid(axis="x", color=GRID, linestyle="-", linewidth=0.7)
    ax_w.set_axisbelow(True)
    # legend swatches
    legend_items = [
        mpatches.Patch(color=RED,    label=f"instant <0.3s  ({counts['instant']})"),
        mpatches.Patch(color=ORANGE, label=f"short   <1.0s  ({counts['short']})"),
        mpatches.Patch(color=GREEN,  label=f"sustained <3s  ({counts['sustained']})"),
        mpatches.Patch(color=ACCENT, label=f"held    ≥3.0s  ({counts['held']})"),
    ]
    ax_w.legend(handles=legend_items, loc="upper right", fontsize=11,
                frameon=True, facecolor=PANEL_BG, edgecolor=DIM,
                labelcolor=FG, ncol=4)

    # ── PANEL 2: WALTZ EVENTS ────────────────────────────────────────
    ax_b = kind_to_ax["waltz"]
    midis = [int(e["midi"]) for e in events]
    midi_min, midi_max = min(midis) - 1, max(midis) + 1
    for e in events:
        x = e["startSec"]; w = e.get("durSec", beat_sec)
        m = int(e["midi"])
        if m <= 40:
            col = YELLOW   # bass / downbeat
            alpha = 0.55
        elif m <= 64:
            col = CYAN
            alpha = 0.42
        else:
            col = PURPLE
            alpha = 0.42
        ax_b.add_patch(mpatches.Rectangle(
            (x, m - 0.42), w, 0.84,
            facecolor=col, alpha=alpha,
            edgecolor=col, linewidth=0.6, zorder=2))
    # bar lines (every 3 beats — waltz)
    bar = 3 * beat_sec
    t = 0
    while t < total:
        ax_b.axvline(t, color=DIM, linewidth=0.7, alpha=0.5, zorder=1)
        t += bar
    ax_b.set_ylim(midi_min, midi_max)
    ax_b.set_yticks([midi_min, (midi_min+midi_max)//2, midi_max])
    ax_b.set_yticklabels([midi_label(m) for m in
                          [midi_min, (midi_min+midi_max)//2, midi_max]],
                         fontsize=10)
    ax_b.set_ylabel("WALTZ", fontsize=18, labelpad=14, color=PURPLE)
    ax_b.tick_params(axis="x", labelsize=12, length=5)
    ax_b.grid(axis="x", color=GRID, linestyle="-", linewidth=0.7)
    ax_b.set_axisbelow(True)
    ax_b.text(0.995, 0.94,
              f"yellow=bass  cyan=mid  purple=high   bar={bar:.2f}s ({bpm} BPM 3/4)",
              transform=ax_b.transAxes, ha="right", va="top",
              fontsize=10, color=FG,
              bbox=dict(facecolor=BG, edgecolor=DIM,
                        boxstyle="round,pad=0.35"))

    # ── PANEL 3: AUDIO + word onsets ─────────────────────────────────
    ax_a = kind_to_ax["audio"]
    if y is not None:
        # downsample for fast plotting
        step = max(1, len(y) // 80000)
        t = np.arange(0, len(y), step) / sr
        ys = y[::step]
        ax_a.plot(t, ys, color=CYAN, linewidth=0.5, alpha=0.85, zorder=2)
        ax_a.fill_between(t, 0, ys, color=CYAN, alpha=0.18, zorder=2)
    for s in subs:
        ax_a.axvline(s["startSec"], color=GREEN,
                     linewidth=0.7, alpha=0.45, zorder=3)
    ax_a.plot([], [], color=GREEN, linewidth=2.5, label="word start")
    ax_a.plot([], [], color=CYAN,  linewidth=2.5, label="waltz audio")
    ax_a.legend(loc="upper right", fontsize=11, frameon=True,
                facecolor=PANEL_BG, edgecolor=DIM, labelcolor=FG)
    ax_a.set_xlim(0, total)
    ax_a.set_ylim(-1.05, 1.05)
    ax_a.set_xlabel("TIME (s)", fontsize=14, labelpad=8)
    ax_a.set_ylabel("AUDIO", fontsize=18, labelpad=14, color=CYAN)
    ax_a.tick_params(axis="x", labelsize=12, length=5)
    ax_a.tick_params(axis="y", labelsize=10)
    ax_a.grid(axis="x", color=GRID, linestyle="-", linewidth=0.7)
    ax_a.set_axisbelow(True)

    plt.tight_layout(rect=[0.01, 0.005, 0.995, 0.97])
    plt.savefig(args.out, dpi=110, bbox_inches="tight",
                facecolor=BG, edgecolor="none")
    print(f"  ✓ {args.out}  ({fig_w:.0f}x{fig_h:.1f}in)")

if __name__ == "__main__":
    main()
