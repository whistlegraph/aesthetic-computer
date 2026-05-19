#!/usr/bin/env python3
# timeline.py — visual-score PNG: storyboard expectations vs whisper
# utterances vs pitchsnap autotune marks vs actual audio waveform.
# eye-validation tool for checking when words are SUPPOSED to fire,
# when whisper THOUGHT they fired (in the source vocal), where
# pitchsnap MOVED them, and where the actual audio peaks land.
#
# style: aesthetic.computer dark — black bg, cream type, saturated
# accent per panel, chunky monospace, big enough to read across the
# room. axis labels are alternated/rotated to avoid vertical overlap.
#
# four panels, top → bottom, sharing the time axis:
#   1. piano roll — one bar per slide (note × time), lyric LARGE inside.
#   2. utterances — whisper word windows.
#   3. autotune — arrows naturalStart → snappedStart (zigzag labels).
#   4. waveform — audio + librosa onsets vs storyboard slide.starts.
#
# usage:
#   .venv/bin/python bin/timeline.py \
#     --storyboard big-pictures/out/amazing.storyboard.json \
#     --audio big-pictures/out/amazing-final.mp3 \
#     [--words ...] [--events ...] \
#     --out ~/Desktop/amazing-timing.png

import argparse, json, os, sys
import numpy as np
import matplotlib.pyplot as plt
import matplotlib.patches as mpatches
import matplotlib.patheffects as pe
from matplotlib.colors import Normalize
from matplotlib.cm import ScalarMappable

# ── AC PALETTE ─────────────────────────────────────────────────────
BG       = "#0a0a14"   # ink-black, slight blue tilt
PANEL_BG = "#10101e"   # alternating panel back
GRID     = "#1f1f30"   # subtle grid lines
FG       = "#f3f0d8"   # cream type
DIM      = "#7f7d68"   # secondary type
ACCENT   = "#5fe8b8"   # mint — primary accent (bell trigger, slide.start)
GREEN    = "#7fe070"   # whisper utterance band
ORANGE   = "#ff8a3d"   # score expected
CYAN     = "#5fd0ff"   # waveform / actual
PURPLE   = "#c87dff"   # secondary
RED      = "#ff5566"   # autotune up
BLUE     = "#5588ff"   # autotune down

# Monospace stack — matplotlib will resolve in order
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

NOTE_BASE = {"C":0,"C#":1,"DB":1,"D":2,"D#":3,"EB":3,"E":4,"F":5,"F#":6,
             "GB":6,"G":7,"G#":8,"AB":8,"A":9,"A#":10,"BB":10,"B":11}
def note_to_midi(n):
    s = n.upper()
    octave = int(s[-1]) if s[-1].isdigit() else 4
    name = s[:-1] if s[-1].isdigit() else s
    return 12 * (octave + 1) + NOTE_BASE[name]

def midi_label(m):
    octave = m // 12 - 1
    pitch = ["C","C#","D","D#","E","F","F#","G","G#","A","A#","B"][m % 12]
    return f"{pitch}{octave}"

def stroke(width=3, fg=BG):
    """text path-effect for max contrast outline"""
    return [pe.withStroke(linewidth=width, foreground=fg)]

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--storyboard", required=True)
    ap.add_argument("--audio", required=True)
    ap.add_argument("--words", default=None)
    ap.add_argument("--events", default=None)
    ap.add_argument("--out", required=True)
    ap.add_argument("--title", default=None)
    args = ap.parse_args()

    sb = json.load(open(args.storyboard))
    slides = sb["slides"]
    total = float(sb.get("audioDuration", sb.get("duration", 0)))
    bpm = sb.get("bpm", 100)
    slug = sb.get("slug", "?")
    title = args.title or f"{slug.upper()}   {bpm} BPM   {total:.1f}s   {len(slides)} slides"

    # ── auto-find optional sources ───────────────────────────────────
    audio_dir = os.path.dirname(os.path.abspath(args.audio))
    audio_stem = os.path.splitext(os.path.basename(args.audio))[0]
    if args.words is None:
        # Prefer the mfa-aligned words (correct lyric text + accurate
        # timing) over whisper output (correct timing but wrong text
        # because pitchsnap distortion confuses recognition).
        for cand in (
            f"{audio_dir}/{slug}-mfa-words.json",
            f"{audio_dir}/{audio_stem}-mfa-words.json",
            f"{audio_dir}/{audio_stem}-words.json",
            f"{audio_dir}/{slug}-perline-words.json",
            f"{audio_dir}/{slug}-7-warm-words.json",
            f"{audio_dir}/{slug}-warm-words.json",
            f"{audio_dir}/{slug}-vocal-words.json",
            f"{audio_dir}/{slug}-sung-words.json",
        ):
            if os.path.exists(cand):
                args.words = cand
                src = "mfa" if "mfa" in cand else "whisper"
                print(f"  words:  {cand}  ({src})")
                break
    if args.events is None:
        for cand in (
            f"{audio_dir}/{audio_stem}.events.json",
            f"{audio_dir}/{slug}-beat.events.json",
        ):
            if os.path.exists(cand):
                args.events = cand
                print(f"  events: {cand}")
                break

    words = json.load(open(args.words)) if args.words and os.path.exists(args.words) else []
    if words: print(f"  {len(words)} whisper words")
    events = []
    if args.events and os.path.exists(args.events):
        ev = json.load(open(args.events))
        events = ev.get("events", ev) if isinstance(ev, dict) else ev
        print(f"  {len(events)} pitchsnap events")

    try:
        import librosa
        y, sr = librosa.load(args.audio, sr=22050, mono=True)
        onsets = librosa.onset.onset_detect(y=y, sr=sr, units="time")
        print(f"  {len(onsets)} onsets")
    except Exception as e:
        print(f"  ! librosa failed: {e}")
        y, sr = None, None
        onsets = []

    # ── figure layout — generous heights for readability ─────────────
    # ORDER (top → bottom): HEARD, SCORE, PITCH-SNAP, AUDIO
    # Reading order matches mental model: "what came out of the speaker"
    # sits in front of "what the score wanted" so the user compares
    # heard → expected → snap-correction → waveform top-down.
    panels_spec = []                            # (kind, height_ratio)
    if words:  panels_spec.append(("words", 1.6))
    panels_spec.append(("score", 5.0))
    if events: panels_spec.append(("events", 2.4))
    panels_spec.append(("audio", 2.8))
    height_ratios = [h for _, h in panels_spec]
    n_panels = len(panels_spec)
    fig_w = max(22, total * 0.85)
    fig_h = sum(height_ratios) * 1.15 + 0.6
    fig, axs = plt.subplots(
        n_panels, 1, figsize=(fig_w, fig_h),
        gridspec_kw={"height_ratios": height_ratios, "hspace": 0.18},
        sharex=True
    )
    if n_panels == 1: axs = [axs]
    fig.suptitle(title, fontsize=28, fontweight="bold", color=FG, y=0.985,
                 path_effects=stroke(4))

    # Map panel kind → axes index
    kind_to_ax = {kind: axs[i] for i, (kind, _) in enumerate(panels_spec)}

    # ── PIANO ROLL ───────────────────────────────────────────────────
    ax_roll = kind_to_ax["score"]
    midis = [note_to_midi(s["note"]) for s in slides]
    midi_min, midi_max = min(midis) - 1, max(midis) + 1
    # alternating pitch lanes
    for m in range(midi_min, midi_max + 1):
        if m % 2 == 0:
            ax_roll.axhspan(m - 0.5, m + 0.5, color=BG, zorder=0)
    cmap = plt.get_cmap("plasma")
    for i, s in enumerate(slides):
        m = note_to_midi(s["note"])
        x, w = s["start"], s["end"] - s["start"]
        col = cmap((i / max(1, len(slides) - 1)) * 0.85 + 0.05)
        ax_roll.add_patch(mpatches.FancyBboxPatch(
            (x, m - 0.42), w, 0.84,
            boxstyle="round,pad=0.02,rounding_size=0.05",
            linewidth=1.2, edgecolor=FG, facecolor=col, zorder=2,
        ))
        # lyric INSIDE bar — auto-shrink for tight ones
        bar_px_estimate = w / total * fig_w * 80
        text_size = 22 if len(s["text"]) <= 3 else (18 if len(s["text"]) <= 5 else 15)
        if bar_px_estimate < 60: text_size = max(11, text_size - 6)
        ax_roll.text(x + w / 2, m, s["text"],
                     ha="center", va="center", fontsize=text_size,
                     fontweight="bold", color=FG, zorder=4,
                     path_effects=stroke(3))
        # bell trigger tick
        ax_roll.plot([x, x], [midi_min - 0.5, m - 0.42],
                     color=ACCENT, linewidth=1.3, alpha=0.55, zorder=3)
    ax_roll.set_yticks(range(midi_min, midi_max + 1))
    ax_roll.set_yticklabels([midi_label(m) for m in range(midi_min, midi_max + 1)],
                            fontsize=13)
    ax_roll.set_ylim(midi_min - 0.6, midi_max + 0.6)
    ax_roll.set_ylabel("SCORE", fontsize=18, labelpad=14, color=ACCENT)
    ax_roll.tick_params(axis="x", labelsize=12, length=5)
    ax_roll.grid(axis="x", color=GRID, linestyle="-", linewidth=0.7)
    ax_roll.set_axisbelow(True)
    ax_roll.plot([], [], color=ACCENT, linewidth=2.5, label="bell trigger")
    leg = ax_roll.legend(loc="upper right", fontsize=13, frameon=True,
                         facecolor=PANEL_BG, edgecolor=DIM, labelcolor=FG)

    # ── UTTERANCES ───────────────────────────────────────────────────
    if words:
        ax_utt = kind_to_ax["words"]
        for w in words:
            x0 = w["fromMs"] / 1000.0
            x1 = w["toMs"] / 1000.0
            ax_utt.add_patch(mpatches.FancyBboxPatch(
                (x0, 0.18), x1 - x0, 0.64,
                boxstyle="round,pad=0.01,rounding_size=0.04",
                facecolor=GREEN, alpha=0.45, edgecolor=GREEN,
                linewidth=1.6, zorder=2))
            ax_utt.text((x0 + x1) / 2, 0.5, w["text"],
                        ha="center", va="center", fontsize=15,
                        fontweight="bold", color=FG, zorder=3,
                        path_effects=stroke(3))
        ax_utt.set_yticks([])
        ax_utt.set_ylim(0, 1)
        ax_utt.set_ylabel("HEARD", fontsize=18, labelpad=14, color=GREEN)
        ax_utt.tick_params(axis="x", labelsize=12, length=5)
        ax_utt.grid(axis="x", color=GRID, linestyle="-", linewidth=0.7)
        ax_utt.set_axisbelow(True)
        # Connect each whisper word to the closest score slide (by
        # text-prefix match if possible, else nearest-time). Drift is
        # the horizontal slope of the connecting line.
        # We draw the connector across the figure between ax_utt's
        # bottom edge and ax_roll's top edge using fig.add_artist.
        from matplotlib.patches import ConnectionPatch
        used_slide_idxs = set()
        for w in words:
            wtext = w["text"].lower().strip(",.!?;: ")
            wmid = (w["fromMs"] + w["toMs"]) / 2000.0
            # Find best matching slide: prefer whichever slide's text is
            # a prefix/substring of the whisper word AND is closest
            # in time and not already used.
            best, best_score = None, 1e9
            for j, s in enumerate(slides):
                if j in used_slide_idxs:
                    continue
                stext = s["text"].lower().strip("-")
                # syllable-of-word match: whisper "amazing" should pair
                # with the FIRST score syllable 'a' (the word's onset).
                prefix_match = wtext.startswith(stext) or stext.startswith(wtext)
                tdist = abs(s["start"] - w["fromMs"] / 1000.0)
                score = tdist + (0 if prefix_match else 1.5)
                if score < best_score:
                    best, best_score = j, score
            if best is None:
                continue
            used_slide_idxs.add(best)
            slide = slides[best]
            x_heard = (w["fromMs"] / 1000.0 + w["toMs"] / 1000.0) / 2
            x_score = slide["start"] + (slide["end"] - slide["start"]) / 2
            drift = x_heard - x_score
            # color drift by magnitude — yellow ≤ 0.2s, orange ≤ 0.6s, red beyond
            if   abs(drift) < 0.20: dcol = "#7fe070"
            elif abs(drift) < 0.60: dcol = "#ffaa33"
            else:                   dcol = "#ff5566"
            con = ConnectionPatch(
                xyA=(x_heard, 0.18), coordsA=ax_utt.transData,
                xyB=(x_score, midi_max + 0.55), coordsB=ax_roll.transData,
                color=dcol, linewidth=1.2, alpha=0.8, zorder=20,
                linestyle=("--" if abs(drift) > 0.20 else "-"),
            )
            fig.add_artist(con)

    # ── AUTOTUNE ─────────────────────────────────────────────────────
    if events:
        ax_at = kind_to_ax["events"]
        max_st = max(abs(e.get("semitones", 0)) for e in events) or 1.0
        for i, e in enumerate(events):
            nat = float(e.get("naturalStart", 0))
            snap = float(e.get("snappedStart", nat))
            st = float(e.get("semitones", 0))
            # color: blue=down, red=up, dim if near zero
            col = BLUE if st < -0.5 else (RED if st > 0.5 else DIM)
            ax_at.annotate(
                "", xy=(snap, 0.5), xytext=(nat, 0.5),
                arrowprops=dict(arrowstyle="-|>", color=col, lw=2.0,
                                shrinkA=0, shrinkB=0, mutation_scale=18),
                zorder=3,
            )
            ax_at.plot(nat, 0.5, "o", color=col, markersize=8, zorder=4,
                       markeredgecolor=FG, markeredgewidth=1.0)
            ax_at.plot(snap, 0.5, "s", color=col, markersize=10, zorder=5,
                       markeredgecolor=FG, markeredgewidth=1.0)
            # ZIGZAG label heights to dodge horizontal collisions
            tag_y, st_y = (0.86, 0.14) if i % 2 == 0 else (0.72, 0.28)
            tag = e.get("targetNote", "")
            ax_at.text(snap, tag_y, tag,
                       ha="center", va="bottom" if i % 2 == 0 else "top",
                       fontsize=13, fontweight="bold", color=FG, zorder=5,
                       path_effects=stroke(3))
            ax_at.text(snap, st_y, f"{st:+.0f}st",
                       ha="center", va="top" if i % 2 == 0 else "bottom",
                       fontsize=11, color=col, zorder=5,
                       path_effects=stroke(2))
        ax_at.set_yticks([])
        ax_at.set_ylim(0, 1)
        ax_at.set_ylabel("PITCH SNAP", fontsize=18, labelpad=14, color=PURPLE)
        ax_at.tick_params(axis="x", labelsize=12, length=5)
        ax_at.grid(axis="x", color=GRID, linestyle="-", linewidth=0.7)
        ax_at.set_axisbelow(True)
        ax_at.text(0.995, 0.96,
                   "● natural   ■ snapped   blue▼ down   red▲ up",
                   transform=ax_at.transAxes, ha="right", va="top",
                   fontsize=12, color=FG,
                   bbox=dict(facecolor=BG, edgecolor=DIM,
                             boxstyle="round,pad=0.4"))

    # ── WAVEFORM ─────────────────────────────────────────────────────
    ax_wave = kind_to_ax["audio"]
    if y is not None:
        t = np.arange(len(y)) / sr
        ax_wave.plot(t, y, color=CYAN, linewidth=0.7, alpha=0.9, zorder=2)
        ax_wave.fill_between(t, 0, y, color=CYAN, alpha=0.20, zorder=2)
    for s in slides:
        ax_wave.axvline(s["start"], color=ORANGE, linewidth=1.2, alpha=0.65, zorder=3)
    for ot in onsets:
        ax_wave.axvline(ot, color=GREEN, linewidth=1.2, alpha=0.65, zorder=4)
    ax_wave.plot([], [], color=ORANGE, linewidth=2.5, label="EXPECTED slide.start")
    ax_wave.plot([], [], color=GREEN,  linewidth=2.5, label="ACTUAL  onset")
    ax_wave.legend(loc="upper right", fontsize=13, frameon=True,
                   facecolor=PANEL_BG, edgecolor=DIM, labelcolor=FG)
    ax_wave.set_xlim(0, total)
    ax_wave.set_ylim(-1.05, 1.05)
    ax_wave.set_xlabel("TIME (s)", fontsize=16, labelpad=10)
    ax_wave.set_ylabel("AUDIO", fontsize=18, labelpad=14, color=CYAN)
    ax_wave.tick_params(axis="x", labelsize=13, length=6)
    ax_wave.tick_params(axis="y", labelsize=11)
    ax_wave.grid(axis="x", color=GRID, linestyle="-", linewidth=0.7)
    ax_wave.set_axisbelow(True)

    # generous outer margin so suptitle / labels don't get clipped
    plt.tight_layout(rect=[0.02, 0.01, 0.99, 0.96])
    plt.savefig(args.out, dpi=120, bbox_inches="tight",
                facecolor=BG, edgecolor="none")
    print(f"  ✓ {args.out}")

if __name__ == "__main__":
    main()
