#!/usr/bin/env python3
"""Map a studied single bar by bar: chords, phrase keys, energy.

Usage:
  .venv/bin/python study/map.py study/out/slug/report.json

Reads the report (audio path, tempo, sections, phrases) and writes MAP.md
plus fig-map.png next to it. The chord lane is a per-bar estimate from
harmonic chroma against major/minor triad templates, downbeat-aligned by
picking the beat phase where chord changes land hardest.
"""

import argparse
import json
from pathlib import Path

import numpy as np
import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt
import librosa

from study import (SURFACE, INK, INK2, GRID, SEQ_CMAP, BANDS, NOTES,
                   sec_color, estimate_key)

# one hue per pitch class, light enough to hold black text
PC_COLORS = ["#2a78d6", "#7a6bd6", "#b05cc4", "#e05a9e", "#e34948",
             "#eb6834", "#eda100", "#b0a100", "#6faf1b", "#1baf7a",
             "#00a3a3", "#4a90d0"]

DEGREES_MAJ = ["I", "bII", "II", "bIII", "III", "IV",
               "bV", "V", "bVI", "VI", "bVII", "VII"]


def triad_templates():
    temps = []
    for root in range(12):
        for third, quality in ((4, ""), (3, "m")):
            t = np.zeros(12)
            t[root], t[(root + third) % 12], t[(root + 7) % 12] = 1.0, 0.9, 0.8
            temps.append((f"{NOTES[root]}{quality}", root, quality,
                          t / np.linalg.norm(t)))
    return temps


TRIADS = triad_templates()


MAJ_SCALE = {0, 2, 4, 5, 7, 9, 11}
MIN_SCALE = {0, 2, 3, 5, 7, 8, 10}


def diatonic_pcs(key_name):
    tonic, mode = key_name.split()
    pc = NOTES.index(tonic)
    scale = MAJ_SCALE if mode == "major" else MIN_SCALE
    return {(pc + s) % 12 for s in scale}


def chord_of(chroma_vec, key_pcs=None):
    """Best triad for one bar of chroma; a small bonus favors triads that
    sit inside the local key, which keeps a buried third from flipping
    major bars to minor."""
    norm = np.linalg.norm(chroma_vec)
    if norm < 1e-6:
        return None, 0.0
    v = chroma_vec / norm
    scores = []
    for name, root, quality, t in TRIADS:
        s = float(v @ t)
        if key_pcs is not None:
            third = (root + (3 if quality == "m" else 4)) % 12
            if {root, third, (root + 7) % 12} <= key_pcs:
                s += 0.06
        scores.append((s, name, root, quality))
    score, name, root, quality = max(scores)
    return (name, root, quality), score


def roman(root, quality, key_pc, key_mode):
    """Chord degree relative to a key, lower-case when the chord is minor."""
    deg = DEGREES_MAJ[(root - key_pc) % 12]
    if key_mode == "minor":  # spell from the relative major for readability
        deg = DEGREES_MAJ[(root - (key_pc + 3) % 12) % 12]
    return deg.lower() if quality == "m" else deg


def downbeat_offset(chroma_beats):
    """Beat phase (0-3) where bar-to-bar chroma change is largest."""
    change = np.zeros(chroma_beats.shape[1])
    for i in range(1, chroma_beats.shape[1]):
        a, b = chroma_beats[:, i - 1], chroma_beats[:, i]
        na, nb = np.linalg.norm(a), np.linalg.norm(b)
        change[i] = 1 - (a @ b) / (na * nb + 1e-9)
    return int(np.argmax([change[off::4].mean() for off in range(4)]))


def main():
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("report")
    args = ap.parse_args()
    out = Path(args.report).parent
    rpt = json.loads(Path(args.report).read_text())
    title = rpt["title"]
    sections = rpt["structure"]["sections"]
    phrases = rpt["structure"]["phrases"]

    y, sr = librosa.load(rpt["master"]["file"], sr=22050, mono=True)
    _, beats = librosa.beat.beat_track(
        y=y, sr=sr, start_bpm=float(rpt["structure"]["tempo_bpm"]),
        units="frames")
    beat_times = librosa.frames_to_time(beats, sr=sr)

    y_harm = librosa.effects.harmonic(y)
    chroma = librosa.feature.chroma_cqt(y=y_harm, sr=sr)
    chroma_beats = librosa.util.sync(chroma, beats, aggregate=np.median)

    off = downbeat_offset(chroma_beats)
    starts = list(range(off, chroma_beats.shape[1] - 3, 4))

    # phrase keys first — they seed the chord lane's diatonic prior
    times = librosa.times_like(chroma[0], sr=sr)
    phrase_keys = []
    for p in phrases:
        m = (times >= p["start_s"]) & (times < p["end_s"])
        k, c = estimate_key(chroma[:, m].mean(axis=1))
        phrase_keys.append({**p, "key": k, "confidence": c})

    def key_pcs_at(t):
        for pk in phrase_keys:
            if pk["start_s"] <= t < pk["end_s"]:
                return diatonic_pcs(pk["key"])
        return diatonic_pcs(rpt["harmony"]["key"])

    # per-bar loudness + six-band energy from one STFT
    S = np.abs(librosa.stft(y, n_fft=2048)) ** 2
    freqs = librosa.fft_frequencies(sr=sr, n_fft=2048)
    frame_t = librosa.times_like(S[0], sr=sr)
    rms_db = 10 * np.log10(S.mean(axis=0) + 1e-12)
    rms_db -= rms_db.max()
    band_db = np.stack([
        10 * np.log10(S[(freqs >= lo) & (freqs < hi)].mean(axis=0) + 1e-12)
        for _, lo, hi in BANDS])
    band_db -= band_db.max()

    bars = []
    for n, b in enumerate(starts):
        t0 = beat_times[b]
        t1 = beat_times[b + 4] if b + 4 < len(beat_times) else float(len(y)) / sr
        (chord, score) = chord_of(chroma_beats[:, b:b + 4].mean(axis=1),
                                  key_pcs_at(float(t0)))
        fm = (frame_t >= t0) & (frame_t < t1)
        bars.append({
            "n": n + 1, "start_s": round(float(t0), 2),
            "end_s": round(float(t1), 2),
            "chord": None if chord is None else chord[0],
            "root_pc": None if chord is None else chord[1],
            "quality": None if chord is None else chord[2],
            "score": round(score, 2),
            "rms_db": round(float(rms_db[fm].mean()) if fm.any() else -60, 1),
        })

    # majority smoothing — a lone odd bar between two agreeing bars flips
    for i in range(1, len(bars) - 1):
        a, b_, c = bars[i - 1], bars[i], bars[i + 1]
        if a["chord"] == c["chord"] != b_["chord"] and a["chord"]:
            b_["chord"], b_["root_pc"], b_["quality"] = (
                a["chord"], a["root_pc"], a["quality"])

    gk_name, gk_mode = rpt["harmony"]["key"].split()
    gk_pc = NOTES.index(gk_name)

    fig_map(out, title, y, sr, bars, sections, phrases, phrase_keys,
            frame_t, band_db, rms_db)
    write_map_md(out, rpt, bars, phrase_keys, gk_pc, gk_mode)
    json_bars = out / "map.json"
    json_bars.write_text(json.dumps(
        {"downbeat_offset_beats": off, "bars": bars,
         "phrase_keys": phrase_keys}, indent=1))
    print(f"→ {out}/MAP.md, fig-map.png, map.json (offset {off})")


def merge_chord_spans(bars):
    spans = []
    for bar in bars:
        if spans and spans[-1]["chord"] == bar["chord"]:
            spans[-1]["end_s"] = bar["end_s"]
            spans[-1]["bars"] += 1
        else:
            spans.append({"chord": bar["chord"], "root_pc": bar["root_pc"],
                          "start_s": bar["start_s"], "end_s": bar["end_s"],
                          "bars": 1})
    return spans


def fig_map(out, title, y, sr, bars, sections, phrases, phrase_keys,
            frame_t, band_db, rms_db):
    fig, axes = plt.subplots(
        3, 1, figsize=(10, 6.4), sharex=True,
        gridspec_kw={"height_ratios": [1.6, 0.7, 1.3]})
    ax_l, ax_c, ax_b = axes

    # loudness + sections + phrase ticks
    ax_l.plot(frame_t, np.clip(rms_db, -40, 0), color="#2a78d6", lw=0.9)
    ax_l.set_ylim(-40, 2)
    ax_l.set_ylabel("level (dB)")
    for sec in sections:
        ax_l.axvline(sec["start_s"], color=INK2, lw=0.8, alpha=0.7)
        if sec["end_s"] - sec["start_s"] > 5:
            ax_l.text((sec["start_s"] + sec["end_s"]) / 2, -3.5,
                      sec["label"], ha="center", fontsize=10,
                      fontweight="bold", color=sec_color(sec["label"]))
    for p in phrases:
        ax_l.axvline(p["start_s"], color=INK2, lw=0.5, alpha=0.35, ls=":")
    for pk in phrase_keys:
        ax_l.text((pk["start_s"] + pk["end_s"]) / 2, -37.5, pk["key"],
                  ha="center", fontsize=6.5, color=INK2)
    ax_l.set_title(f"{title} — song map (phrase keys along the floor)")

    # chord lane
    for span in merge_chord_spans(bars):
        w = span["end_s"] - span["start_s"]
        if span["chord"] is None:
            continue
        ax_c.barh(0, w, left=span["start_s"], height=0.9,
                  color=PC_COLORS[span["root_pc"]],
                  alpha=0.45, edgecolor=SURFACE, linewidth=0.8)
        if w > 3:
            ax_c.text(span["start_s"] + w / 2, 0, span["chord"],
                      ha="center", va="center", fontsize=7, color=INK)
    ax_c.set_ylim(-0.6, 0.6)
    ax_c.set_yticks([])
    ax_c.set_ylabel("chords", rotation=0, ha="right", va="center",
                    fontsize=8, color=INK2)
    ax_c.grid(False)

    # band heatmap
    step = max(1, len(frame_t) // 1200)
    ax_b.imshow(np.clip(band_db[:, ::step], -50, 0),
                cmap=SEQ_CMAP, vmin=-50, vmax=0, aspect="auto",
                origin="lower", interpolation="nearest",
                extent=[frame_t[0], frame_t[-1], 0, len(BANDS)])
    ax_b.set_yticks(np.arange(len(BANDS)) + 0.5)
    ax_b.set_yticklabels([b[0] for b in BANDS], fontsize=7)
    ax_b.grid(False)
    ax_b.set_xlabel("time (s)")

    fig.tight_layout()
    fig.savefig(out / "fig-map.png", dpi=180)
    plt.close(fig)


def write_map_md(out, rpt, bars, phrase_keys, gk_pc, gk_mode):
    lines = [f"# {rpt['title']} — song map",
             "",
             f"global key {rpt['harmony']['key']} · "
             f"{rpt['structure']['tempo_bpm']} bpm · {len(bars)} bars",
             "", "## phrases", "",
             "| # | span | key | conf |", "|---|---|---|---|"]
    for i, pk in enumerate(phrase_keys, 1):
        lines.append(f"| {i} | {pk['start_s']:.0f}–{pk['end_s']:.0f}s |"
                     f" {pk['key']} | {pk['confidence']} |")

    lines += ["", "## chords (8 bars per row, degree vs. global key)", ""]
    for i in range(0, len(bars), 8):
        row = bars[i:i + 8]
        names = " | ".join(
            "—" if b["chord"] is None else b["chord"] for b in row)
        degs = " | ".join(
            "—" if b["chord"] is None
            else roman(b["root_pc"], b["quality"], gk_pc, gk_mode)
            for b in row)
        lines.append(f"**{row[0]['start_s']:.0f}s** · bar {row[0]['n']}")
        lines.append("")
        lines.append(f"`| {names} |`")
        lines.append(f"`| {degs} |`")
        lines.append("")
    (out / "MAP.md").write_text("\n".join(lines) + "\n")


if __name__ == "__main__":
    main()
