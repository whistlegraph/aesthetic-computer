#!/usr/bin/env python3
"""Compare several single-study reports side by side.

Usage:
  .venv/bin/python study/compare.py out/a/report.json out/b/report.json \
      --out study/out/comparison

Writes COMPARISON.md plus three figures: section-strip timelines,
band-balance heatmap, loudness small multiples.
"""

import argparse
import json
from pathlib import Path

import numpy as np
import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt

from study import (SURFACE, INK, INK2, GRID, SEQ_CMAP, BANDS,
                   sec_color)


def load(paths):
    return [json.loads(Path(p).read_text()) for p in paths]


def fig_structures(out, reports):
    fig, ax = plt.subplots(figsize=(9, 0.62 * len(reports) + 0.9))
    for i, r in enumerate(reports):
        y = len(reports) - 1 - i
        for sec in r["structure"]["sections"]:
            ax.barh(y, sec["end_s"] - sec["start_s"], left=sec["start_s"],
                    height=0.52, color=sec_color(sec["label"]),
                    edgecolor=SURFACE, linewidth=1.2)
            if sec["end_s"] - sec["start_s"] > 8:
                ax.text((sec["start_s"] + sec["end_s"]) / 2, y,
                        sec["label"], ha="center", va="center",
                        fontsize=7, fontweight="bold", color=SURFACE)
    ax.set_yticks(range(len(reports)))
    ax.set_yticklabels([r["title"] for r in reversed(reports)], fontsize=8)
    ax.set_xlabel("time (s)")
    ax.set_title("section timelines (letters = repetition classes, "
                 "per track — colors do not match across rows)")
    ax.grid(axis="x")
    fig.tight_layout()
    fig.savefig(out / "fig-compare-structure.png", dpi=180)
    plt.close(fig)


def fig_balance(out, reports):
    mat = np.array([[r["arrangement"]["band_balance_db"][b[0]]
                     for b in BANDS] for r in reports])
    fig, ax = plt.subplots(figsize=(4.8, 0.42 * len(reports) + 1.0))
    ax.imshow(np.clip(mat, -30, 0), cmap=SEQ_CMAP, aspect="auto",
              vmin=-30, vmax=0)
    for i in range(mat.shape[0]):
        for j in range(mat.shape[1]):
            v = mat[i, j]
            ax.text(j, i, f"{v:.0f}", ha="center", va="center", fontsize=7,
                    color=SURFACE if v > -12 else INK2)
    ax.set_xticks(range(len(BANDS)))
    ax.set_xticklabels([b[0] for b in BANDS])
    ax.set_yticks(range(len(reports)))
    ax.set_yticklabels([r["title"] for r in reports], fontsize=8)
    ax.grid(False)
    ax.set_title("band balance (dB rel. loudest band)")
    fig.tight_layout()
    fig.savefig(out / "fig-compare-balance.png", dpi=180)
    plt.close(fig)


def fig_dynamics(out, reports, curves):
    n = len(reports)
    fig, axes = plt.subplots(n, 1, figsize=(5.6, 0.78 * n + 0.5),
                             sharex=True)
    axes = np.atleast_1d(axes)
    for ax, r, (t, db) in zip(axes, reports, curves):
        ax.plot(t, db, color="#2a78d6", lw=1.4)
        ax.set_ylim(-38, 0)
        ax.set_ylabel(r["title"], rotation=0, ha="right", va="center",
                      fontsize=8, color=INK2)
        ax.set_yticks([-30, -10])
    axes[-1].set_xlabel("time (s)")
    axes[0].set_title("short-term RMS loudness (dBFS)")
    fig.tight_layout()
    fig.savefig(out / "fig-compare-dynamics.png", dpi=180)
    plt.close(fig)


def write_markdown(out, reports):
    rows = ["| track | dur | bpm | key | LUFS | LRA≈ | crest | sections |"
            " mean sec | onsets/s | perc share |",
            "|---|---|---|---|---|---|---|---|---|---|---|"]
    for r in reports:
        m, s, a, h = (r["master"], r["structure"],
                      r["arrangement"], r["harmony"])
        secs = s["sections"]
        mean_sec = np.mean([x["end_s"] - x["start_s"] for x in secs])
        rows.append(
            f"| {r['title']} | {m['duration_s']:.0f}s | {s['tempo_bpm']} |"
            f" {h['key']} | {m['integrated_lufs']} |"
            f" {m['loudness_range_db_approx']} | {m['crest_factor_db']} |"
            f" {s['n_sections']} | {mean_sec:.0f}s |"
            f" {a['onsets_per_s_overall']} | {a['percussive_share']} |")
    (out / "COMPARISON.md").write_text(
        "# single-study comparison\n\n" + "\n".join(rows) + "\n")


def main():
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("reports", nargs="+")
    ap.add_argument("--out", required=True)
    args = ap.parse_args()
    out = Path(args.out)
    out.mkdir(parents=True, exist_ok=True)
    reports = load(args.reports)

    # loudness curves are re-derived from each report's audio file so the
    # comparison never needs the originals resampled together
    import librosa
    curves = []
    for r in reports:
        y, sr = librosa.load(r["master"]["file"], sr=22050, mono=True)
        win, hop = int(3 * sr), int(0.5 * sr)
        t, db = [], []
        for start in range(0, max(1, len(y) - win), hop):
            seg = y[start:start + win]
            t.append((start + win / 2) / sr)
            db.append(20 * np.log10(np.sqrt(np.mean(seg ** 2)) + 1e-12))
        curves.append((np.array(t), np.array(db)))

    fig_structures(out, reports)
    fig_balance(out, reports)
    fig_dynamics(out, reports, curves)
    write_markdown(out, reports)
    print(f"→ {out}")


if __name__ == "__main__":
    main()
