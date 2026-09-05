#!/usr/bin/env python3
"""single-study-toolkit — study a finished single from the outside in.

Four layers, outermost first:

  L0 master     the file as a mastered object: loudness, peaks, crest,
                stereo image, spectral tilt
  L1 structure  tempo, beat grid, self-similarity, section boundaries
                and letters (A/B/C… by repetition)
  L2 arrangement six-band energy over time, harmonic/percussive split,
                onset density — who is playing, when
  L3 harmony    chroma, global + per-section key, dominant-voice pitch

Usage:
  .venv/bin/python study/study.py AUDIO --out study/out/slug \
      [--title "One Step"] [--artist oskie]

Writes report.json, REPORT.md and four figures into --out.
compare.py consumes multiple report.json files.
"""

import argparse
import json
import sys
from pathlib import Path

import numpy as np
import scipy.signal
import scipy.cluster.hierarchy as hier
import librosa
import soundfile as sf
import pyloudnorm

import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt
from matplotlib.colors import LinearSegmentedColormap

# ---------------------------------------------------------------- palette --
# Reference dataviz palette (light mode) — categorical order is fixed.
SURFACE = "#fcfcfb"
INK = "#0b0b0b"
INK2 = "#52514e"
GRID = "#e5e4e0"
CATEGORICAL = ["#2a78d6", "#eb6834", "#1baf7a", "#eda100",
               "#e87ba4", "#008300", "#4a3aa7", "#e34948"]
SEQ_CMAP = LinearSegmentedColormap.from_list(
    "seq_blue", [SURFACE, "#9ec4ec", "#2a78d6", "#0f3f78"])

BANDS = [("sub", 20, 60), ("bass", 60, 250), ("lowmid", 250, 1000),
         ("mid", 1000, 4000), ("high", 4000, 10000), ("air", 10000, 16000)]

plt.rcParams.update({
    "figure.facecolor": SURFACE, "axes.facecolor": SURFACE,
    "savefig.facecolor": SURFACE, "text.color": INK,
    "axes.edgecolor": GRID, "axes.labelcolor": INK2,
    "xtick.color": INK2, "ytick.color": INK2,
    "axes.grid": True, "grid.color": GRID, "grid.linewidth": 0.6,
    "axes.axisbelow": True, "font.size": 9,
    "axes.titlesize": 10, "axes.titleweight": "bold",
    "axes.spines.top": False, "axes.spines.right": False,
})


# ---------------------------------------------------------------- L0 master --
def layer_master(path, y_stereo, y, sr):
    dur = len(y) / sr
    meter = pyloudnorm.Meter(sr)
    data = y_stereo.T if y_stereo.ndim == 2 else y_stereo
    lufs = float(meter.integrated_loudness(data))

    # short-term loudness proxy: 3 s RMS windows, 0.5 s hop (dBFS)
    win, hop = int(3 * sr), int(0.5 * sr)
    st_times, st_db = [], []
    for start in range(0, max(1, len(y) - win), hop):
        seg = y[start:start + win]
        rms = np.sqrt(np.mean(seg ** 2) + 1e-12)
        st_times.append((start + win / 2) / sr)
        st_db.append(20 * np.log10(rms + 1e-12))
    st_times, st_db = np.array(st_times), np.array(st_db)
    active = st_db[st_db > st_db.max() - 40]  # crude gate
    lra = float(np.percentile(active, 95) - np.percentile(active, 10))

    peak = float(np.max(np.abs(y_stereo)))
    y4 = scipy.signal.resample_poly(
        y_stereo, 4, 1, axis=-1 if y_stereo.ndim == 2 else 0)
    true_peak = float(np.max(np.abs(y4)))
    rms_all = float(np.sqrt(np.mean(y ** 2)))
    crest = 20 * np.log10(peak / (rms_all + 1e-12))

    stereo = None
    if y_stereo.ndim == 2 and y_stereo.shape[0] == 2:
        l, r = y_stereo
        corr = float(np.corrcoef(l, r)[0, 1])
        mid, side = (l + r) / 2, (l - r) / 2
        side_db = 20 * np.log10(
            (np.sqrt(np.mean(side ** 2)) + 1e-12) /
            (np.sqrt(np.mean(mid ** 2)) + 1e-12))
        stereo = {"correlation": round(corr, 3),
                  "side_vs_mid_db": round(float(side_db), 1)}

    S = np.abs(librosa.stft(y, n_fft=4096))
    freqs = librosa.fft_frequencies(sr=sr, n_fft=4096)
    centroid = librosa.feature.spectral_centroid(S=S, sr=sr)[0]
    rolloff = librosa.feature.spectral_rolloff(S=S, sr=sr, roll_percent=0.95)[0]
    mean_spec = np.mean(S, axis=1)
    mask = (freqs > 100) & (freqs < 16000)
    tilt = np.polyfit(np.log10(freqs[mask]),
                      20 * np.log10(mean_spec[mask] + 1e-9), 1)[0]

    return {
        "file": str(path), "duration_s": round(dur, 2), "sr": sr,
        "channels": 1 if y_stereo.ndim == 1 else y_stereo.shape[0],
        "integrated_lufs": round(lufs, 1),
        "loudness_range_db_approx": round(lra, 1),
        "sample_peak_dbfs": round(20 * np.log10(peak + 1e-12), 2),
        "true_peak_dbtp_approx": round(20 * np.log10(true_peak + 1e-12), 2),
        "crest_factor_db": round(float(crest), 1),
        "stereo": stereo,
        "spectral_centroid_hz_median": int(np.median(centroid)),
        "rolloff95_hz_median": int(np.median(rolloff)),
        "spectral_tilt_db_per_decade": round(float(tilt), 1),
    }, (st_times, st_db)


# ------------------------------------------------------------- L1 structure --
def foote_novelty(ssm, kernel=48):
    k = kernel // 2
    g = scipy.signal.windows.gaussian(kernel, kernel / 4)
    board = np.outer(g, g)
    sign = np.ones((kernel, kernel))
    sign[:k, k:] = -1
    sign[k:, :k] = -1
    kern = board * sign
    n = ssm.shape[0]
    pad = np.pad(ssm, k, mode="edge")
    nov = np.array([np.sum(pad[i:i + kernel, i:i + kernel] * kern)
                    for i in range(n)])
    nov -= nov.min()
    return nov / (nov.max() + 1e-12)


def layer_structure(y, sr):
    tempo, beats = librosa.beat.beat_track(y=y, sr=sr, trim=False)
    tempo = float(np.atleast_1d(tempo)[0])
    beat_times = librosa.frames_to_time(beats, sr=sr)

    chroma = librosa.feature.chroma_cqt(y=y, sr=sr)
    mfcc = librosa.feature.mfcc(y=y, sr=sr, n_mfcc=20)
    rms = librosa.feature.rms(y=y)
    feats = np.vstack([librosa.util.normalize(chroma, axis=0),
                       librosa.util.normalize(mfcc, axis=0),
                       librosa.util.normalize(rms, axis=0)])
    fb = librosa.util.sync(feats, beats, aggregate=np.median)
    fb = librosa.util.normalize(fb, axis=0)

    # full cosine self-similarity — dense enough for checkerboard novelty
    unit = fb / (np.linalg.norm(fb, axis=0, keepdims=True) + 1e-9)
    ssm = unit.T @ unit

    nov = foote_novelty(ssm, kernel=min(48, max(8, ssm.shape[0] // 8)))
    # drops and breaks announce themselves in energy before anything else
    rms_b = librosa.util.sync(rms, beats, aggregate=np.median)[0]
    d_rms = scipy.ndimage.gaussian_filter1d(np.abs(np.gradient(rms_b)), 2)
    d_rms /= d_rms.max() + 1e-12
    nov = 0.6 * nov + 0.4 * d_rms[:len(nov)]
    min_gap = max(4, int(8 * tempo / 60 / 2))  # ≥ ~8 s between boundaries
    peaks, _ = scipy.signal.find_peaks(
        nov, distance=min_gap, prominence=0.15)
    bounds = [0] + [int(p) for p in peaks] + [len(beat_times) - 1]
    bounds = sorted(set(bounds))
    segs = list(zip(bounds[:-1], bounds[1:]))

    # absorb slivers (< ~6 s) into whichever neighbor sounds more alike
    def seg_mean(seg):
        return fb[:, seg[0]:seg[1]].mean(axis=1)

    min_beats = int(6 * tempo / 60)
    while len(segs) > 1:
        lens = [b - a for a, b in segs]
        i = int(np.argmin(lens))
        if lens[i] >= min_beats:
            break
        cands = [j for j in (i - 1, i + 1) if 0 <= j < len(segs)]
        j = min(cands, key=lambda j: np.linalg.norm(
            seg_mean(segs[i]) - seg_mean(segs[j])))
        a, b = min(i, j), max(i, j)
        segs[a:b + 1] = [(segs[a][0], segs[b][1])]

    # letter segments by clustering their mean feature vectors
    seg_means = np.array([seg_mean(s) for s in segs])
    if len(segs) > 1:
        z = hier.linkage(seg_means, method="ward")
        cut = 0.5 * z[:, 2].max()
        raw = hier.fcluster(z, t=cut, criterion="distance")
    else:
        raw = [1]
    letters, order = {}, []
    for c in raw:
        if c not in letters:
            letters[c] = chr(ord("A") + len(letters))
        order.append(letters[c])

    # merge neighbors that ended up with the same letter
    merged = []
    for seg, lab in zip(segs, order):
        if merged and merged[-1][1] == lab:
            merged[-1] = ((merged[-1][0][0], seg[1]), lab)
        else:
            merged.append((seg, lab))

    sections = []
    for (a, b), lab in merged:
        t0, t1 = float(beat_times[a]), float(beat_times[b])
        sections.append({"label": lab, "start_s": round(t0, 2),
                         "end_s": round(t1, 2),
                         "bars_approx": round((t1 - t0) * tempo / 60 / 4, 1)})
    bounds = [seg[0] for seg, _ in merged] + [merged[-1][0][1]]

    # phrase tier: the boundaries before letter-merging — the small waves
    # (fills, drops, lifts) inside the macroform
    phrases = [{"start_s": round(float(beat_times[a]), 2),
                "end_s": round(float(beat_times[b]), 2)} for a, b in segs]
    return {
        "tempo_bpm": round(tempo, 1),
        "n_beats": len(beat_times),
        "n_sections": len(sections),
        "sections": sections,
        "n_phrases": len(phrases),
        "median_phrase_s": round(float(np.median(
            [p["end_s"] - p["start_s"] for p in phrases])), 1),
        "phrases": phrases,
    }, (beat_times, ssm, nov, bounds)


# ----------------------------------------------------------- L2 arrangement --
def layer_arrangement(y, sr, sections):
    S = np.abs(librosa.stft(y, n_fft=4096, hop_length=1024)) ** 2
    freqs = librosa.fft_frequencies(sr=sr, n_fft=4096)
    times = librosa.times_like(S[0], sr=sr, hop_length=1024)
    band_energy = np.array([
        S[(freqs >= lo) & (freqs < hi)].sum(axis=0)
        for _, lo, hi in BANDS])
    band_db = 10 * np.log10(band_energy + 1e-12)
    band_db -= band_db.max()

    H, P = librosa.decompose.hpss(librosa.stft(y))
    e_h, e_p = float(np.sum(np.abs(H) ** 2)), float(np.sum(np.abs(P) ** 2))
    onset_env = librosa.onset.onset_strength(y=y, sr=sr)
    onsets = librosa.onset.onset_detect(y=y, sr=sr, units="time")

    profile = {}
    total_db = 10 * np.log10(band_energy.sum(axis=1) + 1e-12)
    for (name, _, _), db in zip(BANDS, total_db - total_db.max()):
        profile[name] = round(float(db), 1)

    per_section = []
    for sec in sections:
        m = (times >= sec["start_s"]) & (times < sec["end_s"])
        sec_prof = band_db[:, m].mean(axis=1) if m.any() else np.zeros(len(BANDS))
        dens = float(np.sum((onsets >= sec["start_s"]) &
                            (onsets < sec["end_s"])) /
                     max(0.1, sec["end_s"] - sec["start_s"]))
        per_section.append({
            "label": sec["label"], "start_s": sec["start_s"],
            "onsets_per_s": round(dens, 2),
            "band_db": {n: round(float(v), 1)
                        for (n, _, _), v in zip(BANDS, sec_prof)}})
    return {
        "band_balance_db": profile,
        "percussive_share": round(e_p / (e_h + e_p), 3),
        "onsets_per_s_overall": round(len(onsets) / (len(y) / sr), 2),
        "per_section": per_section,
    }, (times, band_db, onset_env)


# --------------------------------------------------------------- L3 harmony --
KS_MAJOR = np.array([6.35, 2.23, 3.48, 2.33, 4.38, 4.09,
                     2.52, 5.19, 2.39, 3.66, 2.29, 2.88])
KS_MINOR = np.array([6.33, 2.68, 3.52, 5.38, 2.60, 3.53,
                     2.54, 4.75, 3.98, 2.69, 3.34, 3.17])
NOTES = ["C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B"]


def estimate_key(chroma_mean):
    best = (-2, None)
    for i in range(12):
        rolled = np.roll(chroma_mean, -i)
        for prof, mode in ((KS_MAJOR, "major"), (KS_MINOR, "minor")):
            r = np.corrcoef(rolled, prof)[0, 1]
            if r > best[0]:
                best = (r, f"{NOTES[i]} {mode}")
    return best[1], round(float(best[0]), 3)


def layer_harmony(y, sr, sections):
    y_harm = librosa.effects.harmonic(y)
    chroma = librosa.feature.chroma_cqt(y=y_harm, sr=sr)
    times = librosa.times_like(chroma[0], sr=sr)
    key, conf = estimate_key(chroma.mean(axis=1))

    per_section = []
    for sec in sections:
        m = (times >= sec["start_s"]) & (times < sec["end_s"])
        if m.any():
            k, c = estimate_key(chroma[:, m].mean(axis=1))
            per_section.append({"label": sec["label"],
                                "start_s": sec["start_s"], "key": k,
                                "confidence": c})

    f0, voiced, _ = librosa.pyin(
        y_harm, fmin=float(librosa.note_to_hz("C2")),
        fmax=float(librosa.note_to_hz("C6")), sr=sr)
    v = f0[voiced.astype(bool)] if voiced is not None else np.array([])
    melody = None
    if v.size > 20:
        melody = {
            "voiced_fraction": round(float(np.mean(voiced)), 2),
            "median_pitch": librosa.hz_to_note(float(np.median(v))),
            "range_semitones": round(float(
                12 * np.log2(np.percentile(v, 95) / np.percentile(v, 5))), 1)}
    return {"key": key, "key_confidence": conf,
            "per_section_keys": per_section, "dominant_voice": melody}, \
        (chroma, times)


# ----------------------------------------------------------------- figures --
def sec_color(label):
    return CATEGORICAL[(ord(label) - ord("A")) % len(CATEGORICAL)]


def draw_sections(ax, sections, ymin, ymax, label_y=None):
    for sec in sections:
        ax.axvline(sec["start_s"], color=INK2, lw=0.7, alpha=0.6)
        if label_y is not None and sec["end_s"] - sec["start_s"] > 5:
            ax.text((sec["start_s"] + sec["end_s"]) / 2, label_y,
                    sec["label"], ha="center", va="center",
                    fontsize=8, fontweight="bold",
                    color=sec_color(sec["label"]))


def fig_structure(out, title, y, sr, st, sections, phrases=()):
    st_times, st_db = st
    t = np.arange(len(y)) / sr
    step = max(1, len(y) // 4000)
    fig, ax = plt.subplots(figsize=(9, 2.8))
    ax.fill_between(t[::step], y[::step], -y[::step],
                    color=GRID, lw=0, alpha=0.9)
    for p in phrases:
        ax.axvline(p["start_s"], ymin=0, ymax=0.05, color=INK2, lw=0.7)
    ax2 = ax.twinx()  # loudness overlays the waveform on its own scale
    ax2.plot(st_times, st_db, color=CATEGORICAL[0], lw=2)
    ax2.set_ylabel("short-term RMS (dBFS)", color=INK2)
    ax2.grid(False)
    ax2.spines["top"].set_visible(False)
    ax.set_yticks([])
    draw_sections(ax, sections, -1, 1, label_y=0.88)
    ax.set_ylim(-1, 1)
    ax.set_xlim(0, t[-1])
    ax.set_xlabel("time (s)")
    ax.set_title(f"{title} — waveform, loudness, sections")
    fig.tight_layout()
    fig.savefig(out / "fig-structure.png", dpi=180)
    plt.close(fig)


def fig_ssm(out, title, ssm, beat_times, bounds):
    fig, ax = plt.subplots(figsize=(4.6, 4.6))
    ax.imshow(ssm, origin="lower", cmap=SEQ_CMAP, aspect="equal",
              extent=[beat_times[0], beat_times[-1],
                      beat_times[0], beat_times[-1]])
    for b in bounds[1:-1]:
        ax.axvline(beat_times[b], color=INK, lw=0.6, alpha=0.5)
        ax.axhline(beat_times[b], color=INK, lw=0.6, alpha=0.5)
    ax.grid(False)
    ax.set_xlabel("time (s)")
    ax.set_ylabel("time (s)")
    ax.set_title(f"{title} — self-similarity (beat-synced)")
    fig.tight_layout()
    fig.savefig(out / "fig-ssm.png", dpi=180)
    plt.close(fig)


def fig_arrangement(out, title, times, band_db, sections):
    fig, ax = plt.subplots(figsize=(9, 2.8))
    ax.imshow(np.clip(band_db, -50, 0), origin="lower", aspect="auto",
              cmap=SEQ_CMAP, extent=[times[0], times[-1], 0, len(BANDS)],
              vmin=-50, vmax=0)
    ax.set_yticks([i + 0.5 for i in range(len(BANDS))])
    ax.set_yticklabels([b[0] for b in BANDS])
    draw_sections(ax, sections, 0, len(BANDS), label_y=len(BANDS) - 0.4)
    ax.grid(False)
    ax.set_xlabel("time (s)")
    ax.set_title(f"{title} — band energy over time (dB rel. max)")
    fig.tight_layout()
    fig.savefig(out / "fig-arrangement.png", dpi=180)
    plt.close(fig)


def fig_chroma(out, title, chroma, times, sections, keys):
    fig, ax = plt.subplots(figsize=(9, 2.6))
    ax.imshow(chroma, origin="lower", aspect="auto", cmap=SEQ_CMAP,
              extent=[times[0], times[-1], 0, 12])
    ax.set_yticks([i + 0.5 for i in range(12)])
    ax.set_yticklabels(NOTES, fontsize=7)
    draw_sections(ax, sections, 0, 12)
    for k in keys:
        ax.text(k["start_s"] + 1, 11.3, k["key"], fontsize=7,
                color=INK, fontweight="bold")
    ax.grid(False)
    ax.set_xlabel("time (s)")
    ax.set_title(f"{title} — chroma + per-section key")
    fig.tight_layout()
    fig.savefig(out / "fig-chroma.png", dpi=180)
    plt.close(fig)


# ------------------------------------------------------------------- report --
def write_markdown(out, rpt):
    m, s, a, h = (rpt["master"], rpt["structure"],
                  rpt["arrangement"], rpt["harmony"])
    lines = [f"# {rpt['title']} — {rpt['artist']}", ""]
    lines += [
        f"- **duration** {m['duration_s']} s · **tempo** {s['tempo_bpm']} bpm"
        f" · **key** {h['key']} ({h['key_confidence']})",
        f"- **loudness** {m['integrated_lufs']} LUFS ·"
        f" LRA≈{m['loudness_range_db_approx']} dB ·"
        f" crest {m['crest_factor_db']} dB ·"
        f" true peak≈{m['true_peak_dbtp_approx']} dBTP",
        f"- **spectrum** centroid {m['spectral_centroid_hz_median']} Hz ·"
        f" tilt {m['spectral_tilt_db_per_decade']} dB/decade",
        f"- **rhythm** {a['onsets_per_s_overall']} onsets/s ·"
        f" percussive share {a['percussive_share']}",
        "", "## sections", "",
        "| # | label | start | end | bars≈ | onsets/s |",
        "|---|-------|-------|-----|-------|----------|"]
    for i, (sec, per) in enumerate(zip(s["sections"], a["per_section"])):
        lines.append(
            f"| {i+1} | {sec['label']} | {sec['start_s']:.0f}s |"
            f" {sec['end_s']:.0f}s | {sec['bars_approx']} |"
            f" {per['onsets_per_s']} |")
    lines += ["", "## band balance (dB rel. loudest band)", ""]
    for k, v in a["band_balance_db"].items():
        lines.append(f"- {k}: {v}")
    (out / "REPORT.md").write_text("\n".join(lines) + "\n")


def study(path, out, title, artist):
    out.mkdir(parents=True, exist_ok=True)
    y_stereo, sr = librosa.load(path, sr=None, mono=False)
    y = librosa.to_mono(y_stereo)

    print("· L0 master")
    master, st = layer_master(path, y_stereo, y, sr)
    print("· L1 structure")
    structure, (beat_times, ssm, nov, bounds) = layer_structure(y, sr)
    print("· L2 arrangement")
    arrangement, (bt, band_db, onset_env) = layer_arrangement(
        y, sr, structure["sections"])
    print("· L3 harmony")
    harmony, (chroma, ct) = layer_harmony(y, sr, structure["sections"])

    rpt = {"title": title, "artist": artist,
           "master": master, "structure": structure,
           "arrangement": arrangement, "harmony": harmony}
    (out / "report.json").write_text(json.dumps(rpt, indent=2))
    write_markdown(out, rpt)

    print("· figures")
    label = f"{artist} — {title}" if artist else title
    fig_structure(out, label, y, sr, st, structure["sections"],
                  structure["phrases"])
    fig_ssm(out, label, ssm, beat_times, bounds)
    fig_arrangement(out, label, bt, band_db, structure["sections"])
    fig_chroma(out, label, chroma, ct, structure["sections"],
               harmony["per_section_keys"])
    print(f"→ {out}")
    return rpt


def main():
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("audio")
    ap.add_argument("--out", required=True)
    ap.add_argument("--title", default=None)
    ap.add_argument("--artist", default="")
    args = ap.parse_args()
    path = Path(args.audio)
    title = args.title or path.stem
    study(path, Path(args.out), title, args.artist)


if __name__ == "__main__":
    main()
