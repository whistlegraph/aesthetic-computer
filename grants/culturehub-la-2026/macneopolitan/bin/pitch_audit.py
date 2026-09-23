"""Measure the exact audio events placed in a preview, before the common mix gain.

Pitch estimators never receive the expected note. WORLD tracks vocal f0;
autocorrelation independently checks each note. Instrument pitch comes from its
rendered waveform. Missing/unvoiced notes remain in the coverage denominator.
"""
import hashlib
import json
import math
from pathlib import Path
import warnings

import numpy as np
import soundfile as sf
from scipy.signal import correlate, find_peaks


def measured_hz(y, sr):
    """Unconditioned normalized autocorrelation, 50–1600 Hz, at most 120 ms."""
    if len(y) < sr * .04:
        return None
    n = min(len(y), round(sr * .12))
    a = (len(y) - n) // 2
    x = np.asarray(y[a:a+n], dtype=float)
    x -= x.mean()
    if np.sqrt(np.mean(x*x)) < 1e-5:
        return None
    c = correlate(x, x, mode="full", method="fft")[n-1:]
    energy = np.r_[0., np.cumsum(x*x)]
    lag = np.arange(len(c))
    c /= np.sqrt(np.maximum((energy[n-lag]) * (energy[n]-energy[lag]), 1e-24))
    lo, hi = max(2, int(sr / 1600)), min(n-2, int(sr / 50))
    peaks, _ = find_peaks(c[lo:hi])
    peaks += lo
    if not len(peaks) or c[peaks].max() < .6:
        return None
    # Earliest strong period avoids choosing a multiple of the fundamental.
    p = peaks[c[peaks] >= max(.6, c[peaks].max() * .93)][0]
    den = c[p-1] - 2*c[p] + c[p+1]
    delta = .5 * (c[p-1]-c[p+1]) / den if den else 0
    return float(sr / (p + delta))


def midi_hz(f):
    return 69 + 12 * np.log2(f / 440)


def audit_render(events, mix, sr, prefix, gain):
    with warnings.catch_warnings():
        warnings.simplefilter("ignore")
        import pyworld as pw
    import matplotlib
    matplotlib.use("Agg")
    import matplotlib.pyplot as plt

    folder = Path(str(prefix) + "-audio")
    folder.mkdir(parents=True, exist_ok=True)
    members = list(dict.fromkeys(e["member"] for e in events))
    stems = {f"{m}-{kind}": np.zeros_like(mix) for m in members for kind in ("vocal", "instrument")}
    report = {"scope": "Exact vocal and instrument events used in this preview; no live hardware measurement.",
              "method": "WORLD vocal f0 at 5 ms; independent autocorrelation, 50–1600 Hz. Expected notes are used only after estimation. Note interiors exclude the first and last 20%.",
              "sample_rate": sr, "mix_gain": float(gain), "notes": [], "members": {}}
    fig, axes = plt.subplots(len(members), 1, figsize=(16, 9), sharex=True, layout="constrained", squeeze=False)
    for event in events:
        y = np.asarray(event["samples"], dtype=np.float64)
        start = event["start"]
        stem = stems[f"{event['member']}-{event['kind']}"]
        pos = int(start * sr)
        a, b = max(0, pos), min(len(stem), pos + len(y))
        if a < b:
            stem[a:b] += y[a-pos:b-pos] * gain
        ax = axes[members.index(event["member"]), 0]
        if event["kind"] == "vocal":
            f0, times = pw.harvest(y, sr, f0_floor=50, f0_ceil=1000, frame_period=5)
            # StoneMask refines the estimates from the actual waveform.
            f0 = pw.stonemask(y, f0, times, sr)
            times += start
            voiced = f0 > 0
            ax.scatter(times[voiced], midi_hz(f0[voiced]), s=2, color="#297f43", alpha=.6)
        for t0, t1, target in event["targets"]:
            lo, hi = t0 + .2*(t1-t0), t1 - .2*(t1-t0)
            x = y[max(0, round((lo-start)*sr)):max(0, round((hi-start)*sr))]
            ac = measured_hz(x, sr)
            row = dict(kind=event["kind"], member=event["member"], text=event["text"],
                       start=t0, end=t1, written_midi=target, autocorrelation_hz=ac, role=event.get("role"))
            if event["kind"] == "vocal":
                window = (times >= lo) & (times < hi)
                z = f0[window & voiced]
                f = float(np.median(z)) if len(z) >= 3 else None
                err = 100*(midi_hz(z)-target) if len(z) else np.array([])
                row.update(voiced_frames=len(z), total_frames=int(window.sum()),
                           frames_within_50_cents=int((np.abs(err)<=50).sum()),
                           p90_abs_cents=float(np.percentile(np.abs(err),90)) if len(err) else None)
                ax.plot([t0,t1], [target,target], color="black", linewidth=1.1)
            else:
                f = ac
                if f: ax.scatter([(t0+t1)/2], [midi_hz(f)], marker="x", s=10, color="#aa5c19")
            row.update(hz=f, cents=float(100*(midi_hz(f)-target)) if f else None,
                       estimator_difference_cents=float(1200*math.log2(ac/f)) if ac and f else None)
            report["notes"].append(row)

    vocal = [n for n in report["notes"] if n["kind"] == "vocal"]
    instrument = [n for n in report["notes"] if n["kind"] == "instrument"]
    for m in members:
        ns = [n for n in vocal if n["member"] == m]
        good = [n for n in ns if n["hz"]]
        frames = sum(n["voiced_frames"] for n in ns)
        total = sum(n["total_frames"] for n in ns)
        pair = [(n["written_midi"], float(midi_hz(n["hz"]))) for n in good]
        report["members"][m] = {
            "notes_expected": len(ns), "notes_measured": len(good),
            "notes_within_50_cents": sum(abs(n["cents"])<=50 for n in good),
            "mean_abs_cents": float(np.mean([abs(n["cents"]) for n in good])) if good else None,
            "voiced_coverage_pct": 100*frames/total if total else 0,
            "voiced_frames_within_50_cents_pct": 100*sum(n["frames_within_50_cents"] for n in ns)/frames if frames else 0,
            "contour_correlation": float(np.corrcoef(np.array(pair).T)[0,1]) if len(pair)>2 and np.ptp([p[0] for p in pair]) else None,
            "estimator_disagreements_over_50_cents": sum(abs(n["estimator_difference_cents"])>50 for n in good if n["estimator_difference_cents"] is not None),
        }
    # Time overlaps are measured against measured audio, including own doubles.
    # Signed, absolute-register intervals retain octave errors (no modulo wrap).
    pairs = []
    for i, left in enumerate(vocal):
        for right in instrument + vocal[i+1:]:
            if left["member"] == right["member"] and right["kind"] == "vocal": continue
            a, b = max(left["start"],right["start"]), min(left["end"],right["end"])
            if b <= a or not left["hz"] or not right["hz"]: continue
            actual = 12*math.log2(left["hz"]/right["hz"])
            expected = left["written_midi"]-right["written_midi"]
            pairs.append(dict(start=a, end=b, left=left["member"], right=f"{right['member']} {right['kind']}",
                              expected_semitones=expected, measured_semitones=actual, error_cents=100*(actual-expected)))
    report["harmony"] = {"measured_pairs":len(pairs), "pairs_over_50_cents":sum(abs(p["error_cents"])>50 for p in pairs), "pairs":pairs}
    report["instruments"] = {"notes":len(instrument), "measured":sum(bool(n["hz"]) for n in instrument),
                              "max_abs_cents":max((abs(n["cents"]) for n in instrument if n["hz"]),default=None)}
    summed = np.zeros_like(mix)
    for name, stem in stems.items():
        sf.write(folder / f"{name}.wav", stem, sr, subtype="FLOAT")
        summed += stem
    for kind in ("vocal", "instrument"):
        sf.write(folder / f"{kind}s.wav", sum(stems[f"{m}-{kind}"] for m in members), sr, subtype="FLOAT")
    report["stem_sum_max_error"] = float(np.max(np.abs(summed-mix)))
    report["mix_wav_sha256"] = hashlib.sha256(Path(str(prefix)+".wav").read_bytes()).hexdigest()
    for i,m in enumerate(members):
        ax=axes[i,0]
        ax.set_ylabel(f"{m}\nMIDI pitch", fontsize=12)
        ax.grid(alpha=.15)
        ns=[n for n in vocal if n["member"]==m]
        if ns: ax.set_ylim(min(n["written_midi"] for n in ns)-3, max(n["written_midi"] for n in ns)+15)
    axes[0,0].set_title("Chorus: written vocal notes (black), measured voice (green), measured instruments (orange)", fontsize=15)
    axes[-1,0].set_xlabel("Seconds from the first note")
    fig.savefig(str(prefix)+"-pitch.png",dpi=150)
    plt.close(fig)
    Path(str(prefix)+"-pitch.json").write_text(json.dumps(report,indent=2,allow_nan=False)+"\n")
    print(json.dumps({k:report[k] for k in ("members","instruments","stem_sum_max_error")},indent=2))
    return report
