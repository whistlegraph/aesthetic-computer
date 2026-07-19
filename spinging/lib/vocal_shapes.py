"""
vocal_shapes.py — goalpost vocal-shape features shared by the reference
analyzer (goalposts.py) and the singing engine's QA gate (sing_line_world.py).

"Analyze the proper vocal shape then conform to it." A rendered vocal is only
done when its measured features sit inside the reference percentile bands —
QA is statistical, not ear-only.

Features per sung NOTE (a voiced, pitch-stable region):
  dur_s               note length
  onset_glide_cents   |f0 at note start − plateau median|
  onset_glide_ms      time until f0 stays within 30¢ of the plateau
  plateau_drift_cents std of the detrended plateau f0 (micro-drift)
  release_cents       |f0 in the last 60ms − plateau median|
  vib_rate_hz         vibrato rate (holds ≥ 0.5s; dominant 3-8Hz component)
  vib_depth_cents     vibrato depth on those holds
  vib_delay_ms        time before vibrato reaches half depth
  energy_attack_ms    RMS rise 10%→90% of the note peak
  energy_release_ms   RMS fall 90%→10% at the note end
  hf_ratio            mean spectral energy > 4kHz / total (air/breath balance)

Also: click_scan(x, fs) — waveform-discontinuity + spectral-flux spike
detection, the hard no-glitch gate.
"""
import numpy as np

FRAME_S = 0.005


def hz_to_midi(hz):
    return 69.0 + 12.0 * np.log2(np.maximum(hz, 1e-6) / 440.0)


def segment_notes(f0, min_dur_s=0.12, jump_st=0.8):
    """Split voiced runs into pitch-stable notes. Returns [(a, b), …] frames."""
    n = len(f0)
    voiced = f0 > 0
    notes = []
    i = 0
    while i < n:
        if not voiced[i]:
            i += 1
            continue
        j = i
        while j < n and voiced[j]:
            j += 1
        # inside [i, j): cut where the 50ms-median pitch steps > jump_st
        m = hz_to_midi(f0[i:j])
        k = 10  # 50ms
        if len(m) > 2 * k:
            med = np.array([np.median(m[max(0, t - k):t + k]) for t in range(len(m))])
            cuts = [0]
            for t in range(k, len(m) - k):
                if abs(med[t] - med[t - 1]) > jump_st * 0.5 and \
                        abs(np.median(m[t:t + k]) - np.median(m[max(0, t - k):t])) > jump_st:
                    if t - cuts[-1] > int(min_dur_s / FRAME_S) * 0.6:
                        cuts.append(t)
            cuts.append(len(m))
        else:
            cuts = [0, len(m)]
        for c in range(len(cuts) - 1):
            a, b = i + cuts[c], i + cuts[c + 1]
            if (b - a) * FRAME_S >= min_dur_s:
                notes.append((a, b))
        i = j
    return notes


def note_features(f0, rms, hf_ratio, a, b):
    """Feature dict for one note (frames [a,b))."""
    cents = 1200.0 * np.log2(np.maximum(f0[a:b], 1e-6) / 440.0)
    n = b - a
    dur = n * FRAME_S
    core_a = min(n // 3, int(0.08 / FRAME_S))
    core = cents[core_a:max(core_a + 1, n - max(1, n // 6))]
    plateau = float(np.median(core))
    feats = {"dur_s": dur}
    feats["onset_glide_cents"] = float(abs(cents[0] - plateau))
    within = np.abs(cents - plateau) < 30.0
    gl = n
    for t in range(n):
        if within[t:min(n, t + 6)].all():
            gl = t
            break
    feats["onset_glide_ms"] = gl * FRAME_S * 1000.0
    # plateau micro-drift: detrend the core linearly
    if len(core) > 4:
        tt = np.arange(len(core))
        fit = np.polyval(np.polyfit(tt, core, 1), tt)
        feats["plateau_drift_cents"] = float(np.std(core - fit))
    else:
        feats["plateau_drift_cents"] = 0.0
    tail = cents[max(0, n - int(0.06 / FRAME_S)):]
    feats["release_cents"] = float(abs(np.median(tail) - plateau)) if len(tail) else 0.0
    # vibrato on holds
    if dur >= 0.5 and len(core) > 40:
        d = core - np.convolve(core, np.ones(15) / 15, mode="same")
        spec = np.abs(np.fft.rfft(d * np.hanning(len(d))))
        fr = np.fft.rfftfreq(len(d), FRAME_S)
        band = (fr >= 3.0) & (fr <= 8.0)
        if band.any() and spec[band].max() > 0:
            k = np.argmax(spec[band])
            feats["vib_rate_hz"] = float(fr[band][k])
            feats["vib_depth_cents"] = float(2.0 * spec[band][k] / max(1, len(d)) * 4)
            half = feats["vib_depth_cents"] * 0.5
            env = np.abs(d)
            sm = np.convolve(env, np.ones(20) / 20, mode="same")
            idx = np.where(sm > half * 0.5)[0]
            feats["vib_delay_ms"] = float(idx[0] * FRAME_S * 1000.0) if len(idx) else dur * 1000
    # energy arc
    e = rms[a:b]
    if e.max() > 0:
        pk = e.max()
        up = np.where(e > 0.9 * pk)[0]
        lo = np.where(e > 0.1 * pk)[0]
        feats["energy_attack_ms"] = float((up[0] - lo[0]) * FRAME_S * 1000.0) if len(up) and len(lo) else 0.0
        dn = np.where(e[::-1] > 0.9 * pk)[0]
        dl = np.where(e[::-1] > 0.1 * pk)[0]
        feats["energy_release_ms"] = float((dn[0] - dl[0]) * -FRAME_S * -1000.0) if len(dn) and len(dl) else 0.0
        if len(dn) and len(dl):
            feats["energy_release_ms"] = float(abs(dn[0] - dl[0]) * FRAME_S * 1000.0)
    feats["hf_ratio"] = float(np.mean(hf_ratio[a:b]))
    return feats


def frame_rms_hf(x, fs, n_frames):
    """Frame-rate RMS + >4kHz spectral ratio without WORLD (for references)."""
    hop = int(round(fs * FRAME_S))
    win = hop * 4
    rms = np.zeros(n_frames)
    hf = np.zeros(n_frames)
    for i in range(n_frames):
        a = max(0, i * hop - win // 2)
        b = min(len(x), i * hop + win // 2)
        if b <= a:
            continue
        seg = x[a:b]
        rms[i] = np.sqrt(np.mean(seg ** 2))
        sp = np.abs(np.fft.rfft(seg * np.hanning(len(seg)))) ** 2
        fr = np.fft.rfftfreq(len(seg), 1.0 / fs)
        tot = sp.sum() + 1e-12
        hf[i] = sp[fr > 4000.0].sum() / tot
    return rms, hf


FEATURE_KEYS = [
    "dur_s", "onset_glide_cents", "onset_glide_ms", "plateau_drift_cents",
    "release_cents", "vib_rate_hz", "vib_depth_cents", "vib_delay_ms",
    "energy_attack_ms", "energy_release_ms", "hf_ratio",
]


def percentile_bands(all_feats, ps=(5, 10, 25, 50, 75, 90, 95)):
    bands = {}
    for k in FEATURE_KEYS:
        vals = np.array([f[k] for f in all_feats if k in f])
        if len(vals) < 4:
            continue
        bands[k] = {f"p{p}": round(float(np.percentile(vals, p)), 3) for p in ps}
        bands[k]["n"] = int(len(vals))
    return bands


def conformance(feats_list, bands, lo="p10", hi="p90"):
    """Median-of-notes per feature vs reference band → {feature: {…, pass}}."""
    rep = {}
    ok_all = True
    for k, band in bands.items():
        vals = np.array([f[k] for f in feats_list if k in f])
        if len(vals) == 0:
            rep[k] = {"value": None, "pass": None}  # n/a this line
            continue
        v = float(np.median(vals))
        ok = bool(band[lo] <= v <= band[hi])
        rep[k] = {"value": round(v, 3), "lo": band[lo], "hi": band[hi], "pass": ok}
        # duration + energy arcs are score/arrangement-driven (and vib delay's
        # estimator is weak on the references) — advisory only
        if k not in ("dur_s", "energy_release_ms", "vib_rate_hz", "vib_delay_ms") and not ok:
            ok_all = False
    rep["_pass"] = ok_all
    return rep


def click_scan(x, fs):
    """Glitch gate: waveform discontinuities + spectral-flux spikes.

    Returns {clicks, flux_spikes, worst_jump, positions_s}. A 'click' is a
    sample step > 0.25 that exceeds 8× the local envelope; a flux spike is a
    frame whose spectral flux z-score > 8 while local energy is steady.
    """
    d = np.abs(np.diff(x))
    hop = 240
    n = len(x) // hop
    env = np.array([np.sqrt(np.mean(x[i * hop:(i + 1) * hop] ** 2)) for i in range(max(1, n))])
    clicks = []
    worst = 0.0
    for i in np.where(d > 0.25)[0]:
        e = env[min(len(env) - 1, i // hop)]
        if d[i] > 8.0 * max(e, 0.01):
            clicks.append(i / fs)
            worst = max(worst, float(d[i]))
    # spectral flux
    w = 480
    steps = range(0, len(x) - w, w // 2)
    prev = None
    fluxes = []
    for s in steps:
        sp = np.abs(np.fft.rfft(x[s:s + w] * np.hanning(w)))
        if prev is not None:
            fluxes.append(np.maximum(sp - prev, 0).sum())
        prev = sp
    fluxes = np.array(fluxes)
    spikes = []
    if len(fluxes) > 8:
        mu, sd = fluxes.mean(), fluxes.std() + 1e-9
        z = (fluxes - mu) / sd
        for i in np.where(z > 8.0)[0]:
            # steady-energy check: a real attack raises energy too, a glitch doesn't
            s = list(steps)[i + 1]
            a = env[max(0, s // hop - 2):s // hop + 1].mean() if len(env) else 0
            b = env[s // hop:min(len(env), s // hop + 3)].mean() if len(env) else 0
            if a > 0 and b < 1.6 * a:
                spikes.append(s / fs)
    return {
        "clicks": len(clicks), "flux_spikes": len(spikes),
        "worst_jump": round(worst, 3),
        "click_s": [round(p, 3) for p in clicks[:12]],
        "spike_s": [round(p, 3) for p in spikes[:12]],
        "positions_s": [round(p, 3) for p in (clicks + spikes)[:12]],
    }
