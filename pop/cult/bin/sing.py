#!/usr/bin/env python3
"""
sing.py — speech-to-singing for the cult remix.

This is the full Saitou (WASPAA 2007) recipe, not a time-stretch. It is a
thicker sibling of pop/bin/pitchsnap_world.py: that script replaces the f0
contour of a slice, this one ALSO lengthens the phonemes to fit a score and
adds the singer's-formant ring — the three things Saitou lists as missing
from speech.

  audio
    → harvest / stonemask        (f0)
    → cheaptrick                 (spectral envelope = vowel identity)
    → d4c                        (aperiodicity = breath, consonants)
    → DURATION CONTROL: warp the frame axis so vowels sustain and
      consonants stay short (a spoken "dash" becomes a held "daaaash",
      not a dragged "d-a-a-a-sh")
    → F0 SUBSTITUTION: write the score's contour, with Saitou's four
      sub-effects — overshoot, preparation, vibrato, fine fluctuation
    → SINGER'S FORMANT: a gentle gaussian lift near 2.8 kHz in the
      envelope (Sundberg 1974). Kept modest — @jeffrey does not want
      piercing presence.
    → synthesize
    → re-impose the voiced/unvoiced structure in the time domain and
      composite the (warped) original back in over the unvoiced regions,
      so "s"/"t"/"k" stay real (SPEECH-TO-SINGING-V2 §6)
    → de-ess, raised-cosine top and tail

Everything SPEECH-TO-SINGING-V2 flags is applied: f0 is interpolated
through unvoiced gaps before synthesis (§2, kills the word-start stutter),
cheaptrick runs with an f0_floor tuned per source (§2, kills held-note
ring), unvoiced regions come from the source (§6), and verification uses
librosa.pyin rather than naive autocorrelation (§1).

v6 adds --autotune hard: an Eiffel-65 "Blue" retune. See the
`hard_autotune_contour` docstring for the DSP.

  sing.py in.wav out.wav --notes "B3:0.35,D4:1.20" [--vibrato-cents 35] ...

--notes is `NOTE:seconds` pairs. The output is exactly the sum of those
seconds (plus the release tail), so the caller scores in real time.
"""
import argparse
import sys

import numpy as np
import soundfile as sf
import pyworld as pw
from scipy.signal import butter, sosfilt

try:
    import librosa
    HAS_LIBROSA = True
except ImportError:  # verification is optional; rendering is not
    HAS_LIBROSA = False

FRAME_MS = 5.0
NOTE_SEMI = {"c": 0, "d": 2, "e": 4, "f": 5, "g": 7, "a": 9, "b": 11}


def note_to_midi(s):
    s = s.strip().lower()
    semi = NOTE_SEMI[s[0]]
    i = 1
    if i < len(s) and s[i] in "#b":
        semi += 1 if s[i] == "#" else -1
        i += 1
    return 12 * (int(s[i:]) + 1) + semi


def midi_to_hz(m):
    return 440.0 * (2.0 ** ((m - 69.0) / 12.0))


# ── duration control ──────────────────────────────────────────────────
def sustain_weights(f0, energy, frame_ms=FRAME_MS):
    """Per-frame willingness to be stretched.

    A frame earns stretch by being voiced, loud, and already a little way
    into its vowel. Consonants (unvoiced) get zero — that is the whole
    point of the duration-control model: only the nucleus sustains.
    """
    voiced = f0 > 0
    w = np.zeros(len(f0))
    if not voiced.any():
        return w
    # time since this voiced run began, in frames
    since = np.zeros(len(f0))
    run = 0
    for i in range(len(f0)):
        run = run + 1 if voiced[i] else 0
        since[i] = run
    onset_guard = np.clip(since * frame_ms / 70.0, 0.0, 1.0)   # 70 ms
    e = energy / (energy.max() + 1e-12)
    w = voiced.astype(float) * onset_guard * np.clip(e, 0.0, 1.0) ** 0.5
    return w


def build_warp(n_src, weights, n_out):
    """Monotone map from output frame index → fractional source frame.

    Extra time is spread across the source frames in proportion to
    `weights`, so silence and consonants pass through at 1x and the vowel
    absorbs the whole hold.
    """
    extra = max(0.0, float(n_out - n_src))
    total_w = weights.sum()
    if total_w <= 1e-9:
        per = np.full(n_src, n_out / max(1, n_src))
    else:
        per = 1.0 + extra * weights / total_w
        if n_out < n_src:                     # compression: scale uniformly
            per = per * (n_out / per.sum())
    cum = np.concatenate([[0.0], np.cumsum(per)])
    return np.interp(np.arange(n_out), cum, np.arange(n_src + 1)).clip(0, n_src - 1)


def shimmer(src_pos, weights_at, rate_hz, depth_frames, frame_ms=FRAME_MS):
    """Micro-wobble the read head inside held regions.

    A frozen cheaptrick envelope held for a second reads as a sterile
    ring; nudging the read position by a couple of frames at ~0.8 Hz
    restores the tiny formant drift a real held vowel has.
    """
    t = np.arange(len(src_pos)) * frame_ms / 1000.0
    wob = np.sin(2 * np.pi * rate_hz * t) * depth_frames * weights_at
    return src_pos + wob


# ── the score's f0 contour ────────────────────────────────────────────
def target_contour(notes, durs, n_out, args):
    """Saitou's f0 model: a stepped target plus four sub-effects."""
    fp = FRAME_MS / 1000.0
    bounds = np.concatenate([[0.0], np.cumsum(durs)])
    idx = (bounds / fp).round().astype(int).clip(0, n_out)
    idx[-1] = n_out

    midis = np.array([note_to_midi(n) for n in notes], float)
    log_t = np.zeros(n_out)
    for k in range(len(notes)):
        log_t[idx[k]:idx[k + 1]] = np.log2(midi_to_hz(midis[k]))
    if idx[0] > 0:
        log_t[:idx[0]] = log_t[idx[0]]

    xf = max(1, int(args.xfade_ms / (FRAME_MS)))
    if len(notes) > 1:                                   # legato in log space
        kern = np.hanning(2 * xf + 1)
        kern /= kern.sum()
        log_t = np.convolve(log_t, kern, mode="same")
        log_t[:xf] = log_t[xf]
        log_t[-xf:] = log_t[-xf - 1]

    cent = np.zeros(n_out)
    t = np.arange(n_out) * fp
    for k in range(len(notes)):
        a, b = idx[k], idx[k + 1]
        if b <= a:
            continue
        local = np.arange(b - a) * fp
        rising = k + 1 < len(notes) and midis[k + 1] > midis[k]

        # overshoot — attack sails ~45¢ past the note and settles in 200 ms
        direction = 1.0
        if k > 0:
            direction = 1.0 if midis[k] >= midis[k - 1] else -1.0
        cent[a:b] += direction * args.overshoot_cents * np.exp(-local / 0.20)

        # preparation — a small dip below target before a rising interval
        if rising:
            prep = np.clip((local - (local[-1] - 0.08)) / 0.08, 0.0, 1.0)
            cent[a:b] -= 22.0 * prep

        # vibrato — only on notes long enough to hold one, faded in
        span = (b - a) * fp
        if args.vibrato_cents > 0 and span > 0.45:
            fade = np.clip((local - args.vibrato_onset_ms / 1000.0) / 0.25, 0.0, 1.0)
            cent[a:b] += (np.sin(2 * np.pi * args.vibrato_hz * local)
                          * args.vibrato_cents * fade)

    # fine fluctuation — sub-audible jitter, lowpassed so it is drift not noise
    rng = np.random.default_rng(args.seed)
    jitter = rng.standard_normal(n_out)
    # The kernel has to be odd and no longer than the signal: numpy's
    # mode="same" returns max(M, N), so a 41-frame kernel over a 40-frame
    # note (0.20 s — the dot-only section's whole vocabulary) comes back
    # one sample long and the broadcast fails.
    kl = min(41, n_out if n_out % 2 else n_out - 1)
    k = np.hanning(max(3, kl))
    k /= k.sum()
    jitter = np.convolve(jitter, k, mode="same")[:n_out]
    jitter /= (np.abs(jitter).max() + 1e-9)
    cent += jitter * args.fine_cents

    return 2.0 ** (log_t + cent / 1200.0)


# ── (v6) HARD AUTOTUNE — the Eiffel 65 "Blue" stair-step ──────────────
# @jeffrey: "can we like quantize it more like 'bad' autotune it / eiffel 65
# style". That sound is not a preset, it is three settings pushed to their
# stop, and all three are implemented here rather than named:
#
#   1. SNAP. Every frame's target pitch is forced onto the nearest member of
#      a scale (B natural minor here). Nothing lands between scale degrees,
#      ever, so the pitch alphabet is seven letters wide.
#   2. ZERO RETUNE TIME. There is no portamento, no legato kernel, no
#      overshoot, no preparation and no vibrato. The target jumps from one
#      scale tone to the next inside a single 5 ms WORLD frame, which is the
#      discontinuity you hear as the artifact. (Smooth mode convolves the
#      target with a Hann kernel; hard mode deliberately does not.)
#   3. RHYTHMIC QUANTIZATION. The frames are bucketed onto a note grid
#      (default 125 ms = a sixteenth at 120 BPM) and each bucket is held at
#      ONE pitch — the median of its own contour — so the steps land in time
#      with the record instead of wherever the syllable happened to move.
#
# The material that gets stepped is the PERFORMANCE's own pitch wander: the
# source f0, measured relative to its own median, scaled by --autotune-drive
# and added to the scored note. That is what makes it read as a retune of a
# real take rather than as a sequenced melody — a spoken "dash" whose pitch
# falls a tone across the vowel comes back as two audible steps down.
#
# Formants are never touched: cheaptrick's envelope goes into synthesize()
# unmodified, so this is retuned, not chipmunked, and the words stay legible.
SCALES = {
    # pitch classes, C = 0
    "b-minor": [11, 1, 2, 4, 6, 7, 9],          # B C# D E F# G A
    "b-minor-pentatonic": [11, 2, 4, 6, 9],
    "b-minor-triad": [11, 2, 6],                # chord tones only — coarsest
    "chromatic": list(range(12)),
}


def snap_to_scale(midi, pcs):
    """Nearest member of `pcs` in any octave. Vectorised, no hysteresis —
    hysteresis is what STOPS the stepping, and here the stepping is the
    point; the grid median below is what keeps it from trilling."""
    cand = np.array(pcs, float)
    out = np.empty_like(midi)
    for i, m in enumerate(np.atleast_1d(midi)):
        oct_base = 12 * np.floor(m / 12.0)
        opts = np.concatenate([cand + oct_base - 12, cand + oct_base,
                               cand + oct_base + 12])
        out[i] = opts[np.argmin(np.abs(opts - m))]
    return out


def hard_autotune_contour(notes, durs, n_out, src_log2, args):
    """Stepped target f0 in Hz. See the block comment above."""
    fp = FRAME_MS / 1000.0
    bounds = np.concatenate([[0.0], np.cumsum(durs)])
    idx = (bounds / fp).round().astype(int).clip(0, n_out)
    idx[-1] = n_out

    # the score, as a hard staircase — no legato kernel anywhere
    midis = np.array([note_to_midi(n) for n in notes], float)
    base = np.zeros(n_out)
    for k in range(len(notes)):
        base[idx[k]:idx[k + 1]] = midis[k]
    if idx[0] > 0:
        base[:idx[0]] = midis[0]

    # The performance's own wander — but only its FAST part. A spoken word
    # falls several semitones from start to end; feeding that whole slope to
    # the snapper walks the melody off the score (measured: "run real fast"
    # landing on A3 instead of D4). So the contour is high-passed against its
    # own ~350 ms moving average: the slow glide, which is the melody's job,
    # is dropped, and the micro-wander that survives is what tips frames
    # across scale boundaries and makes the staircase.
    if src_log2 is not None and np.isfinite(src_log2).any():
        w = max(3, int(round(args.autotune_smooth_ms / FRAME_MS)) | 1)
        ker = np.hanning(w); ker /= ker.sum()
        pad = np.pad(np.nan_to_num(src_log2, nan=np.nanmedian(src_log2)),
                     (w // 2, w // 2), mode="edge")
        slow = np.convolve(pad, ker, mode="valid")[:n_out]
        dev = 12.0 * (np.nan_to_num(src_log2) - slow)
        dev = np.clip(dev, -args.autotune_wander, args.autotune_wander)
        dev *= args.autotune_drive
    else:
        dev = np.zeros(n_out)

    # a breath of jitter BEFORE the snap, so frames sitting on a scale
    # boundary tip across it — the digital trill that a real bad retune has
    rng = np.random.default_rng(args.seed + 7)
    j = rng.standard_normal(n_out)
    kl = min(21, n_out if n_out % 2 else n_out - 1)
    k = np.hanning(max(3, kl))
    j = np.convolve(j, k / k.sum(), mode="same")[:n_out]
    j /= (np.abs(j).max() + 1e-9)
    cont = base + dev + j * (args.autotune_jitter_cents / 100.0)
    # A leash on the steps. Without it a short syllable can hand one grid
    # cell a wander peak and the snap lands a fifth off the written note
    # (measured: "i wanna" going D4 → B3). The steps have to be obvious;
    # they do not have to be wrong. Clamped to a third either side, every
    # step is still a real in-scale leap and the word stays legible.
    if args.autotune_range > 0:
        cont = np.clip(cont, base - args.autotune_range, base + args.autotune_range)

    # rhythmic quantization: one pitch per grid cell, held flat
    step = max(1, int(round(args.autotune_grid_ms / FRAME_MS)))
    pcs = SCALES.get(args.autotune_scale, SCALES["b-minor"])
    out = np.empty(n_out)
    for a in range(0, n_out, step):
        b = min(n_out, a + step)
        out[a:b] = snap_to_scale(np.array([np.median(cont[a:b])]), pcs)[0]

    # retain < 1 walks it back toward the unquantized target; 1.0 = full snap
    if args.autotune_retain < 1.0:
        out = cont + args.autotune_retain * (out - cont)
    return midi_to_hz(out)


# ── singer's formant ──────────────────────────────────────────────────
def singers_formant(sp, fs, fft_size, db, centre=2800.0, width=450.0):
    if db <= 0:
        return sp
    freqs = np.linspace(0.0, fs / 2.0, sp.shape[1])
    lift_db = db * np.exp(-((freqs - centre) / width) ** 2)
    return sp * (10.0 ** (lift_db / 10.0))[None, :]      # sp is power


# ── cleanup ───────────────────────────────────────────────────────────
def deess(y, fs, thresh=0.05, ratio=0.45):
    """Sidechained shelf on 5–9 kHz. WORLD colours fricatives; this pulls
    the sting back out without an EQ boost anywhere else in the band."""
    sos = butter(2, [5000 / (fs / 2), min(0.99, 9000 / (fs / 2))], btype="band", output="sos")
    band = sosfilt(sos, y)
    env = np.abs(band)
    a = np.exp(-1.0 / (0.004 * fs))
    for i in range(1, len(env)):                          # one-pole follower
        env[i] = max(env[i], a * env[i - 1])
    over = np.clip(env / thresh - 1.0, 0.0, None)
    gain = 1.0 / (1.0 + ratio * over)
    return y - band * (1.0 - gain)


def top_and_tail(y, fs, attack_ms, release_ms):
    n = len(y)
    a = min(int(attack_ms / 1000 * fs), n // 2)
    r = min(int(release_ms / 1000 * fs), n // 2)
    if a > 1:
        y[:a] *= 0.5 - 0.5 * np.cos(np.pi * np.arange(a) / a)
    if r > 1:
        y[n - r:] *= 0.5 + 0.5 * np.cos(np.pi * np.arange(r) / r)
    return y


def warp_source(x, src_pos, fs):
    """Granular resynthesis of the ORIGINAL along the same warp, so the
    unvoiced composite lines up frame-for-frame with the WORLD output."""
    hop = int(round(fs * FRAME_MS / 1000.0))
    grain = hop * 4
    win = np.hanning(grain)
    out = np.zeros(len(src_pos) * hop + grain)
    norm = np.zeros_like(out)
    for i in range(len(src_pos)):
        s = int(round(src_pos[i] * hop)) - grain // 2
        o = i * hop
        a0, a1 = max(0, s), min(len(x), s + grain)
        if a1 <= a0:
            continue
        g0 = a0 - s
        seg = x[a0:a1] * win[g0:g0 + (a1 - a0)]
        out[o + g0:o + g0 + len(seg)] += seg
        norm[o + g0:o + g0 + len(seg)] += win[g0:g0 + (a1 - a0)]
    return out / np.maximum(norm, 1e-6)


def main():
    p = argparse.ArgumentParser()
    p.add_argument("in_wav")
    p.add_argument("out_wav")
    p.add_argument("--notes", required=True,
                   help='"NOTE:seconds" pairs, e.g. "B3:0.30,D4:1.40"')
    p.add_argument("--f0-floor", type=float, default=90.0)
    p.add_argument("--f0-ceil", type=float, default=700.0)
    p.add_argument("--vibrato-hz", type=float, default=5.4)
    p.add_argument("--vibrato-cents", type=float, default=32.0)
    p.add_argument("--vibrato-onset-ms", type=float, default=260.0)
    p.add_argument("--overshoot-cents", type=float, default=42.0)
    p.add_argument("--fine-cents", type=float, default=7.0)
    p.add_argument("--xfade-ms", type=float, default=55.0)
    p.add_argument("--formant-db", type=float, default=3.2)
    p.add_argument("--shimmer-hz", type=float, default=0.85)
    p.add_argument("--shimmer-frames", type=float, default=2.2)
    p.add_argument("--attack-ms", type=float, default=14.0)
    p.add_argument("--release-ms", type=float, default=110.0)
    p.add_argument("--deess", type=float, default=0.05)
    p.add_argument("--gain", type=float, default=0.92)
    p.add_argument("--seed", type=int, default=20220120)
    p.add_argument("--autotune", choices=["off", "hard"], default="off",
                   help="hard = Eiffel-65 stair-step retune (see SCALES)")
    p.add_argument("--autotune-scale", default="b-minor", choices=sorted(SCALES))
    p.add_argument("--autotune-grid-ms", type=float, default=125.0,
                   help="rhythmic quantization cell; 125 = 1/16 at 120 BPM")
    p.add_argument("--autotune-drive", type=float, default=2.6,
                   help="how far the take's own pitch wander is exaggerated "
                        "before it is snapped — more drive, more steps")
    p.add_argument("--autotune-smooth-ms", type=float, default=350.0,
                   help="window of the moving average the take's pitch is "
                        "high-passed against before it is stepped")
    p.add_argument("--autotune-wander", type=float, default=2.5,
                   help="clamp on that wander, in semitones")
    p.add_argument("--autotune-jitter-cents", type=float, default=26.0)
    p.add_argument("--autotune-range", type=float, default=3.0,
                   help="how far, in semitones, a step may land from the "
                        "written note; 0 disables the leash")
    p.add_argument("--autotune-retain", type=float, default=1.0,
                   help="1.0 = full snap; below that it bleeds back toward "
                        "the unquantized contour")
    p.add_argument("--verify", action="store_true")
    args = p.parse_args()

    spec = [s for s in args.notes.split(",") if s.strip()]
    notes, durs = [], []
    for s in spec:
        n, _, d = s.partition(":")
        notes.append(n.strip())
        durs.append(float(d) if d else 0.5)
    durs = np.array(durs, float)

    x, fs = sf.read(args.in_wav, dtype="float64")
    if x.ndim > 1:
        x = x.mean(axis=1)

    # ── analyse ───────────────────────────────────────────────────────
    floor = float(args.f0_floor)
    f0_raw, t = pw.harvest(x, fs, f0_floor=floor, f0_ceil=args.f0_ceil,
                           frame_period=FRAME_MS)
    f0 = pw.stonemask(x, f0_raw, t, fs)
    fft_size = pw.get_cheaptrick_fft_size(fs, f0_floor=floor)
    sp = pw.cheaptrick(x, f0, t, fs, fft_size=fft_size, f0_floor=floor)
    ap = pw.d4c(x, f0, t, fs, fft_size=fft_size)
    n_src = len(t)

    hop = int(round(fs * FRAME_MS / 1000.0))
    # WORLD's frame axis can run one or two frames past the samples; guard
    # the empty slice or the whole warp goes NaN.
    energy = np.zeros(n_src)
    for i in range(n_src):
        seg = x[i * hop:i * hop + hop]
        energy[i] = np.sqrt(np.mean(seg ** 2) + 1e-12) if seg.size else 0.0

    # ── duration control ──────────────────────────────────────────────
    n_out = max(4, int(round(durs.sum() / (FRAME_MS / 1000.0))))
    w = sustain_weights(f0, energy)
    src_pos = build_warp(n_src, w, n_out)
    held = np.interp(src_pos, np.arange(n_src), w)          # how stretched here
    src_pos = shimmer(src_pos, held, args.shimmer_hz, args.shimmer_frames)
    src_pos = np.clip(src_pos, 0, n_src - 1)

    lo = np.floor(src_pos).astype(int)
    hi = np.minimum(lo + 1, n_src - 1)
    fr = (src_pos - lo)[:, None]
    sp_o = np.exp((1 - fr) * np.log(sp[lo] + 1e-16) + fr * np.log(sp[hi] + 1e-16))
    ap_o = (1 - fr) * ap[lo] + fr * ap[hi]
    voiced_o = (f0[np.round(src_pos).astype(int)] > 0)

    sp_o = singers_formant(sp_o, fs, fft_size, args.formant_db)

    # ── f0 substitution ───────────────────────────────────────────────
    # Continuous through unvoiced gaps (V2 §2): WORLD pops on 0→target
    # jumps, so the synth always sees pitch and the v/uv structure gets
    # re-imposed on the output instead.
    if args.autotune == "hard":
        # the take's own f0, interpolated through its unvoiced gaps and then
        # read along the SAME warp as the envelope, so the wander that gets
        # stepped is the one that belongs to this vowel
        vi = np.nonzero(f0 > 0)[0]
        if len(vi) >= 2:
            f0_fill = np.interp(np.arange(n_src), vi, f0[vi])
        else:
            f0_fill = np.full(n_src, midi_to_hz(note_to_midi(notes[0])))
        src_o = np.interp(src_pos, np.arange(n_src), f0_fill)
        f0_o = hard_autotune_contour(notes, durs, n_out,
                                     np.log2(np.maximum(src_o, 1e-6)), args)
    else:
        f0_o = target_contour(notes, durs, n_out, args)

    y = pw.synthesize(np.ascontiguousarray(f0_o),
                      np.ascontiguousarray(sp_o),
                      np.ascontiguousarray(ap_o), fs, frame_period=FRAME_MS)

    # ── re-impose voicing, composite the source over consonants ───────
    mask = np.repeat(voiced_o.astype(float), hop)
    if len(mask) < len(y):
        mask = np.pad(mask, (0, len(y) - len(mask)), mode="edge")
    mask = mask[:len(y)]
    ramp = int(0.005 * fs)                                   # 5 ms crossfades
    if ramp > 1:
        k = np.hanning(2 * ramp + 1)
        k /= k.sum()
        mask = np.convolve(mask, k, mode="same")

    xw = warp_source(x, src_pos, fs)
    n = min(len(y), len(xw), len(mask))
    y = mask[:n] * y[:n] + (1.0 - mask[:n]) * xw[:n]

    # ── polish ────────────────────────────────────────────────────────
    if args.deess > 0:
        y = deess(y, fs, thresh=args.deess)
    y = top_and_tail(y, fs, args.attack_ms, args.release_ms)
    peak = np.abs(y).max()
    if peak > 1e-9:
        y = y * (args.gain / peak)
    sf.write(args.out_wav, y.astype(np.float32), fs)

    msg = (f"  sing · {'/'.join(notes)} · {n_src}→{n_out} frames "
           f"({n_src * FRAME_MS / 1000:.2f}s→{durs.sum():.2f}s, "
           f"{durs.sum() / max(1e-6, n_src * FRAME_MS / 1000):.2f}x) "
           f"· voiced {100 * voiced_o.mean():.0f}%"
           + (f" · AUTOTUNE hard/{args.autotune_scale}/"
              f"{args.autotune_grid_ms:.0f}ms/drive{args.autotune_drive:.1f}"
              if args.autotune == "hard" else ""))

    # ── verification (V2 §1: pyin, not autocorrelation) ───────────────
    if args.verify and HAS_LIBROSA:
        errs = []
        bounds = np.concatenate([[0.0], np.cumsum(durs)])
        f0m, vflag, _ = librosa.pyin(y.astype(np.float32), sr=fs,
                                     fmin=70.0, fmax=args.f0_ceil,
                                     frame_length=2048)
        tm = librosa.times_like(f0m, sr=fs)
        for k, nm in enumerate(notes):
            sel = (tm >= bounds[k] + 0.08) & (tm < bounds[k + 1] - 0.05) & vflag
            if sel.sum() < 3:
                continue
            med = np.nanmedian(f0m[sel])
            errs.append(1200 * np.log2(med / midi_to_hz(note_to_midi(nm))))
        if errs:
            msg += "  · pyin " + " ".join(f"{e:+.0f}¢" for e in errs)
    print(msg)
    return 0


if __name__ == "__main__":
    sys.exit(main())
