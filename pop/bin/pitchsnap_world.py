#!/usr/bin/env python3
"""
pitchsnap_world.py — WORLD-vocoder-based pitch correction helper for
pitchsnap.mjs. Replaces the f0 curve of a vocal slice with a target
melody (one or more notes laid across the duration) while preserving
the spectral envelope and aperiodicity — i.e. jeffrey's voice
character is unchanged but the pitch lands on the score.

Pipeline (Saitou 2007 / Morise et al. 2016):
  audio
    → harvest (f0 candidates)
    → stonemask (f0 refinement)
    → cheaptrick (spectral envelope = formants = voice identity)
    → d4c (aperiodicity = breath / consonants)
    → REPLACE f0 with target curve
    → synthesize

Multi-syllable: --notes "C3,D3,Eb3,F3,G3" lays N evenly-spaced
target pitches across the audio. Each segment holds steady; transitions
crossfade in log-pitch space (one frame).

Usage:
  pitchsnap_world.py <in.wav> <out.wav> \
    --notes "C3,Eb3,G3" [--retain 1.0] [--vibrato-hz 5.5] \
    [--vibrato-cents 0] [--xfade-ms 30]
"""
import argparse
import sys
import numpy as np
import soundfile as sf
import pyworld as pw

try:
    import librosa
    HAS_LIBROSA = True
except ImportError:
    HAS_LIBROSA = False

NOTE_SEMI = {"c": 0, "d": 2, "e": 4, "f": 5, "g": 7, "a": 9, "b": 11}

def note_to_midi(s: str) -> int:
    s = s.strip().lower()
    semi = NOTE_SEMI[s[0]]
    i = 1
    if i < len(s) and s[i] in "#b":
        semi += 1 if s[i] == "#" else -1
        i += 1
    octave = int(s[i:])
    return 12 * (octave + 1) + semi

def midi_to_hz(m: float) -> float:
    return 440.0 * (2.0 ** ((m - 69.0) / 12.0))

def main() -> int:
    p = argparse.ArgumentParser()
    p.add_argument("in_wav")
    p.add_argument("out_wav")
    p.add_argument("--notes", required=True,
                   help='Comma-separated notes laid evenly across the audio, e.g. "C3,Eb3,G3"')
    p.add_argument("--weights", default=None,
                   help='Comma-separated relative duration weights per note (default 1 each)')
    p.add_argument("--note-starts", default=None,
                   help='Comma-separated absolute start times (seconds) per note. '
                        'When provided, target curve is anchored at these times '
                        '(useful for whole-audio continuous resynth).')
    p.add_argument("--retain", type=float, default=1.0,
                   help="0 = source f0 unchanged, 1 = full clamp to target (default)")
    p.add_argument("--f0-floor", type=float, default=90.0,
                   help="Lowest f0 WORLD will call voiced, in Hz (default 90, "
                        "tuned for jeffrey-pvc). Frames below the floor read as "
                        "unvoiced and never sing — lower it for a deeper source. "
                        "Costs formant sharpness: a lower floor widens cheaptrick's "
                        "window and can ring on held notes.")
    p.add_argument("--vibrato-hz", type=float, default=0.0,
                   help="Vibrato frequency in Hz (0 = off)")
    p.add_argument("--vibrato-cents", type=float, default=0.0,
                   help="Vibrato depth in cents (peak-to-peak / 2)")
    p.add_argument("--vibrato-onset-ms", type=float, default=200.0,
                   help="Delay before vibrato fades in (ms)")
    p.add_argument("--xfade-ms", type=float, default=80.0,
                   help="Crossfade between adjacent target notes (ms). Larger "
                        "values smooth pitch transitions but blur the attack.")
    p.add_argument("--voicing-ramp-ms", type=float, default=40.0,
                   help="Ramp f0 in/out over this window at voiced/unvoiced "
                        "boundaries (word starts/ends). Smooths the WORLD "
                        "synth pop where pitch goes 0→target abruptly.")
    p.add_argument("--detect-boundaries", action="store_true",
                   help="Use librosa onset detection to find natural syllable "
                        "boundaries in the audio, rather than weight-proportional "
                        "splits. Only applies when there are >= 2 notes.")
    args = p.parse_args()

    notes = [n.strip() for n in args.notes.split(",") if n.strip()]
    if not notes:
        print("✗ --notes required", file=sys.stderr)
        return 1
    target_midis = np.array([note_to_midi(n) for n in notes], dtype=np.float64)
    target_hzs = np.array([midi_to_hz(m) for m in target_midis])

    if args.weights:
        weights = np.array([float(w) for w in args.weights.split(",")], dtype=np.float64)
        if len(weights) != len(notes):
            print(f"✗ --weights length {len(weights)} != notes length {len(notes)}", file=sys.stderr)
            return 1
    else:
        weights = np.ones(len(notes), dtype=np.float64)
    weights = weights / weights.sum()
    cum_w = np.concatenate([[0.0], np.cumsum(weights)])

    x, fs = sf.read(args.in_wav, dtype="float64")
    if x.ndim > 1:
        x = x.mean(axis=1)

    # WORLD decompose. f0_floor=90 (jeffrey-pvc never goes below 90 Hz)
    # tightens cheaptrick's analysis window, which kills the held-note
    # "ring/echo" artifact (low-floor → wide window → formant smearing).
    # Lower it only for a deeper source: frames under the floor read as
    # UNVOICED and fall through to unmodified speech, so a too-high floor
    # silently costs you singing. Trade ring against voicing per speaker.
    f0_floor = float(args.f0_floor)
    f0_raw, t = pw.harvest(x, fs, f0_floor=f0_floor, f0_ceil=600.0, frame_period=5.0)
    f0 = pw.stonemask(x, f0_raw, t, fs)
    fft_size = pw.get_cheaptrick_fft_size(fs, f0_floor=f0_floor)
    sp = pw.cheaptrick(x, f0, t, fs, fft_size=fft_size, f0_floor=f0_floor)
    ap = pw.d4c(x, f0, t, fs, fft_size=fft_size)

    # Build per-frame target f0 curve. N notes laid across the time axis;
    # each note holds its pitch; transitions crossfade in log space over
    # `--xfade-ms` ms.
    n_frames = len(t)
    frame_period_ms = (t[1] - t[0]) * 1000.0 if n_frames > 1 else 5.0
    xfade_frames = max(1, int(args.xfade_ms / frame_period_ms))

    # Weighted segment boundaries in frame indices.
    seg_starts = (cum_w * n_frames).astype(np.int64)

    # Absolute start-time override — pin each note to a specific time
    # in the audio (seconds). Useful for whole-audio resynth where note
    # boundaries should align to whisper word timestamps, not be
    # distributed evenly across the file.
    if args.note_starts:
        starts_sec = [float(s) for s in args.note_starts.split(",") if s.strip()]
        if len(starts_sec) != len(notes):
            print(f"✗ --note-starts length {len(starts_sec)} != notes length {len(notes)}", file=sys.stderr)
            return 1
        seg_starts = np.array(
            [int(round(s / (frame_period_ms / 1000.0))) for s in starts_sec] + [n_frames],
            dtype=np.int64,
        )
        seg_starts = np.clip(seg_starts, 0, n_frames)

    # Optionally replace with librosa-detected syllable boundaries —
    # finds onset frames in the audio and snaps the segment starts to
    # the nearest detected onset. Falls back to weighted splits if
    # detection finds the wrong number of onsets.
    if args.detect_boundaries and HAS_LIBROSA and len(notes) >= 2:
        # librosa needs float32 mono; use the WORLD-analyzed signal.
        try:
            onset_env = librosa.onset.onset_strength(y=x.astype(np.float32), sr=fs, hop_length=256)
            onset_frames = librosa.onset.onset_detect(
                onset_envelope=onset_env, sr=fs, hop_length=256,
                backtrack=True,
            )
            # Convert from librosa hop frames to WORLD frames
            librosa_to_world = (256 / fs) / (frame_period_ms / 1000.0)
            world_onset_frames = (onset_frames * librosa_to_world).astype(np.int64)
            # Always anchor first segment at 0; need (len(notes)-1) onset hits
            need = len(notes) - 1
            if len(world_onset_frames) >= need:
                # Pick the `need` largest-strength onsets
                strengths = onset_env[onset_frames]
                top_idx = np.argsort(strengths)[-need:]
                top_onsets = np.sort(world_onset_frames[top_idx])
                detected = np.concatenate([[0], top_onsets, [n_frames]])
                seg_starts = detected.astype(np.int64)
                print(f"  librosa onsets: {len(onset_frames)} found, used {need} → {top_onsets.tolist()}")
            else:
                print(f"  librosa onsets: only {len(onset_frames)} found, need {need} — using weighted splits")
        except Exception as e:
            print(f"  librosa onset detection failed: {e} — using weighted splits")

    target_log = np.zeros(n_frames)
    for i in range(n_frames):
        # Which segment is this frame in?
        seg = int(np.searchsorted(seg_starts[1:], i, side="right"))
        seg = min(seg, len(notes) - 1)
        center_log = np.log(target_hzs[seg])
        # Crossfade with the next segment if we're near its boundary.
        if seg + 1 < len(notes):
            next_start = seg_starts[seg + 1]
            dist_to_next = next_start - i
            if dist_to_next < xfade_frames:
                tval = 1.0 - (dist_to_next / xfade_frames)
                center_log = (1 - tval) * center_log + tval * np.log(target_hzs[seg + 1])
        target_log[i] = center_log

    target_curve = np.exp(target_log)

    # Vibrato — sine LFO on top of target, fading in after onset.
    if args.vibrato_hz > 0 and args.vibrato_cents > 0:
        time_sec = np.arange(n_frames) * (frame_period_ms / 1000.0)
        onset_sec = args.vibrato_onset_ms / 1000.0
        fade = np.clip((time_sec - onset_sec) / max(0.05, onset_sec), 0.0, 1.0)
        depth_ratio = (args.vibrato_cents / 100.0) / 12.0  # cents → semitones → log2
        lfo = np.sin(2 * np.pi * args.vibrato_hz * time_sec) * depth_ratio * fade
        target_curve = target_curve * (2.0 ** lfo)

    # Build the f0 curve passed to WORLD synth. Two key moves vs naive:
    #
    # 1. INTERPOLATE through unvoiced gaps before synthesis. WORLD pops
    #    on f0=0→target jumps; feeding it a continuous curve eliminates
    #    those phase-resets. Voiced/unvoiced structure gets re-imposed
    #    in the time domain after synthesis (step 2).
    # 2. MUTE unvoiced regions post-synth with 5ms crossfade ramps —
    #    keeps consonants/sibilants from being colored by tone.
    voiced = f0 > 0

    # Build the per-frame target the synth will see (interpolated curve).
    if args.retain >= 0.999:
        # Hard clamp to target on voiced frames; interpolate target
        # through unvoiced gaps so the synth has continuous pitch.
        f0_synth = target_curve.copy()
    else:
        log_src = np.log(np.maximum(f0, 1e-6))
        log_tgt = np.log(target_curve)
        log_blend = (1.0 - args.retain) * log_src + args.retain * log_tgt
        # Interpolate the source side across unvoiced frames so the blend
        # has continuous data; otherwise unvoiced frames blend toward 0.
        if voiced.sum() >= 2:
            voiced_idx = np.where(voiced)[0]
            log_src_interp = np.interp(np.arange(len(f0)), voiced_idx, log_src[voiced_idx])
            log_blend_smooth = (1.0 - args.retain) * log_src_interp + args.retain * log_tgt
            f0_synth = np.exp(log_blend_smooth)
        else:
            f0_synth = np.exp(log_blend)

    f0_new = f0_synth  # name kept for downstream code consistency

    y = pw.synthesize(f0_new, sp, ap, fs, frame_period=5.0)

    # Re-impose voiced/unvoiced structure as a time-domain amplitude
    # mask. Unvoiced regions get muted with 5ms crossfades at every
    # transition so consonants ("s", "k", "th") aren't colored by
    # whatever tone WORLD synthesized through the gap. This is the
    # "WORLD for pitch, original passes through for noise" trick.
    samples_per_frame = int(round(fs * 5.0 / 1000.0))
    voiced_audio_mask = np.repeat(voiced.astype(np.float64), samples_per_frame)
    if len(voiced_audio_mask) < len(y):
        voiced_audio_mask = np.pad(voiced_audio_mask, (0, len(y) - len(voiced_audio_mask)),
                                   mode="edge")
    voiced_audio_mask = voiced_audio_mask[:len(y)]

    # Smooth the 0/1 mask with 5ms cosine crossfades at edges.
    ramp = int(0.005 * fs)
    if ramp > 1:
        edges = np.diff(voiced_audio_mask.astype(np.int8))
        for idx in np.where(edges == 1)[0]:
            for k in range(ramp):
                pos = idx + 1 + k
                if pos < len(voiced_audio_mask):
                    voiced_audio_mask[pos] *= 0.5 - 0.5 * np.cos(np.pi * (k + 1) / ramp)
        for idx in np.where(edges == -1)[0]:
            for k in range(ramp):
                pos = idx - k
                if pos >= 0:
                    voiced_audio_mask[pos] *= 0.5 - 0.5 * np.cos(np.pi * (k + 1) / ramp)

    # Composite: WORLD audio in voiced regions, original audio in
    # unvoiced regions. Preserves natural sibilants and stops while
    # keeping the pitch-corrected vowels. mask is 1 in voiced, 0 in
    # unvoiced (with 5ms ramps at boundaries).
    n = min(len(y), len(x), len(voiced_audio_mask))
    y_composite = np.zeros(n)
    y_composite = voiced_audio_mask[:n] * y[:n] + (1.0 - voiced_audio_mask[:n]) * x[:n]

    sf.write(args.out_wav, y_composite.astype(np.float32), fs)

    # Print a one-line summary so pitchsnap.mjs can show it.
    voiced_pct = 100.0 * np.mean(voiced)
    print(f"  world · {len(notes)} notes · {n_frames} frames · {voiced_pct:.0f}% voiced "
          f"· retain={args.retain} · vibrato={args.vibrato_hz}Hz/{args.vibrato_cents}¢")
    return 0

if __name__ == "__main__":
    sys.exit(main())
