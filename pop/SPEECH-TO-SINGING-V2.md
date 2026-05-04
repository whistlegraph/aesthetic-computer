# speech-to-singing — v2 follow-up

v1 picked the saitou recipe and we built it. world (`pyworld`) lands the median pitch on target to ~2¢. but the holes are now audible — word-start stutters, held-note ringing, harsh sibilants, and outlier octave errors from naive autocorrelation in `pitchcheck.mjs`. v2 goes shopping for fixes to *those specific symptoms*, not for another full pipeline.

three lanes:

— **stay inside world, fix it.** parameter passes, second-pass correction, voicing-edge ramps.
— **swap parts of world out.** psola via parselmouth, sinusoidal modeling, neural pitch detectors.
— **bypass world.** rvc / so-vits-svc / nsf as a "real singer" lane. bigger payoff, bigger install.

## 1. better pitch detection — the quick outlier kill

our `pitchcheck.mjs` autocorrelation is the *measurement* tool, not the renderer — but its octave errors inflate the mean drift report and make us chase ghosts. world internally uses `harvest` already, which is good. the question is: when *we* measure the post-render f0 to verify, what do we use.

| tool | install | gpu | what we get |
|---|---|---|---|
| **librosa.pyin** | already installed in the env | no | viterbi-smoothed yin, monophonic-only — perfect for our use. one function call: `librosa.pyin(y, sr, fmin=70, fmax=600)`. drop-in replacement for `detectPitch()`. |
| **torchcrepe** | `pip install torchcrepe` (~80 mb plus torch) | optional | pretrained cnn, sub-cent accuracy on clean voice. has a `viterbi=True` flag for smoothing. cpu-runnable, ~2x real-time on m-series. |
| **rmvpe** | `pip install rmvpe` (~120 mb model) | optional | best-on-singing benchmark (87.2% rpa on mir-1k vs crepe 85.3%). robust to noise. used internally by rvc/applio. |
| **fcpe / torchfcpe** | `pip install torchfcpe` (~30 mb) | optional | 2026 model. ~5× faster than rmvpe, ~77× faster than crepe. cpu-realtime. accuracy on par with rmvpe on clean monophonic. |
| **penn** | `pip install penn` (~50 mb) | optional | morrison's cross-domain neural pitch + periodicity, 11× rt on cpu, returns confidence — useful for voicing detection. |

**verdict.** `librosa.pyin` is the immediate win — already on disk, two-line swap inside `pitchcheck.mjs`, removes the 170¢ mean-drift outliers in one move. for a future "second-pass autotune" (see §3), `torchfcpe` is the right pick — small install, cpu-friendly, sings well.

integration sketch (pitchcheck.mjs):

```python
# pop/bin/pitchcheck_pyin.py — child process called from pitchcheck.mjs
import sys, librosa, soundfile as sf, numpy as np
y, sr = sf.read(sys.argv[1])
if y.ndim > 1: y = y.mean(axis=1)
f0, voiced, conf = librosa.pyin(y.astype(np.float32), sr=sr,
                                 fmin=70, fmax=600, frame_length=2048)
# emit timestamp, hz, confidence csv
```

## 2. fix the world stutter — voicing-transition pops

the audible "skip" at word starts is the world synth flipping unvoiced→voiced abruptly. our current script removed the `voicing-ramp` because multiplying f0 by 0..1 forced the first 40ms to be unvoiced. correct fix is the opposite — *interpolate f0 across unvoiced regions before synthesis*, then synthesise, then mute the unvoiced portions in the time domain after.

this is the standard f0-contour smoother trick (zeehio / edinburgh speech tools, harvest paper §4):

```python
# fill unvoiced gaps in f0 with linear interpolation
voiced_idx = np.where(f0 > 0)[0]
if len(voiced_idx) >= 2:
    f0_filled = np.interp(np.arange(len(f0)), voiced_idx, f0[voiced_idx])
else:
    f0_filled = f0
# build unvoiced mask in time-domain *after* synth
unvoiced_mask_audio = np.repeat(f0 == 0, samples_per_frame)
y = pw.synthesize(f0_filled, sp, ap, fs)   # smooth pitch through gaps
# crossfade-mute the unvoiced regions (5 ms ramps at boundaries)
y *= smooth_mute(unvoiced_mask_audio, ramp_ms=5)
```

this gives world a continuous f0 curve to work against (no abrupt 0→target jumps), then re-imposes the voiced/unvoiced structure as a smoothed amplitude mask. costs ~10 lines of python; addresses the *single largest* perceptual artifact we currently hit.

related fix: **bump the cheaptrick fft size**. default is auto-computed from `f0_floor=70`. raising `f0_floor` to 90 hz (jeffrey's voice never goes below 90) shrinks the analysis window, which reduces the "echo / ringing" on held vowels — that ring is cheaptrick smearing low-frequency formant estimates over too long a window.

```python
fft_size = pw.get_cheaptrick_fft_size(fs, f0_floor=90.0)
sp = pw.cheaptrick(x, f0, t, fs, fft_size=fft_size, f0_floor=90.0)
```

## 3. second-pass correction — melodyne-lite

even with world's f0-replace, we still see drift on outliers. add a dirt-cheap *second pass*:

1. measure actual f0 of world output (with `torchfcpe` or `librosa.pyin`).
2. compute residual cents-error per frame: `err = 1200 * log2(measured / target)`.
3. if median |err| over a sustained note > 15 cents, build a per-frame correction ratio and re-render with world (only that note). otherwise pass through.

this is what auto-tune internally does after its first snap pass. one extra world round-trip per problem note, ~50 lines, no new deps. works because the *first* pass got us to within ~2¢ median; the second pass surgically fixes the tail.

## 4. psola alternative — when world rings, try grains

td-psola (`psola` on pypi, wraps parselmouth → praat) does pitch-shift in the time domain by repositioning glottal-pulse-aligned grains. **no spectral-envelope estimation step**, so no cheaptrick ringing. the cost: psola is monophonic-only and breathy/unvoiced material can break it.

```python
import psola
# target_pitch: numpy array same length as audio, hz per sample
y = psola.vocode(audio, sample_rate, target_pitch=target_pitch_curve,
                 fmin=70, fmax=600)
```

| | world | td-psola |
|---|---|---|
| held-note ringing | yes (cheaptrick smearing) | no (no spectral step) |
| sibilant clipping | yes (envelope smoothes 's') | yes (psola breaks on unvoiced) |
| word-start stutter | yes (v/uv transition) | less (grains overlap-add naturally) |
| install | `pip install pyworld` | `pip install psola praat-parselmouth` |
| cpu speed | fast | slow (~3× slower) |

**recommended use:** dual-render — psola on sustained vowels, world on consonants — composited per-phoneme. that's a half-day build but addresses two of our four symptoms.

## 5. sinusoidal modeling — the slow lane

`sms-tools` (mtg/upf, xavier serra), `loris` / `loristrck`, and `simpl` decompose audio into time-varying sinusoidal partials + a noise residual. you can shift each partial independently, which gives surgical control over harmonics vs sibilance vs breath.

— `pip install sms-tools` (apple silicon wheels exist as of 2026).
— per-partial pitch shifting: pure tones move, residual noise stays.
— **fixes sibilants directly**: classify partials by frequency band, leave 5–10 khz partials unshifted, shift only the harmonic stack below 4 khz.

cost: orders of magnitude slower than world, more knobs, more code (~300 lines). flag this as a research-track experiment, not a production swap.

## 6. consonant-region artifact suppression — the cleanup pass

even with everything above, world will still occasionally muck up "s", "k", "t" because it tries to apply f0 to noisy regions. the cheap fix is a *post-vocoder* spectral repair pass:

— **de-esser**: dynamic compressor on the 5–8 khz band, sidechain-keyed by an envelope of that same band. classic broadcast trick. `scipy.signal.iirfilter` + a one-pole detector + per-sample gain in ~30 lines. or use the `pedalboard` library (spotify, `pip install pedalboard`) which has `Compressor` + `HighpassFilter` building blocks ready.
— **transient gate**: at frame boundaries that fall inside unvoiced regions, replace world's output with the *original* recording's audio in that region, crossfaded ±5 ms. world handles vowels; original audio handles fricatives and stops. this is the "world for pitch, source for noise" composite.

```python
# composite: keep world output on voiced frames, original audio on unvoiced
voiced_audio = upsample(voiced_mask, fs)  # 0..1 per sample, 5 ms ramps
y_final = voiced_audio * y_world + (1 - voiced_audio) * x_original
```

this is the single biggest "sounds clean" win for the same effort budget as §2. probably do both.

## 7. neural lane — rvc / so-vits-svc / nsf

if §1–§6 still fall short, the field's actual answer in 2026 is voice conversion: train a model on jeffrey's voice, feed it a sung melody (synthesised or hummed), get sung jeffrey out.

| project | status | install | model size | cpu? | what it gives us |
|---|---|---|---|---|---|
| **rvc / applio** | active, large community | `git clone IAHispano/Applio` + cli | ~500 mb base + ~50 mb per voice | yes (~1× rt on m-series for non-real-time) | trained voice clone, melody-driven. uses rmvpe internally. |
| **so-vits-svc-fork** (voicepaw) | actively maintained | `pip install so-vits-svc-fork` | ~200 mb base + ~150 mb per voice | yes (~3× rt cpu) | similar to rvc, slightly older arch. |
| **diff-svc / diffsinger** | research-grade | `git clone prophesier/diff-svc` | ~800 mb | gpu strongly preferred | diffusion, highest quality, slow. |
| **nnsvs + pc-nsf-hifigan** | active, score-driven | `pip install nnsvs` | ~400 mb | yes — nsf models specifically beat hifi-gan on cpu | feeds *score + lyrics*, not a guide vocal. complementary lane. |

**critical caveat for our pipeline.** rvc and so-vits-svc need a *sung* input. they convert sung-anyone → sung-jeffrey. they don't solve speech→sung. so the pipeline becomes:

```
speech → world (saitou f0-replace) → rough sung → rvc (jeffrey clone) → polished sung
```

rvc as a *cleanup pass* downstream of our world output is the highest-ceiling option in this doc. it would mask all of §2/§4/§6 — rvc's neural decoder doesn't care if our world stage stutters, it resamples everything through the trained voice. estimated install + train: 1 day for setup, 4–8 hours of jeffrey audio collection, ~2 hours of training on cpu (overnight) or 30 min on a rented gpu.

— **diff-pitcher** (jhu-lcap, waspaa 2023, `haidog-yaqub/DiffPitcher` on github) is the closest thing to "pitch-correct neurally without a trained singer". diffusion-based correction that takes out-of-tune audio + target midi, returns in-tune audio with timbre preserved. no voice training step. ~600 mb model, cpu-runnable but slow (~5× rt). worth a one-day evaluation if §1–§6 leaves residual artifacts on jeffrey's takes specifically.

## 8. nsf vocoders — better than world without leaving the dsp lane

if we want to *replace* world entirely with something newer but stay non-neural-conversion, **pc-nsf-hifigan** (pitch-controllable neural source-filter, used by nnsvs as of 2025) takes `(f0, mel)` and produces a waveform. quality on singing beats world by a wide margin in the published evals (yamagishi lab samples, source-filter hifi-gan paper 2022). cpu-runnable, ~10× faster than world per second of audio.

integration cost is the install (`pip install nnsvs` brings ~400 mb of torch + checkpoints) and re-fitting our pipeline to emit mel-spectrograms instead of cheaptrick envelopes. about a 2-day rewrite. defer until §1–§6 are exhausted.

## the new symptom-to-fix table

| symptom | root cause | new fix proposed | section |
|---|---|---|---|
| word-start stutter | abrupt 0→target f0 at voicing transitions | interpolate f0 across unvoiced gaps + post-synth amplitude mask | §2 |
| held-note ringing | cheaptrick analysis window too long | raise `f0_floor` to 90 hz, recompute fft_size | §2 |
| harsh sibilants | world tries to apply f0 to fricatives | composite — world on voiced, source audio on unvoiced | §6 |
| residual drift on outliers | one-pass correction insufficient | melodyne-lite second-pass over world output, measured by torchfcpe | §3 |
| octave errors in `pitchcheck.mjs` report | naive autocorrelation | swap to `librosa.pyin` (already installed) | §1 |
| occasional vowel mush | cheaptrick envelope over-smooths | dual-render with td-psola on sustained vowels | §4 |

## shortlist — three things to try next, ranked

**1. f0-gap interpolation + voiced/unvoiced source compositing.** *highest payoff, ~3 hour build, no new deps.* sections 2 and 6 combined. this kills the two worst symptoms (stutter at word starts, harsh sibilants) inside `pitchsnap_world.py` with ~30 lines of numpy. `librosa.pyin` swap in `pitchcheck.mjs` rolls in for free. zero new install.

**2. cheaptrick fft_size tuning + melodyne-lite second pass.** *medium payoff, ~half-day build, adds torchfcpe.* sections 2 (the `f0_floor=90` line) and 3. addresses held-note ringing and tail outliers. `pip install torchfcpe` is the only new dep — small, cpu-friendly, no gpu.

**3. rvc/applio as a downstream cleanup pass.** *highest ceiling, ~2 day build, large install.* the "real" answer if signal-processing exhausts itself. world output → applio cli → polished sung jeffrey. requires collecting clean jeffrey audio (probably already in the recap corpus) and one overnight training run. defer until (1) and (2) ship and we have a tagged "world-only" baseline to a/b against.

## strongest single new finding

**the word-start stutter is fixable inside world without changing the vocoder.** v1 assumed any pop at voicing transitions was a fundamental world limitation; the literature is clear it's a configuration problem. the fix is the standard "interpolate f0 through unvoiced gaps, synthesise on the smoothed curve, then re-impose the voiced mask in the time domain with crossfaded ramps" — used by every production pitch-correction pipeline, missing from our current script because we tried (and removed) the wrong version of it (multiplying f0 by 0→1 envelope, which forced unvoiced and made the problem worse). doing it the documented way (interpolate the contour, mute the *output* not the f0) is ~30 lines and should clean up the stutter completely.

**recommended immediate action:** patch `pitchsnap_world.py` to interpolate f0 across unvoiced frames before synthesis, mute the unvoiced output via a 5 ms-ramped amplitude mask, and bump `f0_floor` to 90 hz in cheaptrick. that single commit should hit two of the four current symptoms. then `librosa.pyin` swap in `pitchcheck.mjs` to retire the autocorrelation outliers from the drift report. day one.
