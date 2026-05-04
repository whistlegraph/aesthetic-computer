# autotune algorithms — non-neural offline pitch correction

scope: the actual internals of pitch-correction, with code we can lift. neural systems (rvc, diffsinger, etc.) are the other agent's beat. this is dsp, c, and python — the stuff that runs on jeffrey's 8gb macbook without a gpu and finishes before lunch.

the problem with `pop/bin/pitchsnap.mjs` today: it slices a vocal by word and runs `rubberband -p N` per segment. that's a *static* shift — every sample inside the word slides up the same n semitones. the source's natural pitch contour rides along, which is the opposite of autotune. autotune *clamps* — it replaces f0 with a target curve.

## 1. what autotune actually does — five sentences

step 1: estimate f0 (pitch) frame-by-frame on the input, ~5 ms hop. step 2: for each frame, snap the detected midi note to the nearest member of an allowed scale (with hysteresis so a wobble around the boundary doesn't flip notes every frame). step 3: compute a per-frame correction ratio `target_f0 / source_f0`. step 4: shift the audio by that ratio while preserving formants — so the timbre stays human, the pitch becomes mechanical. step 5: stitch frames back together with phase coherence (or, in psola/world world, resynth from decomposed parameters).

that's it. the magic is entirely in steps 1, 4, and 5 being good.

## 2. pitch detection — yin / pyin

naive autocorrelation (what `pitchsnap.mjs` uses today) finds octave errors because the autocorrelation peak at lag `t` is matched at `2t`, `3t`, `4t`, etc. any noise pushes the search to the wrong harmonic. yin (de cheveigné & kawahara, 2002) fixes this with the *cumulative mean normalized difference function* — the curve starts at 1 and only dips below when a true period is found, with the lower-lag (higher pitch) match preferred.

```python
# from patriceguyot/Yin (mit license) — compact form
def differenceFunction(x, w, tau_max):
    # d(tau) = sum_i (x[i] - x[i+tau])^2, computed via fft for speed
    x_cumsum = np.concatenate(([0.], (x*x).cumsum()))
    fc = np.fft.rfft(x, size_pad)
    conv = np.fft.irfft(fc * fc.conjugate())[:tau_max]
    return x_cumsum[w:w-tau_max:-1] + x_cumsum[w] - x_cumsum[:tau_max] - 2*conv

def cmndf(df, N):
    # normalize so curve starts at 1, drops < 1 at true period
    return np.insert(df[1:] * range(1, N) / np.cumsum(df[1:]), 0, 1)

def getPitch(cmdf, tau_min, tau_max, harmo_th=0.1):
    tau = tau_min
    while tau < tau_max:
        if cmdf[tau] < harmo_th:
            while tau+1 < tau_max and cmdf[tau+1] < cmdf[tau]:
                tau += 1   # walk down to local minimum
            return tau
        tau += 1
    return 0  # no pitch found (unvoiced)
```

source: `patriceguyot/Yin/yin.py`, also vendored into `nvidia/mellotron`. pyin (mauch & dixon, 2014) extends yin with a probabilistic hmm over candidate periods — much more robust on noisy/breathy vocals. `librosa.yin` and `librosa.pyin` ship both.

## 3. pitch shifting — psola vs phase vocoder vs world resynth

three families, each with a tradeoff:

**td-psola** (charpentier & stella, 1986). estimate f0, find pitch-period peaks, extract two-period hann-windowed grains, re-space the grains at the *target* period, overlap-add. key property: formants are intrinsic to the grain shape, so they survive untouched — no separate formant flag needed. weak on polyphonic / unvoiced material. clean reference: `sannawag/TD-PSOLA`:

```python
def shift_pitch(signal, fs, f_ratio):
    peaks = find_peaks(signal, fs)        # autocorr-based pitch-mark detection
    return psola(signal, peaks, f_ratio)  # window+respace+overlap-add
```

**phase vocoder** (flanagan & golden, 1966; dolson 1986). stft → for each bin, estimate true frequency from the phase derivative across hops → resynthesize at scaled frame rate → resample to original duration. this is what librosa's `effects.pitch_shift` does (time-stretch via phase vocoder, then resample). also the engine inside rubber band's `R2` mode. weakness: "phasiness" — bins drift apart, transients smear. fixes (laroche & dolson 1999, "phase locking") are what rubber band's `R3 (--finer)` mode adds.

**world resynth** (morise et al., 2016). decompose speech into three independent streams: f0, smoothed spectral envelope (cheaptrick), aperiodicity (d4c). modify any of them. resynth. this is the cleanest "replace the f0 curve" pipeline that exists outside the neural world — see section 4.

| method | formants | best on | failure mode |
|---|---|---|---|
| td-psola | preserved (intrinsic) | clean monophonic vocals | breathy / unvoiced |
| phase vocoder | requires extra filter | broadband / polyphonic | phasiness, transient smear |
| world resynth | preserved (envelope split out) | speech / vocals | not free of artifacts on heavy distortion |

## 4. world / pyworld — the f0-replace pipeline

this is the one. `pyworld` is the python wrapper around morise's c library. install: `pip install pyworld`. has wheels for arm64 macos. the canonical autotune flow is six lines:

```python
import pyworld as pw

# 1. decompose
_f0, t  = pw.dio(x, fs)             # raw f0 candidates (or pw.harvest for accuracy)
f0      = pw.stonemask(x, _f0, t, fs)   # refine f0 to sub-frame precision
sp      = pw.cheaptrick(x, f0, t, fs)   # smoothed spectral envelope (formants)
ap      = pw.d4c(x, f0, t, fs)          # aperiodicity (breath, fricatives)

# 2. modify f0 — this is where autotune happens
f0_corrected = quantize_to_scale(f0, scale_midi_notes)  # see section 5

# 3. resynthesize
y = pw.synthesize(f0_corrected, sp, ap, fs)
```

note what's not here: there is no pitch-shift step. you literally write the new f0 curve into the synthesizer. `sp` (formants) and `ap` (breath texture) are unchanged, so the speaker's identity survives perfectly. this is the api surface we want.

`harvest` is slower but recovers from the octave errors that bite `dio` on jeffrey's takes. for vocals always prefer `harvest` + `stonemask`.

## 5. scale quantization with hysteresis

given an instantaneous f0 in hz and a target scale (set of midi numbers), the snap is straightforward — hz → midi via `69 + 12*log2(f/440)`, then nearest-member lookup. the trick is hysteresis: if you're sitting between two scale degrees and the f0 wobbles ±10 cents, naive nearest-snap will *flip notes every frame*, producing a digital trill. fix:

```python
def quantize_to_scale(f0, scale_midi, prev_target=None,
                      hysteresis_cents=30, retain=1.0):
    out = np.zeros_like(f0)
    cur = prev_target
    for i, f in enumerate(f0):
        if f <= 0:                      # unvoiced — keep silent
            out[i] = 0; continue
        midi = 69 + 12*np.log2(f/440)
        # candidates sorted by distance to current frame
        cands = sorted(scale_midi, key=lambda n: abs(n - midi))
        nearest = cands[0]
        if cur is not None and nearest != cur:
            # only switch if we've moved > hysteresis cents past the boundary
            if abs(midi - nearest)*100 + hysteresis_cents > abs(midi - cur)*100:
                nearest = cur
        cur = nearest
        # `retain` interpolates between source pitch (0) and full snap (1)
        corrected_midi = midi + retain * (nearest - midi)
        out[i] = 440 * 2**((corrected_midi - 69)/12)
    return out
```

`retain=1.0` is full hard auto-tune (the t-pain sound). `retain=0.6` is melodyne-style — keeps human bend. `hysteresis_cents=30` to `50` kills the trill without slowing legit pitch transitions.

## 6. shortlist — what we can plug in tonight

| lib | language | offline? | install | what we get |
|---|---|---|---|---|
| **pyworld** | python | yes | `pip install pyworld` | f0 extract + replace + resynth, formants intrinsic |
| psola | python | yes | `pip install psola` | td-psola via parselmouth/praat — needs target-pitch numpy array |
| librosa | python | yes | `pip install librosa` | `yin`, `pyin`, `effects.pitch_shift` (phase vocoder) |
| crepe | python | gpu-light | `pip install crepe` | neural pitch detector — slower but cleanest f0 |
| autotalent | c (ladspa) | yes | source build | reference c implementation, gpl2 |
| rubberband | c++ | yes | `brew install rubberband` (already installed) | phase vocoder pitch shift, no f0-replace |
| sannawag/TD-PSOLA | python | yes | `git clone` | minimal readable td-psola reference |

all of the python options install on apple silicon python 3.14 with native wheels in 2026. pyworld's only build dep is numpy + cython.

## 7. integration sketch — pitchsnap.mjs → world

the right move is to delegate the per-word pitch step to a small python helper that does world-style f0 replacement. `pitchsnap.mjs` keeps its responsibilities (whisper alignment, score parsing, grid snap, slice extraction) and calls the helper for the actual correction:

```python
# pop/bin/pitchsnap_world.py — called per-word from pitchsnap.mjs
import sys, numpy as np, pyworld as pw, soundfile as sf

in_wav, out_wav, target_midi, retain = sys.argv[1:5]
target_midi = float(target_midi); retain = float(retain)

x, fs = sf.read(in_wav, dtype="float64")
if x.ndim > 1: x = x.mean(axis=1)

f0_raw, t = pw.harvest(x, fs, f0_floor=80, f0_ceil=600)
f0  = pw.stonemask(x, f0_raw, t, fs)
sp  = pw.cheaptrick(x, f0, t, fs)
ap  = pw.d4c(x, f0, t, fs)

# replace f0 with target — hold the bend with `retain`
target_hz = 440 * 2**((target_midi - 69)/12)
voiced = f0 > 0
f0_new = np.where(voiced, np.exp((1-retain)*np.log(np.maximum(f0,1e-6))
                                  + retain*np.log(target_hz)), 0)

y = pw.synthesize(f0_new, sp, ap, fs)
sf.write(out_wav, y.astype(np.float32), fs)
```

then in `pitchsnap.mjs`, replace the rubberband call inside the per-word loop with:

```javascript
spawnSync("python3", [
  resolve(import.meta.dirname, "pitchsnap_world.py"),
  sliceWav, shiftedWav, String(targetMidi), String(retain)
], { stdio: "inherit" });
```

next phase: instead of a single `target_midi` per word, write a per-frame target curve that interpolates between syllable notes — that's the syllable-glide we already plan in `pitchsnap.mjs` curve mode, but executed by world instead of cross-fading two rubberband renders.

---

**recommendation: integrate pyworld first.** it's the only option in this list that lets us *replace* the f0 curve instead of shifting it, formants stay intact for free, and the install is one line.

```
pip install pyworld soundfile numpy
```
