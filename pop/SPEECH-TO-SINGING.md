# speech-to-singing — survey

a map of how the field gets sung output from spoken input, and which pieces drop into our existing `pitchsnap.mjs` lane.

scope: jeffrey-pvc tts → sung melody. neural svc / svs lanes are noted but flagged as a separate research track (gpu, trained models, weeks of setup).

## the field

three eras, all still in active use:

**signal-processing era (2007–2014).** saitou et al.'s "speech-to-singing synthesis system" (waspaa 2007 / 2009) is the canonical paper. you read the lyrics out loud, hand it a score, and the system replaces the f0 contour with the score's contour, lengthens phonemes to fit notes, and reshapes the spectrum to add the singer's formant. it runs on the **straight** vocoder (kawahara) — analyse → modify (f0, duration, spectrum) → resynthesise. saitou explicitly lists the three things missing from speech: pitched melody, sustained vowels, and a singer's-formant ring. that decomposition is still the right mental model.

**statistical-parametric era (2010–2018).** **sinsy** (nagoya institute of technology, hmm-based, bsd-licensed, on sourceforge / github) takes musicxml in and produces sung audio. it learns from a corpus what real singers do with a score. spiritually similar but you don't get to inject your own speech timbre — it's full synthesis from a model, not transformation of a recording.

**neural era (2019–now).** parekh et al. (icassp 2020, "speech-to-singing conversion in an encoder-decoder framework") was the first end-to-end learned s2s — input is speech spectrogram + target melody, output is sung spectrogram. on the synthesis side, **diffsinger** (aaai 2022, diffusion + hifigan) and **nnsvs** (r9y9, 2022, the open-source successor to neutrino) take a score and a trained voice and produce a sung waveform. on the conversion side, **so-vits-svc** and **rvc** turn a guide vocal (sung melody) into the target singer's voice — they need a sung melody as input, so they don't solve our problem on their own; they'd sit *downstream* of a working s2s pipeline.

## the techniques

each technique listed with what it actually does to the signal. the problem isn't that we don't have enough techniques — it's that pitchsnap currently uses one (rubberband segment-shifting) and skips the rest.

— **f0 substitution.** decompose audio with a vocoder that gives you `(f0, spectral_envelope, aperiodicity)` as separate streams. throw the source f0 away, write a new f0 contour from the score, resynthesise. spectral envelope unchanged → vowel identity preserved → it still sounds like jeffrey, just with sung pitch. this is the saitou recipe and also what world / pyworld / praat do natively. our current rubberband path *shifts* the existing f0 by an interval, which is why "spoken prosody fights melody" — the prosody is *in* the f0 we're shifting.

— **phoneme-aware time-stretch (vowel sustain).** detect vowel regions, stretch them; leave consonants alone. straight + saitou's "duration control model" does this. world's `synthesizeRequiem` lets you pass a per-frame time-warp. praat's manipulation object exposes a `durationtier` you can set per phoneme. without this step, "real" stretched 4× sounds like "rrrrreal" — every phoneme drags. with it, only the `ee` drags and you get "reeeeeal".

— **phoneme alignment.** **montreal forced aligner** (mfa, kaldi-based, free, has english pretrained models) gives you per-phoneme timestamps from audio + transcript. **charsiu** (lingjzhu, wav2vec2-based, lighter than mfa) does the same with a smaller install and decent quality. either gives consonant/vowel boundaries — required for vowel-sustain to know which spans to stretch.

— **f0 detection.** our current `pitchsnap.mjs` uses naive autocorrelation in `detectPitch()` (lines 150–182), which the comments admit picks 2× / ½× on individual words. **pyin** (mauch & dixon 2014, in librosa as `librosa.pyin`) adds viterbi smoothing and is the standard for monophonic speech/singing. **crepe** (kim et al. 2018) is a cnn, more accurate, needs tensorflow. **dio + stonemask** in world is fast and clean for voice. swapping autocorrelation → pyin or world's dio is a one-day fix that removes octave errors immediately.

— **f0 contour shaping.** real sung notes don't step. saitou models four sub-effects:
  - *overshoot* (~50 cents past the target on attack, decays to centre over ~200 ms)
  - *vibrato* (4.5–6.5 hz, 50–120 cents peak-to-peak, fades in after sustain begins)
  - *preparation* (slight dip below the target before a rising interval)
  - *fine fluctuation* (sub-cent jitter for naturalness)
  add these on top of a stepped target curve and a flat synthetic line starts to read as performed.

— **singer's formant.** sundberg 1974 — trained singers cluster f3/f4/f5 around 2.5–3 khz for "ring" that cuts through an orchestra. in spectral-envelope terms: boost a ~500 hz-wide band centred near 2.8 khz by ~6–10 db. on a vocoder that exposes the spectral envelope (world, straight, praat) this is one filter operation per frame. we can fake it on raw audio with a parametric eq at +6 db / 2.8 khz / q=4, but it's cleaner inside a vocoder where the boost rides the formant rather than ringing.

— **autotune (correction style).** the antares trick: run pyin → snap each frame's f0 to the nearest scale degree → resynthesise via a phase vocoder or psola. with a low retune-time you get the stepped t-pain effect; with higher retune-time you get gentle correction. the *aggressive* variant is identical to "f0 substitution from score" except the target curve comes from snapping rather than from a score.

— **psola / phase vocoder.** **td-psola** (moulines & charpentier 1990, in praat) modifies pitch and duration in the time domain by repositioning glottal-pulse-aligned grains; preserves formants by construction. phase vocoder (frequency-domain) is what rubberband and librosa use under the hood. for singing, psola tends to sound more natural on small shifts, phase vocoder handles bigger transformations better but smears transients.

## the tools

| tool | language | install | gpu? | what it gives us |
|---|---|---|---|---|
| **rubberband** (cli, current) | c++ | already installed | no | pitch + time stretch, phase vocoder. has `--formant`, `--pitchmap`, `--smoothing` flags we're under-using. |
| **world / pyworld** | c++ / python | `pip install pyworld` (~1 mb) | no | analyse → `(f0, sp, ap)` → modify f0 directly → resynthesise. cleanest path to f0 substitution. dio for f0, cheaptrick for envelope, d4c for aperiodicity. |
| **praat** (cli + scripts) | c | `brew install praat` | no | manipulation object with pitchtier + durationtier, td-psola underneath. fully scriptable. heavier orchestration than pyworld but doesn't need python. |
| **librosa** | python | `pip install librosa` (~50 mb) | no | `librosa.pyin` for f0 detection, `pitch_shift` / `time_stretch` (lower fidelity than rubberband). good as a measurement tool, not a render tool. |
| **crepe** | python (tf) | `pip install crepe` (~500 mb tf) | optional | best-in-class f0 detection. heavy install for one feature. |
| **mfa** | python (kaldi) | `conda install -c conda-forge montreal-forced-aligner` (~2 gb incl. acoustic models) | no | per-phoneme alignment from audio + transcript. heavy. |
| **charsiu** | python (torch) | `pip install transformers + ckpt` (~500 mb) | optional | per-phoneme alignment, lighter than mfa, comparable accuracy. |
| **sinsy** | c++ | source build | no | full hmm-based singer from musicxml; outputs sung audio, not a transformation tool — different lane. |
| **nnsvs / diffsinger / so-vits-svc / rvc** | python (torch) | gpu strongly recommended; multi-gb models | yes | parallel research lane. produce excellent results but require trained voices and gpus. flagged out-of-scope-for-now. |

ffmpeg's bundled rubberband filter is not available in our build — confirmed earlier; we shell out to the cli.

## what fixes our specific symptoms

| symptom we hit | root cause | technique that fixes it | tool |
|---|---|---|---|
| "spoken prosody fights melody" | rubberband shifts existing f0 by interval; original speech contour rides on top of every shift | f0 substitution — replace, don't shift | pyworld (dio + stonemask + cheaptrick + synthesize) |
| "vowels don't sustain" | word-level slices stretched uniformly drag consonants too; word-final vowels are too short to ring | phoneme alignment + selective vowel stretch | charsiu (or mfa) → per-phoneme durationtier in praat / world |
| "sounds tonal not sung" | flat stepped pitch, no overshoot / vibrato / fine fluctuation; missing singer's formant ring | saitou f0-contour shaping + spectral-envelope boost at 2.5–3 khz | pyworld envelope edit + post-eq, or praat scripted |
| "octave errors on shifts" | autocorrelation in `detectPitch()` picks 2× / ½× on some words | viterbi-smoothed f0 | librosa.pyin or world.dio |
| "consonants get cut at word edges" | whisper word boundaries land mid-consonant | phoneme alignment, slice on phoneme edges | charsiu |
| "segment boundaries click" | fixed 20 ms crossfade in segmented rendering doesn't align with glottal pulses | psola — grains aligned to f0 periods | praat td-psola, or world resynth (no clicks by construction) |
| "formant character lost on big shifts" | rubberband `--formant` not currently passed in `pitchsnap.mjs` | enable formant preservation | rubberband `--formant` flag (one-line change) |

## shortlist

ranked by effort vs. payoff. opinionated.

**1. swap the rendering core to world (pyworld) for f0 substitution.** *highest payoff, ~2-day build.* this is the saitou recipe and addresses three of our top symptoms in one move:
  - rubberband segment-shift → world `(f0, sp, ap)` decompose, write target f0 from score, resynth.
  - kills "spoken prosody fights melody" (because we replace, not shift).
  - kills octave errors (dio is reliable on voice).
  - removes click artefacts at segment boundaries (no segmentation needed).
  - keeps jeffrey's timbre intact (spectral envelope unchanged).
  - new piece: a python child process called from `pitchsnap.mjs` that takes the slice + target f0 contour json and returns a wav. ~150 lines of python, ~1 mb pip install, no gpu.
  - immediately enables vibrato / overshoot / singer's-formant boost as f0/envelope edits in the same script.

**2. add phoneme alignment via charsiu, switch from word-snap to phoneme-snap.** *medium payoff, ~1-day build, depends on (1) for full effect.* gets us:
  - clean consonant/vowel boundaries, no chopped tails.
  - vowel-sustain becomes a real operation: identify the vowel span, stretch only that.
  - lyric-carrying consonants stay short and crisp; the *note* lives on the vowel.
  - charsiu over mfa for install size — 500 mb vs. 2 gb, comparable quality on english.

**3. saitou f0 sub-effects: vibrato + overshoot + fine fluctuation.** *lowest effort once (1) lands, big perceptual win.* once we own the f0 contour, layer:
  - 5.5 hz sine, 70 cents peak-to-peak, fading in after the first 150 ms of sustain → vibrato
  - 50 cents above target on attack, exponential decay over 200 ms → overshoot
  - low-pass-filtered noise at ±10 cents → fine fluctuation
  - all parameters in a recipe file, per-track tunable.
  this is ~50 lines on top of the world renderer and is the single biggest "sounds sung" lever once the substrate is right.

**deferred (parallel lane).** so-vits-svc / rvc / diffsinger / nnsvs as a separate research track. they produce the best results in the field today, but they need trained models on jeffrey's voice (so-vits-svc / rvc) or are full synthesizers from score (diffsinger / nnsvs) and skip our "preserve the speech recording" posture. start with (1)–(3); evaluate the neural lane only after we know what the signal-processing path actually sounds like on our material.

## the strongest pattern

every working s2s system in the literature — saitou 2007, sinsy 2010, vocalistener 2009, diffsinger 2022, parekh 2020 — separates f0 from spectral envelope and treats them as independent edit streams. our current pipeline doesn't. we shift one composite signal in semitones, which means jeffrey's spoken prosody is still riding on top of every "snapped" note. the single biggest upgrade is moving to a vocoder that exposes those streams separately so we can replace f0 wholesale instead of shifting it.

**top recommendation for the immediate next pitchsnap upgrade:** add a `--engine world` option to `pitchsnap.mjs` that shells out to a small `pitchsnap-world.py` for the per-word render. python takes the wav slice + target midi(s), runs `pyworld.dio` → `pyworld.stonemask` → `pyworld.cheaptrick` → `pyworld.d4c`, replaces f0 with the target curve (with a 200 ms attack overshoot and a 5.5 hz vibrato fading in after the first sustained quarter-second), and resynths with `pyworld.synthesize`. keeps the rest of the snap / scale-walk / score-mapping logic identical. one new dependency, one new flag, no neural models. that's the saitou pipeline and it should turn "tonal speech" into "sung jeffrey" in a single weekend.
