# corpus — phoneme target list + recording protocol

the audio side of the jeffrey harness. ~40 short jeffrey-pvc clips that the fitter optimizes pink trombone parameters against. each clip is the same phonetic target across multiple carrier contexts so co-articulation effects are observable, not a confound.

## scope

we are **not** trying to clone arbitrary jeffrey speech. the harness's job is to land coherent vowels and continuants — the things PT models well — and to handle plosives "good enough" for AC's pixelated-voice aesthetic (per `reference_pink_trombone.md`). the corpus reflects that weighting.

| category    | count | notes                                                |
| ----------- | ----: | ---------------------------------------------------- |
| short vowels |     5 | /æ ɛ ɪ ɒ ʊ/ — bat, bet, bit, bot, book                |
| long vowels  |     5 | /iː uː ɑː ɔː ɜː/ — see, soup, spa, saw, surf         |
| diphthongs   |     5 | /aɪ aʊ eɪ oʊ ɔɪ/ — eye, owl, ate, owe, oil           |
| voiced fric. |     4 | /v ð z ʒ/ — van, this, zoo, vision                   |
| unvoiced fric. |   4 | /f θ s ʃ/ — fan, thin, see, ship                     |
| voiced plos. |     3 | /b d g/ — buy, do, go                                |
| unvoiced plos. |  3 | /p t k/ — pie, two, key                              |
| nasals       |     3 | /m n ŋ/ — me, no, sing                               |
| liquids      |     2 | /l ɹ/ — lo, row                                      |
| glides       |     2 | /w j/ — we, you                                      |
| schwa        |     1 | /ə/ — neutral tract reference                         |
| ~total       |  ~37  |                                                      |

(numbers approximate; final list lives in `corpus/phonemes.json` for code.)

## carrier contexts

each phoneme is recorded in **three** contexts so the fitter sees it under different articulatory loads:

1. **isolation** — sustained for ~1.5s. e.g. for /iː/: just "eeeee". target a steady-state tract pose.
2. **digit carrier** — embedded inside a counted "one… two… three…" or "a… b… c…" sequence. inherits jeffrey-pvc's prosodic envelope.
3. **CVC word** — a real english word containing the phoneme in stressed position. provides a co-articulation reference.

three contexts × ~37 phonemes = ~111 prompts, ~1.5s each → ~170s of audio.

we keep the carrier sentence list **short** (`a b c`, `1 2 3`, "buy", "see", etc.) on purpose. ElevenLabs charges per character, and the per-prompt costs add up if we get expansive. memory: `feedback_jeffrey_pvc_settings.md` — stability ≥ 0.5 to keep voice identity.

## recording settings

- voice: `provider=jeffrey, voice=neutral:0` (the existing jeffrey-pvc PVC)
- stability: `0.5` (memory: identity drift below 0.5)
- similarity: `0.9` (memory)
- style: `0.0` (we want neutral articulation, not performance)
- speed: `0.85` for sustained vowels (gives the fitter ~20% more steady-state to chew on); `1.0` for digit/CVC carriers
- format: server returns mp3; the recorder transcodes to **mono 22050 Hz 16-bit PCM WAV** for librosa/MFCC compatibility (PT's internal sample rate is 48k but the fitter doesn't care, only the spectral envelope matters)
- with-timestamps: **on** — char-level alignment means the fitter can crop directly to the target phoneme's onset/offset without re-running whisper

cache key: sha256 of `{ text, voice, stability, similarity, style, speed }` — same convention as `pop/bin/say.mjs`. reruns of `record-corpus.mjs` are free unless the prompt list changes.

## storage layout

```
corpus/
  phonemes.json                       — master list (committed)
  raw/                                — gitignored
    iso/<phoneme-id>.{wav,json}       — iso recordings + alignment sidecar
    digit/<phoneme-id>.{wav,json}     — digit-carrier recordings
    cvc/<phoneme-id>.{wav,json}       — CVC-word recordings
  cropped/                            — gitignored
    <phoneme-id>__<context>.wav        — cropped to target phoneme span
                                        using alignment timestamps
  manifest.json                        — committed; phoneme → file map +
                                        recording metadata + sha hashes
```

`raw/` and `cropped/` are gitignored because (a) they're regenerable, and
(b) the WAVs are jeffrey-pvc derivatives — the same closed-clone reasoning
that keeps `references/` out of the repo, applied conservatively.

## what we do **not** record

- spontaneous jeffrey speech. the goal is to fit a tract, not to capture
  jeffrey's natural prosody. natural prosody comes back at synth time, not
  at fit time.
- entire words at fitting resolution. PT can't synthesize an arbitrary
  english word coherently from a static parameter set; that's `render.mjs`'s
  job, not the corpus's.
- music or singing. if singing voice ever becomes a target, that's a
  separate corpus with `--style 0.6+` and per-pitch sustains.

## next

- [ ] write `corpus/phonemes.json`
- [ ] implement `bin/record-corpus.mjs` (mirrors `pop/bin/say.mjs` cache pattern)
- [ ] verify: one isolation /iː/ recording, listened to, sounds like jeffrey
- [ ] generate full corpus
- [ ] crop to per-phoneme spans using alignment sidecars
