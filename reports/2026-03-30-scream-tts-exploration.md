# Scream TTS Exploration Report

**Date:** 2026-03-30
**Goal:** Make the `scream` command produce actual screaming TTS audio

## Summary

Explored multiple TTS APIs and approaches to generate screamo/metal-style vocal delivery with intelligible words. No single API perfectly solves "screamo TTS with lyrics" yet, but several promising paths emerged.

## What Was Built

### Code Changes (pushed to main)

1. **`system/netlify/functions/say.js`** — Upgraded OpenAI TTS + added ElevenLabs provider
   - `gpt-4o-mini-tts` model used when `instructions` param provided (emotional/style control)
   - Falls back to `tts-1` for normal speech (cheaper, faster)
   - ElevenLabs provider (`eleven`) with voice mapping and scream mode settings
   - New voices: ash, ballad, coral, sage, verse (OpenAI); Harry, Charlie, Callum, Liam, etc. (ElevenLabs)
   - Cache keys include instructions + scream flag for distinct entries

2. **`system/public/aesthetic.computer/lib/speech.mjs`** — Frontend passes `instructions` and `scream` to API

3. **`system/public/aesthetic.computer/disks/say.mjs`** — New colon options:
   - `say:scream WORDS` — defaults to ElevenLabs with scream settings
   - `say:eleven WORDS` — normal ElevenLabs speech
   - `say:openai:scream WORDS` — OpenAI gpt-4o-mini-tts with screaming instructions

### Infrastructure

- ElevenLabs API key added to vault (`devcontainer.env`) and production lith server (`/opt/ac/system/.env`)
- Lith server restarted, production endpoint verified working

## API Comparison

### OpenAI `gpt-4o-mini-tts` with instructions
- **Strengths:** Words are always intelligible, `instructions` field gives some emotional control
- **Weaknesses:** Still sounds like "speaking loudly" — can't break out of clean speech patterns
- **Best prompt:** "You are performing screamo metal vocals. Guttural, throat-shredding screams..."
- **Verdict:** Not screamy enough

### ElevenLabs TTS (stability=0.0, style=1.0)
- **Strengths:** More expressive than OpenAI, variable delivery, some voices get intense
- **Weaknesses:** Still fundamentally speech synthesis — "unhinged speaking" not "screaming"
- **Best voices for intensity:** Harry (Fierce Warrior), Charlie (Energetic), Callum (Husky Trickster)
- **Key settings:** `stability: 0.0`, `similarity_boost: 0.3-0.5`, `style: 1.0`
- **Verdict:** Getting close but not screamo

### ElevenLabs Sound Effects API (`/v1/sound-generation`)
- **Strengths:** Actually generates screaming sounds — metal vocals, harsh shrieks, death growls
- **Weaknesses:** Cannot reliably produce specific words/lyrics — it's a sound model, not speech
- **Best prompts:** "screamo metal vocals, guttural harsh screaming" / "deathcore breakdown vocals, pig squeal into death growl"
- **Verdict:** Best screams, but no word control

### ElevenLabs Speech-to-Speech (`/v1/speech-to-speech/{voice_id}`)
- **Strengths:** Takes audio input and transforms it through a voice — preserves word timing/content
- **Available on free tier!**
- **Approach:** Generate yelling TTS with OpenAI -> transform through ElevenLabs voice at 0 stability
- **Model:** `eleven_english_sts_v2`
- **Verdict:** Most promising hybrid — needs more testing with extreme source audio

## Test Samples Generated (`output/`)

### OpenAI gpt-4o-mini-tts
| File | Voice | Description |
|------|-------|-------------|
| `normal-test.mp3` | onyx (tts-1) | Baseline, no instructions |
| `scream-test.mp3` | onyx | Angry scream instructions |
| `scream-terror.mp3` | ash | Terror/panic instructions |
| `scream-extreme.mp3` | ash | Maximum intensity instructions |
| `scream-horror.mp3` | verse | Horror movie victim instructions |
| `screamo-openai-ash.mp3` | ash | Metal vocal instructions |

### ElevenLabs TTS (stability=0.0)
| File | Voice | Description |
|------|-------|-------------|
| `eleven-harry-scream.mp3` | Harry | stability 0.15 |
| `eleven-charlie-scream.mp3` | Charlie | stability 0.1 |
| `eleven-callum-scream.mp3` | Callum | stability 0.1 |
| `screamo-harry-0stab.mp3` | Harry | stability 0.0, screamo lyrics |
| `screamo-charlie-turbo.mp3` | Charlie (turbo) | stability 0.0, turbo model — 651K of chaos |
| `screamo-liam-0stab.mp3` | Liam | stability 0.0 |
| `screamo-callum-0stab.mp3` | Callum | stability 0.0 |
| `screamo-george-0stab.mp3` | George | British storyteller losing it |

### ElevenLabs Sound Effects (best raw screams, no word control)
| File | Prompt concept |
|------|---------------|
| `sfx-scream-metal.mp3` | Screamo metal vocals |
| `sfx-scream-horror.mp3` | Blood-curdling horror scream |
| `sfx-scream-deathcore.mp3` | Deathcore breakdown, pig squeal to growl |
| `sfx-scream-screamo.mp3` | Screamo punk like Thursday/Glassjaw |
| `sfx-words-getout.mp3` | Attempted "GET OUT" (garbled) |
| `sfx-words-burnit.mp3` | Attempted "BURN IT DOWN" (garbled) |
| `sfx-words-helpme.mp3` | Attempted "HELP ME" (garbled) |
| `sfx-words-nothing.mp3` | Attempted "I FEEL NOTHING" (garbled) |
| `sfx-just-no.mp3` | Attempted just "NO" repeated |
| `sfx-fire-concert.mp3` | Live concert "FIRE" |

### Speech-to-Speech Transforms (hybrid approach)
| File | Source -> Voice | Description |
|------|----------------|-------------|
| `sts-harry-scream.mp3` | OpenAI yelling -> Harry | stability 0.0 |
| `sts-charlie-scream.mp3` | OpenAI yelling -> Charlie | stability 0.0 |
| `sts-callum-from-openai.mp3` | OpenAI screamo -> Callum | Full screamo through Callum, 182K |

## Next Steps

1. **Voice Cloning (requires ElevenLabs Starter $5/mo):** Clone a voice from one of the good SFX screamo samples, then use it for TTS — would combine screamo quality with word intelligibility

2. **Speech-to-Speech Pipeline:** Chain OpenAI gpt-4o-mini-tts (aggressive instructions) -> ElevenLabs STS (0 stability) as a two-step generation. Could be wired into say.js as `provider: "scream-hybrid"`

3. **Pre-recorded Samples:** For the `scream` broadcast command specifically, use actual screamo audio clips rather than generating them

4. **Suno/Udio Music APIs:** Music generation models can do actual harsh vocals with lyrics — worth exploring if a music gen API becomes available
