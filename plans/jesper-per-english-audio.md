# Jesper Per English Audio Plan

## Current State

- **Source audio**: `system/public/assets/pruttipal/laer-klokken/jesper final.mp3` (88 MB) / `.wav` (1.1 GB)
- **English transcript**: `jesper final (english).txt` (73 KB) - a wall-of-text Whisper translation, no timecodes, no speaker labels, no paragraph breaks
- **Background music**: `baktok-single-no24-instrumental` (wav/m4a/ogg) - He-Man sound design from Mura Klink

## Problem

1. No timecode alignment between the English text and the original Danish audio
2. No way to produce an English-language audio version of the podcast

---

## Phase 1: Get Timecoded Transcription

The Whisper API supports `verbose_json` response format which returns word-level or segment-level timestamps. Re-run transcription with timestamps.

### Option A: Re-transcribe with Whisper `verbose_json` (Recommended)

Run the Whisper API again on `jesper final.mp3` with:
```json
{
  "model": "whisper-1",
  "response_format": "verbose_json",
  "timestamp_granularities": ["word", "segment"]
}
```

This returns segments like:
```json
{ "start": 0.0, "end": 4.5, "text": "You're listening to radio..." }
```

You get both segment-level and word-level timestamps. Store as `jesper final (english-timed).json`.

**Cost**: ~$0.36/hr, so roughly $0.50-1.00 for a full podcast.

### Option B: Whisper.cpp / faster-whisper (local, free)

Run locally with `--output_format json` or `srt`/`vtt` for timecoded subtitles:
```bash
whisper --model large-v3 --language da --task translate \
  --output_format srt \
  "jesper final.mp3"
```

This produces an `.srt` file with timecoded English text directly.

**Pro**: Free, can iterate. **Con**: Needs GPU for speed (CPU is slow on 88 MB).

### Option C: Use `stable-ts` or `whisperX` for better alignment

These tools provide more accurate word-level timestamps and can do forced alignment:
```bash
pip install stable-ts
stable-ts "jesper final.mp3" --task translate -o "jesper-timed.srt"
```

Or whisperX which adds speaker diarization (who is speaking when):
```bash
pip install whisperx
whisperx "jesper final.mp3" --task translate --diarize
```

**Pro**: Speaker labels + better timestamps. **Con**: More setup, needs HuggingFace token for diarization.

---

## Phase 2: Produce English Audio Version

Once you have timecoded English text, there are several approaches to create an English audio podcast:

### Option A: Text-to-Speech with multiple voices (Best quality)

Use a TTS service to read the English transcript with different voices per speaker.

**Services ranked by quality:**
1. **ElevenLabs** - Most natural, supports voice cloning, multilingual. Can clone the original speakers' voices from the Danish audio and have them "speak" English.
   - Cost: ~$5-15 for a full podcast at scale tier
   - API: `eleven_multilingual_v2` model

2. **OpenAI TTS** (`tts-1-hd`) - Good quality, 6 built-in voices, cheapest
   - Cost: ~$0.30 per 10K chars, roughly $2-3 for full transcript
   - Simple API, already have key

3. **Google Cloud TTS (Studio/Journey voices)** - Very natural, many voices

**Workflow:**
1. Split transcript into speaker segments (using diarization from Phase 1)
2. Assign a distinct TTS voice to each speaker (Goodiepal, Gustav/Jesper Per, Celina, Tore, Mura Klink, etc.)
3. Generate audio per segment
4. Stitch together with ffmpeg, matching original timing

### Option B: AI Dubbing (automated)

Services that handle the full pipeline (transcribe + translate + voice-match + dub):

1. **ElevenLabs Dubbing** - Upload audio, select target language, get dubbed version with matched voices and timing. Handles everything automatically.
   - Upload `jesper final.mp3`, set source=Danish, target=English
   - Preserves speaker voices and timing
   - Cost: Uses dubbing credits (~$1/min at scale tier)

2. **Rask.ai** - Similar auto-dubbing with lip-sync (not needed for audio-only)

3. **HeyGen** - More video-focused but does audio dubbing

**This is the easiest path** - one upload, one click, done.

### Option C: Hybrid (human + AI)

1. Use TTS for narrative/game-master sections
2. Keep original Danish audio for atmospheric/musical sections
3. Layer English TTS over original at reduced volume for "documentary" feel
4. Use the He-Man sound effects (`baktok-single-no24-instrumental`) as bed music under the English narration

---

## Phase 3: Post-Production

Regardless of approach, final steps:

1. **Mix audio**: Layer TTS output with original background sounds/music
2. **Add He-Man FX**: The Mura Klink sound design tracks can underscore fight scenes
3. **Export formats**: MP3 for distribution, WAV for archive
4. **Create SRT/VTT**: Subtitle file for the `laer-klokken` piece on AC

---

## Recommended Path

1. **Quick win**: Try ElevenLabs Dubbing API first - upload the MP3, get an English dub back with voice-matched speakers and preserved timing. Minimal effort.

2. **More control**: Re-run Whisper API with `verbose_json` for timecodes, then use OpenAI TTS with assigned voices per speaker segment, stitch with ffmpeg.

3. **Script to automate option 2**:
   ```
   ants/dub-jesper-per.mjs
   ```
   - Calls Whisper API with timestamps
   - Parses segments, assigns speakers
   - Calls TTS per segment
   - Concatenates with ffmpeg
   - Outputs `jesper final (english).mp3`

---

## Cost Estimates

| Approach | Cost | Effort | Quality |
|----------|------|--------|---------|
| ElevenLabs Dubbing | ~$30-60 | Low | High (voice-matched) |
| Whisper API + OpenAI TTS | ~$5 | Medium | Good (generic voices) |
| Whisper API + ElevenLabs TTS | ~$15 | Medium | High |
| Local whisper + local TTS | Free | High | Variable |
