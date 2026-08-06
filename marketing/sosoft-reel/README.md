# Scores for Social Software reel

Production workspace for a vertical, Jeffrey-voiced reel built from the July 16 unboxing/page-through and the June 13 Fuser documentation.

## Editorial premise

The reel begins with the blue inter-department-delivery cover and the physical unboxing. The publication's blue title/colophon card supplies the canonical order. As Jeffrey names each contribution, a strong still or held video frame overlaps the continuing page-through. The work remains a publication first: overlays clarify the objects already being handled rather than replacing the tactile performance.

## Format

- 1080 × 1920, 30 fps delivery
- Jeffrey PVC / `neutral:0`
- lowercase, calm, descriptive narration
- Arial Bold captions using the shared caption pipeline
- shared `/marketing` side stamps; alternating segmented chapter progress,
  with no ornamental timecode
- preserve paper texture, gloves, red string, punched holes, and the blue delivery-folder motif
- target about 1:50–2:00 for the primary cut; the current 254-word script supports that pace
- treat the 11:28 page-through as source footage and cut assertively through it; retain a 2:30–3:00 archival cut only if verified score details justify the extra time

## Sources

- `/Users/jas/Downloads/IMG_4889.MOV` — 11:27.683 vertical 4K/60 unboxing master
- `/Users/jas/Downloads/IMG_4753…IMG_4966` — July 16 stills
- `/Users/jas/Downloads/Social Software — Cycle 2 — Scores for Social Software-1-001.zip` — June 13 Fuser photographs
- `/Users/jas/Downloads/ChellyJin_SoSoftPresentation.mp4` — Chelly Jin presentation video
- `/Users/jas/Downloads/Biophonia_SoSoft_Fuser_V2.mp4` — Thomas Noya / *Biophonía*, 4:35 presentation master

The broader catalog and provenance live in `social-software/scores-for-social-software/`.

## Production sequence

1. Make a low-resolution analysis proxy of `IMG_4889.MOV`.
2. Log every page reveal and title-card appearance.
3. Match each of the ten contributions to a canonical still and one alternate.
4. Generate Jeffrey PVC narration through the same `/api/say` route used by `recap`.
5. Align words/captions with `marketing/bin/align-captions.mjs` and the shared word utilities.
6. Build the vertical composite using shared reframing and side-stamp libraries.
7. Master the delivery to −14 LUFS and check phone playback.

## Recovery finish

The outgoing Signal review is the fallback visual source if the original July
camera masters have been cleaned from `Downloads`. It contains the full edit,
but its first-pass narration and captions precede Casey Reas’s review. Recover
the visual spine, then regenerate the corrected audio/caption master:

```bash
node slab/bin/signal.mjs save 1 --to "Casey Reas"
node marketing/sosoft-reel/recover-from-signal-edit.mjs \
  "$HOME/.local/share/slab/signal/attachments/scores-for-social-software-master-vertical.mov"
node marketing/sosoft-reel/tts.mjs --force --force-timestamps
node marketing/sosoft-reel/alignment-to-words.mjs
node marketing/sosoft-reel/fetch-event-documentation.mjs
node marketing/sosoft-reel/caption-and-mix.mjs
node marketing/sosoft-reel/export-delivery.mjs
```

The recovery layout aspect-fills the sharp 1080×1280 page-through into a
1620×1920 source, then makes a smooth subject crop for the 1080×1920 delivery.
Each chapter begins wide, eases inward toward its object, and eases out before
the next chapter without a crop jump. Every delivered pixel is sharp moving
footage: there is no blurred extension, bottom strip, caption field, or title
backdrop. Captions use twelve envelope-derived blue variations and float above
the lower third at a restrained size with glyph outlines. Artist names stay
inside the narration captions in salmon; work titles stay inside them in teal.
Identity words fall away quickly, always clearing before the next caption,
without a separate title card or persistent corner label. The segmented bottom
timeline alternates blue and teal chapter colors instead of forming a gradient.
Chelly Jin's installation chapter uses a
dedicated vertical focus so the projection, not its blank upper field, occupies
the center. Chelly Jin and Thomas Noya each retain a brief physical-edition
inset over the moving-image excerpt; Em Lugo's card sequence is sampled higher
so the cards clear the subtitle and progress lanes.
The animated SO SOFT double-box identity runs fully inside the side edges as the
reel watermark; the Pals badges and climbing title layer are deliberately off.
The closing event photograph is a verified 4080×3072 official source, edited as
three full-frame, subject-directed Ken Burns shots without a redundant
Fuser/date label. The renderer rejects thumbnail-sized sources. When the event
ZIP is present, `index/generate-index.mjs` preserves selected originals under
ignored `out/event-originals/` before removing its temporary extraction.
The final delivered mix is transcribed through local Whisper and compared with
the screenplay in `out/scores-for-social-software-speech-qa.json`. The separate
`pronunciations.json` queue keeps personal names and non-English titles pending
human approval even when the automatic round-trip matches them; visible spelling
never changes to accommodate TTS.

## Stills cut

`stills.json` selects one to three iPhone stills for every contribution. Each
chapter moves from the tour to the stills over a breathing blur, then returns to
the tour. Build the separate delivery without replacing the canonical cut:

```bash
node marketing/sosoft-reel/caption-and-mix.mjs --stills-cut
node marketing/sosoft-reel/export-delivery.mjs --stills-cut
```

The renderer prefers matching camera originals in `out/iphone-originals/` and
falls back to the preserved catalog derivatives. With an unlocked, trusted
iPhone connected, import the originals with:

```bash
xcrun swift marketing/sosoft-reel/import-iphone-stills.swift \
  marketing/sosoft-reel/out/iphone-originals
```

## Social derivatives

- `node identity-proof.mjs [seconds]` renders a short proof of the animated
  SO SOFT double-box side identity over the delivery master.
- `node export-carousel.mjs` masters the ten selected work stills as a numbered
  1080×1350 Instagram multi-swipe set in `out/carousel/`.

See [SCRIPT.md](SCRIPT.md) and [index.json](index.json).

## Record Jeffrey's narration

The Narrator Wizard presents the revision screenplay one scene at a time with
the corresponding video frame. Each line can be recorded, replayed, replaced,
and explicitly kept. The session is resumable and no generated voice is needed
to record it.

```bash
narrator-wizard/bin/narratorwizard marketing/sosoft-reel/narrator-spec.json
```

After all twelve lines show as kept, assemble and align the human voice, then
rebuild the visuals, captions, mix, and delivery:

```bash
node marketing/sosoft-reel/use-human-narration.mjs --check
node marketing/sosoft-reel/use-human-narration.mjs
node marketing/sosoft-reel/render-realtime-spine.mjs
node marketing/sosoft-reel/caption-and-mix.mjs
node marketing/sosoft-reel/export-delivery.mjs
```

`use-human-narration.mjs` joins the kept WAVs, runs local Whisper word timing,
and writes the same timing/source contract consumed by the existing renderer.
Running `tts.mjs` followed by `alignment-to-words.mjs` deliberately switches the
contract back to the synthetic fallback.

The wizard follows the current macOS Light/Dark appearance. Its input menu can
be refreshed after a Focusrite or other CoreAudio interface is connected; live
monitoring is optional and intended for headphones.
