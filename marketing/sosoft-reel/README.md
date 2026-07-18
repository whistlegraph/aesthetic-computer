# Scores for Social Software reel

Production workspace for a vertical, Jeffrey-voiced reel built from the July 16 unboxing/page-through and the June 13 Fuser documentation.

## Editorial premise

The reel begins with the blue inter-department-delivery cover and the physical unboxing. The publication's blue title/colophon card supplies the canonical order. As Jeffrey names each contribution, a strong still or held video frame overlaps the continuing page-through. The work remains a publication first: overlays clarify the objects already being handled rather than replacing the tactile performance.

## Format

- 1080 × 1920, 30 fps delivery
- Jeffrey PVC / `neutral:0`
- lowercase, calm, descriptive narration
- Arial Bold captions using the shared caption pipeline
- shared `/marketing` side stamps; no progress bar or ornamental timecode
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

## Social derivatives

- `node identity-proof.mjs [seconds]` renders a short proof of the combined
  Pals + animated SO SOFT double-box side identity over the delivery master.
- `node export-carousel.mjs` masters the ten selected work stills as a numbered
  1080×1350 Instagram multi-swipe set in `out/carousel/`.

See [SCRIPT.md](SCRIPT.md) and [index.json](index.json).
