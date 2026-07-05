---
name: reading
description: Turn an Aesthetic Computer essay into a published spoken-word "reading" (podcast episode) in @jeffrey's ElevenLabs voice — narrate, score a lo-fi bed, make a caption video, and publish to Buzzsprout (which fans out to Spotify/Apple/YouTube). Use when @jeffrey says "make a reading of <essay>", "turn this essay into a podcast", "publish <month> as a reading", or references the AC Readings podcast.
---

# reading → AC Readings podcast

Turns an essay (`papers/essay-*/*.tex` or `opinion/*.md`) into a published
episode. Tooling lives in `marketing/podcast/bin/`. Voice is jeffrey-pvc via
`/api/say` (hash-cached, so re-runs are cheap).

## Guardrails (read first)

- **Publish allowlist.** Only publish essays @jeffrey has cleared. NEVER publish
  the REGARDE-adjacent reading `named-markets` ("The Market on Your Name") — it's
  under NDA. `feed.mjs`'s `HOSTED` set and the papers `PODCASTS` set are the
  allowlists; a slug not listed there does not go public.
- **Register the essay first** if it's new: it needs an entry in
  `papers/cli.mjs` (PAPER_MAP + PAPER_COPY + the essays category) with a
  `siteName` like `aesthetic-<month>-26-essay`, and the hosted mp3 uses that
  siteName.
- **Register the podcast link:** add the siteName to `PODCASTS` in
  `papers/cli.mjs` and to `HOSTED` in `marketing/podcast/bin/feed.mjs`.

## Steps

1. **Produce the audio.** From `marketing/podcast/`:
   ```bash
   node bin/produce.mjs ../../papers/essay-<slug>/<base>.tex [--kit felt] [--open]
   ```
   Per-sentence, beat-aligned to a lo-fi bed (kits: felt/brush/wood/808/pulse —
   audition with `node bin/kits.mjs`). Writes `out/<slug>.{mp3,srt,json}` and a
   cover. Register the reading register is storytelling, not salesy.

2. **Make the caption video** (caption-only, centered, per-episode theme):
   ```bash
   node bin/video.mjs <slug>            # theme from THEMES map, or:
   node bin/video.mjs <slug> --bg '#101822' --fg '#EAF0F2' --title '#8CE99A'
   ```
   → `out/youtube/<slug>.mp4`. (This ffmpeg has no libass — captions are baked
   with ImageMagick and concatenated as video clips, not the concat demuxer.)

3. **Publish to Buzzsprout** (hosts audio + RSS, auto-distributes to
   Spotify/Apple/YouTube). Needs `aesthetic-computer-vault/buzzsprout/.env`
   (`BUZZSPROUT_TOKEN`, `BUZZSPROUT_PODCAST_ID`):
   ```bash
   node bin/buzzsprout.mjs <slug>        # publish; --private to stage; list to audit
   ```
   One API call = live everywhere. Writes `out/<slug>.buzzsprout.json`.
   Canonical show URL: `https://pod.prompt.ac` (Buzzsprout custom domain;
   `podcast.aesthetic.computer` 301s there). RSS: `https://feeds.buzzsprout.com/2628235.rss`.

4. **Wire the papers listing.** Host the mp3 + regen the papers index so the
   essay shows a `podcast` link (mirrors `cards`):
   ```bash
   # from repo root, once the essay + PODCASTS entries exist:
   node papers/cli.mjs publish       # rebuilds index.html with the podcast link
   ```
   Then deploy papers (git commit + push → oven). Audio is NOT committed to git;
   it lives on Buzzsprout / the CDN.

## Fallback: self-hosted RSS (no Buzzsprout)

If not using Buzzsprout, host on the asset CDN and self-serve the feed:
```bash
# upload mp3 + cover to s3://assets-aesthetic-computer/podcast/<siteName>.{mp3,-cover.png}
node bin/feed.mjs        # regen out/feed.xml + index.json (HOSTED allowlist)
# upload feed.xml + index.json to the podcast/ prefix; flush the CDN
```
Feed URL: `https://assets.aesthetic.computer/podcast/feed.xml`. Do NOT submit
this feed to Spotify/Apple — Buzzsprout is the distribution path; submitting
both feeds creates duplicate show listings in directories.

## Notes

- Long listens: open in QuickTime (`open -a "QuickTime Player" <mp3>`); slab
  playback gets cut off by the shell.
- Voice/mix knobs: `--bedgain` (music level), `--kit`, `--stability/--similarity/--speed`.
- See `marketing/podcast/SCORE.md` for the full lane doc.
