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
  under NDA. The allowlist is `marketing/podcast/lib/hosted.mjs` (shared by
  `feed.mjs` + `ship.mjs`) plus the papers `PODCASTS` set; a slug not listed
  there does not go public — `ship.mjs` hard-refuses it.
- **Register the essay first** if it's new: it needs an entry in
  `papers/cli.mjs` (PAPER_MAP + PAPER_COPY + the essays category + the
  reading-length map + a catalog abstract) with a `siteName` like
  `aesthetic-<slug>-essay`, and the hosted mp3 uses that siteName.
- **Register the podcast link:** add the siteName to `PODCASTS` in
  `papers/cli.mjs`, and add the `"<slug>": "<siteName>"` line to
  `marketing/podcast/lib/hosted.mjs`. Optional: set a per-episode `substrate`
  in `marketing/podcast/lib/substrates.mjs` (`EPISODE_SUBSTRATE`) — clean / tape
  / radio / night; default tape.

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

3. **Ship it — one command** (`ship.mjs` does the whole publish dance).
   Needs `aesthetic-computer-vault/buzzsprout/.env` (`BUZZSPROUT_TOKEN`,
   `BUZZSPROUT_PODCAST_ID`) and DO Spaces creds in the environment:
   ```bash
   node bin/ship.mjs <slug>              # Buzzsprout + CDN + feed + verify
   node bin/ship.mjs <slug> --private    # stage on Buzzsprout (not public yet)
   node bin/ship.mjs <slug> --papers     # also run papers deploy + index
   ```
   In order, `ship.mjs`:
   1. **refuses** any slug not in `lib/hosted.mjs` (the allowlist guardrail);
   2. **Buzzsprout** — uploads the episode → Spotify / Apple / YouTube (one API
      call = live everywhere; writes `out/<slug>.buzzsprout.json`);
   3. **CDN** — copies mp3 + cover to DO Spaces under the *hosted* name. This is
      **required**, not optional — the papers "podcast" link points at
      `assets.aesthetic.computer/podcast/<siteName>.mp3` and stays dark without it;
   4. **feed** — regenerates `out/feed.xml` + `index.json` (allowlist-filtered);
   5. **verify** — HEADs the live CDN mp3 + papers PDF so a claimed publish
      really landed.

   It then prints the finish block. Audio is never committed to git — it lives on
   Buzzsprout + the CDN. Canonical show URL: `https://pod.prompt.ac`
   (`podcast.aesthetic.computer` 301s there); RSS
   `https://feeds.buzzsprout.com/2628235.rss`.

4. **Light the papers listing** — deploy the PDF + regen the site index so the
   essay shows a `podcast` link (mirrors `cards`), then push + deploy lith
   (papers is served by lith, and `system/public/**` is a live-served path):
   ```bash
   node bin/ship.mjs <slug> --papers    # (or) node papers/cli.mjs deploy && node papers/cli.mjs index
   # commit ONLY the episode's files (never `git add -A` — the shared tree
   # accumulates build churn; see the finish block ship.mjs prints):
   git add papers/essay-<slug>/<slug>.tex papers/cli.mjs \
           marketing/podcast/bin/feed.mjs marketing/podcast/lib/hosted.mjs \
           marketing/podcast/lib/substrates.mjs \
           system/public/papers.aesthetic.computer/<siteName>.pdf \
           system/public/papers.aesthetic.computer/thumbs/<siteName>.jpg \
           system/public/papers.aesthetic.computer/index.html
   git commit -m "podcast: <slug> reading" && git pull --rebase --autostash && git push
   fish lith/deploy.fish
   ```
   Housekeeping: `papers/cli.mjs build/deploy` regenerates many `.log`/`.toc`
   build artifacts across all papers. Do NOT commit them — revert with
   `git checkout -- $(git diff --name-only | grep -E '\.(log|toc|aux)$')`.

## Notes

- Long listens: open in QuickTime (`open -a "QuickTime Player" <mp3>`); slab
  playback gets cut off by the shell.
- Voice/mix knobs: `--bedgain` (music level), `--kit`, `--stability/--similarity/--speed`.
- See `marketing/podcast/SCORE.md` for the full lane doc.
