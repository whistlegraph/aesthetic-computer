# Archiving the @whistlegraph grid via iPhone Mirroring

The only path that **actually changes grid visibility** AND can't be flagged as a
bot: drive the **real Instagram app on the real iPhone** through macOS iPhone
Mirroring. (IG web has no Archive in its menu; the Graph API has no archive
endpoint; the private-API route got `login_required` and twitched the account.
See memory `whistlegraph-grid-pruning`.)

Per-post **Archive** (the "⋯" menu on an opened post) is the action that removes a
post from the grid into your private Archive — reversible, engagement preserved.

## Pieces

- **`toolchain/macos/iphone-tap`** — eyes+hands CLI (`shot`/`ocr`/`tap`). Build it
  first; see its README for permissions and the black-capture fallback.
- **`bin/ig-archive-mirror.mjs`** — the orchestrator. Walks the profile feed,
  classifies each post, archives photos/carousels, **skips reels**, logs + resumes.

## What it protects

- **All reels/videos** — `345` video posts excluded from the plan, plus an
  on-screen reel guard (OCR hints: "views"/"plays"/"original audio"; vision
  fallback). Keep all reels = honored.
- **The oldest "Hi!" post** (`sDmBX0ir5u`, 2014) and any `--keep=<shortcode>`.
- **Resume**: shortcodes already in `curated/whistlegraph-archived.jsonl` are skipped.

Plan today: **2,415** photos+carousels eligible, out of 2,761 grid posts.

## Vision key (recommended)

The reel guard and control-finding fall back to OpenAI vision when OCR is unsure.
Export a key first (from the vault `.env`):

```bash
export OPENAI_API_KEY=...   # without it: OCR-only, weaker reel detection
```

## 5pm calibration checklist (do this live, once)

1. **Connect + mirror the phone.** Open **iPhone Mirroring**, unlock, leave it
   foreground. Build the CLI: `toolchain/macos/iphone-tap/build.sh`.
2. **Prove capture isn't black:**
   ```bash
   node portraits/jeffrey/bin/ig-archive-mirror.mjs --calibrate
   open /tmp/ig-mirror-shot.png
   ```
   - Real screenshot + OCR lines listed → good.
   - Blank/black → start a QuickTime Movie Recording of the phone and switch the
     read window (see iphone-tap README). Re-run `--calibrate`.
3. **Tune `COORDS`** at the top of `ig-archive-mirror.mjs` if needed: open a post
   in the app, run `--calibrate`, eyeball `/tmp/ig-mirror-shot.png`, and adjust
   `moreOptions` (the ⋯), `nextPost` (advance), `back`. (With a vision key, ⋯ and
   Archive are found automatically — coords are just the fallback.)
4. **One live post, watched:**
   ```bash
   # open the post you want to start from in the IG app, then:
   node portraits/jeffrey/bin/ig-archive-mirror.mjs --live --limit=1
   ```
   Confirm it opened ⋯ → tapped Archive → the post left the grid.
5. **Then small batches**, paced, watching the first few:
   ```bash
   node portraits/jeffrey/bin/ig-archive-mirror.mjs --live --limit=50
   ```
   Default pace 4–9s/post; raise `--min-delay`/`--max-delay` if anything feels hot.
   It hard-stops on a login/challenge wall, a blank capture, or 3 consecutive
   misses. Re-run to resume.

## Flags

```
--live              actually archive (default = dry-run plan)
--calibrate         dump a labelled shot+ocr to tune coords
--limit=N           stop after N archives
--keep=<shortcode>  extra post to never archive
--min-delay / --max-delay   seconds between posts (default 4 / 9)
--model=<id>        vision model (default gpt-4o-mini)
--no-vision         OCR-only, skip OpenAI fallback
```

## Safety notes

- Watch the first batch. This taps a live account; pace conservatively.
- Dry-run prints exactly what it *would* tap (control + source: ocr/vision/coord).
- Local backup is intact (`whistlegraph-grid.json` + downloaded originals), so
  archived posts remain recoverable independent of Instagram.
