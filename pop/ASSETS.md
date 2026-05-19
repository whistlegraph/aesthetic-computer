# pop — media & raw-asset management

The question this answers: where do a track's **media files** (final
mix, instrumental bed, raw vocal stems, caches) live so we can work
**across machines**, reproduce stems from the repo later, and never
re-pay for something we already generated.

## the principle: code is source, media is backup

`pop/` is bottom-up ([SCORE.md](SCORE.md)). Almost every asset is
**reproducible at $0** from the repo:

- synths (`skrill` / `hoover` / `zitar` / `supersaw` / `sinepower`) and
  the bed builders (`bin/render.mjs`, `recap/bin/trance.mjs`) are pure
  code. Re-running them reproduces the bed **bit-for-bit**.

The *only* asset that is **not** free to regenerate is the **ElevenLabs
jeffrey-pvc vocal** — `say.mjs` bills real money. It is content-hash
cached locally (`<stem>.mp3` + `<stem>.mp3.hash`), so a second render
on the *same machine* is free. The job of the asset system is to make
that cache — and the finished media — survive **across machines and
time**.

So:

| asset | reproducible? | lives in |
|---|---|---|
| synths, render.mjs, sing.mjs, `.np`, `.txt` | — (it *is* the source) | **git** |
| instrumental bed mp3 | yes, $0 (`render.mjs`) | assets (convenience) |
| **raw vocal stem + `.hash` + words/alignment json** | **no — billable** | **assets (must preserve)** |
| pitched / stretched / `.tmp` intermediates | yes, $0 (from the vocal stem) | nowhere — gitignored |
| final mix mp3 (the deliverable) | from stem + bed | **assets** |

## where media lives

The AC assets system is DigitalOcean Spaces
(`s3://assets-aesthetic-computer`) mirrored to the gitignored
`system/public/assets/` tree (same system papers and the
trancenwaltz single already use). pop media lives under a structured
per-track path:

```
system/public/assets/pop/<lane>/<slug>/
  <slug>.mp3                 # final mix — the deliverable
  bed.mp3                    # instrumental bed (cheap, kept for convenience)
  vocal/
    <slug>-vocal.mp3         # RAW jeffrey-pvc stem  ── billable original
    <slug>-vocal.mp3.hash    # say.mjs cache key  ── restores = free re-render
    <slug>-vocal-words.json  # whisper word windows
    <slug>-vocal-words.json.hash
    <slug>-pitched-alignment.json
```

Intermediates (`*-pitched.mp3`, `*-stretched.mp3`, `out/.tmp/`) are
**not** stored — they regenerate locally from the vocal stem in seconds.

## the per-track process

After a track is rendered + (optionally) sung:

```bash
# 1. render the bed (free, deterministic)
node pop/hippyhayzard/bin/render.mjs --mode song
# 2. (optional) sing it — bills ONCE, then cached
node pop/hippyhayzard/bin/sing.mjs
# 3. stage durable media into the assets tree
node pop/bin/archive.mjs hippyhayzard hippyhayzard
# 4. back up to Spaces (syncs ONLY system/public/assets/pop)
npm run pop:assets:up
```

On another machine (or later, fresh checkout):

```bash
git pull
npm run pop:assets:down          # restores beds, finals, vocal cache
node pop/hippyhayzard/bin/sing.mjs   # re-render is FREE — say cache hit
```

`pop:assets:up` / `pop:assets:down` sync **only the
`system/public/assets/pop/` subtree** (fast — not the whole multi-GB
assets bucket), mirroring the same DO Spaces endpoint/flags as the
repo-wide `assets:sync`.

`pop/bin/archive.mjs <lane> <slug>` is the per-track stager: it copies
the lane's durable media (final, bed, vocal stem + hash + json) into
`system/public/assets/pop/<lane>/<slug>/` so step 4 has something to
push. Re-runnable; never touches intermediates.

## why this shape

- **git stays small** — no binaries; `pop/*/out/` is gitignored.
- **cross-system** — `git pull` + `pop:assets:down` reproduces a track,
  and any further `sing.mjs` is free because the billable vocal cache
  is restored alongside the audio.
- **future stems** — the raw vocal is preserved, so we can always
  re-mix, re-pitch, or pull a clean stem out of an old track without
  re-paying ElevenLabs or hoping a laptop still has the cache.
- **the bucket is the backup**, not git and not one machine's `~`.
