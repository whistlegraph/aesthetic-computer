# keeps series — generative → Electron → @jeffrey → mint

A pipeline for authoring **KidLisp keeps sub-series** and getting them into the
DB under **@jeffrey**, ready to mint into the live v11 keeps contract
(`KT1Q1irsjSZ7EfUN4qHzAB2t7xLBPsAWYwBB`).

The flow, end to end:

```
generate.mjs  ──▶  manifest.json  ──▶  bridge.mjs ──CDP──▶  Electron app
 (KidLisp source)                       (posts source)      (production AC,
                                                             logged in as @jeffrey)
                                                                    │
                                          $code  ◀── kidlisp-code-created
                                            │
                                         keeps.mjs mint $code  ──▶  Tezos
```

Why this shape: a piece run in a **logged-in** AC session auto-caches its source
via `/api/store-kidlisp` using that session's auth token (`api.authorize()`), so
the `kidlisp` doc records `user = <jeffrey's sub>`. We don't juggle CLI tokens —
we ride the webview session. The Electron app already exposes CDP on `:9222`, so
an external script can drive it with **no app changes**.

## The three series

| series   | engine                                        | what varies per piece            |
|----------|-----------------------------------------------|----------------------------------|
| feedback | accumulating pixel buffer (no per-frame wipe) | seed shape, motion, palette      |
| clock    | wall-clock second triggers (`Ns` / `Ns...`)   | tick behavior, reset, palette    |
| melody   | `(melody …)` + `amplitude`-reactive form      | scale/key/feel/waveform, palette |

## Run it

**1. Generate the source set** (writes `manifest.json`):

```bash
node tezos/series/generate.mjs            # 17 pieces
node tezos/series/generate.mjs --print    # eyeball the source first
node tezos/series/generate.mjs --seed 7   # re-roll palettes (motion stays);
                                          # already-captured $codes are preserved
```

**2. Launch the desktop app on PRODUCTION and log in as @jeffrey** (manual, once):

```bash
cd ac-electron && npm run start:prod      # CDP :9222 is on by default
```

At the prompt, confirm the handle shows **@jeffrey** (the bridge refuses to run
anonymously unless you pass `--allow-anon`).

**3. Run the bridge** — feeds each piece in, captures the `$code`:

```bash
node tezos/series/bridge.mjs                 # all uncoded pieces
node tezos/series/bridge.mjs --preview       # render each ~1.5s, capture nothing
node tezos/series/bridge.mjs --series clock  # one series
node tezos/series/bridge.mjs --force         # re-run even if code already set
```

Captured `$codes` are written back into `manifest.json` (idempotent —
`store-kidlisp` dedupes by source hash, so re-runs are safe).

**4. Mint each as a keep** (2.5 XTZ keep fee, into v11):

```bash
node keeps.mjs mint $<code>
```

## Notes / gotchas

- **Production, not dev.** The bridge warns if the webview target is a localhost
  url — `$codes` must save to the live DB, so use `start:prod`.
- **Audio.** `melody` pieces won't make sound during CDP injection (no user
  gesture), so `amplitude` reads ~0 while caching. That's cosmetic — the source
  is what's stored; it plays for collectors on tap.
- **Identity.** The bridge reads `window.acUSER` and refuses to proceed if it's
  null. It does a loose `/jeffrey/` check and warns if the session looks like
  someone else.
- **Sub-series tagging.** These mint into the existing v11 collection. The
  `series`/`title` live in `manifest.json`; if we later want the trait on-chain,
  add it to the TZIP-21 metadata in `keeps.mjs` mint path.
- **Transport.** CDP target discovery + the `Cdp` client mirror
  `ac-electron/testing/cdp-latency.mjs`. Inbound message is handled by
  `boot.mjs` (`kidlisp-reload`); the `$code` comes back via
  `kidlisp-code-created` (`bios.mjs` `post-to-parent`).
