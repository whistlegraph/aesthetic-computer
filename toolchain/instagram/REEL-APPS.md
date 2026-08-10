# Instagram Reel apps

Oskiewar established the contract: deterministic slot, native 9:16 render,
media gate, staged review, dry-run payload, explicit live switch, receipt, and
ledger. Whistlegraph and Aesthetic Computer use that same contract while
keeping their sources and account credentials separate.

| App | Source | Default cadence | Queue |
|---|---|---:|---|
| `whistlegraph-ig.mjs` | visually reviewed, watermark-audited archive takes | 1/day | `tmp/whistlegraph-reels/queue/` |
| `aesthetic-ig.mjs` | proven audiovisual performances of AC pieces | 1/day | `tmp/aesthetic-reels/queue/` |

No command posts by default.

```bash
# Build today's deterministic slot.
node toolchain/instagram/whistlegraph-ig.mjs
node toolchain/instagram/aesthetic-ig.mjs

# Inspect the queue and write the exact dry-run payload.
node toolchain/instagram/whistlegraph-ig.mjs --queue
node toolchain/instagram/whistlegraph-ig.mjs --publish <id>

# Human-triggered live post.
node toolchain/instagram/whistlegraph-ig.mjs --publish <id> --live

# Clockwork. This posts only when the media gate passes and the account's
# vault file explicitly contains WHISTLEGRAPH_IG_AUTO=1 / AESTHETIC_IG_AUTO=1.
node toolchain/instagram/whistlegraph-ig.mjs --auto
node toolchain/instagram/aesthetic-ig.mjs --auto
```

## Source gates

Whistlegraph reads `downloads/reels-shortlist/audit.json`. The entire batch
must have `visualReviewed: true`; each selected clip must be `ocr-clear`. It
preserves the clean native 9:16 archive file without adding TikTok furniture
or inventing resolution through upscaling.
Run the existing shortlist audit to refresh the pool:

```bash
node toolchain/whistlegraph/reels-shortlist.mjs --limit 30
```

Aesthetic Computer rotates proven AV recipes: two notepat performances,
bubble taps, and the square-wave clock score. The live site supplies the
pixels and synthesized sound; the existing AV Reel capturer and side-stamp
renderer make the artifact.

Both lanes reuse Oskiewar's cover, 10%-thumbnail, codec, aspect, duration,
size, and loudness checks. A failed check holds the Reel in its queue.

## Separate Meta apps and vaults

Provision one Meta developer app per Instagram account, using **Instagram API
with Instagram Login**. Each account must be public and Professional
(Business or Creator). Add these permissions:

- `instagram_business_basic`
- `instagram_business_content_publish`
- `instagram_business_manage_insights`

Store only the resulting user ID, long-lived token, and Spaces credentials:

```text
<aesthetic-computer>/vault/whistlegraph/instagram.env
  WHISTLEGRAPH_IG_USER_ID=...
  WHISTLEGRAPH_IG_TOKEN=...
  WHISTLEGRAPH_IG_AUTO=0

<aesthetic-computer>/vault/aesthetic/instagram.env
  AESTHETIC_IG_USER_ID=...
  AESTHETIC_IG_TOKEN=...
  AESTHETIC_IG_AUTO=0
```

Start at `0`. Verify `me`, `quota`, three staged Reels, one dry-run payload,
and one human-triggered live post before changing the auto gate to `1`.
Tokens refresh through the shared monthly command:

```bash
node toolchain/instagram/ig.mjs refresh --all
```

Suggested cron after the review gate is lifted:

```cron
7 10 * * * bash -lc 'cd ~/aesthetic-computer && node toolchain/instagram/whistlegraph-ig.mjs --auto >> ~/.local/state/whistlegraph-reels.log 2>&1'
17 11 * * * bash -lc 'cd ~/aesthetic-computer && node toolchain/instagram/aesthetic-ig.mjs --auto >> ~/.local/state/aesthetic-reels.log 2>&1'
```
