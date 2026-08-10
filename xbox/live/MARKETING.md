# oskiewar's marketing output function

The game posts itself. A date goes in, a finished 1080×1920 reel comes out,
tagged for a named market, staged for review. Nobody decides what to post
today — the slot number decides, and the operator's only job is to watch three
videos and say yes.

Code: `xbox/live/marketing/`. Staged output: `tmp/oskiewar-reels/queue/`.
Every Reel passes through the Oskiewar Replay Oven
(`marketing/replay-oven.mjs`): deterministic demo playback, no HUD, 60 fps,
JPEG-100 source frames, a CRF-14 H.264 master, and offline-only color/detail
passes. Future ray/depth treatments belong at that seam, never in live play.
Account ledger row: `social/accounts.json` → `oskiewar-ig`.

**Nothing posts to Instagram without `--live` and a token in the environment.**
The account does not exist yet; see *Provisioning* below for the steps only a
human can do.

---

## The four stages

Each runs and verifies on its own.

| Stage | File | What it decides | How you check it |
|---|---|---|---|
| 1 · Source | `source.mjs` | which match, which market | `reel.mjs --segments` |
| 2 · Render | `render.mjs` | the actual footage + audio | frames + `rounds` in the log |
| 3 · Dress | `dress.mjs` | caption, tags, cover, spec check | `reel.json` → `meta.checks` |
| 4 · Publish | `publish.mjs` | the queue, the payload, the ledger | `payload.json` |

### 1 · Source — mechanical, never taste-based

`slot = daysSince(2026-01-01) × slotsPerDay + index`. The slot picks the market
out of a fixed rotation and the seed string is `YYYY-MM-DD#index`. One slot in
five (`slot % 5 === 4`) pulls a **real recorded round** from
`/api/oskiewar-replays` instead of a synthetic one, so the feed is not only
robots sparring; which round is still arithmetic — the seed indexes the
filtered list. If the store is unreachable or has nothing over seven seconds,
it falls back to self-play and says so.

Seeding works because `oskiewar.js` calls `Math.random` **exactly once**, to seed
the match name; everything downstream — round names, ball kind, the fighters —
falls out of it. `render.mjs` replaces `Math.random` with a seeded generator
before the game boots.

**What a seed does and does not buy.** Verified by rendering the same seed
twice: the match identity reproduces (`ow-chetty900` both times), the **frames
do not** (different MD5 at t=8s). The browser advances the sim on the wall
clock, so tick alignment jitters. The copy in `segments.mjs` is written to
claim only the first. Do not upgrade it to "same seed, same frames" unless the
renderer is moved onto a fixed timestep.

### 2 · Render — the real game, at the real shape

Headless Chrome runs `mac-test.html` + `oskiewar.js` through the same frame driver
a player gets. Two halves, both borrowed from `marketing/av-reels`:

- **Video** — CDP `Page.startScreencast` → timestamped JPEG frames, concatenated
  with per-frame durations so playback is true speed. Measured 59.8–60.0 fps.
- **Audio** — `AudioNode.prototype.connect` is patched before boot so anything
  routed to `ctx.destination` also tees into a `MediaStreamDestination` an
  in-page `MediaRecorder` records. This is the established AC technique; there
  is no second audio path.

Capture is **1080×1920 natively** — recorded at 9:16, never cropped or scaled
into it. The game lays itself out for the viewport it is handed and goes
compact on its own, so the reel is the real thing at the real shape. An earlier
version captured 1:1 and composed it into a letterbox; that is gone, and
`inspect()` now *fails the build* if the aspect drifts off 9:16, because a
drift means something rescaled the video.

**A reel is one whole match, uncut.** Recording waits out whatever round was
already running so it can begin on a round's own first frame, then stops on the
result card. Head and tail are trimmed; nothing inside is cut.

> **The trim boundary is subtle and was wrong at first.** A round announces its
> end by POSTing its replay, and that POST lands at the *start* of the result
> card — `roundResultUs` of card still has to play before the next round
> begins. Treating the POST as the end of the round put ~3 s of the previous
> round's card on the front of every reel and ~0.6 s of the next round's intro
> on the back: about 8% of a 40 s reel was a neighbouring round. `cardClearMs`
> waits the card out before recording; `tailHoldMs` stops just short of it.
> Verified by eye on the first and last frames — the reel opens on its own
> countdown and closes on its own card.

> **What "a match" means here.** `oskiewar.js` carries `matchWins = 5`, but
> nothing accumulates round wins toward it — self-play calls `startSelfPlay`
> again at every round end and normal play returns to the title. All 464 rounds
> in the replay store have `roundIndex: 0`. So a match *is* a round today:
> intro countdown, fight, KO or time, result card. If best-of-five ever gets
> wired up, `renderReel({ rounds: n })` already records more than one.

Three things the renderer cuts. The live-spectator WebSocket is stubbed (a
marketing render is not a match anyone should walk in on) — that alone halved
render time. `page.fetch` is rewritten so replay POSTs land on the local shell:
`saveReplay` posts to the **absolute production URL**, so a local server cannot
intercept it by serving a path, and without the rewrite every render files its
robot sparring in the real replay store next to matches people actually played.
That rewrite is also how the factory knows a round just ended. And the debug
hitboxes that used to flash on every impact for everybody are now gated behind
VIEW/tab, so ordinary play — and every recording of it — is clean.

### 3 · Dress — the words, and only the words

**Nothing is drawn on the video.** No plate, no bands, no blurred backdrop, no
burned-in type. What is left of this stage is the part that was never pixels:
caption, hashtags, the cover frame Meta asks for, a 10% review thumbnail, and
the spec check that decides whether the file is publishable at all.

The thumbnail is still worth looking at, but it now tests something else: with
no type to read, what it checks is whether the *fight* reads at thumbnail size
— which is what decides whether anyone stops scrolling. Cover and thumbnail are
sampled at **45% of the reel's duration**, not a fixed offset: a fixed 6 s used
to land inside the opening countdown, so every cover was two motionless
figures.

`inspect()` also checks **audio loudness** (volumedetect: peak above −20 dB,
mean above −45 dB). That row is not about the codec — a dead capture measures
around −91 dB, and a silent match is a reel nobody should post. It used to be
a ≥64 kbps bitrate gate, until homebrew's ffmpeg 8 started spending ~21 kbps
on loud-but-sparse synth SFX that ffmpeg 7 padded to 128 — the proxy failed
healthy reels, so the check now asks the waveform directly. Bitrate still
prints, as information. On macOS the mux prefers `aac_at` (AudioToolbox),
which treats sparse content best; elsewhere it falls back to ffmpeg's native
`aac`.

Caption = one line of copy, the address, then nine hashtags. Meta allows 2200
characters and 30 tags; a keyword dump is not a post.

### 4 · Publish — dry by default

`reel.mjs --publish <id>` writes `payload.json`: the exact container body, the
status-poll URL, and the publish call it *would* send. Nothing leaves the
machine. Adding `--live` uploads the mp4 + cover to DO Spaces (Meta cURLs
`video_url`, so it must be publicly reachable) and runs the real three-step
sequence, then appends to `ledger.json`.

The ledger records segment, seed, slot, kind, round, timestamp, media id, and
later the retrieved insights — which is the whole reason a segment is recorded
at all. `reel.mjs --report` rolls it up per market.

---

## Measured throughput, and the slot grid it justifies

On one M-series laptop, Chrome headless, nothing else running:

| | |
|---|---|
| Capture rate | **59.8–60.0 fps** at 1080×1920 (median frame gap 16.70 ms), encoded at 60 CFR |
| Reel length | one full round — **35–40 s** |
| Render wall clock | **~107 s** per reel, warm-up round included |
| Ratio | **2.4–3.0× realtime** |
| Output | 1080×1920, H.264, AAC 127 kbps / 48 kHz, ~5 MB |

**Three slots a day.** The defence is arithmetic, not vibes:

- Three reels cost ~5 minutes of machine time, most of it the warm-up round
  each one waits out. A day's output still fits in the gap between two builds.
- Meta's published ceiling is 50 API posts per rolling 24 hours (the docs also
  say 100 in one place — read `content_publishing_limit` at runtime and believe
  the account). Three is under it by a factor of sixteen, so the grid can triple
  before the API is the constraint.
- `social/SCORE.md` already sets the house cadence at 4–7 reels/week for the
  growth accounts. Three a day is 21/week — deliberately above it, because
  below 1k followers a flop costs nothing and these are at-bats. If reach data
  says otherwise, cut `--slots-per-day` and the rotation reslices itself.

Slots are numbered, not timed; when each one posts is the scheduler's business.
A sane spread is 09:00, 14:00, 19:00 local.

---

## The market table

`reel.mjs --segments` prints it. Rotation is a fixed ten-slot cycle, so each
market gets a guaranteed share instead of whatever the operator felt like.

| key | market | share | the true thing it says |
|---|---|---|---|
| `fgc` | fighting game / FGC | 3/10 | hitboxes, hitstun, grabs, a ball nobody asked for |
| `gamedev` | indie gamedev | 2/10 | no engine, one file, fixed timestep, headless tests |
| `retro` | retro / pixel + arcade | 2/10 | drawn with lines at runtime, no sprites, arcade rules |
| `gen` | generative / computational art | 2/10 | the date seeds the match; the audio is synthesized live |
| `homebrew` | Xbox / console homebrew | 1/10 | same source on console under JavaScriptCore, no port |

Change the `rotation` array in `segments.mjs` to change the market mix. Nothing
else moves.

---

## Running it

```bash
node xbox/live/marketing/reel.mjs                       # today's slot 0
node xbox/live/marketing/reel.mjs --day 2026-08-09 --slots 3
node xbox/live/marketing/reel.mjs --segment fgc         # force a market (review only)
node xbox/live/marketing/reel.mjs --seconds 20 --no-replays
node xbox/live/marketing/reel.mjs --queue               # what is staged
node xbox/live/marketing/reel.mjs --segments            # the market table
node xbox/live/marketing/reel.mjs --report              # per-market performance
node xbox/live/marketing/reel.mjs --publish <id>        # dry run — writes payload.json
node xbox/live/marketing/reel.mjs --publish <id> --live # the only call that posts
```

**Clockwork.** Three crons on neo (where the vault and Chrome live), one
slot each at 9:00, 14:00 and 19:00 Pacific:

```cron
7 9 * * *   bash -lc 'cd ~/aesthetic-computer && set -a && source vault/oskiewar/instagram.env && set +a && node xbox/live/marketing/reel.mjs --index 0 --slots 1 --auto >> ~/.local/state/oskiewar-reels.log 2>&1'
7 14 * * *  bash -lc 'cd ~/aesthetic-computer && set -a && source vault/oskiewar/instagram.env && set +a && node xbox/live/marketing/reel.mjs --index 1 --slots 1 --auto >> ~/.local/state/oskiewar-reels.log 2>&1'
7 19 * * *  bash -lc 'cd ~/aesthetic-computer && set -a && source vault/oskiewar/instagram.env && set +a && node xbox/live/marketing/reel.mjs --index 2 --slots 1 --auto >> ~/.local/state/oskiewar-reels.log 2>&1'
```

`--auto` renders the slot and posts it only when BOTH gates pass — Meta's
spec table and the sync meter. A reel that fails either is held in the queue
for a human, loudly, and the day goes on. @jeffrey lifted the human-per-post
review on 2026-08-09, after approving the pipeline reel by reel; `--publish
<id> --live` remains the manual override, and `ledger.json` records every
post either road makes. The monthly token refresh rides its own cron:
`node toolchain/instagram/ig.mjs refresh --all` on the 1st.

---

## Provisioning — the parts only @jeffrey can do

**Done 2026-08-09.** @oskiewar exists (Professional/Creator), Meta app 969732799453332, IG app 1681994666247197, IG user 17841443390705938. Long-lived token + Spaces creds in vault/oskiewar/instagram.env; refresh monthly. The dry-run and read-only API calls are verified; --live still waits on the three-reel review below.

Ordered. Nothing in stage 4 works until all of these are done.

1. **Create the Instagram account** `@oskiewar` and set it to a **Professional
   (Business or Creator)** account. The API cannot touch a consumer account.
   Keep it public — Trial Reels needs public + professional + ≥1,000 followers.
2. **Create a Meta app** at developers.facebook.com and add the **Instagram**
   product. Use **Instagram API with Instagram Login** — since the 2024 API this
   needs **no Facebook Page**, which is one fewer thing to own. (The Facebook
   Login path still exists and is not deprecated; it just costs a Page.)
3. **Connect @oskiewar to the app as its own account.** Publishing to an account
   you own needs only **Standard Access** — *no App Review and no Business
   Verification*. Scopes: `instagram_business_basic`,
   `instagram_business_content_publish`, and `instagram_business_manage_insights`
   for stage 4's reporting.
4. **Get a long-lived token.** Authorize → short-lived token (1 hour) → exchange
   at `GET graph.instagram.com/access_token?grant_type=ig_exchange_token` for a
   **60-day** token → refresh with `grant_type=ig_refresh_token`. The refresh
   only works when the token is **at least 24 hours old** and not yet expired;
   past 60 days there is no revival, only a fresh OAuth round-trip. Put a
   monthly refresh on the calendar.
5. **Vault the credentials** — `vault/oskiewar/instagram.env`, never this repo:
   ```
   OSKIEWAR_IG_USER_ID=...
   OSKIEWAR_IG_TOKEN=...
   OSKIEWAR_SPACES_BUCKET=art-aesthetic-computer
   OSKIEWAR_SPACES_KEY=...
   OSKIEWAR_SPACES_SECRET=...
   ```
6. **Decide the public host** for `video_url`. Meta cURLs it, so it must be
   publicly reachable with no auth. DO Spaces is the default and is already the
   AC CDN. There is no local-upload path on Instagram Login — resumable upload
   is Facebook Login only.
7. **Watch three reels and say yes** before the first `--live`.

### The rule that is not negotiable

Publishing goes through the **official content-publishing endpoints only**.
No `instagrapi`, no session cookies, no scripted app login. That road produced
a `login_required` and a soft-locked account during a ban wave — the
`whistlegraph_grid_pruning` history. `publish.mjs` contains no other path and
should not grow one.

---

## Meta's specs, as of the docs on 2026-08-08

Verified against developers.facebook.com, not memory. `dress.mjs → inspect()`
checks every row that a file can be checked against and fails the build loudly.

**Reels video** — MP4/MOV, moov atom at the front, no edit lists · H.264 or
HEVC, progressive, closed GOP, 4:2:0 · AAC, **≤48 kHz**, 1–2 channels, 128 kbps
· **23–60 fps** · max **1920** horizontal pixels · aspect 0.01:1–10:1 (9:16
recommended) · video bitrate ≤25 Mbps VBR · **3 s – 15 min** · **≤300 MB**.
Cover photo: JPEG, ≤8 MB, sRGB, 9:16 or it gets centre-cropped.

**Endpoints** — `POST /{ig-user-id}/media` (`media_type=REELS`, `video_url`,
`caption`, `cover_url`, `share_to_feed`) → poll
`GET /{container-id}?fields=status_code` **once a minute, no more than five**
(`IN_PROGRESS` / `FINISHED` / `ERROR` / `EXPIRED` / `PUBLISHED`; containers
expire at 24 h) → `POST /{ig-user-id}/media_publish` with `creation_id`.

**Rate limit** — Meta's own pages disagree: the content-publishing guide says
**100 posts / 24 h**, the carousel note and the `content_publishing_limit`
sample both say **50**. Treat 50 as the budget and read
`GET /{ig-user-id}/content_publishing_limit` at runtime — `publish.mjs` does,
and refuses to post when the quota is spent. Publishing is under business-use-case
limits (`4800 × impressions` calls/24 h), so a cold account has a small call
budget on top of the post cap.

**Insights** — `views`, `reach`, `likes`, `comments`, `saved`, `shares`,
`total_interactions`, `ig_reels_avg_watch_time`, `ig_reels_video_view_total_time`.
`plays`, `impressions` and `clips_replays_count` were retired in April 2025;
don't ask for them. Several are flagged "in development" or "estimated", so
expect drift from the app UI.

**Caption limits** — 2200 characters, 30 hashtags, 20 @-mentions.

### Known follow-ups

- `render-social-preview.mjs` and `tests/blackbox-rounds.mjs` each keep their
  own copy of the shell's module list. `marketing/shell.mjs` is the version
  meant to win; folding the other two onto it changes the burner's content hash
  and needs a re-burn in the same commit.
- Stage 4's scheduler is a crontab line, not a service. If the cadence sticks,
  it wants to become an oven job next to the other pollers.

### Not verified

- ~~Whether the real cap is 50 or 100~~ — resolved 2026-08-09: this account answers quota_total 100 per 24h.
- Any byte-range / redirect requirement on `video_url` beyond "publicly
  accessible server".
- The widely repeated 90-second Reels-tab eligibility ceiling — it appears on no
  Meta page.
- How long after publishing insights become available. Poll defensively.
- Every stage-4 call against the live API. The account does not exist, so
  `uploadPublic`, `publishLive`, `remainingQuota` and `pullInsights` have been
  written and read but **never run**.
