# SCORE.md — /social

The operating doc for Aesthetic Computer's social presence: account growth strategy,
stat tracking, and a single index of every social/publishing API tool scattered across
the monorepo. Read this before touching social tooling or planning a campaign.

Sibling docs: `pop/SCORE.md` (music mill), `recap/SCORE.md` (narrated video), `marketing/README.md` (promo gen).

---

## The accounts

Tracked in `accounts.json` (append a snapshot when you log fresh numbers).
Two brands, one funnel: **@whistlegraph** (established audience) feeds **@aesthetic.computer** (growth target).

| Handle | Platform | Role | API access today |
|---|---|---|---|
| **@aesthetic.computer** | Instagram | **primary growth target** (1k → Trial Reels) | ⚠️ unofficial (silo private-api / session cookies) — no publish |
| **@aesthetic.computer** | TikTok | primary growth target (391 fo · 2026-07-03) | ⚠️ OAuth app exists (silo); posting still manual |
| **Aesthetic Dot Computer** | YouTube | primary growth target (4 subs · 70 views) | ✅ **full** — `yt.mjs` (upload/edit/thumb/delete) |
| **@aesthetic.computer** | Bluesky | changelog + paintings feed (81 fo) | ✅ **full** — `at/*.mjs` app password |
| **AC Readings** | Podcast (pod.prompt.ac) | essay audio → Spotify/Apple/YT | ✅ publish via Buzzsprout API (`reading` skill) |
| @whistlegraph | Instagram | established funnel source | ⚠️ archive/read tooling only (instaloader/instagrapi) |
| @whistlegraph | TikTok | established funnel source (2.6M fo · 99M likes) | ⚠️ download/analyze only (`toolchain/whistlegraph/`) |
| @whistlegraph | YouTube | established funnel source (5.3k subs · 2.1M views) | ✅ **full** — `yt.mjs --as whistlegraph` (2026-07-05) |
| @promptDOTac | X | dormant AC-brand outlet | ❌ none — no developer app |
| @whistlegraph | X ([x.com/whistlegraph](https://x.com/whistlegraph)) | dormant funnel | ❌ none |
| Aesthetic Dot Computer | Spotify (artist) | music distribution (via DistroKid) | ⚠️ **S4A claim BLOCKED on an IG post** — see `pop/spotify-for-artists-claim-reply.md` |
| (jeffrey) | Are.na | curation / research surface | ⚠️ API proven (`gigs/are-na-annual-vol-8/*.mjs`, `ARENA_TOKEN`) — token not vaulted |
| papers / essays | prompt.ac (self-hosted) | long-form home base | ✅ fully owned — `papers/` → oven → lith; no gatekeeper |

**Legend:** ✅ scripted publish from this repo · ⚠️ partial/unofficial · ❌ nothing yet.

### API access build-out (next moves, in value order)

1. **Instagram official publishing — the keystone.** Today we have read-only unofficial access (silo `instagram-private-api`, instaloader archives; see `reports/instagram-api-migration-2026-03-29.md`). Build the official path via the **Instagram API with Instagram Login** (no Facebook Page needed since the 2024 API): both accounts must be Professional; create a Meta app at developers.facebook.com, add the Instagram product, connect our own accounts as testers — **own-account publishing works in dev mode without app review**. Then an `ig.mjs` CLI in `toolchain/` mirroring `yt.mjs` (`--as whistlegraph`). Triple payoff: (a) scripted reels toward the 1k Trial-Reels gate, (b) same for @whistlegraph funnel posts, (c) **the first trancenwaltz IG post unblocks the Spotify for Artists claim** (draft reply waiting in `pop/spotify-for-artists-claim-reply.md`).
2. **X developer app** — nothing exists. Free tier (~500 writes/mo) covers a mirror cadence for @promptDOTac + @whistlegraph; a small `x.mjs` CLI posts the same short-form drops as Bluesky. Needs jeffrey to create the app at developer.x.com under whichever account owns the brand.
3. **TikTok Content Posting API audit** — silo holds the OAuth app (key/secret, callback `silo.aesthetic.computer/api/tiktok/callback`, sessions in Mongo `tiktok-sessions`). Unaudited = private/draft posting only; apply for the audit to get scripted publish where the 2.6M audience is.
4. **Are.na token → vault** — scripts already speak the API; store `ARENA_TOKEN` at `vault/arena/.env` and lift the gig scripts into a small generic CLI when needed.
5. **Buzzsprout stats pull** — wire download numbers into `accounts.json` snapshots (podcast id 2628235; publish already works via the `reading` skill).
6. **YouTube quota bump** — both channels share ~6 uploads/day; request more if cadence grows.

Done: ~~YouTube whistlegraph~~ (2026-07-05, `--as whistlegraph`) · Bluesky (full) · Buzzsprout publish · papers (self-owned).

---

## Current objective: @aesthetic.computer → 1,000 IG followers

**Why 1k matters:** Instagram **Trial Reels** (test reels on non-followers first, then promote
the winners) require a public professional account with **≥1,000 followers**. Below that the
toggle never appears — it's a hard gate, not a soft one. @aesthetic.computer is currently under.
Once over 1k (public + Creator/Business + latest app), the feature unlocks, possibly after a
rollout delay.

**Positioning (resolves the "AI content" worry):** @aesthetic.computer is the *hand-built-instrument*
brand — "I made the tool that made this." That premise is the strongest defense against the AI-slop
read of any AC handle. The backlash is against AI-as-shortcut, not AI-as-instrument-you-engineered.
**Lead with the making, not the made.** Visible authorship (KidLisp coming together, a synth
responding live, a command rendering a piece) inoculates against the slop read. @whistlegraph stays
its own handle; don't merge audiences — funnel from it.

**Growth levers, in priority order:**
1. **Regular reels, consistent cadence (4–7/wk).** Reels are the only IG surface with non-follower reach. At <1k a flop costs nothing — these are at-bats.
2. **Process-over-product shorts** from material you already make: KidLisp builds, generative loops, instrument demos, "wait, that's live code?" moments.
3. **Same short → IG Reels + TikTok every time.** One edit, two surfaces. TikTok's small-account reach is more generous; it may feed IG.
4. **One repeatable series** people can anticipate ("daily KidLisp", "make a sound in 15s").
5. **Funnel from @whistlegraph** — "made with aesthetic.computer" tags, no audience merge.
6. **Outward engagement** in creative-coding / generative-art / music circles for the first few hundred.
- **Do NOT:** follow/unfollow churn, engagement pods, bought followers — they poison the non-follower targeting Trial Reels and the algorithm depend on. (See also `whistlegraph_grid_pruning` memory: archive via IG native tools, never sketchy apps = ban risk.)

---

## Tooling index

Everything below already exists in the monorepo. This is the map; `/social` is the front door.
**Credentials live in the encrypted vault, never here** — `aesthetic-computer-vault/`, unlock with
`fish vault-tool.fish unlock`.

### Instagram
| Path | What it does |
|---|---|
| `portraits/jeffrey/bin/ig-login.py` | Authenticate via instaloader, save session cookies (2FA support) |
| `portraits/jeffrey/bin/ig-export-cookies.py` | instaloader session pickle → Playwright JSON cookies |
| `portraits/jeffrey/bin/ig-import-cookies.py` | Import cookies into instaloader |
| `portraits/jeffrey/bin/ig-archive.fish` | Master @whistlegraph archive pipeline |
| `portraits/jeffrey/bin/ig-grid-archive.mjs` | Playwright-drive IG web UI to archive grid posts |
| `portraits/jeffrey/bin/ig-grid-archive-api.py` | Archive grid via private mobile API (instagrapi) |
| `portraits/jeffrey/bin/ig-archive-reels.mjs` | Archive reels via macOS iPhone Mirroring |
| `portraits/jeffrey/bin/ig-archive-mirror.mjs` | Archive grid via real iPhone app (Mirroring) |
| `portraits/jeffrey/bin/ig-index.mjs` / `ig-grid-index.mjs` / `ig-meta-extract.mjs` | Build indexes/catalogs from instaloader archives |
| `portraits/jeffrey/bin/ig-pull-comments.py` | Fetch comments from archived posts |
| `portraits/jeffrey/bin/ig-social-graph.py` + `ig-social-*.mjs` | Follower graph build → art-world classify → query → report (social-graph suite) |
| `silo/server.mjs` | Backend IG client (instagram-private-api), persistent `insta-sessions` in MongoDB |

Creds: `vault/silo/instagram.env`, `vault/silo/.env`. Sessions: `portraits/jeffrey/sessions/`. Archives: `portraits/jeffrey/ig-archive/<account>/`.

### TikTok
| Path | What it does |
|---|---|
| `toolchain/whistlegraph/grab.mjs` | Download a @whistlegraph TikTok; extract audio + metadata |
| `toolchain/whistlegraph/analyze.py` | Extract tempo / key / whistled-melody notes (feeds `/pop`) |
| `pop/bin/tiktok.mjs` | Render 9:16 video from storyboard (NVIDIA Flux slides → MP4) |
| `silo/server.mjs` | TikTok OAuth2 handler; `tiktok-sessions` in MongoDB; `/api/tiktok/callback` |

Creds: `vault/silo/.env` (TIKTOK_CLIENT_KEY/SECRET/REDIRECT_URI). Skill: `whistlegraph-pop`.

### YouTube
| Path | What it does |
|---|---|
| `toolchain/youtube/yt.mjs` | Zero-dep Data API v3 upload CLI (OAuth, resumable, metadata/thumbnail/playlist); `--as whistlegraph` drives the whistlegraph channel |
| `pop/bin/sample-from-youtube.mjs` | Fetch audio samples from YouTube |

Creds: `vault/youtube/client.json` + `token.json`. Receipt: `<video>.youtube.json`.

### Bluesky / ATProto
| Path | What it does |
|---|---|
| `at/post-to-bluesky.mjs` | Post text + image to @aesthetic.computer Bluesky |
| `at/publish-changelog.mjs` / `publish-commits.mjs` | Batch threads from changelog / commits |
| `at/bulk-share-paintings.mjs` / `share-latest-painting.mjs` / `share-random-painting.mjs` | Share paintings |
| `at/cli.mjs` | Main ATP CLI (user/painting management) |
| `at/query-posts.mjs` / `query-profile.mjs` | Read feed / profile |
| `silo/bluesky-ingest.mjs` | Pull headlines from trusted accounts |

Creds: `vault/at/.env` (BSKY_IDENTIFIER, BSKY_APP_PASSWORD), `vault/at/deploy.env`. Docs: `at/ADMIN.md`.

### Music distribution
| Path | What it does |
|---|---|
| `pop/bin/distrokid-submit.mjs` | Playwright-drive DistroKid upload; stops at review for human publish |
| `pop/bin/finalize.mjs` | Master audio + assemble metadata |

Creds: persistent Chrome profile `~/.distrokid-profile`. Status: `pop/RELEASES.md`.

### Promo / video generation (feeds the social surfaces)
| Path | What it does |
|---|---|
| `marketing/bin/gen-promo.mjs` | gpt-image-2 campaign image gen |
| `marketing/kidlisp-reels/` | KidLisp $code → 9:16 pals side-stamp reel (clean capture + resample) |
| `marketing/bin/capture-ac-native.mjs` | Puppeteer screenshots of AC native (real refs) |
| `marketing/bin/{title-card,compose-widescreen,storyboard-widescreen,opener-anim}.mjs` | Video composition pieces |
| `marketing/lib/jeffrey-refs.mjs` | Shared SHOOT+SELFIE identity refs |
| `recap/cli.mjs` + `recap/pipeline.fish` | Narrated 1080×1920 vertical video pipeline (TTS→align→photos→slides→subs→compose) |
| `pop/bin/gen-motion.mjs` | Image→video via Seedance 2.0 on fal.ai (animates illy panels) |
| `pop/bin/storyboard.mjs` | storyboard.json from beat timeline + FLUX |
| `clip-wizard/` · `shot-wizard/` · `wave-wizard/` | macOS GUI clip/shot/wave tools |

Docs: `marketing/README.md`, `recap/SCORE.md`, `pop/SCORE.md`. Motion notes: `pop_motion_pipeline` memory.

---

## Output pipelines → surfaces

Which content mill feeds which account. Plan campaigns by picking a row and a cadence.

| Pipeline | Produces | Lands on |
|---|---|---|
| `pop/` (music mill) | tracks + 1920×1080 visualizers + 9:16 cuts | DistroKid → streaming; YT (AC channel); IG/TikTok reels |
| `marketing/kidlisp-reels/` | 9:16 KidLisp process reels | IG + TikTok (@aesthetic.computer) — the Trial-Reels at-bats |
| `recap/` | narrated 1080×1920 verticals | IG/TikTok |
| `papers/` essays → `reading` skill | audio readings + caption video | Buzzsprout → Spotify/Apple/YT |
| `at/` scripts | changelog threads, painting shares | Bluesky |
| whistlegraph archive (`portraits/jeffrey/bin/`) | back-catalog of 950+ TikToks / 193 YT videos | re-cut fodder for funnel posts ("made with aesthetic.computer") |

---

## Conventions

- **Stats:** append a dated snapshot to the right account in `accounts.json`; never overwrite history.
- **Credentials:** vault only. Never commit tokens/passwords here (this monorepo is not the secret store).
- **New tooling:** if you build a new social/publishing tool anywhere in the monorepo, add a one-line row to the index above so `/social` stays the single front door.
- **PII / public-repo hygiene:** keep identity/handles that aren't already public out of committed files (cf. `feedback_slab_public_repo_pii`).
