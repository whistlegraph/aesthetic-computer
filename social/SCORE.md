# SCORE.md — /social

The operating doc for Aesthetic Computer's social presence: account growth strategy,
stat tracking, and a single index of every social/publishing API tool scattered across
the monorepo. Read this before touching social tooling or planning a campaign.

Sibling docs: `pop/SCORE.md` (music mill), `recap/SCORE.md` (narrated video), `marketing/README.md` (promo gen).

---

## The 4 accounts

Tracked in `accounts.json` (append a snapshot when you log fresh numbers).

| Handle | Platform | Role | Goal |
|---|---|---|---|
| **@aesthetic.computer** | Instagram | **primary growth target** | **1,000 followers → unlocks Trial Reels** |
| **@aesthetic.computer** | TikTok | primary growth target | grow in tandem; feed cross-traffic to IG |
| @whistlegraph | Instagram | established funnel source | maintain; occasional "made with aesthetic.computer" funnel |
| @whistlegraph | TikTok | established funnel source | maintain; funnel to primary |

**Current stats: TODO** — numbers not yet filled into `accounts.json`. Pull them and snapshot.

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
| `toolchain/youtube/yt.mjs` | Zero-dep Data API v3 upload CLI (OAuth, resumable, metadata/thumbnail/playlist) |
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
| `marketing/bin/capture-ac-native.mjs` | Puppeteer screenshots of AC native (real refs) |
| `marketing/bin/{title-card,compose-widescreen,storyboard-widescreen,opener-anim}.mjs` | Video composition pieces |
| `marketing/lib/jeffrey-refs.mjs` | Shared SHOOT+SELFIE identity refs |
| `recap/cli.mjs` + `recap/pipeline.fish` | Narrated 1080×1920 vertical video pipeline (TTS→align→photos→slides→subs→compose) |
| `pop/bin/gen-motion.mjs` | Image→video via Seedance 2.0 on fal.ai (animates illy panels) |
| `pop/bin/storyboard.mjs` | storyboard.json from beat timeline + FLUX |
| `clip-wizard/` · `shot-wizard/` · `wave-wizard/` | macOS GUI clip/shot/wave tools |

Docs: `marketing/README.md`, `recap/SCORE.md`, `pop/SCORE.md`. Motion notes: `pop_motion_pipeline` memory.

---

## Conventions

- **Stats:** append a dated snapshot to the right account in `accounts.json`; never overwrite history.
- **Credentials:** vault only. Never commit tokens/passwords here (this monorepo is not the secret store).
- **New tooling:** if you build a new social/publishing tool anywhere in the monorepo, add a one-line row to the index above so `/social` stays the single front door.
- **PII / public-repo hygiene:** keep identity/handles that aren't already public out of committed files (cf. `feedback_slab_public_repo_pii`).
