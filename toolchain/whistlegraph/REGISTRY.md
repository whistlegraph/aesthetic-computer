# Whistlegraph Registry

The master index of the artform — every known whistlegraph, its sources, and
what media we hold. This file grows slowly, like an institution. The public
face is [whistlegraph.org](https://whistlegraph.org)
(`system/public/whistlegraph.org/index.html`, the `WGS` table); this registry
is the back room where candidates wait for codes, glyphs, and pages.

**The code table** — `downloads/CODES.json` is the master: every one of the
424 clustered songs has a unique short code (like `imab`), a title, a kind
(`graph` = a performed whistlegraph · `talk` = tutorial/promo/commentary ·
`other` = wordless), performance count, combined views, date span, and the id
of its most-viewed clip (whose final frame is the glyph). 277 are graphs. The
ten site codes are reserved. Rebuild after re-clustering: naming is an
LLM pass over `downloads/NAMING.json` (batched to subagents) → merged by
`codes.mjs`. Titles/kinds are first-pass and meant to be corrected in place.

**Aggregate reach** — `node aggregate-views.mjs` totals verified views across
platforms (`downloads/YOUTUBE.json` + CATALOG). Confirmed so far: **1.72B**
= TikTok @whistlegraph 1.47B · our YouTube channel 2.1M · 2 confirmed YouTube
reposts 253.5M. The reposts dwarf our own channel — one unauthorized Butterfly
Short (Viral Queens, `xslk5WclnRw`) alone is **252M**. Pull with
`youtube-views.mjs` (`--channel` · `--ids a,b` to confirm reposts · `--search`
→ candidates). **Caveat:** YouTube search is too noisy to auto-count —
"whistlegraph butterfly" collides with the Smile.dk "Butterfly" eurodance song
(124M), Die Antwoord, and butterfly memes, so search hits land in
`YOUTUBE-CANDIDATES.json` (gitignored) for human confirmation, never counted
automatically. Instagram reels are still TODO (no easy discovery API; likely a
supply-URLs-then-pull-stats flow like `--ids`).

**Data sources**

- `downloads/CATALOG.json` — full @whistlegraph TikTok metadata: **963 videos**
  (2019-10-15 → present, 1.47B combined views), refreshed via
  `yt-dlp --flat-playlist -J --no-warnings "https://www.tiktok.com/@whistlegraph"`.
  Descriptions, view/like/comment/save counts, timestamps, durations, music
  tracks, thumbnail URLs (signed, expire ~5 days — mirror what we keep).
- `papers/whistlegraph-platter/dropbox-files.json` — 12,903-file Dropbox
  manifest. `TikTok/2020 Out of App TikToks/` = one folder per song;
  `Music/.../Final Audios and Drafts/` = final mixes;
  `Scores/Misc. Scores/` = digitized scores.
- `portraits/jeffrey/ig-archive/whistlegraph/` — 619 local mp4s (2020–2022),
  captions in `portraits/jeffrey/curated/whistlegraph-meta.jsonl`.
- `downloads/INDEX.json` + `grab.mjs` — per-video pulls with pitch analysis.

**Pipeline** (per candidate): `grab.mjs <url>` → mp4 + wav + melody analysis →
WhisperX the audio for lyrics → `ffmpeg -sseof -0.4` final frame for the glyph
(back off a few seconds when a video ends on a title card or fade) → assign a
short code → add to the site's `WGS` table. Comments are NOT harvestable via
yt-dlp (extractor lacks support) — would need TikTok web API or browser
automation.

## Indexed on whistlegraph.org

| Code | Title | By | Year |
|------|-------|----|------|
| imab | Butterfly Cosplayer | Jeffrey Alan Scudder | 2019 |
| l8ly | Lately When I Fly | Jeffrey Alan Scudder | 2020 |
| grow | Time To Grow | Alex Freundlich | 2020 |
| idni | I Don't Need an iPhone | Whistlegraph | 2020 |
| ppl  | People Pleaser | Jeffrey Alan Scudder | 2021 |
| wiyh | What's Inside Your Heart? | Whistlegraph | 2021 |
| lonr | Loner | Camille Klein | 2021 |
| sdog | Slinky Dog | Alex Freundlich | 2021 |
| w0w  | Mommy Wow | Whistlegraph | 2021 |
| puzz | Puzzle | Camille Klein | 2021 |

Plus `m2w2` — *Music 2 Whistlegraph 2* (album, Dec 2022), assets local at
`system/public/assets/whistlegraph/music-2-whistlegraph-2/`.

## Candidates — video already in the repo (fastest to publish)

All in `portraits/jeffrey/ig-archive/whistlegraph/` unless noted. No digitized
scores in-repo for these yet.

| Title | Date | Local video |
|-------|------|-------------|
| Techno Sheep ("Troubled Sleep") | 2020-02-26 | `2020-02-26_B9BfwMklGwG.mp4` |
| When a Monster Flies Into the Screen (w/ bo en) | 2020-03-31 | `2020-03-31_B-aaMsrF_7N.mp4` |
| Fuzzy Spiral (w/ bo en) | 2020-04-08 | `2020-04-08_B-u2okSFx1E.mp4` |
| The Sun Keeps Beaming Down | 2020-08-31 | `2020-09-08_CE3ipNNlBdK.mp4` · tt 6867196363029990661 (934K) |
| Jobs (w/ Camille, prod. Charlie) | 2020-09-28 | `2020-09-28_CFqWsTElYR-.mp4` |
| Pumpkin Carver (comp. Adelaide Dalio) | 2020-11-01 | `2020-11-01_CHCh05mlVir.mp4` |
| Bandaged Heart | 2020-11-12 | `2020-11-12_CHgUmQ0FAjC.mp4` · tt 6894105205001014534 (1.8M) |
| Venus Flytrap (comp. Dalio) | 2020-11-23 | `2020-11-23_CH6pFTpFvBh.mp4` |
| Snowflakes On My Tongue (comp./sung Dalio) | 2020-12-17 | `2020-12-17_CI6wC0tFCFz.mp4` |
| Pointing, Painting, Taking | 2021-01-11 | `2021-01-11_CJ5kAbYF-xL.mp4` |
| Bunny in a Bowl | 2021-02-18 | `2021-02-18_CLc7cIGFx9_.mp4` · tt 7379862163621514527 |
| Lumilipad na Isda (Filipino, w/ @jasemiotics) | 2021-09-23 | `2021-09-23_CUJNC23o9zy.mp4` |
| Five in the Corner x2 | 2026-06-14 | `downloads/` mp4+wav+analysis · remix `pop/cornerfive/` |

## Candidates — one `grab.mjs` away (TikTok ids in CATALOG.json)

Highest-value first by reach:

| Title | Date | TikTok id | Views |
|-------|------|-----------|-------|
| Flower Eater (first lyric "Looked for so long for the one that I love"; audio + Goodiepal session in Dropbox, dated 2021-04-07. NOT the 2019-11-15 clip 6759484642547518726 — that's an earlier role-painting game with the same name) | 2021-04-09 | 6949031877718117638 | — |
| Triangles (chalk overdub; score in Dropbox) | 2021-11-28 | 7035561380962929966 | 23.2M |
| Hey There, Apple (score in Dropbox) | 2020-10-29 | 6889170390904589574 | 13.9M |
| Cheerleader | 2020-11-09 | 6893192988348927237 | 3M |
| Sad Mushroom | 2022-02-11 | 7064005105946938671 | 2.6M |
| Frog Tiara (final audio in Dropbox) | 2020-11-08 | 6892902732252892421 | 1.4M |
| Five Ghosts | 2022-10-10 | 7152762372862594346 | 797K |
| Scared of Stairs | 2019-12-21 | 6772964383090953477 | — |
| Crawling in the Corner | 2019-12-25 | 6774401338664029446 | — |
| I'm a Ghost (comp. Camille & Alex) | 2020-10-30 | 6889306528814222597 | — |
| Empty Soda Cup | 2020-11-01 | 6890210980664446213 | — |
| My Neighbor is My Best Friend | 2020-11-03 | 6890725296090156294 | — |
| Computer Art | 2020-11-03 | 6890801076853869830 | — |
| Four Lucky Heads | 2020-11-05 | 6891487693151243526 | — |
| Living Gift Exchange | 2020-11-06 | 6891802756965747974 | — |
| Please Try | 2020-11-08 | 6892857025106136326 | — |
| Flower For Two | 2020-11-14 | 6894823268633660677 | — |
| Tic-Tac-Toe | 2020-11-20 | 6897000543475092741 | — |
| Battle Between Smiley Faces | 2020-11-21 | 6897691407876672773 | — |
| Gentleman in the Sunshine | 2020-12-12 | 6905470393121819910 | — |
| Giant's Building | 2020-12-19 | 6908107353061592325 | — |
| BART | 2021-01-26 | 6922208789752057094 | — |
| Bouncing Around | 2021-03-26 | 6944105937594551557 | — |
| Dog Bite | 2021-07-18 | 6986349236858178821 | — |
| Sad Fire / Sad Campfire | 2020-10-04 | 6879891442865605893 | — |
| Crush | 2022-09-30 | 7148978035356749098 | — |

## Candidates — Dropbox-only (audio/score/video not yet local)

Scores in `Scores/Misc. Scores/`: Hey There Apple · It's Me ("It Me"/"Key") ·
In My Mind · Triangles.

Song folders in `TikTok/2020 Out of App TikToks/` (♪ = final audio also in
`Final Audios and Drafts/`): Bound By A Signal ♪ (incl. vocoder mix) ·
Creatures in Windows · Distant Hills ♪ · Fallen Flower ♪ · Foot of the
Grave · I AM ALIEN · I Don't Know What to
Draw · I Miss My Girlfriend · I Was Tripped · Little Curl ♪ · Little Lost
Kitty ("Lost Forest Cat") ♪ · Little Rock · Pen Whip Army · Rule Breaker ·
Shallow Secrets ♪ · Some of the Time · Sometimes Friendships End · Spooky
Spiral ♪ · Sprout in the Grass · Staring at the Sparkle · Thoughts On Mars
("Mars Thoughts") ♪ · When Im Sick · Let You Know Informer ♪ (3 versions) ·
I Know A Guy ♪ · Whistlegraph Practice ♪ · It's Too Hot, No It's Not (YT
thumb) · Seasons Changing (YT thumb).

## The Longest Whistlegraph Ever (So Far)

Rhizome commission; ~22 minutes, chalk on a 4×6-foot blackboard with a bell on
top, New Museum audience (arxiv paper §Music, line 163). Complete film +
`.srt` + stills + microsite media in Dropbox
(`Exhibitions/Rhizome Presents The Longest Whistlegraph Ever/`); 19.24 GB
media-archive zip at Dropbox root. Nothing local yet. Microsite:
sites.rhizome.org/the-longest-whistlegraph-ever-so-far/.

## Notes

- Some IG-era one-offs (kitty heads, gothic boy, pig, pierce-ur-ear) are
  performances without confirmed titles — excluded until named.
- Deduped false leads: "X My Circle" is a Mommy Wow lyric; "Nuthin But The
  Hole" is WIYH's Ableton subtitle.
- 147 of the 963 TikToks use external music tracks (Lil Peep, Chopin, etc.) —
  flag before any commercial reuse; 816 are original sound.
