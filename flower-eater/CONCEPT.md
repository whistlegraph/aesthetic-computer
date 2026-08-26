# Flower Eater

A revival. The girl who eats flowers has been drawn, coded, painted, and
sung across a decade of Jeffrey's work; this lane brings her back as a
living title at flower-eater.com, run on the oskiewar.com pattern.

## The four canonical layers

1. **2015 — the character.** Two drawings on drawings-rey-sc.jas.life:
   *Flower Eater* (#3636, pencil — wild-eyed girl mid-bite, stems around
   her, one nervous flower with a face) and *Tall Flower Eater* (#3639,
   ink — the same figure striding, flowers and stars raining down the
   margin). Her line is the art direction: hand-drawn ink, big eye,
   flowing hair, striding legs.
2. **2017 — the constraint.** The platter's software history: "One button
   web game about a girl who eats flowers," with the domain and a scaffold
   repo (`whistlegraph/flower-eater.com` — the game itself never landed
   there). The real engine work is the `mood` engine in C, source and
   videos in the Dropbox archive (see the excavation note below); BW-Flower
   is its native-C sibling from the same year.
3. **2019 — the mood.** A second Flower Eater: the role-painting game
   (TikTok 6759484642547518726, 2019-11-15 — the whistlegraph registry
   disambiguates it from the song). Role and mood as the mechanic, not
   points.
4. **2021 — the song.** Whistlegraph `[flwe]`, 2021-04-09: "I set a flower
   down at the foot of the grave of my friend. Looked for so long for the
   one that I love… Mystery flower, please don't cover. I am angry now."
   Goodiepal session audio in Dropbox; Mystery Flower folded in later.
   The verses are already a game loop: searching, grabbing stems, flowers
   that cover, anger.

## The fused design

Keep 2017's radical constraint and give it 2019's depth:

- **One button.** She strides on her own. The only input is the chomp.
  Timing a bite on a passing flower is the whole physical skill —
  playable on a phone with a thumb, on a pad with any button, on a
  keyboard with space.
- **Mood is the score.** Each flower species shifts her mood, and mood
  bends the world: palette, stride tempo, music, what sprouts next.
  Appetite as emotional weather. Chomping air stumbles her. The song's
  emotional arc (grief → searching → mystery → anger) is the mood-space
  the flowers move her through.
- **Her line is the renderer.** Ink strokes on paper, traced from the
  2015 drawings — the same "the fight speaks" restraint as oskiewar's
  stick fighters, but in her hand.
- **The song is the title theme.** The 2021 whistlegraph opens the game;
  the mood engine can quote its phrases per mood.

## Infra: the oskiewar.com pattern

Everything below is proven in production by the oskiewar lane as of
2026-08-26; this lane clones the shape, not the code.

- **Domain**: flower-eater.com is owned but parked at GoDaddy. Un-park →
  point at lith (see the lith direct-DNS recipe) → Caddyfile block beside
  oskiewar.com's.
- **The piece**: one self-contained game source + a web shell.
  Deterministic 60 Hz sim, a test suite whose failures are diffed against
  a recorded baseline, a monotonic buildVersion whose commit subjects are
  the changelog, a social-preview burner gated in deploy.
- **Replays**: `ac.flowerdemo` format, an upload endpoint in
  system/netlify/functions, pronounceable round URLs — with a blocklist
  in the name seeder (lesson: oskiewar's generator rolled slur-adjacent
  names into public URLs).
- **Reels**: an ACCOUNTS entry in toolchain/instagram/ig.mjs when the IG
  account exists; changelog captions from day one ("flower eater v1 — …"
  over "tell reelboy what to plant."); reelboy routes are already
  account-parameterized, so the feedback loop is a `bind` away.
- **Prototype first**: the AC piece `flower-eater` (disks/flower-eater.mjs)
  is the lab — hot reload, phone testable, shareable at
  aesthetic.computer/flower-eater — where stride, chomp and mood get their
  feel before the standalone carve. `i.mjs` was the earlier seed and its
  TODO named this exact excavation.

## The decree: api surface, contract, stack

Everything below is a consequence of one sentence, learned the hard way in
the oskiewar lane on 2026-08-26:

> **The game is a pure function of (clock, input) → (pixels, sounds,
> signals, demos) — and the server never trusts a result it didn't
> re-simulate.**

1. **One host contract, already proven.** The game is one self-contained
   `.mjs` file speaking only through injected host functions — the exact
   surface oskiewar defined (`runtime`, `gamepad`, `drum`, `synth`,
   draw primitives, `gameSignal`, `saveReplay`). That contract is why one
   oskiewar file runs identically in the browser shell, the native BIOS,
   the offline reel oven, and a `new Function` test harness — and why a
   production API bug could be found by simulating a round headlessly and
   posting it. Flower Eater inherits the surface verbatim; shells are the
   reusable half of the estate.
2. **The demo IS the api.** `ac.flowerdemo` v1: seed, build, and the chomp
   ticks. One button means a complete playthrough is a list of integers —
   a run is literally a rhythm, tweet-sized. So the server-side contract
   collapses to: `POST /api/flower-runs` receives {seed, build, chomps[]},
   **re-simulates them with the same .mjs sim in Node**, derives the
   score/mood-arc itself, and stores the canonical result. No trusted
   client scores, no row-length regexes to drift (the 26→32 checkpoint
   lesson): the validator and the game import ONE shared contract module
   (`shared/` exists for exactly this) and the ultimate validator is the
   sim itself.
3. **Surface, whole:**
   - `POST /api/flower-runs` — submit chomps; server resims; returns canon.
   - `GET /api/flower-runs?day=…` — the day's garden (leaderboard feed).
   - `GET /api/flower-stats` — the oskiewar-stats twin.
   - `flower-eater.com/<run-name>` — watch any run re-simulated in place
     (pronounceable names, blocklisted).
   - **The daily meadow**: seed = date hash, everyone eats the same field;
     ghosts come free (any run's chomp-rhythm can walk as a translucent
     girl in your meadow — replay overlay, no multiplayer server at all).
4. **Stack: JavaScript all the way down, no exceptions.** The estate's
   native tongue is dependency-free `.mjs` — the piece runtime, lith's
   Express functions, the test harness, the reel oven, reelboy. The same
   sim file is the client, the server validator, and the replay renderer;
   introducing a second language would split the decree's one truth into
   two. The 2017 mood engine's C ideas port INTO the sim; a native C++
   BIOS host can carry her to console later exactly as it carries
   oskiewar, without changing a line of the game.
5. **Storage**: MongoDB collection beside oskiewar-replays; no Redis, no
   session-server in v1 (one button, single player, ghosts are replays).

## Domains (checked 2026-08-26 via registry RDAP)

flower-eater.com is already owned (parked at GoDaddy). Around it:

| domain | status |
| --- | --- |
| flowereater.com | taken |
| flowereater.net / .org | **available** |
| flower-eater.net | **available** |
| flowereater.game / flower-eater.game | **available** |
| flowereater.gg | **available** |
| floweater.com | **available** |
| flwe.co | **available** (registry code, short-link material) |
| eatflowers.com, mysteryflower.com | taken |

Porkbun prices (live via `npm run domain check`, 2026-08-26):
flowereater.org $7.98 · floweater.com $11.08 · both .nets $12.52 ·
flowereater.gg $51.80 · flwe.co $109.47 (premium) · both .game forms
$309.47 (premium). The primary move is still un-parking the owned .com;
.org and floweater.com are the cheap defensive grabs, .game is a vanity
tax. Keys live in `aesthetic-computer-vault/.env` (PORKBUN_API_KEY /
PORKBUN_SECRET_API_KEY — if the tool says they're missing, the decrypted
.env is stale against .env.gpg; re-decrypt). Registration + DNS + Caddy
wiring is `npm run domain` (toolchain/domains/).

## Excavation still owed (needs @jeffrey)

- The `mood` engine C source + videos in the Dropbox archive — worth
  reading before the standalone build, in case the 2017 engine holds
  mechanics the fusion should honor.
- Un-parking the domain at the registrar.
- Deciding whether the 2021 song's stems (Goodiepal session) can be cut
  into the title loop, and whether the reissue wants its own IG handle.
