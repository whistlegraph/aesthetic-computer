# oskiewar on Steam — the operator's manual

Research current as of **2026-08-09**. Every load-bearing claim below is sourced
to a Valve-owned page listed at the bottom. This is a new lane, separate from
the Xbox store path (`xbox/PUBLISHING.md`) — and a far shorter one. Steam has no
concept approval, no achievement mandate, no gamertag rule, and no platform-layer
rewrite waiting behind the door. The gates are a $100 fee, two review queues,
and two waiting periods, all of which run in parallel with the build work.

Store copy and the asset shot-list live in `store-page/`.

---

## The bill and the calendar

| | |
|---|---|
| App fee | **$100 per product**, non-refundable, but recoupable — paid back "after your product has at least $1,000.00 Adjusted Gross Revenue" |
| Revenue share | Valve's standard 70/30 split |
| Identity / tax / bank verification | tax verification runs **2–7 business days** via a third party |
| Fee → release | a hard **30-day waiting period** between paying the app fee and releasing |
| Coming-soon page | must be publicly visible **at least 2 weeks** before release |
| Store page review | **3–5 business days**; Valve says submit at least 7 days before you need it |
| Build review | **3–5 business days**; store page must be through review before the build can be submitted |
| Launch | manual — an approved title "will not release itself"; a human presses the green button |
| Minimum asset lift | 8 required store/library images + ≥5 screenshots at 1920×1080 + 2 icons; most are generatable (see `store-page/assets-needed.md`) |

**The realistic path.** Pay the fee on day 0 — the 30-day clock starts there and
nothing else does, so it goes first. Assets and copy are one focused week
(the copy is already drafted; most art renders out of the game itself). Store
page submitted end of week 1, approved mid-week 2, **coming-soon page live
around day 10–14**. The 2-week coming-soon minimum and the 30-day fee clock
then expire together around day 30, which is the earliest legal launch.
Realistically: **coming-soon live in 2–3 weeks, launch in 6–8 weeks**, with the
Electron shell, Steamworks wiring, and a Steam Deck pass filling the gap. None
of the waiting periods stack against the build work — they all run underneath it.

The one thing worth *not* rushing: wishlists accumulate only while the
coming-soon page is up, and wishlist count at launch is what makes the launch
email blast worth anything. A longer coming-soon window costs nothing and the
reel factory (`xbox/live/MARKETING.md`) is already generating the traffic to
point at it. Steam is explicit that wishlists are "not a factor" in algorithmic
visibility — they are a mailing list, not a ranking signal — but a mailing list
is exactly what a launch needs.

---

## Human-only steps

Flagged in order. Nothing in the next section blocks on any of these except
where noted; do step 1–3 early because the calendar starts there.

1. **HUMAN — Create the Steamworks account** at
   [partner.steamgames.com/steamdirect](https://partner.steamgames.com/steamdirect).
   Needs a Steam account, legal name, and address. Decide the
   publisher-of-record name here — it is public on every store page. Unlike
   ID@Xbox's onboarding form, Steam Direct's tax flow handles individuals
   (W-9-type information for US persons), so no entity is required to start;
   whether an entity is *wanted* for tax reasons is a different question and
   also a human one.
2. **HUMAN — Pay the $100 app fee** (credit card). Per product. Starts the
   30-day clock. Recouped in the payout after $1,000 adjusted gross revenue.
3. **HUMAN — The tax/identity/bank interview.** Tax questionnaire (SSN or EIN),
   identity verification, and bank details whose account-holder name must match
   the legal identification exactly. Verification is 2–7 business days and may
   come back asking for documents. Nothing publishes until this clears.
4. **HUMAN — Name the app.** Claiming the app in Steamworks fixes the name the
   appid is registered under. "oskiewar" is presumably uncontested, but the
   choice is a signature, not a form field.
5. **HUMAN — Set the price.** Steam pricing is set per-currency from a USD
   base with suggested conversions. The number is strategy, not engineering.
6. **HUMAN — Press "Mark as ready for review"** on the store page, and later on
   the build — each submission is a deliberate act with a 3–5 day queue behind it.
7. **HUMAN — Choose the release date and press Release.** Approved titles sit
   until someone clicks "Release App" → "Publish Now" → "Release Now". The
   account needs the "Publish app changes to Steam" and "Manage pricing and
   discounts" permissions — both live on the account created in step 1.
8. **HUMAN — Review the Steam Deck verdict.** After Valve's compatibility
   review there is roughly one week to read the results before they publish
   automatically. Contesting or fixing-and-requesting-re-review is an operator
   decision.

## Buildable now, no account required

- **The shell.** See *Build strategy* below. All of it can land in
  `xbox/steam/shell/` today; Steamworks even ships a public test appid
  (480, "Spacewar") that the SDK initializes against, so overlay and
  achievement calls can be exercised end-to-end before oskiewar has an appid
  of its own.
- **Store assets.** Nearly every required image renders out of the game —
  `snapshot.mjs` emits the fighters as resolution-independent vectors and the
  reel factory's renderer produces true gameplay at any viewport. The full
  shot-list with dimensions is `store-page/assets-needed.md`.
- **Store copy.** Drafted in `store-page/copy.md` and
  `store-page/metadata.md`. English only for now, deliberately.
- **A 16:9 capture lane.** The factory records 9:16 for Instagram; Steam wants
  1920×1080 screenshots and a ≤1080p trailer. Same headless Chrome, same
  frame driver, different viewport — the game lays itself out for whatever
  shape it is handed, so this is a parameter, not a port.
- **Achievements design + icons.** Optional on Steam (there is no equivalent
  of Xbox's XR-055 mandate), capped at 100 initially, cheap to wire, and the
  game's event stream already names the moments worth celebrating. Candidates
  in the SDK section below.
- **Depot and build scripts.** `app_build.vdf` / depot scripts for steamcmd,
  three OS depots, uploadable from CI the day the appid exists.
- **The Deck pass plan.** Written below; executable on a Deck (or SteamOS VM)
  before any submission.

---

## The gates, in order

Zero to shipped, with the rule that guards each gate.

1. **Account + fee + verification** — human steps 1–3 above. 2–7 business days
   of third-party verification; the 30-day release clock starts at fee payment.
2. **Build the store page.** Required: the graphical asset set, at least **5
   screenshots** (1920×1080 minimum, 16:9), short + long description, tags,
   categories, system requirements, and the content survey. Screenshots must
   show "what your game is actually like to play" — Valve's review rejects
   concept art, cinematics, award badges, and marketing copy inside
   screenshots. Capsules carry "just your game logo and artwork," no review
   quotes. The store page "should only contain features and content that will
   be available at launch" — which for oskiewar means: no netplay claims, no
   best-of-five claims (the sim carries `matchWins = 5` but nothing accumulates
   round wins yet — see `xbox/live/MARKETING.md`), nothing the shipped build
   cannot demonstrate.
3. **Store page review** — 3–5 business days. Approval unlocks two things at
   once: the coming-soon page can go public, and the build can be submitted.
4. **Coming-soon, publicly visible ≥2 weeks.** The wishlist-accumulation
   window. Point the reel factory's captions here (`steam://` or the store URL
   next to `oskiewar.com`).
5. **Upload + submit the build** — a substantially-final build on the default
   branch, all advertised features present, startable on every OS the page
   claims. Review is 3–5 business days; updates can continue afterward without
   re-review.
6. **Release** — manual button, earliest at max(fee + 30 days, coming-soon +
   14 days).
7. **Launch visibility** — automatic and unpurchasable. The New Releases Queue
   shows the title to logged-in users, "prioritizing the titles that have the
   least amount of views since release"; New & Trending happens "if the title
   is doing well"; wishlist emails go out. Valve: "You can't pay for your game
   to show up to more customers." What the operator controls is accurate tags,
   accurate copy, and the outside-Steam funnel — which is what the Instagram
   factory already is.

---

## Build strategy

### What the repo already has

Three runtimes for one game file, in very different states of Steam-readiness:

| Runtime | State | Steam fit |
|---|---|---|
| **Browser** — `mac-test.html` + `frame-driver.mjs` + `hello.js` | The most-exercised runtime in the repo: the reel factory renders it three times a day, the blackbox tests drive it headlessly, the social-preview burner hashes it. Chromium's Gamepad API already plays it. | Ships anywhere Chromium ships. |
| **macOS native** — `xbox/macos-native/main.swift` | 806 lines of AppKit/JavaScriptCore, no WebView: CoreGraphics rasterizes the draw commands, AVAudioEngine synthesizes the drums, GameController reads pads. Installed and working today. | Steam-shippable for macOS after a Steamworks binding — but macOS only. |
| **Windows native** — `xbox/native-bios/` | QuickJS + D3D11 + XAudio2 under **UWP** (C++/CX, CoreWindow). | Steam ships plain Win32 executables, not UWP/MSIX packages. `PUBLISHING.md` already established this platform layer is a rewrite for any non-UWP target; Steam does not change that. There is no Linux shell at all. |

### The three options

**Electron.** Wrap the browser runtime — the exact `hello.js` + frame-driver
pair the factory verifies daily — in Electron for Windows, macOS, and Linux
from one shell. [`steamworks.js`](https://github.com/ceifa/steamworks.js/)
provides Steamworks bindings with a documented Electron path, including
`electronEnableSteamOverlay()` for the overlay (the overlay hooks the GPU
present call, and Electron's out-of-process GPU breaks that without the
in-process-gpu switch the helper applies). Steam Deck takes the Linux build
natively. The precedent is not obscure: Vampire Survivors spent its first,
chart-topping year on Steam as a web-tech game in an Electron-family wrapper.
Cost: a ~250 MB download for a 264 KB game, and the "no engine, one file"
story acquires an ironic footnote — which the store page can own out loud
rather than hide.

**Extend the native shells.** macOS is nearly done — binding the Steamworks
flat C API (`steam_api_flat.h`) into Swift is routine. But Windows needs a new
Win32 shell (the QuickJS engine and shaders port; the UWP platform layer does
not), Linux needs a third shell or a bet on Proton, and Steamworks gets bound
three times in three languages. That is the multi-month shape `PUBLISHING.md`
budgets for the GDK — spent on Steam, where nothing demands it.

**Tauri.** One shell, but three *different* system webviews — WebView2 on
Windows, WKWebView on macOS, WebKitGTK on Linux. The Steam overlay has no
established story for hooking system webviews; WebKitGTK is not part of the
Steam Linux Runtime container, which makes the Deck-native build fragile; and
a game whose sim and audio are tuned against Chromium would ship on three
engines it has never run on. Not a fit.

### Recommendation: Electron at 1.0, native as the encore

Ship all three platforms from one Electron shell in `xbox/steam/shell/`,
wired through `steamworks.js`, keeping the Steamworks calls in the **main
process** behind a small IPC surface (the renderer keeps `contextIsolation` on
and never sees the native module). The reasoning is entirely about what the
repo already trusts: the browser runtime is the one with three-a-day
production mileage, headless test coverage, and a content-hash burner watching
it — the Steam build should be the runtime the factory already proves, not a
fourth one. The macOS native shell doesn't die: Steam depots are replaceable
after launch without touching the store page, so the boutique AppKit build can
take over the macOS depot in a later update, once it carries its own
Steamworks binding. The Win32 QuickJS shell remains the GDK port's problem,
where it is actually mandatory.

### Steamworks SDK integration points

- **Init.** `steam_appid.txt` beside the binary in dev; call
  `SteamAPI_RestartAppIfNecessary` (steamworks.js: `restartAppIfNecessary`) so
  a bare launch bounces through Steam. Test against appid 480 until oskiewar
  has one.
- **Overlay.** `electronEnableSteamOverlay()` in the main process. Verify on
  all three OSes — the overlay is the piece with the most platform variance in
  Electron; macOS especially wants an eyeball before the build submission.
- **Achievements.** ISteamUserStats via `client.achievement.activate(id)`.
  Optional, capped at 100 initially, each needs achieved + unachieved icons
  (64×64 JPG at the client; author at 256×256). The game already knows its
  moments — candidates: first KO; win a round without shielding; land a grab
  through a shield; KO with the ball; full-fall ground-pound crater; beat the
  training dummy; win with no damage taken; a round decided on the clock.
  Design once, deadpan names, ship at launch — achievements are free
  store-page furniture and Deck-profile glue.
- **Cloud saves.** The game keeps almost no local state (replays post to the
  server; settings are thin). Steam Auto-Cloud is a config-only path when
  there is something worth syncing; defer, nothing requires it.
- **Input.** The game already speaks gamepad on every runtime — Chromium's
  Gamepad API in the shell, and its glyph set is already Xbox-flavored
  (`inputFamily: "xbox"`), which is exactly what a Deck shows. Ship an
  official Steam Input configuration; check the "Full Controller Support"
  category only after the whole flow — boot to rematch — runs pad-only, which
  it already does by design.
- **Remote Play Together.** The sleeper. oskiewar is two players on one
  machine; Remote Play Together makes that internet-playable with **zero
  netcode** — Steam streams the second seat. A category checkbox plus a real
  two-network test session, and the store page honestly gains "play with a
  friend online" without the sim ever learning what a rollback is.
- **DRM.** Skip the Steam DRM wrapper. It is optional, and wrapping an
  Electron binary is asking for trouble for no benefit on a game whose whole
  source ships in the reels.

---

## Steam Deck

Valve's review sorts titles into **Verified / Playable / Unsupported /
Unknown**. Reviews start automatically for titles Valve deems relevant, or on
developer request through the partner site (the docs note request access is
currently limited — check once the account exists). After the review the
developer gets about a week to read the results before they publish; re-review
on request after fixes, and Valve re-tests on new Proton releases anyway.

If a Linux build exists, Valve tests it by default; otherwise the Windows
build runs under Proton, and Valve keeps "whichever set of test results is
more favorable." Shipping the Electron Linux build makes the question moot.

What Verified demands, against what oskiewar is:

| Requirement | oskiewar today |
|---|---|
| Full controller support, correct glyphs, default config reaches all content | Pad-first by design; glyphs already Xbox-style (Deck's ABXY). Verify the title/select taps have pad equivalents everywhere. |
| No compatibility warnings, launcher (if any) pad-navigable | No launcher. Electron boots straight into the game. |
| Text input via Steamworks keyboard APIs or built-in pad entry | The game has no text entry. Nothing to do. |
| 30 fps minimum at 800p | A fixed 60 Hz timestep drawing lines and circles; the perf harness (`OSKIEWAR_PERF`) already measures sim+paint in single-digit ms on far weaker constraints. |
| Smallest font ≥9 px at 1280×800; native 1280×800 support | The game lays out per-viewport and "goes compact on its own" — the one item needing a real measurement pass at 1280×800. |

One honest wrinkle: the game reaches network APIs at boot (mood, chat,
handle colors). A Deck in airplane mode is a normal Deck. The shell must
degrade offline without a stall or an error card — that is good engineering
regardless of the badge.

---

## What could not be verified

Stated plainly rather than smoothed over.

- **The short-description character cap.** Valve's own doc says only "a few
  hundred characters"; the partner UI is widely reported to enforce 300. The
  draft in `store-page/copy.md` is written to 300.
- **Achievement icon size** comes from Valve's guide pages and community
  documentation (64×64 JPG at display, larger accepted at upload), not from a
  single authoritative spec table.
- **Whether a trailer is formally required.** The required-asset list does not
  include one; the review doc discusses trailers as if present. Treat it as
  required in practice — a game about motion with a motionless page is a
  self-inflicted wound — and note you **cannot press release while a trailer
  is still encoding**.
- **macOS signing/notarization for Steam-distributed builds.** Valve's public
  docs are quiet; Steam's installer path generally sidesteps Gatekeeper
  quarantine, but this wants a test on a clean Mac, not an assumption.
- **Steam Deck review request access** — the docs say developer-initiated
  requests are available to a limited set of partners "currently."
- **Electron overlay behavior on macOS** — historically the flakiest corner of
  the overlay; measure before the build review, not after.

---

## Sources

All accessed 2026-08-09.

**Onboarding and fees**
- Steam Direct (fee, recoupment, tax/identity/bank) — https://partner.steamgames.com/steamdirect
- Steam Direct onboarding (verification timing, 30-day and 2-week rules) — https://partner.steamgames.com/doc/gettingstarted/onboarding

**Store page and release**
- Store graphical assets (all capsule/library dimensions) — https://partner.steamgames.com/doc/store/assets
- Screenshots (minimum of 5, gameplay-only guidance) — https://partner.steamgames.com/doc/features/screenshots
- Store page written description — https://partner.steamgames.com/doc/store/page/description
- Trailer specifications — https://partner.steamgames.com/doc/store/trailer
- Review process (store page + build review, 3–5 business days) — https://partner.steamgames.com/doc/store/review_process
- Release process (checklists, manual release, permissions) — https://partner.steamgames.com/doc/store/releasing
- Visibility on Steam (New Releases Queue, New & Trending, wishlists) — https://partner.steamgames.com/doc/marketing/visibility

**Features**
- Stats and achievements (100-achievement initial cap, icon pairs) — https://partner.steamgames.com/doc/features/achievements
- Steam Deck compatibility review (categories, Verified requirements, Proton) — https://partner.steamgames.com/doc/steamdeck/compat

**Community / third-party (marked as such above)**
- steamworks.js (Electron bindings, `electronEnableSteamOverlay`) — https://github.com/ceifa/steamworks.js/
