# oskiewar — store page fill sheet (appid 5280790)

**Filled and saved 2026-09-15.** Every store-presence checklist item is
green. This sheet stays as the record of what went in, and as the recipe if
a field has to be re-entered. Anything marked *human* was, or still is, a
decision rather than a paste.

Still open on the page: **the price**, which is decided ($9.99) but blocked
on Valve (see Pricing below), and the items that fall out of the **build
upload** (platform support, package/depot matching, at least one build).
The trailer is rendered by `render-trailer.mjs` and uploads under the
Trailers tab.

Editor: https://partner.steamgames.com/apps/landing/5280790 → **Edit Store Page**.

## Basic Info

| Field | Value |
|---|---|
| Application name | `oskiewar` |
| Developer | `Jeffrey Alan Scudder` *(human: or "Aesthetic Computer" — public on the page)* |
| Publisher | same as developer |
| Franchise | leave blank |
| Release date | Oct 15 2026, 10:00 AM PDT, shown to customers as "Coming Soon" |
| Supported languages | English — Interface ✓ · Full Audio ✓ · Subtitles ✗ |
| Genre (primary) | Action |
| Genre (secondary) | Indie |
| Tags (first five weigh) | Fighting · 2D Fighter · Local Multiplayer · Physics · Arcade · PvP · Multiplayer · Singleplayer · Action · Indie · Minimalist · Funny · Retro · Controller · Competitive |
| Categories | Single-player ✓ · Multi-player ✓ · PvP ✓ · Shared/Split Screen PvP ✓ · Full Controller Support — *only after the pad-only pass* · Remote Play Together — *only after a two-network test* |
| Controller support | **Partial** (declared via the wizard). The shell has no pad-reachable quit, which is one of Valve's full-support criteria |
| Accessibility | *human* — no text entry, no chat; nothing to declare yet |
| Anti-cheat | None |
| Social links | https://oskiewar.com · https://instagram.com/whistlegraph *(human: which ones)* |
| Support | mail@aesthetic.computer · https://oskiewar.com/support.html |
| Legal line | `© 2026 Jeffrey Alan Scudder` |

## Description

| Field | Value |
|---|---|
| Short description (≤300) | *copy.md → Short description* (294 chars) |
| About this game | *copy.md → Long description*, headers in bold, no images at first pass |
| Mature content description | leave blank; content survey covers it |
| System requirements — Windows | Min: Windows 10 64-bit · dual-core · 4 GB RAM · any GPU with hardware acceleration · 500 MB. Additional notes: `The game is 264 KB. The other 499 MB is the browser it ships inside.` |
| System requirements — macOS | Min: macOS 11 · Apple silicon or 64-bit Intel · 4 GB RAM · 500 MB |
| System requirements — Linux + SteamOS | Min: Ubuntu 22.04 / SteamOS 3.x · dual-core · 4 GB RAM · 500 MB |

## Graphical Assets (upload from `assets/`)

| Slot | File |
|---|---|
| Header capsule 920×430 | `header-capsule.png` |
| Small capsule 462×174 | `small-capsule.png` |
| Main capsule 1232×706 | `main-capsule.png` |
| Vertical capsule 748×896 | `vertical-capsule.png` |
| Screenshots (5+) | `screenshot-1.png` … `screenshot-5.png` — tick all as "suitable for all ages" |
| Page background 1438×810 | `page-background.png` |
| Library capsule 600×900 | `library-capsule.png` |
| Library header 920×430 | `library-header.png` |
| Library hero 3840×1240 | `library-hero.png` |
| Library logo | `library-logo.png` (1280×342, transparent) — position: centre-bottom |
| Client icon 256×256 | `client-icon.png` |
| Community icon 184×184 | `community-icon.jpg` |

## Trailers

`node xbox/steam/store-page/render-trailer.mjs` renders whole rounds through
the same Replay Oven the Instagram reels use, at 1920×1080 and 60 fps, and
encodes to Valve's reference preset (H.264 high, ~20 Mbps, AAC 192k at
48 kHz, +faststart). Upload `assets/trailer.mp4`; the required thumbnail is
`assets/trailer-thumbnail.jpg`, which is a frame of the video itself.

Clips use `hud: "reel"` — the reel dress (matchup card, clean round, winner
called afterwards) rather than the full oven UI, whose intro/fight/outro
progress bar reads as a video scrubber on a store page. Cuts fall between
whole rounds, so nothing inside a round is cut.

Do not press Release while a trailer is still encoding — Valve blocks it.

**Uploaded 2026-09-16** (movie item 1331515): 66.3s, 1920x1080 at 60 fps,
cut as `dummy-1(KO) · dummy-2(KO) · fight-1(TIE, trimmed) · climb-1(SUMMIT)`.
Category **Gameplay** — "mostly shows what it's like to play the game",
which is the literal truth here: whole rounds, no cinematics. *Visible on
Store* and *Show before screenshots* both came up ticked from the upload and
were left that way. Steam generated its own poster frame; the slot still
takes a drag of `assets/trailer-thumbnail.jpg` if a chosen frame is wanted
over Valve's pick.

## Store Settings / Content survey

Cartoon violence between stick figures; no blood or gore; no text chat, no
user-generated content, no gambling, no nudity, no mature themes. Answer
from the Steam build, which has the community surfaces compiled out.

## Pricing

**$9.99 USD base. Decided 2026-09-16.** Per-currency conversions from
Valve's suggestion tool at propose time.

Two things stood between the decision and the field, and only one of them
yielded.

**The app had no packages at all.** Pricing lives on a store package, not on
the app, and 5280790 was created without the standard set — Valve's Pricing
FAQ names this a known app-creation bug and points at the green `Create
Standard Packages` button on the Associated Items page. Pressed 2026-09-16,
which minted three: **1827387** `oskiewar` (the store package, hidden until
release), 1827386 `oskiewar for Beta Testing`, 1827385 `oskiewar Developer
Comp`.

**Propose Pricing is still refused**, and the wording names the gate:

> This partner account (Jeffrey Alan Scudder) does not have access to edit
> pricing on this package. Only users in the partner account which is
> actively being paid can edit pricing on a package.

That is the payee gate, not a permissions checkbox — it lines up with the
identity review this lane has been waiting on since 2026-09-01, which the
onboarding page priced at up to 10 business days. Nothing to fix here; the
field opens when Valve finishes with the paperwork.

When it does: `partner.steamgames.com/store/packagelanding/1827387` →
**Propose Pricing** → $9.99 USD base → a conversion method → submit. Valve
reviews pricing in one to two business days, and the two pricing checklist
items (`Pricing For At Least One Package`, `Published Pricing For At Least
One Package`) go green from there.

## Publish

Order: save every tab → **Mark as ready for review** on the store page →
3–5 business days → coming-soon goes public (that is the wishlist button) →
build review → release on or after the later of 2026-10-01 and two weeks of
coming-soon.
