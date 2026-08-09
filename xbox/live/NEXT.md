# oskiewar — next build

Everything queued as of 2026-08-07, in the order it wants doing. Each item
says what it is, why it is next, and how you know it worked. Items already
shipped today are in the git log, not here.

## In flight

**Entry becomes the dummy fight.** No title screen, no pal select — whatever
you land on is a live anonymous round against the dummy. Selection deprecated
behind a flag rather than deleted, with a marked seam where auth attaches.
Title wordmark grows and wiggles under the pointer; `start` bounces per
letter. *Agent running.*

## The model this build is heading toward

Dummy play is free and anonymous. Logging in — or being recognized — happens
from inside that fight, never in front of it. Auth opens **doorways** to the
bot and to other people, so it buys opponents rather than entry. Nothing
anonymous persists, so there is no guest-to-account merge to write.

## Ready to build

**Self-play fighters stop closing distance after the opening exchange.**
Since the morning movement commits (117a2a407 "Give a landed fighter their
legs back", cc509c1b0 and siblings), every self-play round plays the same
way: a real exchange in the first ~7 seconds, then both bots plant and hold
ground for the remaining ~28 while their input logs keep firing attacks out
of range. Four out of four reel-factory renders on 2026-08-09 ended
"match over · tie · 33s" — Friday's builds produced mid-round clashes and
KOs from the same seeds' slots. Storyboard evidence: frames at 10/15/20/25/30s
show identical standing poses while keycaps churn. The reel factory
(xbox/live/marketing/) depends on lively self-play; its pipeline is verified
healthy, so quiet reels trace here, not to capture. *Verify:* render
`node xbox/live/marketing/reel.mjs --slots 1` and watch the middle third of
the reel — fighters should close, clash, and someone should usually win
before the clock does; ties should be the exception again, not 4/4.


**Harden the piece against a negative clock.** `App.cpp`'s int64 QPC overflow
caused four separate failures in one day: a title screen that would not
advance, an input log that went silent, a crash in the beach ball's palette,
and a state where `hitStunUntil = 0` made every attack impossible so no round
could be won. The native fix is written but needs a signed MSIX to ship, so
the piece should survive a negative `monotonicUs` on its own. Sentinels
compared with `>= 0`, deadlines initialized to `0`, and any modulo of a
time-derived index are the shapes to hunt. *Verify:* drive the harness from
a negative start clock and play a full round.

**Living particle field.** Wind particles become part of the physics world
with air/liquid drift, displaced when a body passes over them. Cosmetic flies
and butterflies ride the same field. Held all day on frame budget; the frame
is now 9.0 ms against 16.67, so there is room. *Verify:* measure the frame on
console before and after, not just locally.

**Multiplayer, in four steps.** Architecture is in
`oskiewar-world/ARCHITECTURE.md` — authoritative tick server, client
prediction, cap of 8, Cloudflare for identity/matchmaking/ranking and never
for the tick loop. Build order, each independently verifiable:

1. Fixed timestep in the sim, decoupled from wall clock.
2. Save, restore and re-simulate locally against synthetic delay.
3. `oskiewar-world` services — identity, matchmaking queue, signaling,
   result recording. Testable with no netcode at all.
4. Transport last: `node-datachannel` directly, not Geckos.io.

Determinism is already better than assumed — the match-name seeding landed
today, and two Node processes now agree on a SHA-256 over sixty seconds.

## Blocked or waiting

**GDK CI has never compiled.** The workflow, PC backend and platform header
are committed on the knot, but GitHub's `main` has diverged and the workflow
404s there, so no run has happened. Needs a push to GitHub — deliberately not
done, since the repo is knot-first and a careless dual push has stranded
commits before. First failure is most likely `CreateWindowExW` in the runner
session; the fix if so is one flag on the smoke invocation.

**Xbox GDK renderer is a rewrite, not a port.** Console has no D3D11, no
Direct2D and no DirectWrite — D3D12 only. Six shaders, the post chain, the
sprite path and the stencil pass, plus a glyph atlas rasterized from the TTF
since there is no system text. Do it on PC first, where it is testable
against WARP without a devkit.

**Store submission** wants ID@Xbox and a GDK port; `xbox/PUBLISHING.md` and
`papers/arxiv-oskiewar-store/` have the sourced detail. Program fees are now
zero. The live lane compiles out of a retail build rather than being disabled
at runtime.

## Smaller, unscheduled

- Combat legend on demand, not only during the round intro.
- Mouse combat on web — the pointer drives menus but not a fight.
- Non-standard gamepads are refused outright; a narrow table for known Xbox
  controller IDs would be a knowable layout rather than a guess.
- `artifacts/` at the repo root should be gitignored; two build lanes write
  there.
- `xbox/README.md` still says Developer Mode costs $19. It never did — that
  was the Partner Center registration fee, and it is now waived.
