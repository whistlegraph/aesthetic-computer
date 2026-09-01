# oskiewar on GeForce NOW — the cloud-streaming rider

Research current as of **2026-08-31**. Sources at the bottom, all Valve- or
NVIDIA-owned.

This is not a fourth store. GeForce NOW has no submission, no review queue of
its own, no fee, and no build to upload — it streams the *Steam* copy of the
game off NVIDIA's hardware. Everything here is a rider on the Steam lane in
`STEAM.md` and none of it is actionable until the appid exists. The whole
lift, once it does, is an addendum signature and two checkboxes.

It earns a document anyway for one reason: @jeffrey holds a **GFN Ultimate**
subscription, which turns the service into a test rig. Opting in is how
oskiewar becomes launchable on a machine that isn't in the house.

---

## The two shelves

| | Ready-to-Play | Install-to-Play |
|---|---|---|
| What it is | NVIDIA pre-installs and curates the title | The member installs it from their own Steam library into session storage |
| Who gets it | All tiers | Performance and Ultimate |
| Storage | NVIDIA's problem | 100 GB single-session; wiped at session end unless the member buys a persistent plan |
| Catalog size | ~2,300 | Took the total past 4,500 |
| Gate | NVIDIA curation | Publisher opted the game into Steam Cloud Play |

Install-to-Play is the newer and far wider shelf, and it is the one oskiewar
lands on by default. As of **April 30, 2026**, Performance and Ultimate members
keep Install-to-Play access after exhausting their 100 monthly hours, so the
test rig does not expire mid-month.

Both shelves hang off the same publisher opt-in. There is no way to sideload:
GFN runs no free desktop and no browser you can point at a web build, so the
Steam app is the only door. A web-only oskiewar is unreachable from GFN no
matter what tier the account holds.

---

## The opt-in, exactly

1. **The Actual Authority user signs the Steam Cloud Gaming Addendum** —
   logged into Steamworks, one link, one signature. For a sole proprietorship
   the Actual Authority user is @jeffrey himself, so this is not a
   coordination problem the way it is at a studio.
2. **Edit Store Page → Basic Info → Cloud Play.** Two boxes: *"Enable your
   game to run streamed from the Cloud, hosted by Valve, and the following
   service providers"* and, under it, **NVIDIA GeForce NOW**.
3. **The game needs Steam Cloud enabled, or its own online save system.**
   Valve names this as the substantive requirement; most titles need nothing
   else.
4. **NVIDIA enables it on the live service within 2 US business days.** No
   review board, no scheduling. The same door swings the other way: if NVIDIA
   finds a problem between the game and GFN it disables the title unilaterally,
   opt-in or not.

Cost: $0. Calendar impact on the Steam plan: none — this runs entirely inside
the store-page edit that is already happening.

---

## What this asks of oskiewar that Steam alone did not

**The save-state requirement retires a "defer."** `STEAM.md` parks Steam
Auto-Cloud on the grounds that the game keeps almost no local state — replays
post to the server, settings are thin. That reasoning is still true, but the
Cloud Play requirement now gives the config-only Auto-Cloud path an actual
reason to exist: without it, the honest answer to "Steam Cloud or your own
online save system?" is *neither*, and a member's Install-to-Play session
wipes whatever the game did keep. Enabling Auto-Cloud is cheap and it is the
cleanest way to satisfy the gate. Do it in the same pass as the opt-in.

**Two players, one stream.** oskiewar is a two-seat local game and GFN streams
a single seat. The couch case is unchanged — two pads into one client machine,
which is what the game already expects. Whether GFN passes *both* gamepads
through to the VM is the open question flagged below. Note also that Remote
Play Together layers a second streaming hop on top of an already-streamed
session; treat that combination as untested rather than as a feature to
advertise.

**Network at boot stops being a wrinkle.** `STEAM.md` flags the mood/chat/
handle-color calls the game makes at startup as a Steam Deck concern, since a
Deck in airplane mode is a normal Deck. A GFN VM is the inverse — it sits in a
datacenter. The offline-degradation work is still owed to the Deck; GFN just
does not add to it.

---

## What could not be verified

- **Electron under GFN.** GFN runs ordinary Windows builds and Electron games
  do ship there, but the Steam overlay's in-process-GPU workaround
  (`electronEnableSteamOverlay`, see `STEAM.md`) has no documented behavior
  under NVIDIA's streaming stack. Measure, don't assume.
- **Multiple local gamepads over the stream.** Whether both pads on the
  client reach the VM as two distinct controllers is undocumented in what was
  reviewed here, and it is the difference between oskiewar being playable or
  merely watchable on GFN.
- ~~Whether Cloud Play can be opted into before release.~~ **Resolved
  2026-09-01, and it is the answer we wanted:** NVIDIA's Developer Platform
  (GDP) carries a test feature that lets a developer run a game on GFN
  *before* it is released or opted into Steam Cloud Gaming. This is the short
  path to seeing oskiewar on a GFN session — it does not wait on the store
  page, the coming-soon window, or the 30-day fee clock. It does still want a
  Windows build, since GFN streams Windows VMs.
- **Install-to-Play behavior for a brand-new, low-population title.** The
  shelf is described in terms of an existing catalog; no source reviewed says
  whether a freshly-released game appears immediately.

---

## Sources

Accessed 2026-08-31.

- Steam Cloud Play (Beta) — addendum, Basic Info checkboxes, save requirement, 2-business-day enablement — https://partner.steamgames.com/doc/features/cloudgaming
- Opt in to GeForce NOW on Steam (NVIDIA GFN Developer Portal) — https://developer.geforcenow.com/learn/guides/offerings-opt-in
- Opting-in a Steam Game to GFN (NVIDIA GFN Developer Portal) — https://developer.geforcenow.com/learn/guides/offerings-bring-game-to-GFN
- GeForce NOW Install-to-Play overview (tiers, 100 GB single-session storage) — https://nvidia.custhelp.com/app/answers/detail/a_id/5675/~/geforce-now-install-to-play-overview
- GeForce NOW Developer Platform (test/debug tooling, usage dashboards) — https://developer.nvidia.com/geforce-now
- Install-to-Play access retained past the 100-hour limit from 2026-04-30 — https://en.gamegpu.com/news/igry/dostup-k-install-to-play-v-geforce-now-sokhranitsya-posle-100-chasov-igry
