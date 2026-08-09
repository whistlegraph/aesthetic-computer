# oskiewar — Steam store copy (draft, English only)

Written in the game's own register — the reel captions in
`xbox/live/marketing/segments.mjs` are the tuning fork. Every claim below is
something the shipped build does; Steam's review rejects pages that describe
features "that will not be available at launch," so the rule here is the same
as the reel factory's: **none of the copy claims anything the game does not
do.**

---

## Short description

Target 300 characters (Valve's docs say "a few hundred"; the partner UI is
widely reported to cap at 300). This draft is 294 characters.

> A hand-built stick-figure fighting game. Real joints, real weight, real
> hitboxes. Shield stops strikes — it does not stop a grab. There is a ball,
> and it is fully legal. Everything drawn in lines and circles at runtime,
> every sound synthesized as it happens. Two players, one machine, no menus.

## Long description ("About This Game")

Formatting notes for the partner UI: bold the section headers, keep it
skimmable, one animated GIF per section maximum (each image under 5 MB, all
extras under 15 MB total — the reel factory can cut these).

---

**THE RULES**

Strikes beat nothing in particular. Shield stops strikes. Grab goes straight
through shield. That is the whole rock-paper-scissors, and every knockout is
some arrangement of it.

**THE FIGHTERS**

Stick figures with real joints, real weight, and real hitboxes. They are not
sprites — there are no sprites. Every fighter is drawn with lines and circles
at runtime, every frame, and the hits land where the geometry says they land.

**THE BALL**

There is a ball in the fighting game and it is fully legal. Kick it at
someone.

**THE POUND**

Double-tap down in the air and you are committed. Holding down buys speed, the
crater scales with how far you actually fell, and the landing leaves you flat
on the floor — a trade, never a free hit.

**THE SOUND**

Nothing you hear is a sample. Every hit, step, whoosh and bell is synthesized
at the moment it happens.

**THE MACHINE**

No engine. One JavaScript file and a fixed 60 Hz timestep. The same file runs
in a browser, on a console dev kit, and here. The store build wraps it in
Chromium, which means the 264 KB game arrives inside a 250 MB shell — we are
aware, and we find it funny too.

**WHO YOU FIGHT**

A friend on the same machine — two pads, or pad and keyboard. Or the training
dummy, who has X eyes and one flat smile and deserves everything that happens
to him. Or the bot, who does not block for your benefit.

---

### Feature bullets (for the sidebar / capsule tour)

- Local versus for two players; pad-first, keyboard welcome
- Strike / shield / grab — shield does not stop a grab
- A fully legal ball
- Ground pound with fall-scaled craters; grenades; powerups on a timer
- Every frame drawn as vectors at runtime — no sprites, no assets
- Every sound synthesized live — no samples
- One hand-written JavaScript file, fixed 60 Hz timestep, no engine
- Remote Play Together — bring the second seat over the internet
  *(include only if the checkbox ships tested)*

### Claims deliberately left out, and why

| Claim | Why it stays out |
|---|---|
| Online multiplayer / netplay | Does not exist. The network layer is one-way spectator publishing and replay upload. Remote Play Together is the honest version of this sentence. |
| Best of five | `hello.js` carries `matchWins = 5` but nothing accumulates round wins — a match is a round today (`xbox/live/MARKETING.md`). If that changes, the copy can too. |
| "24 seconds on the clock" | The retro reel hook says 24; the sim says `roundDurationUs = 30000000`. The copy just says a round clock and stays right either way. |
| Deterministic replays you can browse | True on the web (`/api/oskiewar-replays`), but not a shipped Steam-build feature until the shell exposes it. Determinism itself is safe to mention; a replay browser is not. |
| Achievements | Add the category and a line only once the list is designed and wired. |
