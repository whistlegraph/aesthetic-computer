# The MacNeoPolitan Trio — band members

Piece 2 of the CultureHub program: three MacBook Neos, one downbeat, conducted
by a single `.mbscore` (format + conductor: `slab/menuband/scores/`,
`slab/menuband/bin/conduct.mjs`).

Each band member has an **autobiography of use** — a first-person account
generated from the machine's real history: setup date, battery cycles, shell
history, agent sessions, chosen Menu Band voice. The autobiographies are the
compositional material; the trio plays who the machines have actually been.

## Members

| member | born | cycles | sessions | voice |
|---|---|---|---|---|
| `neo` | 2026-04-24 00:37 | 212 | 1,121 | whistle (GM 78) |
| `blueberry` | 2026-06-11 00:43 | 85 | 925 | whistle (GM 78) |
| `third` | not yet | — | — | — |

Both living members independently settled on whistle and tuned to NTS 1.

Color proposal (from the broadside's "indigo, citrus, blush — Neapolitan, in a
block"): blueberry = indigo, neo = citrus, third = blush.

## Service records (who was used more)

| | `neo` | `blueberry` |
|---|---|---|
| records begin | Apr 19 (5 days pre-setup) | May 28 (14 days pre-setup) |
| console / tty logins | 119 / 6,305 | 69 / 692 |
| reboots / shutdowns | 111 / 49 | 64 / 34 |
| longest unbroken stretch | 5.9 days | **13.1 days** |
| battery cycles per day | ≈1.6 | 1.0, metronomic |
| disk | 211 GB used, 98% full | 437 GB used, 94% |
| sessions hosted | **1,122** (859 claude + 262 codex) | 925 (all claude) |
| busiest month | May, 298 | **July, 514** — fleet record |
| top lifetime process | WindowServer, then coreaudiod | mediaanalysisd, then photolibraryd |
| MenuBand CPU (this boot) | — | 2.4 h |
| prox rocks right now | 9 | 1 |

Neo is typed at, carried, and rebooted; blueberry holds still and works in
seasons. Neo runs warm (load 4–6), blueberry cool (≈1.5). Both were working
before their official setup dates.

## Files

- `members/<name>/facts.json` — raw harvested identity, refreshable
- `members/<name>/profile.json` — raw service record (logins, uptimes,
  workload, live prox rocks), refreshable
- `members/<name>/journey.json` — the commit journey: the agent-memory
  post-commit ledger (neo), or the clone's reflog where the ledger hook was
  never wired (blueberry, ~90-day horizon; the hook is wired as of Sep 5)
- `members/<name>/deep.json` — second-layer record: macOS lineage, sleep/wake
  + Menu Band wake-holds, paired radio and audio devices, inventories,
  ssh/Wi-Fi acquaintances (counts only for networks), footprints
- `members/<name>/autobiography.md` — the first-person account
- `members/<name>/profile.md` — the readable service record
- `members/<name>/journey.md` — the readable journey (lanes, storms, what
  each has been through and whom each knows)
- `bin/harvest.sh [host]` / `bin/profile.sh [host]` / `bin/journey.sh [host]`
  / `bin/deepen.sh [host]` — the harvesters; local by default, over ssh with
  a hostname. Run all four for `third` on its first day.
- `book/the-macneopolitan-trio.tex` + `bin/book.sh` — the band book PDF (one
  autobiography page + one journey page per living member)
- `members/<name>/voice.json` — the aesthetivox voice identity: cast macOS
  voice (neo=Fred, blueberry=Kathy, third=Junior) + singing parameters
  derived from the biography (vibrato = 4x battery cycles/day, breath =
  longest uptime). Each member always renders its own text on its own
  hardware.
- `members/<name>/autobiography.m4a` — the voiceover, spoken by the member
  in its cast voice (`bin/speak.sh <name>`)
- `members/<name>/sung-proof.m4a` — the singing proof: a spoken line lifted
  onto a melody by WORLD f0 replacement (`bin/sing-proof.py`), envelope
  untouched so it stays the member's voice. Record-grade singing goes
  through the spinging chain; realtime on stage via `live/` singer.c
  (Menu Band adoption open). Native `say` TUNE/PHON singing is dead on
  macOS 26 (verified) — WORLD is the pitch handle.

Refresh before the show so the numbers on stage are true that night.

## From data to score (open, Week 1)

The mapping from autobiography to `.mbscore` voices is a residency decision,
not settled here. Seeds: birth minutes (:37, :43) as phrase lengths; battery
cycles (212 / 85) as pulse counts per member; the shared whistle as the trio's
common voice (`whistle-hocket.mbscore` is the precedent in the library); the
autobiographies spoken aloud by each machine between movements.
