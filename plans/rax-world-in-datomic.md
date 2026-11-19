# rax world state in Datomic — schema, second draft

Status: proposal, redrawn 2026-11-18 against the tower, the yard and the
rename to rax. Nothing here is built. Motion stays out of Datomic by decision
(`rax/walks.mjs`); this is for facts.

## What rax keeps today

Three ledgers on lith under `~/.local/share/rax/`, each rewritten whole on
every change, and one log:

| File | Holds |
| --- | --- |
| `objects.json` | `owners` (sha256 of identity token → owner uuid); per room: objects with versions, removed objects, `$code` history (64), screens, walls, wall resolution, painted tiles |
| `tower.json` | `floors[id] = { id, handle, number, at, held, accountId }` — a floor's room is `floor-<id>`, its number is an address that can change hands |
| `geography.json` | `rests[handle] = { room, x, z, yaw, at }`, `opened` housing blocks, the retired houses (`pods`, `placements`) |
| `walks/<day>.jsonl`, `.motion` | visits and per-tick motion, 35 days |

None of them can say what a room looked like yesterday, who moved a thing, or
which number a floor had last week. Three files that must agree are changed
by three separate writes.

## Decided (@jeffrey, 2026-11-18)

- **Same database as kidlisp.** Things reference `$codes` and users directly.
- **A guest's things stay the guest's.** Signing in later does not move
  earlier builds to the handle.
- **Datomic holds human-rate world facts only.** Builds, paints, joins,
  floors changing hands.
- **Gesture is kept, in its own index.** Every simulated tick, delta-coded
  and deflated, one file a day. A visit points into it.
- **The name is rax**, and it is infrastructure: no public destination.

## Shape: current facts, annotated transactions, blobs outside

Entities hold what is true now. Datomic's history answers "as of". Every
transaction says who did it and why, on the transaction itself. Anything
large is stored once by hash outside the database. (The first draft compared
this with a plain mirror and with an event log, and chose it.)

```clojure
;; ── who ────────────────────────────────────────────────────────────────
:rax.actor/owner      uuid    unique identity   ; server-issued, as today
:rax.actor/token      string  unique value      ; sha256 of the identity token
:rax.actor/user       ref → :user/sub entity    ; a signed-in actor; a guest has none
:rax.actor/name       string                    ; a guest's two-word name

;; ── where: one kind of entity for every place a body can stand ─────────
:rax.place/room       string  unique identity   ; "lobby", "w2s1", "floor-1a2b3c4d", "botbop-x"
:rax.place/kind       keyword                   ; :plaza :yard :block :floor :instance
:rax.place/cell       tuple [long long]         ; lattice i, j — plaza, yard and blocks
:rax.place/tiles      string                    ; one palette digit per square
:rax.place/walls      ref many, component → wall

:rax.wall/place+face  tuple [ref keyword] unique identity
:rax.wall/code        ref → kidlisp
:rax.wall/res         long

;; ── the tower: a floor is a place with a holder and a number ───────────
:rax.floor/id         string  unique identity   ; the 8 hex of its room, permanent
:rax.floor/place      ref → place, unique
:rax.floor/holder     ref → :user/sub entity    ; the handle is read from the user
:rax.floor/number     long    unique value      ; the address; retracted and re-asserted on a trade
:rax.floor/held       boolean                   ; true once arrived or asked for by name
:rax.floor/since      instant

;; ── things standing in a place: builds, items, effects, screens ────────
:rax.thing/id         uuid    unique identity
:rax.thing/place      ref → place, indexed      ; retracted when taken out
:rax.thing/kind       keyword                   ; :object :item :effect :screen
:rax.thing/owner      ref → actor
:rax.thing/x, /z, /yaw   double
:rax.thing/version    long
:rax.thing/code       ref → kidlisp             ; this version's $code
:rax.thing/geometry   string                    ; sha256 of the geometry blob
:rax.thing/item, /effect   string

;; ── where a handle rests between visits ────────────────────────────────
:rax.rest/user        ref → :user/sub entity, unique identity
:rax.rest/place       ref → place
:rax.rest/x, /z, /yaw double                    ; :db/noHistory — see below

;; ── visits ─────────────────────────────────────────────────────────────
:rax.visit/id         string  unique identity   ; the walk log's 8 hex
:rax.visit/actor      ref → actor
:rax.visit/place      ref → place
:rax.visit/entered, /left   instant
:rax.visit/seated     boolean
:rax.visit/distance   double
:rax.visit/walk       string                    ; "2026-11-18#392ed3a4" into the motion index

;; ── on every transaction ───────────────────────────────────────────────
:rax.tx/actor         ref → actor
:rax.tx/visit         ref → visit
:rax.tx/cause         keyword   ; :build :revise :move :remove :restore :paint :wall :clear
                                ; :arrive :take :trade :admit :keeper
:rax.tx/job           uuid      ; the build job, to join diagnostics
```

### What the tower changes from the first draft

- **A place is one entity kind.** Plaza, yard cell, housing block, floor and
  game instance differ only in `:rax.place/kind`. Things, visits and rests
  point at a place and never care which kind. The yard growing from nine cells
  to forty-six on 2026-11-18 would have been no schema change.
- **A floor's number is a fact with history.** `take 89` and a trade are each
  one transaction that retracts and asserts `:rax.floor/number` on two floors,
  so they can never half-happen, which two JSON writes can. "Who held 89 on
  the first of the month" is an `as-of` read.
- **Uniqueness does the tower's rules.** `:rax.floor/number` is unique, so the
  database refuses two holders of one number. Today `tower.mjs` enforces that
  in memory.
- **Yard cells need no rows until something happens there.** A place entity is
  created the first time a thing, a tile or a visit refers to it. The lattice
  itself stays in code (`park.mjs`, `geography.mjs`).
- **The retired houses are not carried over.** `pods` and `placements` stay in
  `geography.before-houses.json` on disk as the record.

### Why this scales

- **No blobs in datoms.** Geometry is written once to
  `<state>/geometry/<sha256>.json`; the datom holds the hash.
- **No version lists.** A thing has one current version; earlier ones are the
  same attributes read at an earlier time.
- **Removal is retraction of `:rax.thing/place`.** The thing keeps its
  history, "put back" is one assertion, and the 64-entry history cap goes.
- **Tiles are one string per place.** A stroke is one datom.
- **Rests carry no history.** A rest is rewritten every time a handle leaves.
  With `:db/noHistory` it stays one datom per attribute per handle however
  often people come and go. Where someone was at a past time is the walk
  log's job.
- **Visits are the one growing set.** Two transactions each, enter and leave.
  At a thousand visits a day that is under a million transactions a year,
  well inside one transactor. If it ever is not, visits move to their own
  database; nothing else refers to them.
- **Reading a place is one index range** over `:rax.thing/place`, however
  many places exist. A server loads a place when its first visitor arrives
  and drops it when the last leaves, instead of holding every room forever.

### What it does not hold

Positions, rounds, chalk in flight, Braincell counts, the lattice. Those are
motion (walk log), live match state (memory) or code.

## Path

1. Sidecar: `ac.rax.schema`, and routes to transact a change, read a place,
   read a place as-of, read the tower, list visits.
2. Dual-write behind `RAX_DATOMIC=on`, as kidlisp did. The JSON ledgers stay
   the read path; a failed sidecar write never blocks a room.
3. Backfill `objects.json`, `tower.json` and the rests once.
4. Flip reads, one ledger at a time: tower first (smallest, and it gains the
   most from uniqueness), then things, then rests.
5. Retire the ledgers.

## Still open

1. Geometry blobs: lith's disk, or Spaces from the start?
2. Is `as-of` replay of a place a feature (a time slider) or an operator
   tool? It decides whether the read route is public.
3. How long is gesture kept? 35 days today, to match the visit analytics.
4. Should a visit be recorded for watchers too, or only for seated handles?
   Guests only watch in production, so today most visits have no path.
