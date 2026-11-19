# rax world state in Datomic — schema proposals

Status: proposal, 2026-11-18. Nothing here is built. The walk log
(`rax/walks.mjs`) is separate and stays on disk either way.
Drawn against the two-seat rax; see "Still open" for what the tower adds.

## What rax keeps today

One file, `objects.json`, rewritten whole on every change:

- `owners` — sha256(identity token) → owner uuid
- `rooms[room]` — objects: id, owner, x, z, item, effect, versions[{version, code, geometry}]
- `history[room]` — the last 64 published builds
- `screens[room]`, `walls[room]`, `wallRes[room]`, `tiles[room]`

It cannot answer "what did this room look like yesterday", a removed object
is gone, and a rewrite of the whole file per paint stroke will not survive
many rooms.

## Three shapes

**A. Mirror.** Rooms and objects as entities holding current facts. Datomic's
own history answers every "as of" question. Smallest. Loses *who* did each
thing unless every entity carries an author field.

**B. Event log.** Every action is an immutable event entity; state is a fold.
Familiar, but it rebuilds inside Datomic what Datomic already is, doubles the
datoms, and makes "the room now" a query over all time.

**C. Mirror + annotated transactions (recommended).** A, plus every
transaction carries who and why as facts on the transaction entity itself, and
anything large lives outside as a content-addressed blob. State reads are
plain current-value lookups; history, authorship and replay come from the log.

## C in full

Same database as kidlisp, so refs to `:user/sub` and `:kidlisp/code` work.
All attributes namespaced `rax.*`.

```clojure
;; who — one entity per identity, guest or signed in
:rax.actor/owner     uuid    unique identity   ; server-issued, as today
:rax.actor/token     string  unique value      ; sha256 of the identity token
:rax.actor/user      ref → :user/sub entity    ; a signed-in actor; guests have none
:rax.actor/name      string                    ; guest two-word name

;; rooms
:rax.room/key        string  unique identity   ; "lobby", "studio"
:rax.room/tiles      string                    ; 324 chars, one palette digit per square
:rax.room/walls      ref many, component → wall

:rax.wall/room+face  tuple [ref, keyword] unique identity
:rax.wall/code       ref → kidlisp
:rax.wall/res        long

;; things standing in a room: objects, items, effects, screens
:rax.thing/id        uuid    unique identity
:rax.thing/room      ref → room, indexed       ; retracted when removed
:rax.thing/kind      keyword                   ; :object :item :effect :screen
:rax.thing/owner     ref → actor
:rax.thing/x, /z, /yaw   double
:rax.thing/version   long
:rax.thing/code      ref → kidlisp             ; the $code of this version
:rax.thing/geometry  string                    ; sha256 of the geometry blob
:rax.thing/item, /effect   string

;; visits
:rax.visit/id        uuid    unique identity
:rax.visit/room      ref → room
:rax.visit/actor     ref → actor
:rax.visit/entered, /left   instant
:rax.visit/seated    boolean
:rax.visit/distance  double
:rax.visit/walk      string                    ; "2026-11-18#392ed3a4" into the motion index

;; on every transaction
:rax.tx/actor        ref → actor
:rax.tx/visit        ref → visit
:rax.tx/cause        keyword                   ; :build :revise :remove :paint :restore :clear :wall
:rax.tx/job          uuid                      ; the build job, to join diagnostics
```

### Why this scales

- **No blobs in datoms.** Geometry is written once to
  `<state>/geometry/<sha256>.json` (later Spaces); the datom holds the hash.
  Versions that reuse a shape cost nothing.
- **No version list.** A thing has one current version. Earlier versions are
  the same attributes read `as-of` an earlier transaction, so a thing never
  grows.
- **Removal is retraction of `:rax.thing/room`.** The thing stays, with its
  history, and "put back" is asserting the room again. Replaces the 64-entry
  history list with a query that has no cap.
- **Tiles are one string per room.** A stroke is one datom, not 324 entities,
  and who painted it is on the transaction.
- **Human-rate writes only.** Builds, paints, joins. Never positions. One
  transactor is comfortable into millions of such transactions.
- **Rooms are unbounded.** `:rax.thing/room` is indexed, so reading a room is
  one index range regardless of how many rooms exist.

### What it does not hold

Positions, rockets, chalk strokes in flight, braincell counts. Those are
either telemetry (walk log) or live match state (memory).

## Path

1. Sidecar: `ac.rax.schema`, and four routes — transact a change, read a
   room, read a room as-of, list visits.
2. Dual-write behind `RAX_DATOMIC=on`, as kidlisp did. `objects.json` stays
   the read path; a failed sidecar write never blocks the room.
3. Backfill `objects.json` once.
4. Flip reads: the server loads a room from Datomic when its first visitor
   arrives and keeps it in memory, instead of holding every room forever.
5. Retire `objects.json`.

## Decided (@jeffrey, 2026-11-18)

- **Same database as kidlisp.** Things reference `$codes` and users directly.
- **A guest's things stay the guest's.** Signing in later does not move
  earlier builds to the handle. `:rax.actor/user` is set for the sessions
  after sign-in only; nothing is re-attributed.
- **Datomic holds human-rate world facts only.** Builds, paints, joins.
- **Gesture is kept, in its own index.** Poses and motion are not world
  facts, but they are not thrown away: `rax/walks.mjs` keeps every simulated
  tick (the rate input is applied at) as delta-coded, deflated frames, one
  file a day. `:rax.visit/walk` points into it.

## Still open

1. Geometry blobs: lith's disk, or Spaces from the start?
2. Is `as-of` replay of a room a product feature (a time slider in rax) or an
   operator tool? It changes whether step 4's read route is public.
3. How long is gesture kept? 35 days today, to match the visit analytics.
4. The schema above was drawn against the two-seat room. The tower, floors
   and geography (GEOGRAPHY.md) need the same treatment before step 1.
