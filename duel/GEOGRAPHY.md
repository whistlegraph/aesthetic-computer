# dool geography

Dool is a public park with personal pods. Guests can walk its paths and visit
pods. Each pod has a permanent ID and content room; its placement in the park
can change. The first two plots remain where they were when upgraded.

## Pods and placements

`geography.json` version 2 stores three independent records:

- `pods[id]`: handle, founding date and, after verified sign-in, the private
  account ID. A handle change preserves the pod ID. Public responses omit the
  account ID and expose only the handle and its public profile URL.
- `placements[id]`: block and lot. A curator can move multiple pods in one
  transaction, including swaps. Identity and contents do not move between stores.
- `rests[handle]`: where an avatar stopped. A rest inside a pod uses the pod ID;
  a rest in the park remains at its geographic position.

The object store uses the permanent `pod-…` room key for each interior, including
objects, screens, paint and history. Existing builds in the plaza, residential
blocks and other rooms stay there; migration does not guess which pod owns them.
The first load of a v1 ledger preserves `geography.v1.json` before writing v2.
Rollback to a server that only understands v1 requires restoring that backup;
export later placement changes first.

A pod's exterior occupies the existing 3.5 × 7 m lot. An open frame carries an
outside tag with the owner's handle and saved profile colors (default colors
while the profile service is unavailable). Walk through the tagged entrance or
choose a pod in the **go** menu to visit. Interior rooms currently use the
existing 16 × 16 m simulation. The **park** menu action returns visitors to the
pod's current entrance, even if it was moved during the visit.

Pod interiors are public to visit. Only their verified owner may build, restore,
paint, remove, or change screens there. Shared park rooms retain their existing
public building behavior. Account IDs are never sent to visitors.

## Curation

`GET /dool/geography` includes the layout revision, pod IDs and placements.
`GET /dool/pod?id=pod-…` resolves a permanent pod address. Share
`/dool/?pod=pod-…` to visit the same interior after a rearrangement.

`POST /dool/park/layout` requires an AC admin bearer token and a JSON body:

```json
{
  "revision": 2,
  "moves": [
    {"pod": "pod-example-a", "block": 1, "lot": 6},
    {"pod": "pod-example-b", "block": 1, "lot": 7}
  ]
}
```

Replace the example IDs and revision with values from the current map. The whole
plan must fit open, unoccupied lots; lots vacated by this same plan may be used.
A stale revision or invalid plan changes nothing. Moving friends together means
including their pods in the same plan. Connected clients receive fresh tags,
home locations and exit destinations after the write completes. The operational
read-only service token does not grant curation access.

## Park travel and future shards

Open blocks connect at the four path ends. The client requests a crossing; the
server checks its own player position and issues a short-lived, one-use passage
bound to that player's stored identity. The next room restores the landing
position and facing. Client-provided coordinates cannot choose the landing.
Walking currently reconnects between block simulations, so transitions can have
a short loading pause; neighboring blocks are not drawn through the boundary yet.

The simulation keys are separate from placement: a park block uses its lattice
address, and a pod interior uses its permanent ID. Those are the units a future
shard router can assign to hosts without moving homes. **This implementation
still runs on one server.** Cross-host routing, capacity handoff and shared
storage coordination are not implemented; changing a map is not a shard move.

The lattice rules below still govern park growth. Tests cover migration,
identity, curation, ownership and travel in `test/pods.test.mjs`; the browser
check in `test/pods-browser.mjs` renders tags and walks between two blocks.

## Who the world is for (measured 2026-11-13)

| | |
|---|---|
| Handles registered | 3,076 |
| Handles signed in over the last 30 days | 118 (38 with five or more page loads) |
| Visits to aesthetic.computer a day | 100 to 650 |
| Visits that entered dool a day, since counting began 11-06 | 2 to 15 |
| People on the whole network at one moment | 3 to 6 |

Two facts decide the design. There are a thousand times more handles than people
present at any moment, so a world sized for the handles would be empty if only the
people present were drawn. And nearly everyone who comes is alone when they arrive.
The world therefore has to show who lives there, not only who is online.

## What earlier worlds learned

- **Ultima Online (1997).** Houses were placed on the open map and the land ran out.
  A March 1998 report describes builders rushing to every clearing. The remedies were
  decay (a house unattended for 90 days crumbles and its land goes to the first
  claimant) and new land: Trammel in 2000 mirrored the whole map, and Malas later added
  room for 1,500 homes. Every addition set off another land rush. *Fixed land plus free
  claims makes scarcity; scarcity makes the claim matter more than the place.*
- **AlphaWorld (Active Worlds, 1995).** One flat plane addressed in metres from Ground
  Zero at 0,0, free to claim by building, with teleport to any coordinate. Satellite
  maps from 1996, 1998 and 2001 show a city dense at the origin and thinning outward.
  *Unlimited land plus free travel spreads people until nobody has a neighbour.*
- **LambdaMOO (1990).** Rooms joined by exits anyone could dig, limited by a building
  quota that an Architecture Review Board was created to administer. *A graph of rooms
  has no distance and cannot be mapped; a quota without a rule becomes a committee.*
- **Second Life (2003).** A grid of 256 m regions, each run by its own simulator, with
  one coordinate system across the mainland. *A lattice of independent authorities can
  still be one continuous place.*
- **EVE Online (2003).** One universe for all players: over 5,000 systems at first and
  about 2,500 more added in 2009, never reset. *One shard gives one history. But at our
  size a universe of mostly empty systems would hide everyone.*

## Method

1. **One world, one authority per block.** A block is a 16 m square: the room the server
   runs today, with its sixteen seats, its builds and its chalk. Nothing about the
   netcode changes to begin.
2. **A fixed lattice.** Blocks sit on an integer grid. The lobby is block 0 at the
   origin. Every point in the world is a number of metres east and north of the lobby's
   middle, and that block address is permanent. Pods can be placed in different lots.
3. **Growth in one order.** Blocks open one at a time along a square spiral out of the
   lobby. Block *n* has the same coordinate forever, whether or not it is open yet, so
   the geometry is fixed and only its extent grows.
4. **Density decides growth.** The next block opens when three quarters of the open lots
   are settled. Nobody chooses to open land and nobody can spread the world thin.
5. **One pod per account.** A residential block has eight lots of 3.5 × 7 m around two
   lanes that cross on its axes. Lanes meet the neighbours' lanes at the edges, so the
   lattice is also a street grid. A handle receives a pod on a free lot. Curators may rearrange pods within open blocks. A home is never taken back: there is always room, so
   there is no need for decay.
6. **The avatar stays.** Leaving saves the avatar's block and position. It wakes there,
   and while its person is away it is drawn where it rests. Eight residents and their
   resting avatars are the population a lone visitor meets.
7. **Guests visit.** Without a handle there is no home and nothing is kept. Guests
   arrive in the lobby. A handle is settled on the first free lot when it first arrives.
8. **Three ranges of sight.** In your block you see everything at full rate. In the eight
   blocks around it you see avatars and builds at a low rate through open edges. Across
   the world you see the map: every block, who lives in it and who is awake now.

## How big it gets

Eight lots a block, opened at three quarters full, crossed at the avatar's 10 m/s run:

| Handles settled | Blocks open | World across | Time to cross |
|---|---|---|---|
| 8 | 2 | 48 m | 5 s |
| 100 | 17 | 80 m | 8 s |
| 1,000 | 167 | 208 m | 21 s |
| 3,076 (every handle today) | 513 | 368 m | 37 s |
| 30,000 | 5,001 | 1,136 m | 2 min |
| 1,000,000 | 166,667 | 6.5 km | 11 min |

If every handle registered today settled, anyone could reach anyone in under 40 seconds.
Past about 30,000 settlers walking stops being enough and the world will need transit;
that is a later problem and the lattice leaves room for it.

## Open decisions

- **Lot size.** Eight lots of 3.5 × 7 m is a guess made before measuring what people
  build. Four larger lots a block doubles the world's width.
- **Travel by name.** Walking keeps distance meaningful. Typing `@handle` to arrive at
  someone's home is the Aesthetic Computer habit and would erase it. Proposed: recall
  to your own home and to the lobby only.
- **Park building boundaries.** Pod interiors enforce owner-only editing. Shared park
  builds still use the existing room permissions; exterior lot permissions need design.
- **Handle changes.** Verified account IDs now retain pod identity across handle changes.
  Pre-existing pods bind their account on the next verified sign-in; conflicting claims fail.

## Build order

1. Lattice, spiral, lots, ledger and growth rule (`geography.mjs`). **Done.**
2. Server: the ledger on disk beside the object store; `GET /dool/geography` for the
   map (blocks, homes, lot rectangles, who is awake, who rests where); a handle is
   settled the first time it arrives; the avatar's rest is saved on leave and used on
   join in the same room. **Done.**
3. The `map` piece: the world from above, as data.
4. Client: settling a chosen lot; crossing a block's edge moves to the neighbour's room
   carrying position; resting avatars drawn in the room.
5. Sight into the eight surrounding blocks.
6. Smaller snapshots and interest, from "Toward unidool" in the README.

## Sources

- [Lessons from the history of MMO housing (Engadget, 2014)](https://www.engadget.com/2014/09/19/working-as-intended-lessons-from-the-history-of-mmo-housing/)
- [Houses in the wild becoming a problem (UO Stratics, 1998)](https://uo.stratics.com/secrets/tc/tc_042.shtml)
- [Ultima Online demolishing old houses (GameIndustry)](https://www.gameindustry.com/?p=13750)
- [Ultima Online (Wikipedia)](https://www.wikipedia.org/wiki/Ultima_Online)
- [Atlas of Cyberspace: MUDs and virtual worlds, AlphaWorld satellite maps (Dodge)](https://personalpages.manchester.ac.uk/staff/m.dodge/cybergeography/atlas/muds_vw.html)
- [Crone and Schroeder on Active Worlds coordinates (2001)](https://www.dhi.ac.uk/san/waysofbeing/data/data-crone-schroeder-2001.pdf)
- [LambdaMOO help: the Architecture Review Board](https://sites.cc.gatech.edu/classes/cs8113e_99_winter/LTAND.html)
- [EVE Online Apocrypha hands-on (Engadget, 2009)](https://www.engadget.com/2009-03-06-massivelys-apocrypha-expansion-hands-on-true-exploration.html)
