# rax geography

> **2026-11-17: the houses are gone.** A home is a floor of the tower (`tower.mjs`). When the
> server starts it retires every house still on a lot (`Geography.retire`), moves what stood inside
> to its handle's floor (`ObjectStore.moveRooms`) and keeps both ledgers as they stood
> (`geography.before-houses.json`, `objects.before-houses.json`). Blocks are open field. The
> house drawing, the door messages (`enter-pod`, `exit-pod`), `GET /rax/pod`,
> `POST /rax/park/layout` and `test/pods-browser.mjs` are removed; what follows about lots,
> pods and curation describes the old ledger, which `Geography` can still read.


Rax is a public park with personal pods. Guests can walk its paths and visit
pods. Each pod has a permanent ID and content room; its placement in the park
can change. The first two plots remain where they were when upgraded.

## AC Tower (2026-11-16)

Home is a floor of the tower, not a lot. The tower stands on the plaza and has
one floor for every handle on the network (`tower.mjs`, ledger `tower.json`).
Floors are given out oldest handle first, read from
`/api/handles?order=created` at start and every ten minutes after.

- **A number is an address, like a domain name.** One holder at a time.
  `take 89` in the elevator makes 89 yours if it is free. If it was only
  pencilled in for a handle that has never come to rax, you trade numbers
  with them. Once a handle has arrived or has taken a number, it is theirs.
- **A floor's room follows its permanent id** (`floor-xxxxxxxx`), never its
  number, so what is built there moves with its owner.
- **The elevator** is a glass car on the north edge of the plaza and of every
  floor (`elevator.mjs`), open to the south. Stand in it and press Enter: a number,
  a handle, `home`, `g` for the ground, `top` for the roof, or the up and down
  arrows for the next floor. The server checks that the rider stands in the car.
- **The roof** (2026-11-20) is a room of its own, `roof`, on the slab over the
  highest floor: open air, a parapet round its edge, the car as on every floor.
  It is nobody's floor and is kept as it is (only the keeper arranges it). Up
  from the highest floor is the roof and down from the roof is that floor. In
  a ride's sums, in the `tower` message and on the map its number is one over
  the highest floor (`roofNumber`), with `roof: true`.
- **Nobody stands on air.** Whoever is seen on the tower from outside has the
  slab of their storey drawn under them, and the car if they stand in the
  shaft, on a storey that many numbers share as on one with a number of its
  own (`standsOn` in `elevator.mjs`).
- **Kiosks** (the pieces on panels: two `bleep` pads on the lobby's front, the
  song hall inside it east of the door, the notepat board in the yard) stand
  where `kiosks-plan.mjs` works them out from the tower's measurements. The
  page mounts them from it and the server takes a press only on one of them.
- **A floor is public to visit; only its handle arranges it.**
- **One world, one address.** The plaza, the park and every floor are all
  `aesthetic.computer/rax`. Entering rax sets a handle down where it last
  stood (the server keeps each avatar's rest; a first visit starts on the
  plaza at the foot of the tower). Only instances, such as a game's arena,
  have addresses of their own. `GET /rax/tower` is the directory and
  `GET /rax/tower?q=89` one floor.
- **Real heights.** The tower stands over the plaza on four legs; the first
  floor is 12 m up and each floor 7 m above the last (a build can stand 6 m). A ride travels that
  distance in a glass car on the north edge, and the car's screen over the
  door names each floor as it passes and is where the floor is typed.

Houses already standing on the park's lots stay; arriving no longer settles a
new one, so `/rax/geography` (the `map` piece) lists only the homes from
before the tower.

## The map

`GET /rax/map` (`map.mjs`) is the world as a drawing: every block, lot and
house, the tower, what stands in the rooms and who is there now, as plain
shapes in world metres (`flat`, `box`, `stack`, `panel`, each with a `role`).
Anything that shows rax from outside draws from it and keeps no geometry of
its own; the desktop wallpaper's model does. The shapes are worked out from
the same measurements the server and the page use, and `test/map.test.mjs`
fails when a solid is missing. Something new that stands in the world gets
its shapes in `map.mjs`.

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

A home is a house on a 7 × 7 m lot, one lot to each quadrant of a block
(2026-11-14; before that a quadrant held two 3.5 × 7 m lots under open frames).
The house is 5.2 m square with 3 m walls and no roof, set 0.6 m back from the
lanes and 1.2 m from the block's edge, with a 1.6 m doorway onto the east-west
lane. Its walls are solid on the server and in the client's prediction
(`park.mjs` `houseWalls`). A ledger written with eight lots is set down again
on the four in founding order when it is first read. The wall over the door carries an
outside tag with the owner's handle and saved profile colors (default colors
while the profile service is unavailable). A home is its lot (2026-11-14):
walking through the frame is only walking, so a visitor passes through every
home on a block without leaving it. The client no longer enters a separate
interior room; `?pod=pod-…` links still open the old interior, and the
`enter-pod` and `exit-pod` messages remain on the server for them.

Pod interiors are public to visit. Only their verified owner may build, restore,
paint, remove, or change screens there. Shared park rooms retain their existing
public building behavior. Account IDs are never sent to visitors.

## Curation

`GET /rax/geography` includes the layout revision, pod IDs and placements.
`GET /rax/pod?id=pod-…` resolves a permanent pod address. Share
`/rax/?pod=pod-…` to visit the same interior after a rearrangement.

`POST /rax/park/layout` requires an AC admin bearer token and a JSON body:

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

Open blocks connect along their whole shared side, the plaza included (it is
block 0 and its walls stand only where no block is open). The client requests a crossing; the
server checks its own player position and issues a short-lived, one-use passage
bound to that player's stored identity. The next room restores the landing
position and facing. Client-provided coordinates cannot choose the landing.
The arrival is set down 7.3 m from the middle, clear of the edge it came through.

`GET /rax/world?room=<block>` answers that block and every open block around
it — neighbours, lots, standing objects and painted tiles. The client draws
them in place, asks again every six seconds, keeps the last answers in
`localStorage` (`rax-world`) so the neighbourhood is there before the socket
opens, and dresses the next block from it when a crossing begins. Walking still
reconnects between block simulations: people, chalk and screens of a
neighbouring block are not seen across the join, and the crossing is a short
step. One simulation for the whole world is the next stage.

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
| Visits that entered rax a day, since counting began 11-06 | 2 to 15 |
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
5. **One pod per account.** A residential block has four lots of 7 × 7 m around two
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

- **Lot size.** Four lots of 7 × 7 m (eight of 3.5 × 7 m until 2026-11-14) is a guess made before measuring what people
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
2. Server: the ledger on disk beside the object store; `GET /rax/geography` for the
   map (blocks, homes, lot rectangles, who is awake, who rests where); a handle is
   settled the first time it arrives; the avatar's rest is saved on leave and used on
   join in the same room. **Done.**
3. The `map` piece: the world from above, as data.
4. Client: settling a chosen lot; crossing a block's edge moves to the neighbour's room
   carrying position; resting avatars drawn in the room.
5. Sight into the eight surrounding blocks.
6. Smaller snapshots and interest, from "Toward unirax" in the README.

## Sources

- [Lessons from the history of MMO housing (Engadget, 2014)](https://www.engadget.com/2014/09/19/working-as-intended-lessons-from-the-history-of-mmo-housing/)
- [Houses in the wild becoming a problem (UO Stratics, 1998)](https://uo.stratics.com/secrets/tc/tc_042.shtml)
- [Ultima Online demolishing old houses (GameIndustry)](https://www.gameindustry.com/?p=13750)
- [Ultima Online (Wikipedia)](https://www.wikipedia.org/wiki/Ultima_Online)
- [Atlas of Cyberspace: MUDs and virtual worlds, AlphaWorld satellite maps (Dodge)](https://personalpages.manchester.ac.uk/staff/m.dodge/cybergeography/atlas/muds_vw.html)
- [Crone and Schroeder on Active Worlds coordinates (2001)](https://www.dhi.ac.uk/san/waysofbeing/data/data-crone-schroeder-2001.pdf)
- [LambdaMOO help: the Architecture Review Board](https://sites.cc.gatech.edu/classes/cs8113e_99_winter/LTAND.html)
- [EVE Online Apocrypha hands-on (Engadget, 2009)](https://www.engadget.com/2009-03-06-massivelys-apocrypha-expansion-hands-on-true-exploration.html)
