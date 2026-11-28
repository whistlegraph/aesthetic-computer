# rax geometry (proposal, 2026-11-27)

**Status: a proposal.** One step is live (2026-11-27): home is `origin`, an
open platform (`world.mjs` `HOME_ROOM`), and the old plaza is its own room at
`rax.ac/~plaza`. Nothing else here is built. It replaces the world described
in `GEOGRAPHY.md` (plaza, park blocks, tower, basement, planets), which is
deprecated and kept as one room to visit. @jeffrey asked for one universal
geometry: Cartesian on three axes, fractal, not built from planets, and like
AC's own 2D world.

## The rule

Space is a lattice of cubes. A place is four whole numbers:

```
level : x , y , z
```

- **x, y, z** count cubes east, up and north. They run without end in both
  directions. `0,0,0` is the origin.
- **A cube is 16 m on each edge at level 0.** This is today's block
  (`geography.mjs` `BLOCK`), so one cube is one room, and one room is one
  authority (the "cell" of README "Toward unirax").
- **level** is the scale. A cube at level `n + 1` is 4 cubes of level `n` on
  each edge (64 inside it). Going down a level is going into a cube: it
  becomes a lattice of 64 smaller cubes. Going up a level is stepping back:
  the whole neighbourhood becomes one cube among others.

That is the whole geometry. The same rule holds at every level, which is what
makes it fractal: no level is the top and none is the bottom.

## What it replaces

| Today | In the new geometry |
| --- | --- |
| The plaza (`lobby`) | The origin cube, `0:0,0,0`, an open platform |
| Park blocks `e1n0`, `w2s3` | Cubes on the ground plane: `0:1,0,0`, `0:-2,0,-3` |
| Tower floors | Cubes stacked on y: floor 3 of the origin column is `0:0,3,0` |
| The basement | `0:0,-1,0` |
| The planets | Gone. Height is the y axis, reached like any other direction |
| A room made by name (`~studio`) | Stays as it is: a room outside the lattice |

A handle's home becomes a cube it holds, as a floor number is held today.

## How it reads on the page

- **Address.** AC's own form: `rax.ac/~1,0,2` is a cube at level 0, and
  `rax.ac/~2:0,0,0` names a level. The bare address is still wherever you
  last stood.
- **HUD.** The line under the braincells shows `level : x,y,z` of the cube
  and your metres inside it.
- **In words.** `rax.state()` gives the same four numbers, and `rax.walkTo`
  takes a face (`east`, `up`, `north` and their opposites).

## Moving

- **Across a face.** Walking off an edge enters the next cube, as crossing a
  block line does now. Up and down use the same handover on y.
- **Between levels.** To be decided (see below). The body keeps its own size;
  the world's size around it changes.

## Order of work

1. **The lattice at level 0, on the ground plane.** Cube keys in place of
   block names; the origin is an open platform; neighbours on four faces.
   This reuses the block handover that exists.
2. **The y axis.** Neighbours above and below; a way to climb (stair, lift or
   flight).
3. **Levels.** Going into a cube and back out.
4. **The old world as one room.** `~plaza` opens the plaza as it stands
   today, with its tower lobby and kiosks, and nothing leads out of it.
5. **Homes and what was built.** Move each handle's floor to a cube.

Steps 1 and 4 can ship first, behind the `ONE_ROOM` switch that is on today.

## To decide

1. **What does going down a level mean?** (a) You shrink into the cube, and
   the 16 m room becomes a 64-cube neighbourhood of rooms, each 16 m to you.
   (b) Levels are only a map zoom: the lattice is one size, and a level is how
   much of it one address covers.
2. **How many cubes per edge between levels?** 4 is proposed. 2 gives an
   octree (8 inside); 3 keeps a centre cube.
3. **How do you climb y?** Stairs in every cube, a lift like the tower's, or
   flight (the bike already flies).
4. **Is an empty cube solid ground or open air?** That is, is there a floor
   at every y, or only where someone builds one?
