# Whistlegraph piece API census

Read off the knot on 2026-10-10: 52 threads by 4 handles, 227 versions, 38 heads (the version each piece is at now). Heads are what a console would play; all versions show what the model reaches for while working. Made by `apple/whistlegraph/tools/piece-api-census.mjs`.

## Shape

| | min | p25 | median | p75 | max |
| --- | --- | --- | --- | --- | --- |
| head source, characters | 1076 | 2288 | 3374 | 5293 | 27581 |

Parses: 38 (100%) of heads. Hand-rolled 3D in 2D calls: 6. Async hooks: 0. Imports: 0. Dynamic import: 0. Math.random: 6. Date.now: 1.

## Plan level a head would need

| level | heads |
| --- | --- |
| L1-2d | 37 (97%) |
| L2-wide | 1 (3%) |

L1-2d: only 2D primitives, screen, pen, events, num/help. L2-wide: 2D plus API names outside that set (named). L3-3d: forms, cubes, a camera. unbounded: browser globals, imports, or dynamic import, which no plan can provide.

## Hooks exported

| hook | heads |
| --- | --- |
| paint | 30 (79%) |
| sim | 17 (45%) |
| boot | 15 (39%) |
| act | 13 (34%) |
| leave | 4 (11%) |

## API names destructured (heads)

| name | heads | all versions |
| --- | --- | --- |
| ink | 28 (74%) | 190 (84%) |
| screen | 28 (74%) | 184 (81%) |
| wipe | 26 (68%) | 178 (78%) |
| line | 24 (63%) | 100 (44%) |
| event | 13 (34%) | 47 (21%) |
| circle | 13 (34%) | 64 (28%) |
| clock | 11 (29%) | 61 (27%) |
| box | 9 (24%) | 54 (24%) |
| sound | 8 (21%) | 52 (23%) |
| oval | 7 (18%) | 55 (24%) |
| paintCount | 6 (16%) | 20 (9%) |
| tri | 5 (13%) | 23 (10%) |
| write | 5 (13%) | 28 (12%) |
| shape | 4 (11%) | 13 (6%) |
| text | 2 (5%) | 4 (2%) |
| store | 1 (3%) | 70 (31%) |
| delta | 1 (3%) | 1 (0%) |
| pen | 1 (3%) | 1 (0%) |
| pens | 1 (3%) | 4 (2%) |
| hud | 1 (3%) | 1 (0%) |
| speak | 1 (3%) | 3 (1%) |

## Members read off API objects (heads)

| member | heads |
| --- | --- |
| screen.height | 26 (68%) |
| screen.width | 26 (68%) |
| clock.time | 10 (26%) |
| clock.resync | 9 (24%) |
| sound.synth | 6 (16%) |
| text.width | 2 (5%) |
| event.is | 2 (5%) |
| pen.x | 1 (3%) |
| pen.y | 1 (3%) |
| sound.chirp | 1 (3%) |
| hud.label | 1 (3%) |
| api.line | 1 (3%) |
| api.oval | 1 (3%) |
| api.ink | 1 (3%) |
| api.box | 1 (3%) |
| api.circle | 1 (3%) |

## Primitives and method calls (heads)

| call | heads |
| --- | --- |
| ink | 38 (100%) |
| wipe | 36 (95%) |
| Math.min | 35 (92%) |
| line | 31 (82%) |
| Math.sin | 29 (76%) |
| Math.max | 27 (71%) |
| Math.cos | 18 (47%) |
| circle | 17 (45%) |
| Math.round | 16 (42%) |
| e.is | 16 (42%) |
| Math.abs | 13 (34%) |
| Math.floor | 13 (34%) |
| clock.time | 13 (34%) |
| clock.resync | 12 (32%) |
| sound.synth | 10 (26%) |
| box | 9 (24%) |
| oval | 9 (24%) |
| Math.random | 6 (16%) |
| Math.hypot | 5 (13%) |
| tri | 4 (11%) |
| Math.exp | 4 (11%) |
| write | 4 (11%) |
| shape | 4 (11%) |
| c.map | 3 (8%) |
| Math.pow | 3 (8%) |
| Math.ceil | 3 (8%) |
| pts.push | 3 (8%) |
| points.map | 2 (5%) |
| voice.kill | 2 (5%) |
| voice.update | 2 (5%) |
| text.width | 2 (5%) |
| Math.atan2 | 2 (5%) |
| event.is | 2 (5%) |
| Array.from | 2 (5%) |
| faces.push | 1 (3%) |
| points.reduce | 1 (3%) |
| faces.sort | 1 (3%) |
| b.map | 1 (3%) |
| color.map | 1 (3%) |
| Number.isFinite | 1 (3%) |
| flock.some | 1 (3%) |
| a.map | 1 (3%) |
| WORDS.map | 1 (3%) |
| flock.push | 1 (3%) |
| list.filter | 1 (3%) |
| down.push | 1 (3%) |
| down.find | 1 (3%) |
| rings.push | 1 (3%) |
| rings.filter | 1 (3%) |
| words.forEach | 1 (3%) |
| performance.now | 1 (3%) |
| grid.push | 1 (3%) |
| rows.forEach | 1 (3%) |
| s.synth | 1 (3%) |
| held.delete | 1 (3%) |
| drums.map | 1 (3%) |
| pads.findIndex | 1 (3%) |
| p.strike | 1 (3%) |
| live.add | 1 (3%) |
| held.get | 1 (3%) |

## Event kinds asked for

| event | heads |
| --- | --- |
| touch | 13 (34%) |
| draw | 6 (16%) |
| lift | 5 (13%) |
| reframed | 4 (11%) |
| keyboard:down:space | 3 (8%) |
| speech:completed | 1 (3%) |
| speech:error | 1 (3%) |

## 3D identifiers

| identifier | heads |
| --- | --- |

## Browser globals (a plan cannot provide these)

| global | heads |
| --- | --- |
| none | 0 |

## Loop depth inside paint (heads)

| nesting | heads |
| --- | --- |
| 0 | 14 (37%) |
| 1 | 13 (34%) |
| 2 | 11 (29%) |

Largest numeric loop bound seen in a head paint: 460.

## Every head

| code | owner | head | chars | level | api | last request |
| --- | --- | --- | --- | --- | --- | --- |
| wgDuram | @jeffrey | v18 | 27581 | L1-2d | box circle clock event ink line oval sound | can the camera tilt and rotate and have depth of field |
| wgNirin | @jeffrey | v7 | 18586 | L1-2d | box clock ink line screen sound tri wipe | make the map more linear remove the score make enemies 3d |
| wwTovoz | @jeffrey | v8 | 9612 | L1-2d | box circle clock ink line oval screen shape sound text tri wipe | Create or revise the playable piece using this mixed speech-and-sound input. Fol |
| wgLodaf | @jeffrey | v6 | 9554 | L1-2d | clock ink line screen sound wipe | add a second, smaller crescent moon near the first |
| wgDefen | @jeffrey | v7 | 9311 | L1-2d | box circle clock ink line oval screen sound wipe | make it spin |
| wwVufor | @jeffrey | v14 | 8425 | L1-2d |  | Remote source edit |
| wgRevug | @jeffrey | v6 | 8022 | L1-2d | clock event ink line oval screen sound speak | volumetric make the guy and give him dog and cat |
| wwDabev | @jeffrey | v7 | 6079 | L1-2d |  | the colors are too flickery |
| wwKoduv | @jeffrey | v2 | 5765 | L1-2d |  | Interpret this combined request. |
| wgDozik | @jeffrey | v1 | 5293 | L1-2d |  | Interpret this Whistlegraph performance as coordinated drawing, voice and sound. |
| wwKabag | @jeffrey | v4 | 4777 | L1-2d | circle clock ink line oval screen wipe | Interpret this combined request. |
| wgPodel | @jeffrey | v2 | 4551 | L1-2d | box clock ink line oval screen wipe | in 3d first person |
| wwMupol | @jeffrey | v4 | 4390 | L1-2d | circle event ink line screen wipe write | Create or revise the playable piece using this mixed speech-and-sound input. Fol |
| wwReguv | @jeffrey | v4 | 4329 | L1-2d | event screen | Remote source edit |
| wwDiled | @jeffrey | v1 | 4313 | L1-2d | event ink line paintCount screen wipe | Interpret this combined request. |
| wwMolig | @jeffrey | v1 | 4290 | L1-2d |  | Interpret this combined request. |
| wwBuhof | @jeffrey | v4 | 3720 | L1-2d | circle clock event ink line paintCount screen sound wipe | Interpret this combined request. |
| wwTamof | @jeffrey | v1 | 3608 | L1-2d | event ink line screen wipe write | Create or revise the playable piece using this mixed speech-and-sound input. Fol |
| wwBipoz | @jeffrey | v2 | 3374 | L1-2d |  | make it more performant |
| wwMegok | @jeffrey | v6 | 3274 | L1-2d | box clock ink paintCount screen wipe write | bounce em |
| wwBuzim | @jeffrey | v1 | 3166 | L2-wide:delta | circle delta event ink line pen screen tri wipe | Create or revise the playable piece using this mixed speech-and-sound input. Fol |
| wwGizof | @jeffrey | v4 | 3159 | L1-2d |  | make it sexy |
| wwNabad | @jeffrey | v2 | 3129 | L1-2d | box event ink line screen text wipe write | Create or revise the playable piece using this mixed speech-and-sound input. Fol |
| wwLehoh | @jeffrey | v3 | 2974 | L1-2d | box ink screen tri wipe | Remote source edit |
| wwHolum | @jeffrey | v11 | 2864 | L1-2d | ink line screen shape wipe | Fix wwHolum with the following complete replacement source. Apply it exactly as  |
| wwVazeh | @jeffrey | v1 | 2824 | L1-2d |  | Create or revise the playable piece using this mixed speech-and-sound input. Fol |
| wwLabuk | @jeffrey | v4 | 2579 | L1-2d | circle event ink pens screen sound wipe | that fill whole screen |
| wwBonog | @jeffrey | v2 | 2521 | L1-2d | event ink line screen tri wipe | more reflective now |
| wwGikur | @jeffrey | v4 | 2288 | L1-2d | circle ink line paintCount screen wipe | Create or revise the playable piece using this mixed speech-and-sound input. Fol |
| wwVataz | @jeffrey | v5 | 2237 | L1-2d | box ink line oval paintCount screen wipe | Interpret this combined request. |
| wwGihap | @jeffrey | v2 | 2147 | L1-2d | circle ink line screen shape wipe | Create or revise the playable piece using this mixed speech-and-sound input. Fol |
| wwTenef | @jeffrey | v1 | 2100 | L1-2d |  | Create or revise the playable piece using this mixed speech-and-sound input. Fol |
| wwKegog | @jeffrey | v1 | 1752 | L1-2d | circle event ink line screen wipe | Interpret this combined request. |
| wgKozil | @jeffrey | v1 | 1717 | L1-2d | ink line screen wipe | Interpret this combined request. |
| wgMurof | @jeffrey | v2 | 1538 | L1-2d | clock ink line screen shape wipe | make it wonky and wiggly like the v1 gesture |
| wwTirig | @jeffrey | v1 | 1512 | L1-2d | circle event hud ink line paintCount screen wipe | Interpret this combined request. |
| wwDokel | @jeffrey | v1 | 1116 | L1-2d | circle ink line screen wipe write | Interpret this combined request. |
| wwHesop | @jeffrey | v31 | 1076 | L1-2d | ink screen store wipe | move down |
