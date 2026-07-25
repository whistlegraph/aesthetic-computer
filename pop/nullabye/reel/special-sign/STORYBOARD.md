# Special Sign — sine-system reel

Vertical social reel built from six colored-pencil illustrations and the locked
`Special Sign` master. The Ashland revision uses documentary Instagram-platter
references of Jeffrey's actual room: its steep timber A-frame, black end window,
white production desk, red hanging bulbs, green rug, rolling chalkboard, speakers,
patch cables, and lived-in studio clutter. The visual story stays inside that
room and grows
cumulatively: Jeffrey does not leave the computer when the system wakes; he
keeps programming, listens, begins to move, and adds material until the room is
playing with him.

## Cut

| Beat | Time | Illustration | Action |
| --- | ---: | --- | --- |
| 1 | 0:00.000–0:06.316 | `01-quiet-code.png` | Quiet room. Jeffrey types on Neo; the globe is dark. |
| 2 | 0:06.316–0:11.053 | `02-first-sine.png` | One key/cable wakes one thin sine inside the globe. |
| 3 | 0:11.053–0:15.789 | `03-listen-vibe.png` | He keeps coding, leans toward the tone, and begins to sway. |
| 4 | 0:15.789–0:22.105 | `04-add-rings.png` | More code adds rings, bubbles, color, and spatial motion. |
| 5 | 0:22.105–0:28.421 | `05-gremlin-build.png` | Coding becomes a compact gremlin dance as the system accelerates. |
| 6 | 0:28.421–0:37.895 | `06-full-system.png` | Full Attic Gremlin state: still adding material, room fully alive. |

The reel is 12 bars at 76 BPM (`37.894737s`) from the start of the final master.
Beat lengths are 2, 1.5, 1.5, 2, 2, and 3 bars, so every cut lands on a
musical bar/half-bar boundary. No dialogue, captions, readable
code, or UI appears inside the illustrations.

## Visual laws

- Colored pencil, wax pencil, and dry pastel on cool neutral-white fibrous paper.
- Saturated indigo shadows; electric cyan and hot coral system light; emerald
  rug; acid yellow-green Neo. No beige or yellow paper cast.
- Exactly one Jeffrey, one yellow-green Neo, and one separate globe.
- Neo remains connected to the globe, but its screen stays dark or faces away.
- Each beat inherits all earlier additions; the system never resets between cuts.
- Essential action stays in the central vertical safe area for a 9:16 crop.

## Ashland motion pass

The revised vertical illustrations live in `illys-ashland/`; the documentary
source contact is `refs/ashland-house-platter-contact.jpg`, and the six-panel
review sheet is `special-sign-storyboard-ashland-contact-sheet.jpg`.

`gen-motion-ashland.mjs` sends each panel through fal.ai Seedance as a separate
9:16 pencil-animation shot. Generated shots are cached in `motion-ashland/` with
queue sidecars so interrupted work resumes without resubmitting. Assembly makes:

- a 37.895-second 1080×1920 Instagram Reel aligned to the six musical cuts;
- a silent seamless eight-second 540×960 Spotify Canvas from the final system.

Run `node gen-motion-ashland.mjs --dry-run` to review prompts and cost, then
`node gen-motion-ashland.mjs`, followed by
`node gen-motion-ashland.mjs --assemble`.
