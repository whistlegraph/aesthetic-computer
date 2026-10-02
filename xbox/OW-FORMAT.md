# The .ow package

A `.ow` is one text file that carries an oskiewar **level**, **objects**, or
both. It is object-lisp (`xbox/OBJECT-DIALECT.md`) with section banners, so
the reader that reads an object reads it, and an object section is
byte-for-byte the `.lisp` it came from. Written 2026-10-02 for the monowheel
desert; the shipped example is `xbox/live/levels/monowheel-desert.ow`.

```
;; ow 1
;; level monowheel-desert
title "monowheel desert"
kind island
home 2840 0
middle 7340 0
island radius 7400 shore 900 sea 40 deep 300
dunes 150 70 14
supply chalk 420
supply paint 760
supply monowheel 260
;; object monowheel
…the object's source, unchanged…
```

- The file opens with `;; ow 1`. A banner `;; level <name>` or
  `;; object <name>` starts a section; names are kebab-case. One level a
  package, any number of objects, in any order. Everything else is ordinary
  object-lisp: `;` comments, a word-led line is a call, strings in quotes.
- **Objects** replace the game's by the name the game asks for
  (`gameObjects.<name>`): today `monowheel` and `figure`. An object the game
  never draws costs nothing; a replaced one compiles on its next ask.
- **Levels** have a `kind`:
  - `island` — the monowheel desert machine. `home x z` (where you start,
    flat sand), `middle x z` (the island's centre; pushed out past home so
    the dunes run on before the sea; a level with a home and no middle is
    centred on its home), `island radius shore sea deep`, `dunes a b c`
    (the three dune amplitudes; they grow with distance from home), `supply
    chalk|paint|monowheel <ring radius>`. The world's grid is sized from
    middle and radius. What a level leaves out keeps the shipped default
    (`ISLAND_DEFAULTS` in `ow.mjs`). `title` is what the title screen and
    the HUD call it.
  - `arena` — the 2D map, exactly `ac.oskiewar.map` v1 (`oskiewar-map.mjs`):
    `flat from to [lift]`, `bank|transition from to rise dir [lift]`, `deck
    col cols row`, `spawn a b`, `pickup KIND col amount` (a kind with a space
    in quotes), `skateboard no`. `levelToMap` / `levelFromMap` convert both
    ways, and the workshop and the coach keep speaking JSON.
  - `pool`, `park`, `indoor`, `halfpipe` — a built-in freeskate course by
    name, nothing else.

## Loading one

- **Web:** `oskiewar.com/?ow=<url>` fetches the file before the game boots;
  `?level=desert|pool|indoor|halfpipe|park` opens freeskate on a course.
- **Any host:** set `globalThis.__oskiewarOw` to the text before the game
  loads; the first freeskate reads it.
- **Live, from the coach:** `coach_workshop` with `op: "level"` and `ow:
  <text>` installs it under the rider at once; `course: "pool"` swaps a
  built-in course. The rider keeps what they ride and hold (a go-kart only
  between 3D courses). The pause menu's `level` row does the same by hand.
- **Code:** `installOw(text, now)` in oskiewar.js; `swapCourse(course, now)`
  is the fast swap underneath.

## Tools and tests

```
node xbox/tools/ow.mjs check  levels/monowheel-desert.ow     # read, validate, compile each object
node xbox/tools/ow.mjs pack   --level map.json --object monowheel=objects/monowheel-flat.lisp > out.ow
node xbox/tools/ow.mjs unpack out.ow dir/                     # level.json | level.ow + one .lisp an object
node --test xbox/live/tests/ow.test.mjs
```

`ow.mjs` has no imports — the game carries it inside the `gameObjects` block
(`node xbox/tools/embed-objects.mjs` after editing it, or the game keeps the
old one) — and takes the reader as an argument (`readOw(text, { read })`).

## Rooms

Everyone on a course shares one public park room, named by the course
(`desert1`, `park1`, `indoor1`, `pipe1`, `long1`) in the speakable-id shape
every host and the relay accept, so friends meet from the website and the
Xbox without a link. `?park=<name>` or `OSKIEWAR_ROOM` picks a private room
that stays private through course swaps; following a public room's link
opens its course. Presence is shared on the 3D courses (desert, park); the 2D
courses publish none yet.
