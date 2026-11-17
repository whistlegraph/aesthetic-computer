# dool film

Scripted films shot inside dool: actors that walk and ride on cue, a camera
the script directs, titles and captions, and a recording at the end.

```
node duel/film/shoot.mjs tower          # a take of films/tower.mjs
node duel/film/shoot.mjs tower --open   # …and open the camera page in Chrome
```

`shoot` starts a private dool (fresh state, a cast that signs in by name; no
production data), prints a camera-page URL, and waits for that page. Open it
in a Chrome window that stays visible and in front: a browser stops drawing a
tab it thinks nobody can see, and `shoot` fails any take with a stall in it.
The take lands in `duel/film/out/<film>-<time>.mp4` (1280 × 720, 60 fps, no
sound yet).

## Writing a film

A film is a module in `films/` (see `films/tower.mjs`):

- `handles`: who has a floor of the tower. `set({ tower, store })` dresses the
  stage before the camera is up.
- `direct(d)` is the screenplay, top to bottom:
  - `const a = await d.cast('jeffrey', { at: [x, z], facing })`
  - `await a.walkTo(x, z, { pace })`, `a.face(bearing | [x, z])`, `a.jump()`,
    `a.say(text)`, `await a.ride('home' | 89 | '@handle' | 'g')`,
    `await a.take(89)`
  - `d.camera.shot({ kind: 'still' | 'move' | 'follow' | 'orbit', … })`
    (`camera.mjs` lists every field), `await d.camera.cut(room)`,
    `d.camera.open()`, `await d.camera.sees(handle)`
  - `await d.card(title, sub, seconds)`, `d.caption(text)`,
    `d.counter(from, to, { label })`, `await d.fade(to, seconds)`
  - `await d.roll()` starts the recording; returning from `direct` ends it.

## Parts

| | |
|---|---|
| `camera.mjs` | where the camera stands for a shot at a moment (pure; tested) |
| `actor.mjs` | a scripted player over dool's own socket |
| `stage.mjs` | the camera page: director's camera, titles, recorder |
| `shoot.mjs` | the private server, the director's commands, the transcode |

The client loads `stage.mjs` only at `?film`, and only `shoot.mjs` serves it.
A camera page joins with `watch=1`, which looks on without a seat.
