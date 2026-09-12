<!-- how a piece draws on the AC canvas
     Bundled with Easel from SCREEN.md in the Aesthetic Computer repository.
     Do not edit here — edit the source and run `npm run context`. -->

# The Screen — a drawing guide for Aesthetic Computer pieces

companion to `HAND.md`, `GRAPHICS.md` and `papers/VOICE.md`. HAND governs how the code reads, GRAPHICS how a doc draws, VOICE how the papers sound; this governs how a **piece draws on the AC canvas**.

it exists because of a failure mode that has no other home. a piece can be perfectly written by HAND's rules and still land wrong — a readout under the corner label, a button that eats the back tap, a layout that only fits the screen it was authored on. those are not code-style mistakes. they're screen mistakes, and until now nothing in the repo wrote them down, so every agent rediscovered them by covering the label.

## the one rule

**the piece does not own the whole screen.** the system draws on it too, and what the system draws is load-bearing — the label is how you know where you are and how you get back. draw *around* it, or hide it on purpose. never on top of it by accident.

## the corner label

the system paints it at `(6, 6)` in the default 6×10 typeface (`lib/disk.mjs`, `defo = 6`). one line of it occupies roughly `y` 6–16.

- **reserve the top-left ~20 rows.** a top bar is fine if it starts below that; text at `y: 10` is not.
- **the zone grows.** `hud.qr(url)` draws a QR at `(leftPad + 2, 4)` and pushes the label right — same corner, much taller. swiping the label reveals `share`, which extends it rightward.
- **it's interactive.** tapping it navigates back. a button underneath it fights the user for the same pixels and the user loses.
- **you can take it over** rather than dodge it: `hud.label(text, color, offset)` replaces it, `hud.label()` hides it, `hud.tinyLabel()` switches to MatrixChunky8, `hud.suffix(".com")` adds a superscript, `hud.currentLabel()` reads it back.

so: **readouts go along the bottom, or right-aligned.** that's the default. a top bar is a decision, not a reflex.

## paint

- return nothing to keep animating; return `false` to paint once and freeze (`disk.mjs`: `noPaint = paintOut === false || …`).
- `wipe` every frame or never. a half-wiped frame reads as a bug, not as a trail — if you want a trail, draw it.

## layout

- **never hardcode a screen size.** phones, desktop panes and the frameless preview are all different, and the same piece meets all three.
- recompute geometry on `reframed`. that event is the only signal you get.
- when rebuilding a `ui.Button` there, reassign `btn.box = new geo.Box(...)` — don't `new ui.Button` over it. a fresh button drops `down`/`over`, and anything it was holding (a note, a drag) hangs.

## touch

- `ui.Button` + `btn.act(e, { down, push, over, out, cancel }, pens?.())`. **`pens()` is what makes multitouch work** — leave it off and the piece is one-finger-only on a phone, which is where it's being watched.
- `cancel` is not optional when a button holds something. `lib/ui.mjs` fires it on edge-cancel and global reset; without it, held state survives a gesture that already ended.
- `over` with `btn.up = false` is the idiom for sliding onto a pad mid-gesture.

## sound

- a held voice is `synth({ duration: "🔁", … })` and ends with `.kill(fade)`. a fade of `0` clicks.
- **kill every voice in `leave`.** a piece that hands off with a note still ringing rings into the next one.

## the phone is the review

pieces are pushed live to a phone as they're saved. that makes two things true: small edits beat one big rewrite, and **the small screen is the real target** — if the layout only works at desktop width, it doesn't work.
