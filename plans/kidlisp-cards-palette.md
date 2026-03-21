# KidLisp Cards — Palette of Koans

Tiny programs (2-4 lines) that produce rich static patterns.
Each one teaches a KidLisp concept. Each one is a single frame.

These are the **KidLisp Cards** — small visual koans.

---

## Strategy: Single-Frame Richness

KidLisp runs every frame, but these programs produce the same image
every time because they use no randomness (`?`, `wiggle`), no time
(`f`, `frame`, `sin f`, `cos f`, timing expressions), and no
accumulation (`bake`, `scroll`). Just loops, math, and drawing.

For an **iterative renderer** (future): a `(frames N)` directive could
tell the card system "run this for N frames and capture the last one."
That unlocks `bake` + `scroll` + `blur` patterns — the most popular
KidLisp idiom — for cards too. Marked with `[iterative]` below.

---

## The Palette

### Drawing — Lines

**1. Starburst**
```
(wipe black)
(ink white)
(repeat 72 i (line 80 60 (+ 80 (* 55 (cos (* i 5)))) (+ 60 (* 55 (sin (* i 5))))))
```
72 lines from center, fanning out in a circle. Teaches: `repeat`, `cos`/`sin` with index math.

**2. Crosshatch**
```
(wipe white)
(ink 0 0 0 40)
(repeat 40 i (line (* i 4) 0 (* i 4) 120))
(repeat 40 i (line 0 (* i 4) 160 (* i 4)))
```
Vertical + horizontal lines with low alpha. Overlap creates a woven texture. Teaches: alpha channel, grid from lines.

**3. Vanishing Point**
```
(wipe navy)
(ink cyan)
(repeat 30 i (line 0 (* i 4) 160 60))
(repeat 30 i (line 160 (* i 4) 0 60))
```
Lines converge at center from both sides. Teaches: perspective illusion from simple coordinates.

**4. Envelope**
```
(wipe black)
(ink lime)
(repeat 50 i (line 0 (* i 2.4) (* i 3.2) 0))
(repeat 50 i (line 160 (- 120 (* i 2.4)) (- 160 (* i 3.2)) 120))
```
Two curve envelopes from opposite corners. String art. Teaches: line envelopes create curves.

---

### Drawing — Circles

**5. Ripples**
```
(wipe black)
(repeat 30 i (ink 0 (* i 8) 255) (outline) (circle 80 60 (* i 4)))
```
Concentric rings fading from blue to cyan. Teaches: `outline`, color from index.

**6. Dot Grid**
```
(wipe white)
(ink black)
(repeat 10 y (repeat 10 x (circle (+ 12 (* x 16)) (+ 10 (* y 12)) 3)))
```
Clean 10x10 dot grid. Ben-Day pattern. Teaches: nested `repeat` for x/y grids.

**7. Target**
```
(repeat 8 i (ink (... red white)) (circle 80 60 (- 64 (* i 8))))
```
Red and white alternating rings. Teaches: `...` cycling, concentric shapes.

Wait — `...` cycles over time. Alternative with `if`:
```
(repeat 8 i (if (% i 2) (ink red) (ink white)) (circle 80 60 (- 64 (* i 8))))
```

**8. Eclipse**
```
(wipe navy)
(ink yellow)
(circle 80 60 40)
(ink navy)
(circle 90 55 38)
```
Moon eclipsing a sun. Teaches: layered shapes, subtractive drawing.

---

### Drawing — Boxes

**9. Mondrian**
```
(wipe white)
(ink red) (box 5 5 50 40)
(ink blue) (box 60 50 100 65)
(ink yellow) (box 5 50 50 30)
(ink black) (stroke 3) (outline) (box 5 5 155 115)
```
Mondrian-style composition. Teaches: `stroke`, `outline`, `fill` modes.

**10. Pixel Quilt**
```
(resolution 16 12)
(repeat 12 y (repeat 16 x (ink (* x 16) (* y 20) 128) (box x y 1 1)))
```
16x12 pixel grid where color = position. Teaches: `resolution`, color from coordinates.

**11. Nested Squares**
```
(wipe black)
(repeat 10 i (ink (* i 25) 100 (- 255 (* i 25))) (box (* i 7) (* i 5) (- 160 (* i 14)) (- 120 (* i 10))))
```
Squares shrinking inward with shifting hue. Teaches: shrinking geometry from index.

---

### Drawing — Shapes & Triangles

**12. Sierpinski Row**
```
(wipe black)
(ink white)
(repeat 128 i (if (= 0 (& i (/ (- 128 i) 2))) (box (* i 1.25) 80 2 2)))
```
One row of Sierpinski triangle via bitwise AND. Teaches: bitwise math makes fractals.

**13. Chevrons**
```
(wipe black)
(repeat 8 i (ink (* i 30) 100 255) (tri 0 (* i 15) 80 (+ 10 (* i 15)) 160 (* i 15)))
```
Stacked chevron/arrow shapes in gradient color. Teaches: `tri`, layered geometry.

---

### Colors & Gradients

**14. Spectrum**
```
(resolution 160 1)
(repeat 160 x (ink x (* 1.5 (abs (- x 80))) (- 255 x)) (box x 0 1 1))
```
160-pixel horizontal gradient cycling R/G/B. Teaches: resolution as a painting tool, RGB from math.

**15. Color Field**
```
(resolution 32 24)
(repeat 24 y (repeat 32 x (ink (* x 8) (* y 10) 128) (plot x y)))
```
Soft 32x24 color field. Teaches: `plot`, positional color.

**16. Bars**
```
(wipe black)
(repeat 16 i (ink (? red blue lime yellow cyan magenta orange)) (box (* i 10) 0 10 120))
```
Wait — `?` is random. Let me use cycling:
```
(wipe black)
(repeat 8 i (ink (% (* i 50) 256) (% (* i 80) 256) (% (* i 110) 256)) (box (* i 20) 0 20 120))
```
8 vertical bars with computed colors. Teaches: modular color arithmetic.

---

### Math & Geometry

**17. Sine Wave**
```
(wipe white)
(ink navy)
(repeat 160 x (plot x (+ 60 (* 40 (sin (* x 0.05))))))
```
Sine curve plotted point by point. Teaches: `sin` for wave shapes.

**18. Lissajous**
```
(wipe black)
(ink lime)
(repeat 600 t (plot (+ 80 (* 60 (sin (* t 0.03)))) (+ 60 (* 50 (cos (* t 0.02))))))
```
Lissajous curve from sin/cos with different frequencies. Teaches: parametric curves.

**19. Spiral**
```
(wipe black)
(ink white)
(repeat 500 t (plot (+ 80 (* (/ t 10) (cos (* t 0.1)))) (+ 60 (* (/ t 10) (sin (* t 0.1))))))
```
Archimedean spiral via expanding radius + rotation. Teaches: polar coordinates.

**20. Circle Packing**
```
(wipe black)
(repeat 6 y (repeat 8 x (ink (* x 30) (* y 40) 200) (circle (+ 10 (* x 20) (* (% y 2) 10)) (+ 12 (* y 20)) 9)))
```
Hex-packed colored circles (offset every other row). Teaches: hex grid via modulo offset.

---

### Iterative Cards `[iterative]`

These need N frames to build up. Future `(frames N)` directive.

**21. Drift Weave** `[iterative: 200 frames]`
```
(ink white 10)
(line)
(scroll 1 0)
(bake)
```
Each frame: random line, shift right 1px, bake. After 200 frames: a woven drift texture.

**22. Blur Bloom** `[iterative: 100 frames]`
```
(ink rainbow)
(circle 80 60 5)
(blur 1)
(bake)
```
A dot that blooms into a soft rainbow halo. Teaches: `blur` + `bake` accumulation.

**23. Spin Mandala** `[iterative: 60 frames]`
```
(ink cyan 30)
(line 80 0 80 120)
(spin 6)
(bake)
```
Line rotates 6 degrees per frame, baked. After 60 frames: a full mandala.

**24. Scroll Rain** `[iterative: 300 frames]`
```
(ink white 8)
(repeat 5 i (plot (? 160) 0))
(scroll 0 1)
(bake)
```
Dots rain down, scrolling and baking. After 300 frames: gentle vertical streaks.

**25. Zoom Tunnel** `[iterative: 80 frames]`
```
(ink rainbow)
(outline)
(circle 80 60 60)
(zoom 0.97)
(bake)
```
Circle drawn, zoomed in slightly, baked. Repeats into a tunnel. Teaches: `zoom` + `bake`.

---

## Naming Ideas

Each card could have a short poetic name:

| # | Name | Concept |
|---|------|---------|
| 1 | Starburst | line + trig |
| 2 | Weave | alpha overlap |
| 3 | Vanish | perspective |
| 4 | Envelope | string art |
| 5 | Ripple | outline rings |
| 6 | Dots | nested grid |
| 7 | Target | alternating fill |
| 8 | Eclipse | subtraction |
| 9 | Mondrian | composition |
| 10 | Quilt | resolution + position color |
| 11 | Nest | shrinking squares |
| 12 | Sierpinski | bitwise fractals |
| 13 | Chevron | triangles |
| 14 | Spectrum | 1px gradient |
| 15 | Field | color field |
| 16 | Bars | modular color |
| 17 | Wave | sine curve |
| 18 | Lissajous | parametric |
| 19 | Spiral | polar coords |
| 20 | Honeycomb | hex packing |
| 21 | Drift | scroll + bake |
| 22 | Bloom | blur + bake |
| 23 | Mandala | spin + bake |
| 24 | Rain | scroll rain |
| 25 | Tunnel | zoom + bake |

---

## Next Steps

1. **Test these** — paste each into kidlisp.com and verify the output
2. **Curate** — pick the ~12 most beautiful for the first card set
3. **Iterative renderer** — add `(frames N)` support to kidlisp evaluator so cards 21-25 work
4. **Render to card format** — screenshot at card aspect ratio using oven or DOM renderer
5. **Each card = a published $code** — save each koan as a kidlisp piece so it has a permanent link and shows up in the community
