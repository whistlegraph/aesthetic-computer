// Resource: aesthetic-computer://piece-examples
// Real top-hit KidLisp pieces from aesthetic.computer (500+ views)

export const pieceExamplesResource = {
  uri: "aesthetic-computer://piece-examples",
  name: "KidLisp Top Hit Examples",
  description:
    "Real KidLisp pieces from aesthetic.computer with 500+ views — proven patterns for generative art, animation, and interaction",
  mimeType: "text/markdown",
};

export function getPieceExamples() {
  return `# KidLisp Top Hit Examples

Real pieces from aesthetic.computer with 500+ views. Each one is live at \`aesthetic.computer/CODE\`.

---

## "bop" — 7,388 views
The most-viewed KidLisp piece. Shows how bare words and comma syntax work.

\`\`\`lisp
purple, ink, line, blur 5
\`\`\`

**Pattern:** Bare color sets background, \`ink\` picks random color, \`line\` draws random line, \`blur 5\` softens. Runs every frame → builds up dreamy lines.

---

## "pie" — 4,702 views
Animated cross with cycling background colors and frame-based scrolling.

\`\`\`lisp
(fps 24)
(0.25s (wipe (... red yellow blue)))
(ink green)
(line 0 height/2 width height/2)
(ink red)
(line width/2 0 width/2 height)
(scroll frame frame)
\`\`\`

**Pattern:** \`...\` cycles through colors every 0.25s. \`scroll frame frame\` creates diagonal motion tied to frame count.

---

## "roz" — 3,251 views (by @jeffrey)
Rich layered composition with fade gradients, spin, zoom, contrast, and random ink.

\`\`\`lisp
fade:red-blue-black-blue-red
ink (? rainbow white 0) (1s... 24 64)
line w/2 0 w/2 h
(spin (2s... -1.125 1.125)) (zoom 1.1)
(0.5s (contrast 1.05))
(scroll (? -0.1 0 0.1) (? -0.1 0 0.1))
ink (? cyan yellow magenta) 8
circle w/2 h/2 (? 2 4 8)
\`\`\`

**Pattern:** Fade gradient background, cycling spin direction, random ink alpha with timed cycling, center circle. Transforms accumulate for evolving visuals.

---

## "ceo" — 3,087 views
Minimal but powerful: coat overlay with animated fade, periodic zoom, and scroll.

\`\`\`lisp
(1s (coat fade:black-red-rainbow-red-black:frame 64)) (0.3s (zoom 0.5)) (scroll 1)
\`\`\`

**Pattern:** \`coat\` applies semi-transparent overlay (alpha 64). Fade gradient animated by frame. Periodic zoom-out creates tunnel effect. Scroll adds drift.

---

## "39i" — 2,049 views (by @jeffrey)
Generative black/white composition with floods, transforms, and timed events.

\`\`\`lisp
black
(0.1s (ink (? black white) 32)
(circle ? ? 32))
(ink (? white black gray) (? 32 96))
flood ? ?
scroll (2s... 1 0 -1) (3s... -1 0 1)
(1.5s (zoom (? 0.5 4)))
(1s (blur 0.5))
(2s (contrast 2))
(0.3s (ink (? white black)) (repeat 30 point))
(1s... (spin (0.5s... -0.1 0.1))
\`\`\`

**Pattern:** Random circles + flood fills + aggressive transforms. \`?\` and \`...\` create unpredictable, evolving compositions. Multiple timed events at different intervals.

---

## "cow" — 2,320 views (by @jeffrey)
Layered composition embedding two other pieces with transparency.

\`\`\`lisp
($39i 0 0 w h 128)
($r2f 0 0 w h 128)
(contrast 1.5)
\`\`\`

**Pattern:** \`$codeId\` embeds other pieces as layers. Arguments: x, y, width, height, alpha. Contrast boost unifies the layers.

---

## "wezo" — 1,686 views
Hypnotic zebra-rainbow gradient with extreme zoom.

\`\`\`lisp
(wipe fade:zebra-rainbow-zebra-rainbow-zebra)
(zoom 0.25)
\`\`\`

**Pattern:** Complex fade gradient mixing zebra (black/white) and rainbow. Extreme zoom-out (0.25) each frame creates fractal-like recursion.

---

## "4bb" — 1,611 views
Multi-layer bake technique with four directional scrolls.

\`\`\`lisp
black, ink (? yellow black) 48, line, scroll 1
bake, ink (? magenta erase) 64, line, scroll -1
bake, ink (? lime erase) 16, line, scroll 0 1
bake, ink (? cyan erase) 48, line, scroll 0 -1
burn, blur 8, contrast 1.25
\`\`\`

**Pattern:** Each line: set ink → draw random line → scroll in one direction → bake to background. Four layers scrolling in opposite directions. \`erase\` color creates transparency holes.

---

## "r2f" — 1,506 views (by @jeffrey)
Salmon-based palette with flood fills, random transforms, and spin.

\`\`\`lisp
salmon
ink fade:palegreen-purple (? 20 48)
box ? ? (? 2 4 32 64)
ink (? c0 c4 c7 rainbow) (? 32 64 96)
(repeat 2 (flood ? ?))
contrast (? 1.05 0.97 1)
scroll 0.1
(0.1s (zoom (? 1.89 1 1.1 1.2)))
spin (? -0.1 0 0 0 0.1)
scroll 0 (? 1 -1)
blur 0.05
\`\`\`

**Pattern:** Weighted randomness with \`?\` — e.g. \`(? -0.1 0 0 0 0.1)\` makes 0 the most likely spin. Sub-pixel scroll (\`0.1\`) for slow drift. Flood fills create large areas.

---

## "reeb" — 1,337 views
Simple erase painting technique.

\`\`\`lisp
red, ink erase, line, bake
\`\`\`

**Pattern:** Red background, ink set to \`erase\` (transparent), draw random line that erases, bake. Creates organic erosion of the red surface.

---

## "inz" — 1,293 views (by @jeffrey)
Text-based animation with scroll, blur, and zoom.

\`\`\`lisp
(beige)
(ink (0.25s... 127 0 rainbow))
(write X 3 3)
(scroll 18 3)
(blur 0.1)
(1.25s (zoom (? 0.25 1.5)))
\`\`\`

**Pattern:** Single character "X" written each frame, scrolled diagonally, slightly blurred. Zoom periodically jumps between in/out for visual variety.

---

## "eel" — 1,086 views
Text animation with random scroll directions.

\`\`\`lisp
(beige)
(ink (0.5s... gray black rainbow))
(0.1s (write (? x X) width/2 height/2))
(0.05s (scroll (? 18 -18) (? 32 -32))
(blur 0.1)
(1.25s (zoom (? 2 1.5 0.5)))
\`\`\`

**Pattern:** Random case character written centrally, aggressive random scrolling, periodic zoom jumps. Creates chaotic text trails.

---

## "otoc" — 1,181 views
Noise-driven spinning vortex.

\`\`\`lisp
(once noise) (spin (0.5s... 1 -1)) (suck 1) (contrast 1.01)
\`\`\`

**Pattern:** \`noise\` fills screen with random pixels once. \`spin\` alternates direction every 0.5s. \`suck\` creates vortex pull. \`contrast\` slowly intensifies colors. Minimal code, maximum effect.

---

## "air" — 596 views (by @jeffrey)
Gentle generative art with fade gradient and random points.

\`\`\`lisp
fade:pink-gray-pink
ink (? orange yellow) 32
line
(0.15s (zoom 0.2))
scroll 1 0.25
(0.1s (contrast 1.01))
(0.5s (ink rainbow 96) (repeat 10 point))
\`\`\`

**Pattern:** Pink-gray gradient background. Low-alpha random lines accumulate. Extreme zoom-out pulses. Occasional rainbow point bursts. Subtle contrast ramp.

---

## "lei" — 545 views (by @jeffrey)
Rainbow line rain with cycling scroll directions.

\`\`\`lisp
blue
ink rainbow
line ? 0 100 ?
scroll (... 1 0 -1 -2) (? 1 1 -2)
(1s (blur 1))
(0.8s (scroll width/2 height/2))
\`\`\`

**Pattern:** Rainbow-colored lines from random x at top to 100 at random y. Scroll direction cycles through values. Periodic large scroll jumps (width/2, height/2) create displacement. Periodic blur softens.

---

## Common Patterns in Top Hits

1. **Feedback loops** — scroll + zoom + blur creates self-evolving trails
2. **Weighted randomness** — \`(? value value value rare_value)\` makes common values likely
3. **Multi-speed timers** — different intervals (0.1s, 0.5s, 1s, 2s) for layered rhythm
4. **Bare word shortcuts** — \`black\`, \`line\`, \`ink\` work without parens
5. **Erase ink** — \`ink erase\` creates transparency holes in backgrounds
6. **Embedded composition** — \`$codeId\` layers multiple pieces with alpha
7. **Fade gradients** — \`fade:color1-color2-color3\` for dynamic backgrounds
8. **\`...\` cycling** — \`(2s... -1 1)\` alternates values over time
9. **Minimal code** — many top hits are under 5 lines
`;
}
