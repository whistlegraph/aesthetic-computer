// Resource: aesthetic-computer://kidlisp-reference
// Comprehensive reference for KidLisp — the full 118-function API

export const kidlispReferenceResource = {
  uri: "aesthetic-computer://kidlisp-reference",
  name: "KidLisp Language Reference",
  description:
    "Complete reference for KidLisp: a creative coding Lisp dialect with 118 functions across 12 categories for generative art on aesthetic.computer",
  mimeType: "text/markdown",
};

export function getKidLispReference() {
  return `# KidLisp Language Reference

KidLisp is a minimal Lisp dialect for creating generative art and interactive experiences on aesthetic.computer.
It uses S-expressions where the first element is a function name followed by arguments.

## Syntax Basics

\`\`\`lisp
; Comments start with semicolon
(function-name arg1 arg2 ...)

; Shorthand comma syntax (no parens needed for simple commands)
purple, ink, line, blur 5

; Variables
(def name value)
(def x 10)

; Functions
(later name param1 param2 body...)
(later star x y (ink "yellow") (circle x y 20))
(star 100 100)

; Conditionals
(if condition then else)

; Loops
(repeat count expression...)
(repeat count iterator expression...)
(bunch 50 (plot (wiggle width) (wiggle height)))  ; bunch = alias for repeat
\`\`\`

**Identifier rules:** Must start with letter or underscore. Can contain letters, digits, underscores. **No dashes** (parsed as subtraction). Use \`my_var\` not \`my-var\`.

**String literals:** Both \`"text"\` and \`'text'\` work.

**Shorthand:** \`w\` = width, \`h\` = height, \`f\` = frame. \`?\` = random, \`...\` = cycle through values.

## Screen Management

| Function | Usage | Description |
|----------|-------|-------------|
| \`wipe\` | \`(wipe color)\` | Clear screen with color |
| \`resolution\` | \`(resolution w h)\` | Set canvas resolution |
| \`coat\` | \`(coat color alpha)\` | Semi-transparent overlay |
| \`mask\` | \`(mask x y w h)\` | Restrict drawing to region |
| \`unmask\` | \`(unmask)\` | Remove drawing mask |

## Drawing Primitives

| Function | Usage | Description |
|----------|-------|-------------|
| \`ink\` | \`(ink color)\` or \`(ink r g b)\` or \`(ink color alpha)\` | Set drawing color |
| \`line\` | \`(line x1 y1 x2 y2)\` or \`(line)\` for random | Draw line between two points |
| \`lines\` | \`(lines points...)\` | Draw connected lines |
| \`box\` | \`(box x y w h)\` | Draw rectangle (respects fill/outline mode) |
| \`circle\` | \`(circle x y radius)\` | Draw circle (respects fill/outline mode) |
| \`tri\` | \`(tri x1 y1 x2 y2 x3 y3)\` | Draw triangle |
| \`plot\` / \`point\` | \`(plot x y)\` | Set single pixel |
| \`flood\` | \`(flood x y)\` | Flood fill area |
| \`shape\` | \`(shape points... "fill")\` | Draw polygon |

Shapes accept explicit mode overrides: \`(circle x y r "fill")\`, \`(circle x y r "outline")\`, \`(circle x y r "outline:5")\`

## Fill/Outline Modes

| Function | Description |
|----------|-------------|
| \`(fill)\` | Set global fill mode (default) |
| \`(outline)\` | Set global outline mode |
| \`(stroke)\` | Alias for outline |
| \`(nofill)\` | Alias for outline |
| \`(nostroke)\` | Alias for fill |

## Color System

Colors can be specified as:
- **Named:** \`"red"\`, \`"blue"\`, \`"lime"\`, \`"navy"\`, \`"coral"\`, etc. (all CSS color names)
- **Bare words:** \`red\`, \`blue\`, \`purple\` (no quotes needed)
- **RGB:** \`(ink 255 0 0)\`
- **With alpha:** \`(ink "red" 128)\` or \`(ink red 128)\` (0-255, where 255 is opaque)
- **Special:** \`rainbow\` (cycling colors), \`zebra\` (black/white alternating), \`erase\` (transparent)
- **Fade gradients:** \`fade:red-blue-black\`, \`fade:zebra-rainbow-zebra\`, \`fade:color1-color2:direction\`

## Math & Numbers

| Function | Usage | Description |
|----------|-------|-------------|
| \`+\` | \`(+ a b c...)\` | Addition |
| \`-\` | \`(- a b)\` | Subtraction |
| \`*\` | \`(* a b c...)\` | Multiplication |
| \`/\` | \`(/ a b)\` | Division. Also works as inline: \`width/2\`, \`h/2\` |
| \`%\` / \`mod\` | \`(% a b)\` | Modulo |
| \`sin\` | \`(sin x)\` | Sine |
| \`cos\` | \`(cos x)\` | Cosine |
| \`random\` / \`?\` | \`(random max)\` or \`(? a b c)\` | Random number or random choice |
| \`wiggle\` | \`(wiggle amount)\` | Random ±amount/2 |
| \`min\` | \`(min a b c...)\` | Minimum value |
| \`max\` | \`(max a b c...)\` | Maximum value |
| \`abs\` | \`(abs x)\` | Absolute value |
| \`sqrt\` | \`(sqrt x)\` | Square root |
| \`pow\` | \`(pow base exp)\` | Power |
| \`floor\` | \`(floor x)\` | Floor |
| \`ceil\` | \`(ceil x)\` | Ceiling |
| \`round\` | \`(round x)\` | Round |

## System Properties

| Name | Description |
|------|-------------|
| \`width\` / \`w\` | Canvas width |
| \`height\` / \`h\` | Canvas height |
| \`frame\` / \`f\` | Current frame number |
| \`clock\` | UTC timestamp |
| \`pi\` | Math constant |

## Control Flow & Variables

| Function | Usage | Description |
|----------|-------|-------------|
| \`def\` | \`(def name value)\` | Define variable |
| \`later\` | \`(later name params body...)\` | Define function |
| \`if\` | \`(if cond then else)\` | Conditional |
| \`once\` | \`(once expr)\` | Execute only once per session |
| \`now\` | \`(now var value)\` | Update variable immediately |
| \`not\` | \`(not expr)\` | Logical negation |
| \`die\` | \`(die)\` | Stop execution |
| \`repeat\` / \`rep\` / \`bunch\` | \`(repeat count iter expr...)\` | Loop with optional iterator |
| \`choose\` / \`?\` | \`(? a b c)\` | Random selection from options |
| \`...\` | \`(... a b c)\` | Cycle through values over time |

## Animation & Timing

Timing expressions control when code runs:
- \`1s\` — Execute after 1 second
- \`2s...\` — Cycle through values every 2 seconds (repeating)
- \`0.5s!\` — Execute once after 0.5 seconds
- \`0.1s\` — Execute every 0.1 seconds (fast timer)

\`\`\`lisp
(once (wipe "black"))
1s (ink "red") (circle 100 100 50)
2s (ink "blue") (box 150 150 40 40)
(0.25s (wipe (... red yellow blue)))        ; Cycle background color
(0.1s (ink (? black white) 32) (circle ? ? 32))  ; Random circles periodically
\`\`\`

## Pixel Transformations (11 functions)

| Function | Usage | Description |
|----------|-------|-------------|
| \`scroll\` | \`(scroll dx dy)\` or \`(scroll dx)\` | Translate pixels with wrapping |
| \`zoom\` | \`(zoom factor)\` | Scale from center (>1 zoom in, <1 zoom out) |
| \`spin\` | \`(spin angle)\` | Rotate canvas (degrees) |
| \`suck\` | \`(suck strength [cx cy])\` | Radial vortex displacement |
| \`blur\` | \`(blur amount)\` | Gaussian blur |
| \`contrast\` | \`(contrast amount)\` | Adjust contrast (>1 increase, <1 decrease) |
| \`sort\` | \`(sort)\` | Sort pixels by brightness |
| \`pan\` | \`(pan dx dy)\` | Pan camera view |
| \`unpan\` | \`(unpan)\` | Reset camera position |
| \`resetSpin\` | \`(resetSpin)\` | Reset rotation |
| \`smoothspin\` | \`(smoothspin angle)\` | Smooth rotation |
| \`bake\` | \`(bake)\` | Commit current drawing to background layer |

## Images & Media

| Function | Usage | Description |
|----------|-------|-------------|
| \`paste\` | \`(paste url x y [scale])\` | Paste image at position (URLs can be unquoted) |
| \`stamp\` | \`(stamp url x y [scale])\` | Paste image centered |
| \`painting\` | \`(painting x y)\` | Paste current user's painting |
| \`steal\` | \`(steal)\` | Copy current buffer |
| \`putback\` | \`(putback)\` | Restore copied buffer |
| \`tape\` | \`(tape !CODE x y w h [speed])\` | Embed a tape video |

### Text

| Function | Usage | Description |
|----------|-------|-------------|
| \`write\` | \`(write text x y [bg] [size])\` | Draw text |
| \`len\` | \`(len text)\` | Get text length |

## Audio & Sound

| Function | Usage | Description |
|----------|-------|-------------|
| \`mic\` | \`(mic)\` | Access microphone |
| \`amplitude\` | \`(amplitude)\` | Get audio amplitude |
| \`speaker\` | \`(speaker)\` | Audio output control |
| \`melody\` | \`(melody notes)\` | Play musical sequence |
| \`overtone\` | \`(overtone freq)\` | Generate harmonic tones |
| \`noise\` | \`(noise)\` | Generate white noise |

## 3D Graphics

| Function | Usage | Description |
|----------|-------|-------------|
| \`cube\` | \`(cube id)\` | Create/reference 3D cube |
| \`quad\` | \`(quad)\` | Create quad primitive |
| \`form\` | \`(form objects...)\` | Render 3D forms |
| \`trans\` | \`(trans form transforms...)\` | Transform 3D objects (move, scale, spin) |
| \`cubespin\` | \`(cubespin x y z)\` | Animate cube rotation |
| \`cubepos\` | \`(cubepos x y z)\` | Set cube position |
| \`cubescale\` | \`(cubescale factor)\` | Scale cube |
| \`camrot\` / \`camspin\` | \`(camrot x y z)\` | Camera rotation / animation |

## Embedding & Navigation

| Function | Usage | Description |
|----------|-------|-------------|
| \`embed\` | \`(embed $codeId x y w h alpha)\` | Embed another piece with position/size |
| \`$codeId\` | \`($codeId)\` or \`($codeId x y w h alpha)\` | Execute/embed cached code by ID |
| \`jump\` | \`(jump "piece")\` or \`(jump $id)\` | Navigate to another piece |
| \`hop\` | \`(hop url)\` | Navigate to URL |

## Utility

| Function | Usage | Description |
|----------|-------|-------------|
| \`tap\` | \`(tap expr)\` | Handle touch/click events |
| \`draw\` | \`(draw)\` | Force redraw |
| \`label\` | \`(label text color offset)\` | HUD label overlay |
| \`fps\` | \`(fps rate)\` | Set frame rate |
| \`debug\` | \`(debug value)\` | Debug logging |
| \`yes\` / \`no\` | Boolean true / false |

## Best Practices

1. **Start with a background** — \`(wipe color)\` or just a bare color name like \`black\`
2. **Set \`(ink color)\` before drawing** — all primitives use the current ink
3. **Use \`?\` for randomness** — \`(? red blue green)\` picks randomly
4. **Use \`...\` for cycling** — \`(... red blue green)\` cycles over time
5. **Use \`wiggle\` for organic movement** — \`(circle (wiggle w) (wiggle h) 10)\`
6. **Combine transforms for feedback loops** — scroll + zoom + blur creates trails
7. **Use \`once\` for setup** — \`(once (wipe "black"))\` runs only on first frame
8. **Use \`bake\` for layering** — commits drawing to background
9. **Timing expressions drive animation** — \`0.1s\`, \`1s...\`, etc.
10. **Bare words work** — \`red\` = \`(wipe "red")\`, \`line\` = \`(line)\` with random coords
`;
}
