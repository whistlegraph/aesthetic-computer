# MacNeoPolitan — illustration brief

Campaign art for *MacNeoPolitan*, the new work premiering on the CultureHub LA
program alongside *Special Sign* (see
`grants/culturehub-la-2026/acceptance/`). Billed **Whistlegraph presents**.

## The piece

A spatial audio play for **three MacBook Neos** — indigo, citrus, blush —
each played through **Menu Band**, the menu-bar piano. Neapolitan: three flavors
in a block.

It runs as a **`.mbscore`** (see `slab/menuband/scores/README.md`): a JSON score
declaring `machines: 3` and one **voice** per machine. The conductor fires every
voice at one shared `startEpoch`, so all three lock to the same downbeat over
NTP-synced wall clocks — no LAN, no cable between them. `lead` is the count-in
before that downbeat.

## What makes it worth drawing

The dialogue is real and it is in the file format:

- **Hocket** (`whistle-hocket.mbscore`) — one melody split note-by-note so it
  bounces machine to machine, each playing every other eighth note.
- **Circle trade** (`circle-trade.mbscore`) — call-and-response; the machines
  trade arpeggios every bar, and whoever is not soloing holds the bass root.
- **One downbeat** — three separate computers, no wires, one pulse.

So: three machines that pass a tune between them, take turns, and hold each
other up. That is a conversation, and it is what the pictures should show.

## The instrument stack on each machine

- **Menu Band** — a mini multi-segment keyboard drawn *inline in the menu bar*,
  beside the clock, the WiFi, and the battery. Popover shows a staff with
  colored note dots, chord chips, and a piano-mapped QWERTY grid.
- **TrackDrum** (`slab/tracktramp/`) — turns the trackpad into Menu Band's
  percussion surface. It is **not** a plain membrane with ripples: it is an
  outside-to-centre instrument map of nested rounded-rect zones, taken straight
  from `Sources/TrackDrumIcon.swift` — sage **hi-hat** outermost, terracotta
  **snare** with diagonal wires, ochre **tom**, dark umber **kick** at the
  centre. Draw order in the source is sage → terracotta → ochre → umber; keep it.
  Render the real artwork with `./render-icon.sh out.icns workdir` and use the
  1024px PNG it leaves in the work dir; do not draw it from memory.
- **No dots.** @jeffrey: *"i dont want spots on the trackdrum parts."* The two
  white touch points and the accent tether were removed from the icon source —
  it now reads *"No touch dots or finger tether yet."* Nothing sits on top of the
  zones. Re-render the reference icon after any change to that file.

The keyboard is the piano. The trackpad is the drum. The menu bar is where the
instrument lives.

### The TWELVE semitone keys

Derived from `labelByMidiNotepat` in
`slab/menuband/Sources/MenuBand/KeyboardIconRenderer.swift:613` — a semitone is
`midi % 12 ∈ {1,3,6,8,10}`. The raised black/accidental keys are:

`z`=A♯3 · `v`=C♯4 · `s`=D♯4 · `w`=F♯4 · `r`=G♯4 · `q`=A♯4 · `t`=C♯5 · `y`=D♯5 ·
`u`=F♯5 · `o`=G♯5 · `p`=A♯5 · **`'`=C♯6**

That is **twelve**, not eleven. An earlier version of this brief committed to
"exactly eleven — Q W R T Y U O P S Z V" and omitted the apostrophe, which is a
real semitone at the top of the range. The apostrophe key must be present in the
drawn layout *and* accent-coloured.

**E and I stay white naturals** (`e`=E4, `i`=D5), as do A D F G H J K L, the
semicolon `;`=C6, and X C B N M.

Do not use the E/Y/D/G/K pattern shown inside the Menu Band popover screenshot —
that is the popover's own compact layout picker, not the physical keycaps.
"Notepat" is one of Menu Band's three modes, so the notepat map is the correct
one for the machines in this piece.

## Colorways — verified against Apple, 2026-08-09

MacBook Neo shipped March 2026 at $599 in **four** colours: **blush, indigo,
silver, citrus**. Apple: *"Choose from four stunning colours with
colour-coordinated keyboards,"* which *"extend to the Magic Keyboard in lighter
shades."*

**There is no "blueberry" and no "rose."** Use Apple's names.

| Machine | Official colour | What it actually looks like | Accent semitone keys |
| --- | --- | --- | --- |
| neo | **citrus** | bright lemon yellow — *"a bright yellow gold… the most aggressive laptop color Apple has made since the days of the tangerine iBook"* | blue-violet |
| blueberry | **indigo** | dark slate navy — *"a somewhat lighter cousin to the Midnight MacBook Air"* | citrus yellow |
| the third | **blush** | soft dusty desaturated pink, warm and matte — *"less bubblegum and more barely-there"* | deep teal |

Body colour and accent colour stay contrasting partners, per the established rule
in `slab/menuband/marketing/notepat-launch/`. Giving the indigo machine *citrus*
accents ties the trio to itself.

### Two corrections worth keeping in mind

- **Citrus is yellow, not green.** `refs/neo-overhead-citrus.jpg` and
  `refs/neo-twotone-citrus-blue.jpg` in this repo both read pale mint green, and
  the older prompts in `recap/` call it "chartreuse." Apple's own press photo is
  unambiguously lemon yellow. The repo refs are the odd ones out — check against
  the real machine, since @jeffrey owns it.
- **Blush is very desaturated.** Much paler and dustier than a poster pink wants
  to be. Pushing saturation for legibility is a deliberate choice, not accuracy.

### Measured body colours

Averaged from a patch inside the lid of each Apple press photo (studio-lit, so
these read a little lighter than the real anodising):

| colourway | measured | rgb |
| --- | --- | --- |
| blush | `#dfc7c7` | 223, 199, 199 |
| indigo | `#495369` | 73, 83, 105 |
| citrus | `#d8d680` | 216, 214, 128 |

**The citrus test: R ≈ G.** Real citrus measures R216 G214 — a true yellow. Any
value where G exceeds R by more than a few points has drifted to chartreuse,
which is the mistake every earlier take made.

Keycaps are pale near-white tinted *toward* the body colour, never darker than
it — true of the real product and required by the piano rule, where the naturals
are the white keys.

### Deterministic renders

`refs-extra/canon-neo-{citrus,indigo,blush}.png` are stamped by
`toolchain/keyboard/render-laptop.mjs`, not generated. The semitones come off
the same map the software plays from, so they cannot drift. Use them as
references — and as the thing generated art is checked against.

```bash
node toolchain/keyboard/validate-keyboard.mjs
node toolchain/keyboard/render-laptop.mjs out.png --colorway blush
```

### Reference photos

`refs-extra/apple-neo-{blush,indigo,citrus,color-lineup}.jpg` are **Apple press
photos, © Apple**, published by Apple Newsroom for editorial use and kept here as
colour reference. They are committed to a public repository, so treat them as
what they are: someone else's photographs. Do not composite them into published
designs, and do not present them as our own work. Re-fetchable from the URLs in
this brief if they ever need to be removed.

## Style

Continues `slab/menuband/marketing/notepat-launch/illy-*.png`: warm hand-drawn
painterly illustration on a soft off-white paper ground, confident inked
outlines, gentle gouache shading, hand-drawn grain, subtle drop shadows.
Poster-like, never photographic. No text, no logos, no brand marks.

## Outputs

Generated 2026-08-09, gpt-image-2, 1536×1024 (3:2), marketing pipeline,
`physical-accuracy` contract. Prompts beside each output; provenance in `gens/`.

**`gens/block.png`** — overhead Neapolitan block. Direct sequel to the
notepat-launch series. Rerolled twice; earlier takes in `gens/archive/`.
> Alt: Three thin laptops lie open in a row, seen from directly above — dark
> slate indigo, bright lemon citrus, soft dusty blush. Each has an ordinary QWERTY
> keyboard in which twelve keycaps — Q, W, R, T, Y, U, O, P, S, Z, V and the
> apostrophe — are filled in a contrasting colour as a piano's black keys: yellow
> on the indigo machine, violet on the citrus, teal on the blush. Each trackpad
> carries a set of nested rounded rectangles in sage, hatched terracotta, ochre
> and dark brown — hi-hat, snare, tom and kick — with nothing resting on
> them. Every screen shows the same small interface: a lit keyboard
> strip in the menu bar, a dark panel with three coloured notes on a staff, a row
> of chord chips, and a grid of keys. Musical notes hop in dotted arcs from the
> first machine to the second and the second to the third, and a long arc curves
> back underneath from the third to the first, closing the circle.

**`gens/trio.png`** — Jeffrey in the middle, three machines in dialogue.
> Alt: A man with shoulder-length brown hair sits cross-legged on a wooden floor
> in the centre, hands resting open on his knees, listening. Three open laptops
> stand on the floor around him in a wide ring — dark slate indigo at his left,
> lemon citrus behind, dusty blush at his right — each turned inward toward the
> circle. Every screen shows the same small interface: a lit keyboard strip in the
> menu bar, a dark panel with coloured notes on a staff, a row of chord chips and
> a grid of keys. Musical notes travel in coloured arcs from machine to machine
> around the ring, and a faint pale circle is drawn on the floorboards through all
> three, like a chalk line.

**`gens/menubar.png`** — the argument in close-up.
> Alt: Extreme close-up of the top corner of a dusty blush-pink laptop screen.
> Along the menu bar, evenly spaced and all the same size, sit a WiFi symbol, a
> battery symbol, a small segmented keyboard strip with three segments lit in
> teal, and an unreadable suggestion of a clock. Five small musical notes rise
> from the lit segments and drift off the top of the frame.

### Notes on the results

- **Retracted:** an earlier version of this brief claimed `block.png` was
  "correct on all three counts." It was not. The keymap was drawn to an
  eleven-key spec that was itself short one semitone (`'`), and the trackpads
  carried touch dots that no longer exist in the source.
- **Current `block.png` (18:07 take)** — all twelve semitones present and
  coloured on all three machines, including `'`; `;` correctly left white beside
  it; `I` correctly left white; trackpads bare with no dots or tether; zone order
  sage → hatched terracotta → ochre → umber, matching the icon source; colours
  matching Apple's press photos. Verified by zooming each keyboard, not assumed.
- A diffusion model cannot be trusted to *count* twelve keycaps. It got there
  here, but every future take needs the same zoom check — which is exactly the
  argument for `toolchain/keyboard/` stamping the layout deterministically.
- `trio.png` and `menubar.png` **are now out of date on two counts**. They carry a
  generic piano strip rather than the notepat eleven, `trio.png`'s screens do not
  show the Menu Band interface, and — more visibly — both use the old wrong
  colours: a green "citrus" and a saturated "rose" that the real machines do not
  have. `menubar.png`'s bezel in particular is far pinker than real blush. Reroll
  both against `block.illy.txt`'s colour block and sections 1–3 before either is
  shown to anyone.
- Small residual drift in `block.png`: the trackpad zone order reads
  sage → ochre → terracotta → umber, where the real icon is
  sage → terracotta → ochre → umber. Not worth a reroll unless it bothers you.
- The rose machine is **invented** — there is still no reference photo. Verify
  against the real machine before publication.

### Prompt trap — do not repeat

"Colour every keyboard like a piano" made the model **delete the QWERTY keyboard
and draw a literal piano** on the laptop body. The fix is to say explicitly that
the machine keeps its ordinary computer keyboard, that no piano keyboard appears
anywhere in the picture, and that the piano idea lives only in how the existing
square keycaps are coloured. See section 1 of `block.illy.txt`.
