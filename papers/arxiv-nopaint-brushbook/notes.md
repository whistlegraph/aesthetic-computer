# The No Paint Brushbook — notes

Begun 2026-11-18 on the working tree at 2bfe1fc1ff.

## Occasion
@jeffrey, while hardening the 2021 No Paint build so it can be improved for a
while: "an e2e test for the brushes to make sure they all work", then "let's
build out a manual", "a latex /papers style nopaint.art brushbook which has all
brushes and screenshots of their effects on blank canvases", using "the shared
modules of /spells and our spellbook", "our whole render e2e pipeline". The
book is a sister to Aesthetic Spells (papers/arxiv-magicians-manual).

## How it is made
1. `extract-brushes.mjs` reads the game's help pages (function ShowHelp) and
   its two weighted tables out of the Construct source, `nopaint/construct/`
   (a private product directory), and writes `brushes.json`.
2. `tests/browser/brushbook.e2e.mjs` runs every brush in headless
   Chrome on a fleet Mac (panda), twice, through the bridge that
   `toolchain/nopaint/patch-classic.mjs` appends to the build. `npm run
   test:nopaint:brushes` does the copying and brings the results home.
3. `gen-entries.mjs --from <results>` trims the results into
   `e2e-summary.json`, makes one strip per brush in `shots/` and `cover.jpg`,
   and writes `entries.tex`, `tables.tex` and `tally.tex`.
`npm run nopaint:brushbook` does 2 and 3.

## Shared with Aesthetic Spells
- `papers/ac-booklet.sty`: the half-letter page, margin side-heads, running
  cues, fields and heads. Lifted from magicians-manual.tex, which still has
  its own copy; it was not changed, so its recorded visual review stays valid.
- `tests/browser/book-kit.mjs`: launch, page log, screenshots, clips with
  taped audio, results file, a folder server and a network block.
  `spellbook.e2e.mjs` was moved onto it on 2026-11-18.

## Design (second pass, 2026-11-18)
@jeffrey on the first build: "needs like boxes around typographic elements",
"more alignment / color coding in the grid", "everything feels super floaty",
"the spine we can lax cuz its not meant to be printed with a spine", "we need
more of a design system there". What changed:
- `ac-booklet.sty` got a `[screen]` layout: one page design, a left rail for
  side-heads, full-width blocks over rail and column.
- Each brush is a card of fixed height, two to a page: a band in the kind's
  colour (green brush, blue filter, dark by name), the sentence, two ruled
  grids (options left; odds, typed, ran as, test right), three framed pictures
  with coloured labels along the bottom edge.
- Everything typed is in a box; options are orange; painters are on black in
  the colour the game's help gives them; test results are green or red chips.
- Stamp's 17 painters moved to a back table so every card has the same parts.
  Cards with more than four option rows pair them.

## Read in the source, not run
- The even pick between the two tables: `AdvancedRandom.setProbabilityTable(choose("Brushes", "Filters"))` in the Index and Core sheets.
- Blank or `?` at the prompt picks at random (Core, function GoToBrush).
- The page credit is the `author` meta tag of the served index.html.
- Save gives an eight-character code (system/backend/nopaint-classic.mjs); the
  book's "shown in the corner" follows the game's Load help text.

## Seen in the test
- A new painting is a flat colour with grain, different on each run. Why (a
  random tint and the noise script Core_Event16_Act4 is my reading) was not
  traced in the source.
- Options the game chose are quoted from its `parameterHint`.

## Found while making it
- The game's help gives `Softy: 128 128 ? S` as an example. Run on 2026-11-18
  it became `128 128 ? ?`: the options are X Y Size Color, so `S` in the colour
  place is dropped. The book prints `Softy: 128 128 S ?`, which ran as a small
  streak in a colour the game chose.
- Pressing Paint while a playlist is between its own steps left the game's
  Painting flag up for 100 seconds and it did not return (one run, through the
  bridge, not a finger). The test now only watches a playlist.

## Not verified
- Nobody followed the book at a real phone.
- Sound was not checked. The clips have it taped; nobody listened.
- `Load` was not run. `Playlist` ran as `Playlist: Smiley` only.
- `Camera` ran with Chrome's stand-in camera.
- The odds are computed from the weights; no run counted real proposals.
- The pictures show one run with random options. Another run looks different.
