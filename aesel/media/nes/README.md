# NES toolkit

`/medium nes` makes a real NES cartridge from one 6502 assembly file. The
assembler (`asm.mjs`) ships with Aesel, so there is nothing to install.

A project holds `main.asm`, its `nes.json` recipe and, after a build,
`game.nes` with `build.json`. Saving source assembles it. A source that does
not assemble is still saved with its line-numbered errors, and the last good
ROM stays the preview; export refuses until the source builds again.

## Source

The file is the whole cartridge, top to bottom: the 16-byte iNES header, the
program ending in `.dw nmi, reset, irq` at `$FFFA`, then the graphics.
`starter.asm` is an original NROM example: a diamond moved with the D-pad, a
beep on A.

The dialect is asm6's, as most NES tutorials use:

- labels end in a colon; `@name` labels belong to the label above them
- `name = value` constants; `$hex`, `%binary`, `'c'`; `<` and `>` take the low and high byte
- `.db` `.dw` `.dsb COUNT, FILL` `.hex`
- `.base ADDR` sets the address; `.org ADDR` and `.pad ADDR` fill up to it; `.align` `.fillvalue`

Official opcodes only. There are no macros, includes or `.incbin`, so graphics
are `.db` rows in the same file.

## Checks

`rom.mjs` requires an iNES or NES 2.0 header, a file exactly as long as the
header declares, a reset vector inside the cartridge, and a mapper AC's player
runs: 0, 1, 2, 3, 4, 5, 7, 9, 11, 34, 38, 66, 71, 79, 94, 118, 119, 140, 180,
240, 241. The player is `system/public/nes-player` (JSNES 2.1.0); the test
suite boots the starter in it and presses buttons.

No commercial ROM or Nintendo asset is bundled.
