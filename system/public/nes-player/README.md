# NES in MIME

The feed plays `.nes` files as `application/x-nes-rom`. The website's **+**
opens the ROM importer; signed-in handles can publish through `/api/mime`.
The native MIME feed uses the same player and has a Files importer.
NES cards use `nes~<code>` identifiers, such as `https://mime.ac/nes~qst`.
`/nes-player/` also opens a local ROM without uploading it.

The pinned JSNES 2.1.0 core supports iNES and NES 2.0 headers and these cartridge
mappers: 0, 1, 2, 3, 4, 5, 7, 9, 11, 34, 38, 66, 71, 79, 94, 118, 119, 140,
180, 240, 241. Unsupported console variants and mappers show an error. Uploads
are bounded to 8 MB and checked for header sizes, trainers and truncation before
the emulator allocates ROM banks. Archives and Famicom Disk System images are
not handled by this player. Compatibility depends on the core and cartridge.

Keyboard: arrows, X for A, Z for B, Enter for Start, Shift for Select. Touch
supports simultaneous buttons; standard gamepads use their first two face
buttons, D-pad/stick, Start and Select. Sound starts after a gesture. Offscreen
cards, covered feed views and hidden documents pause the CPU and audio, release
buttons and retain their game while the runtime remains in MIME's nearby-card
pool. Save states and battery RAM persistence are not implemented.

Validation:

```sh
node --test system/backend/mime-nes.test.mjs
PLAYWRIGHT_CHANNEL=chrome node --test system/tests/mime-nes-browser.test.mjs
```

The tests assemble an original NROM diagnostic from
`system/tests/fixtures/nes-input.mjs`; no third-party game ROM is included.
JSNES's source, version and license are recorded in `vendor/`.
