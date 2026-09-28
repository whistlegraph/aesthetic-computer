# Game Boy toolkit

This is the existing Aesthetic Computer Game Boy stack made portable:
`kidlisp-gameboy/README.md` → `C-QUICKSTART.md` → `build.sh` → GBDK `lcc`.
The ROM output is intended for the existing AC WasmBoy runtime under
`system/public/aesthetic.computer/dep/wasmboy`, or a compatible emulator/cart.
It is real cartridge software, not a mock Game Boy skin or a new compiler.

`hello.c` is copied verbatim from `kidlisp-gameboy/src/hello.c`. `starter.c` is
an original interactive sketch using that stack's documented `vsync`, `joypad`,
and sprite patterns: move a diamond with the D-pad and recenter with A. No
commercial ROM, Nintendo game assets, or boot ROM is bundled.

A project contains editable `main.c`, its `gameboy.json` recipe, and—only after
a successful GBDK build—`game.gb` plus `build.json` provenance. GBDK creates the
standard bootable cartridge header. Validation checks its logo, declared ROM
size, header checksum and global checksum. The source and compiler binary hashes
are saved with the ROM hash. Source edits keep the last successful preview and
mark `metadata.sourceAhead`; export requires the source to match the build.
The shared Aesel artifact history owns versioning and rollback.

Install [GBDK-2020](https://github.com/gbdk-2020/gbdk-2020/releases), following its
[getting-started guide](https://gbdk.org/docs/api/docs_getting_started.html).
Set `GBDK_HOME` to the extracted `gbdk` directory. An explicitly configured but
missing toolchain is an error, never silently substituted. Otherwise discovery
checks the monorepo's `kidlisp-gameboy/gbdk`, Aesel's optional local GBDK 4.5.0
pack, then PATH. The optional pack location is
`~/.local/share/easel/toolchains/gbdk-4.5.0/gbdk`.

Builds invoke the same `lcc -o game.gb main.c` workflow as AC, adding only a fixed
AESEL cartridge title. They run in an isolated temporary artifact directory,
with fixed arguments, no shell, no forwarded credential environment, a 20-second
process-group timeout, and bounded diagnostics. Failed compiles preserve the
previous ROM. Source paths and outputs reject symlinks; project file writes are
confined to the artifact.

This first lane supports one GBDK C source, built-in SDK headers, original DMG
hardware, and embedded arrays for graphics/audio. It deliberately rejects custom
includes, inline assembly, compiler pragmas and token-pasting. It does not claim
a hostile-compiler security sandbox, full KidLisp-to-ROM support, assembly editing,
CGB bank management, or native compiler redistribution. Without GBDK, create
still gives editable source and installation instructions; no ROM is fabricated.

Local validation used the official GBDK 4.5.0 macOS arm64 release, with archive
SHA-256 `289ee60e46c5a2785a21e35533f84a5131ed4a063b21b0dbdedc9a10af15bf78`
matching its GitHub release asset digest. Both the original AC Hello source and
the interactive starter compiled into verified 32768-byte cartridges.
