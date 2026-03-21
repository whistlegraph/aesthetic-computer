![hUGEDriver](https://github.com/SuperDisk/hUGEDriver/assets/1688837/a6079751-20b5-4db3-bb48-0e748234f8ca)
===

This is the repository for hUGEDriver, the music driver for the Game Boy which plays music created in [hUGETracker](https://github.com/SuperDisk/hUGETracker).

If you want help using the tracker, driver, or just want to chat, join the [hUGETracker Discord server!](https://discord.gg/abbHjEj5WH)

## Quick start (RGBDS)

1. Export your song in "RGBDS .asm" format in hUGETracker.
2. Choose a *song descriptor* name. This is what you will refer to the song as in your code. It must be a valid RGBDS symbol.
3. Place the exported `.asm` file in your RGBDS project.
4. Load `hl` with your song descriptor name, and `call hUGE_init`
5. In your game's main loop or in a VBlank interrupt, `call hUGE_dosound`
6. When assembling your game, be sure to specify your music file and hUGEDriver.asm in your call to `rgbasm`/`rgblink`!

Be sure to enable sound playback before you start!

```asm
ld a, $80
ld [rAUDENA], a
ld a, $FF
ld [rAUDTERM], a
ld a, $77
ld [rAUDVOL], a
```

See the `rgbds_example` directory for a working example!

## Quick start (GBDK)

1. Export your song in "GBDK .c" format in hUGETracker.
2. Choose a *song descriptor* name. This is what you will refer to the song as in your code. It must be a valid C variable name.
3. Place the exported .C file in your GBDK project.
4. `#include "hUGEDriver.h"` in your game's main file
5. Define `extern const hUGESong_t your_song_descriptor_here` in your game's main file
6. Call `hUGE_init(&your_song_descriptor_here)` in your game's main file
7. In your game's main loop or in a VBlank interrupt, call `hUGE_dosound`
8. When compiling your game, be sure to specify your music file and `hUGEDriver.o` in your call to `lcc`!

Be sure to enable sound playback before you start!

```c
NR52_REG = 0x80;
NR51_REG = 0xFF;
NR50_REG = 0x77;
```

See `gbdk_example/src/gbdk_player_example.c` for a working example!

## Usage

This driver is suitable for use in homebrew games. hUGETracker exports data representing the various components of a song, as well as a *song descriptor* which is a small block of pointers that tell the driver how to initialize and play a song.

hUGETracker can export the data and song descriptor as a `.asm` or `.c` for use in RGBDS or GBDK based projects, respectively. Playing a song is as simple as calling hUGE_init with a pointer to your song descriptor, and then calling `hUGE_dosound` at a regular interval (usually on VBlank, the timer interrupt, or simply in your game's main loop)

In assembly:
```asm
ld hl, SONG_DESCRIPTOR
call hUGE_init

;; Repeatedly
call hUGE_dosound
```

In C:
```c
extern const hUGESong_t song;

// In your initializtion code
__critical {
    hUGE_init(&song);
    add_VBL(hUGE_dosound);
}
```

Check out `player.asm` for a full fledged example of how to use the driver in an RGBDS project, and `gbdk_example/gbdk_player_example.c` for usage with GBDK C likewise.

### `hUGE_mute_channel`

**Caution**:
As an optimization, hUGEDriver avoids loading the same wave present in wave RAM; when "muting" CH3 and loading your own wave, make sure to set `hUGE_current_wave` to `hUGE_NO_WAVE` (a dummy value) to force a refresh.

## License

hUGETracker and hUGEDriver are dedicated to the public domain.
