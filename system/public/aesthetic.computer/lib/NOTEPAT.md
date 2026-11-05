# Notepat web controls

The two 4 × 3 octave grids are the main touch interface. They stack in portrait
and sit side by side in landscape.

Tap the sound name to open Sound & MIDI. Record a phrase starts capture and
closes the panel; tap REC to reopen it and stop/play the recording. Choose a Notepat voice or any
of the 128 General MIDI instruments. On a keyboard, type 1–128 within 800 ms
between digits; 0 toggles local sound, leaving selected MIDI output active.
Tab continues to cycle Notepat voices. Escape or Stop all notes releases voices.

Connect MIDI requests browser permission. Choose all inputs or a specific input,
an output (off by default), and output channel 1–16. Input supports exact note
pitch, velocity, velocity-zero note-off, sustain CC64, ±2-semitone pitch bend,
program change, reset controllers CC121, all notes off CC123 and all sound off
CC120. Device removal, input changes, page blur and leaving the piece release
owned notes. Incoming notes never echo to the selected output. Touch and typing
send notes to that output. Port names and IDs remain local.

Web MIDI needs a supporting browser and a secure context. It uses existing
hardware or OS MIDI ports; it cannot create the native app’s virtual CoreMIDI
port. Modulation, aftertouch, MPE, MIDI clock and SysEx are not implemented.
Percussion shortcut keys keep their existing local sound behavior.

GM samples use the main browser AudioContext through `notepat-gm-bridge.mjs`.
Pending notes are canceled on release so a slow download cannot play a late
note. Sample voices share speaker gain and recording routing; worklet room and
glitch effects currently apply to the original Notepat synthesis path.

Checks: `node --test system/tests/notepat-midi.test.mjs
system/tests/notepat-pathway.test.mjs system/tests/visit-tracking.test.mjs`.
Rebuild and verify the disk worker when changing its event bridge.
