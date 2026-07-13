# Building a stripped-down Live template (`als-template.mjs`)

Live 12's stock "New Live Set" hands you 2 MIDI + 2 audio + 2 return tracks. The fleet wants a
genuinely blank Set — **one input channel + the Main out, nothing else**. Live has no CLI for that,
and its UI can't always be driven (a locked Mac has no UI, and Live throws blocking modals), so we
shape the `.als` directly.

```bash
node als-template.mjs                                  # 1 audio + Main, no MIDI, no returns
node als-template.mjs --audio 1 --midi 0 --returns 0 --out AC-Blank.als
node als-template.mjs --xml-out AC-Blank.xml           # also emit the XML to read
```

## Format

An `.als` is **gzipped UTF-8 XML** rooted at `<Ableton><LiveSet>` — see `docs/ableton-als-structure.md`.
The repo already *reads* `.als` three ways (`saxes` here, `pako`/JSZip in `bios.mjs`, native
`DecompressionStream` + regex in `wipppps.mjs`); this is the only thing that *writes* one.

`live-12-blank.xml` is a stock blank Set, already decompressed — the base we carve from.

## What the strip has to get right

Removing whole `<MidiTrack>` / `<AudioTrack>` / `<ReturnTrack>` elements is most of the job, but one
invariant is easy to miss: **every track carries one `<TrackSendHolder>` per return track**. Drop the
returns without truncating each surviving track's `<Sends>` and the Set is inconsistent. With zero
returns, every `<Sends>` must be empty.

Everything else is self-contained, which is what makes this safe:

- `TracksListWrapper` / `VisibleTracksListWrapper` / `ReturnTracksListWrapper` / `SendsListWrapper`
  are empty `LomId="0"` stubs — they do **not** enumerate track IDs, so there is nothing to renumber.
- The stray `Id="12"/"13"/"14"` hits elsewhere in the file are device-macro locals
  (`ModulationTarget`, `NamedRemoteableKeyMidi`), **not** track references.
- `<SendsPre>` and `<SendsListWrapper>` are left alone — matching on the exact string `<Sends>`
  (with the closing angle bracket) never catches them.

## Verify by opening it

Hand-built XML is only correct if Live agrees. Open the result in Live — it should load with exactly
the requested tracks and no repair prompt. (Confirmed for the 1-audio/0-MIDI/0-return default.)

Note the base is Live 12.2.1 schema (`MinorVersion="12.0_12203"`); 12.4.2 opens it fine and
re-serializes on save.

## Installing it

`toolchain/macos/fleet/ableton-template.mjs` builds this and drops it in each fleet Mac's
`~/Music/Ableton/User Library/Templates/`, so it shows up in Live's browser under **Templates > AC
Blank** — one click to a genuinely blank Set.

## Making it the auto-default (⌘N) — the menu-item-by-name trap

**`click menu item "Save Live Set As Default Set..."` silently invokes the WRONG command.** It
reports success, but what actually fires is **"Save Live Set As Template…"**, the item immediately
above it in the File menu. That is why every attempt merely dropped a file into
`User Library/Templates/` and popped a rename field, and why ⌘N never changed.

Press it **by index** instead — item 17 of the File menu (16 is the Template item):

```applescript
tell application "System Events" to tell process "Live"
  perform action "AXPress" of menu item 17 of menu 1 of menu bar item "File" of menu bar 1
end tell
```

With the stripped Set open, that press did the real thing: Live came back with a Set whose entire
mixer was `Audio` + `Main`. **Not yet verified across a Live restart** — that check is the one thing
between this and a fleet-wide installer.

Do NOT trust `click menu item "<name>"` for Live's File menu generally; verify by index and confirm
the effect, because a wrong-but-adjacent command is worse than an error.

## The default Set is a file — and the FILENAME is the whole trick

```
~/Library/Preferences/Ableton/Live <ver>/BaseFiles/DefaultLiveSet.als
```

That is the Set `New Live Set` (⌘N) opens. `DefaultLiveSet` is one of the strings in the Live binary,
and the file turned up for real on neo (which had a hand-made default). `ableton-template.mjs
--default` installs there, backing up any existing default first.

Copies named **anything else** are ignored no matter where they sit — that is what made this so
slippery. `Template.als` also appears in the binary but is a red herring for this purpose; dropping
one into `User Library/`, `User Library/Templates/`, `User Library/Defaults/`,
`Preferences/Live <ver>/`, or `BaseFiles/` changes nothing (all checked across full Live restarts).
`BaseFiles/` is otherwise just where Live copies a Set opened from outside the User Library.

No pointer is written to `Preferences.cfg` — no `.als` path, no default key. It is purely this file.

**Still unverified:** installing `DefaultLiveSet.als` has not been confirmed to change ⌘N across a
restart. Launch Live, press ⌘N, count the tracks.
