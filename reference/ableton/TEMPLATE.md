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

## Known gap: this is NOT the auto-default (⌘N)

Making this the Set that **New Live Set** opens is unsolved. ⌘N still gives Ableton's stock
2-MIDI/2-audio/2-return Set. Three dead ends, all verified across full Live restarts, so nobody
repeats them:

- **`BaseFiles/` is not the default-set source.** `~/Library/Preferences/Ableton/Live <ver>/BaseFiles/`
  is just where Live copies a Set that was opened from *outside* the User Library, as a working copy.
  Dropping an `.als` there changes nothing.
- **`File > Save Live Set As Default Set…` behaves like a template save.** It writes the current Set to
  `User Library/Templates/<name>.als` (offering "Untitled" in a rename field) and ⌘N still opens the
  stock Set afterwards.
- **No pointer lands in `Preferences.cfg`.** After the above, the file contains no reference to the
  saved Set, no `.als` path, and no default/template key — so the default isn't recorded there either.

Whatever selects the default lives somewhere this black-box probing didn't reach. Next thing to try
is probably watching Live's file I/O (`fs_usage`/`opensnoop`) during ⌘N to see what it actually reads.
