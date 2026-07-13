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

## Known gap: this is not yet the *default* Set

Getting Live to use this for **New Live Set** is unsolved. Two dead ends worth recording so nobody
repeats them:

- `~/Library/Preferences/Ableton/Live <ver>/BaseFiles/` is **not** the default-set source. Live copies
  any Set opened from outside the User Library into there as a working copy. Dropping an `.als` there
  changes nothing — verified across a full Live restart.
- `File > Save Live Set As Default Set…` did not visibly commit a default in testing; it surfaced a
  rename field on a new `Templates/Untitled.als` instead. The mechanism still needs pinning down.
