# Menu Band fleet scores

A **score** is a saved, re-runnable composition for *N* computers. Each
`*.mbscore` file (JSON inside) declares how many machines it is composed for and
one **voice** per machine.

## Double-click to play (host = this machine)

`.mbscore` is a registered Menu Band document type. **Double-click one in Finder**
(or `open foo.mbscore`) and the Menu Band on *that* machine becomes the host and
plays the whole composition locally — all voices layered at one shared start,
robot badge lit. No conductor, no ssh. This is the portable, shareable form. The conductor (`../bin/conduct.mjs`) assigns each voice to a host
and fires them all at one shared `startEpoch`, so the machines lock to the same
downbeat — NTP-synced wall clocks, no LAN or Multipeer required (it posts a
`…menuband.play` DistributedNotification directly on each host, locally or over
ssh).

## Run

```bash
node ../bin/conduct.mjs --list                     # list saved scores + machine count
node ../bin/conduct.mjs prelude-in-c               # show requirements, don't play
node ../bin/conduct.mjs prelude-in-c blueberry neo # perform it across two machines
```

A host token runs **locally** when it matches this machine's hostname (or is
`local`/`self`); otherwise it is reached over **ssh**. Voices are assigned to
hosts in order. If you pass fewer hosts than the score needs, it prints the
requirement and stops.

## Format

```jsonc
{
  "title": "Prelude in C — split",
  "composer": "J.S. Bach (BWV 846), arr. for Menu Band fleet",
  "machines": 2,          // composed for N computers (defaults to voices.length)
  "bpm": 76,
  "lead": 3.0,            // seconds between firing and the downbeat
  "description": "…",
  "voices": [             // one per machine, assigned in order
    { "name": "arpeggio",       "program": 6, "velocity": 100, "notes": "…" },
    { "name": "basso continuo", "program": 6, "velocity": 88,  "notes": "…" }
  ]
}
```

Each voice is a `…menuband.play` payload. Besides `notes` a voice may carry any
play key — `notes2`/`notes3`/`notes4` (parallel tracks on that machine),
`velocity2`…, per-voice `program`/`bpm`, etc. `notes` syntax is
comma-separated `token:beats`, where token is a MIDI note number (60 = middle C)
or a drum letter (`k s h ho c rd cr`), and `r` is a rest —
e.g. `60:0.25,r:0.5,67:1`.

## Collections (an album in one file)

A `.mbscore` may hold **tracks instead of voices**: a list of sibling scores.
Opening it in Menu Band shows the track list — sections, member dots, bpm,
length — and a double-click (or Return / Play) performs that track exactly as
opening its own file would. Stop halts whatever is sounding. ⌘-Space previews
the list; the Finder thumbnail is the album sleeve.

```jsonc
{
  "title": "The MacNeoPolitan Trio",
  "composer": "The machines — neo, blueberry and frisbee — with jeffrey",
  "machines": 3,
  "description": "…",
  "gap": 4,                                   // seconds of hall between tracks when played in order
  "tracks": [
    { "file": "trio-i-birth.mbscore",         // relative to this file (or absolute)
      "title": "I. Birth",                    // hints: title, section, machines, bpm, seconds,
      "section": "Movements",                 //   requiresFleet — cached from the sibling so a
      "machines": 3, "bpm": 132, "seconds": 83.2 }   //   reader can list without opening it
  ]
}
```

A collection has `tracks` and no `voices`; that pair is the whole signal.
`file` is the only required key per track — a reader fills missing hints from
the sibling (and the sibling stays the truth). A track may itself be a
collection; it opens to its own list.

The MacNeoPolitan Trio's album is
`grants/culturehub-la-2026/macneopolitan/scores/macneopolitan.mbscore`, written
by `bin/collection.mjs` there; `node bin/trio.mjs scores/macneopolitan.mbscore`
lists it, `--track N` conducts one track and `--setlist` conducts them all.

## Composing for more machines

Set `machines` to 3+ and add that many voices (SATB choir, a rhythm section, a
round). The conductor scales to whatever roster you hand it; a score composed
for 4 needs 4 hosts. Clock sync is NTP across the fleet (~tens of ms in
practice), tight enough for ensemble playing.
