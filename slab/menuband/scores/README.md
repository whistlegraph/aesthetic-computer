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

## Composing for more machines

Set `machines` to 3+ and add that many voices (SATB choir, a rhythm section, a
round). The conductor scales to whatever roster you hand it; a score composed
for 4 needs 4 hosts. Clock sync is NTP across the fleet (~tens of ms in
practice), tight enough for ensemble playing.
