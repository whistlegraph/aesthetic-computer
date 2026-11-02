# 01 · Positioning notepat and AC Native OS

**Shelves:** all · **Consumer:** `arxiv-laptop-performance`

Four claims, each with its nearest prior, how strong it is, and the study that
would settle it.

## 1. The mapping is literate, not positional

notepat's `c d e f g a b` keys play C D E F G A B; the next octave runs
`h i j k l m n`; sharps sit on `v s w r q`
(`system/public/aesthetic.computer/disks/notepat.mjs:634`).

- **Nearest priors.** Steiner's *TypistInstrument* (2005) also derives pitch
  from the letter rather than its position, but by English letter frequency
  (`ETAOINSR` become the scale). US6444888B1 (2001) assigns pitches by how
  often they are played. Virtual Piano sheets write music as letters, but the
  letters name keys, not pitches.
- **Status.** No orthographic prior found in two targeted searches.
  Provisional.
- **Settle it.** A wider search (patents, Japanese and German web pianos,
  pre-web shareware), then a learnability study: novices learn AWSED vs.
  notepat, scored on unprompted recall after 24 hours.

## 2. The instrument is the laptop's own keys, on a laptop that boots into it

- **Nearest priors.** Fiebrink, Wang & Cook (2007) for native inputs; MOD
  live-USB for booting a PC into one music appliance; dyne:bolic and Puredyne
  for booting old computers into media studios; Satellite CCRMA, Bela, norns
  and Organelle for the dedicated box.
- **Status.** Defensible only in its narrow form: boot-to-instrument (not
  boot-to-studio), played on the built-in keyboard (no interface required),
  on surplus x86 laptops.
- **Settle it.** Document cold-boot-to-first-note time across the fleet, and
  compare against a MOD live-USB boot on the same hardware.

## 3. The latency is inside the intimacy budget on HDA hardware

`arxiv-latency` reports ~3–4 ms key-to-DAC on HDA-direct codecs and
12–22 ms on SOF. Wessel & Wright set 10 ms with 1 ms jitter.

- **Status.** Measured for HDA; SOF is outside. Jitter is not yet reported.
- **Settle it.** Report jitter, not just the mean. Run a Claptoscore-style
  tap-to-click calibration on AC OS, the browser build, and GarageBand on one
  machine.

## 4. A typed melody answers the liveness critique

The critique (Cascone; Stuart; Collins) says the audience cannot see the
cause. Bown et al. (2014) found that listeners hear triggering as live.
A typed melody is pure triggering: one key, one note.

- **Status.** Argued, not tested.
- **Settle it.** Replicate Bown et al. with audio-only excerpts of notepat
  performances, then add a video condition that shows the hands and the
  on-screen letters.

## Hardware ceilings to state honestly

USB boot-protocol keyboards report six keys plus modifiers. Laptop matrices
ghost or jam on some three-key chords. There is no velocity unless the
hardware adds it (the NuPhy analog stream in AC OS; the accelerometer in
"The 'E' in QWERTY"). Chords larger than three notes are therefore unreliable
on stock laptops. That is a constraint on the repertoire, not a bug to hide.
