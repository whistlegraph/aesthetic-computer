# Voicings — *Note(s)pat(ial) Native* in other orchestras

One score, several orchestras. The composer maps each instrument family to a
recipe, so the same 12:54 can be heard as sine stacks, as a mallet ensemble,
as the engine's own flute, harp and piano, or as General MIDI programs, and
the studio can try a family at a time. Nothing about form, timing, seats or
the hops changes; only what each voice is made of. Drums, taps and the crash
are the same in every voicing. Percussion carries the beat through all
eleven chapters; `--kick` adds a restrained kick on their main steps.

```sh
node fedac/native/tools/compose-notespatial-native.mjs                       # sine (the September 22 orchestra, default)
node fedac/native/tools/compose-notespatial-native.mjs --voicing mallets
node fedac/native/tools/compose-notespatial-native.mjs --voicing native
node fedac/native/tools/compose-notespatial-native.mjs --voicing gm
node fedac/native/tools/compose-notespatial-native.mjs --fx studio            # sine, with the effects ribbons
node fedac/native/tools/compose-notespatial-native.mjs --voicing mallets --set held=whistle,bass=sawBass --fx studio
```

Each run writes `scores/notespatial-native<-tag>.nsscore` and one file per
chapter (`…<-tag>-8-the-lift`), so any mix can be deployed whole or a chapter
at a time. The tag is the voicing, any `--set` pairs, and `-fx`.

## The families and what is on the menu

| Family | Where it plays | sine | mallets | native | gm |
|---|---|---|---|---|---|
| held | the laptop in the hands, nine chapters | bell (sine + octave + twelfth, warble) | vibraphone | **whistle**: the engine's STK flute waveguide, breath and 5 Hz vibrato built in | 73 flute |
| ring | the walk, the Overture's passes, the Waltz tune, the Lullaby cradle | pluck | marimba (rosewood) | **harp**: Karplus-Strong | 12 marimba |
| echo | the ring's answers to the theme | soft sine + octave | kalimba | harp | 8 celesta |
| pad | three orbiting pads, Climb, Lift, Lullaby, Return | sine, 600 ms attack | gamelan bar struck softly, two partials | detuned sawtooth pair (−7 cents) | 89 warm pad |
| bass | seat 4; oom; the Sneak's creep | triangle + residue octave and twelfth | bass marimba with residue | sawtooth + octave | 38 synth bass 1 |
| top | the Climb's high line | sine | glockenspiel | whistle | 79 ocarina |
| brass | the Fanfare | triangle + sine octave | gamelan | sawtooth + square at +6 cents (chorus) | 61 brass section |
| theme | the travelling theme in the Lift | bell-like sine + octave | marimba | harp | 11 vibraphone |
| answer | the Lift's counter-hopping answer | triangle | xylophone | **piano**: the Salamander bank | 71 clarinet |
| stacc | the Chase's runners, the Sneak's scurry | sine (triangle for the pursuer) | xylophone | harp | 13 xylophone |

Recipes available to `--set`, by name: `bell pluck soft padSine bassResidue
topSine brassTri themeSine answerTri staccSine` (sine), `marimba xylophone
vibraphone glockenspiel gamelan kalimba woodblock gong bassMarimba` (modal
stacks from `pop/marimba/synths/marimba.mjs`: Rossing's ratios and T60s, a
30 ms mallet tick, rings halving every two octaves), `whistle harp piano
sawBass sawBrass sawPad squareReed` (the engine), `gmFlute gmMarimba gmCelesta
gmPad gmBass gmOcarina gmBrass gmVibes gmClarinet gmXylo` (GM by number).

How the runtime hears them: a `wave` is passed as the synth `type`; an event
with `gm` is passed as `gmProgram`. ACOS implements all 128 GM programs
(`gm_program_implemented` in `src/gm_synth.c` covers 0 to 127), the same
engine notepat's instruments play through, so every laptop has the whole
menu; the wave is only a fallback for an older image. `piano` is the
Salamander bank the image carries in `/samples/piano`.

## Effects: `--fx studio`

The engine has four global effects with smoothed dry/wet mixes: **room** (a
delay-line reverb), **drive** (tanh soft clip), **wobble** (a flanger) and
**glitch** (sample-hold plus bit-crush). The score carries them as ribbons
over its duration (`fxRoom`, `fxDrive`, `fxWobble`, `fxGlitch`), with a
per-seat override in `seatFx`; the playback piece applies them ten times a
second and returns every mix to zero when the piece stops. The studio plan:

| Chapter | room | drive | wobble | glitch |
|---|---|---|---|---|
| Overture | .25 | | | |
| The Walk | .10 | | | |
| Waltz | .15 | | | |
| Chase | 0 | | | .70 for 0.5 s at each crash |
| Sneak | .10 | | .30 | |
| Lullaby | .40 | | | |
| The Climb | .15 | | .15 | |
| The Lift | .10 | .35 | | |
| Fanfare | .20 | .25 | | |
| Return | .25 | | | |
| Vanish | .50 | | | .70 for 0.5 s at the hang-up |

The held laptop's room sits 0.15 above the ring's everywhere. Values glide
over the first second after each chapter door. An effect is colour on a
seat, not a way to move a sound (digest 05, R3): the room is global per
machine, so a wetter seat is a wetter *place*, which is the point.

Edit the plan in the composer (`ROOM`, `DRIVE`, `WOBBLE`, the `glitchOf`
window) and rebuild; or set a mix by hand from the prompt on one laptop.

## Voice budget

The runtime has 32 voices per machine and steals the oldest when full. Peak
polyphony with continuous percussion: sine 26, mallets 29, native 23,
gm 17; mallets with `--kick` peaks at 28. A caller asking for a
plain sound (the tutti chords, fills and pickup laps) gets the fundamental
alone, as in the sine voicing.

## Trying it in the studio

```sh
# hosts in seat order: five ring laptops clockwise from the front, then the held laptop
node fedac/native/tools/spatial-rehearsal.mjs deploy --score notespatial-native-mallets-8-the-lift H1 H2 H3 H4 H5 HELD
node fedac/native/tools/spatial-rehearsal.mjs cue H1 H2 H3 H4 H5 HELD
node fedac/native/tools/spatial-rehearsal.mjs deploy --score notespatial-native-native-3-waltz H1 H2 H3 H4 H5 HELD
node fedac/native/tools/spatial-rehearsal.mjs deploy --score notespatial-native-fx H1 H2 H3 H4 H5 HELD
```

Deploy ships the playback piece with the score, so the GM pass-through and
the effects need no reflash. Listen for, per voicing:

- **mallets**: does the marimba walk still *place* on a laptop (the 30 ms
  tick is what carries it), and does the vibraphone in the hands hold the
  long Overture bells or die too early?
- **native**: the whistle's breath noise and vibrato on the small speaker;
  whether harp plucks flam across seats in the Chase; the piano answer's
  level against the harp theme.
- **gm**: the flute and the brass section are waveguides and may need their
  gain trimmed against the sine stacks; the celesta echoes may want +3 dB.
- **fx**: the room on the held laptop during the Lullaby, the drive on the
  Lift, and whether the glitch on the crash reads as a hit or a fault.

Previews heard from the center (`--fast` parametric head) for the Waltz and
the Lift in each voicing render in about 20 s each:

```sh
node fedac/native/tools/notespatial-native-render.mjs fedac/native/scores/notespatial-native-mallets.nsscore --section 8 --fast --out ~/Desktop/notespatial-voicings/mallets-8-lift.mp4
```

The render stands in for whistle, harp and piano with harmonic sketches.
GM scores and effects scores are rejected rather than silently dropping
their timbres or effects; the laptops are the place to hear those. Mallet
partials render directly from the score. Add `--sub` to model the room's
separate bass output; see [the full render recipe](NOTESPATIAL-ARRANGEMENT.md#mallets-kick-and-sub-preview).
