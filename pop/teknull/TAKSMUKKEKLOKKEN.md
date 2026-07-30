# taksmukkeklokken

The current pass is a two-minute C render at 140 BPM in A minor. Its 4,216
events combine acid, an eight-chord orchestral sentence, bowed and pizzicato
strings, sustained harmonic pads, the CC0 Salamander grand-piano bank used by
AC OS, a four-entry canon, contrary-motion violin, and 7-against-5 clock hands.
The opening withholds kick for two bars, then grows through single-hit and
half-time stages; piano notes retain extended sampled pedal tails.

One binaural listener hears moving sound bodies through interaural delay, head
shadow, elevation-dependent pinna combs, distance, three exact-turn rotations,
and four elastic wobble windows. Kick and sub remain centered.

```sh
sh pop/teknull/c/render-taksmukkeklokken.sh
TAKSMUKKE_HARSH=.82 sh pop/teknull/c/render-taksmukkeklokken.sh
```

The render writes:

- `out/taksmukkeklokken-spatial-orchestral-c.24bit-48k.wav`
- `out/taksmukkeklokken-spatial-orchestral-c.mp3`
- `out/taksmukkeklokken-spatial-orchestral-c.events.json`
- `out/taksmukkeklokken-spatial-orchestral-harsh-c.{24bit-48k.wav,mp3,events.json}`

Optional release-cleared percussion one-shots use the three filenames documented
in `samples/arcade/README.md`. Set `TAKSMUKKE_ARCADE_SAMPLES` to that directory.
Missing files become original synthesized arcade impacts; ripped game audio is
not bundled.

The earlier JavaScript Clock-parser renders remain available for A/B checks:

```sh
node --test pop/teknull/bin/clock-offline-scheduler.test.mjs
node pop/teknull/bin/render-taksmukkeklokken.mjs
node pop/teknull/bin/render-taksmukkeklokken.mjs --cooler
```
