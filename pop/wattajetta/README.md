# wattajetta

a fighter jet made entirely of water. part of the *pixsies* body —
companion track to the water-jet image gens (jeffrey in a cockpit where
the fuselage, canopy and control stick are all clear liquid).

## the law of this lane

everything is water, so everything is one of exactly two materials:

- **sine** — water holding a shape. phase-increment oscillators only.
  the kick (pitch-swept, 12ms hole attack), the sub bass (fuselage),
  the doppler flybys, the droplet pings — one material, different
  vessels.
- **spray** — water losing its shape. white noise carved by a gliding
  bandpass. wingtip streams, cloud-punch splashes, afterburner steam,
  the veil bed.

no samples, no other waveforms. the kick sidechains everything else —
water pushed aside, rushing back in.

## arc

138 BPM felt in halftime (kicks on 1+3). taxi → takeoff → climb (two
flybys) → canopy dissolve (kick out, sub rises an octave into vapor) →
afterburner (pentatonic droplet runs) → mist out.

## run

```bash
node pop/wattajetta/bin/render-wattajetta.mjs           # → out/wattajetta.mp3
node pop/wattajetta/bin/render-wattajetta.mjs --score   # print the baked score
```

the composition bakes a text score; `c/wattajetta.c` renders it
(f32le stereo 48k) and `c/run-c.mjs` masters to mp3.
