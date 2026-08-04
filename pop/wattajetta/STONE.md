# Wattajetta stone form

The canonical source is the original `wattajetta.mp3` section beginning around
1:51: the bar-64 stone drop. Its identity is locked to these ingredients:

- a continuous tempo climb from roughly 127 BPM to 138 BPM;
- sustained sub plus short offbeat sine gallops;
- dense, swung water-bloop percussion;
- granite FEM glass runs and stone bowls;
- the original 0.85 crunch and global platter accelerando;
- mostly tiny FEM strikes during the first 45 seconds, crossed by five explicit
  10–14 second glass-body blooms, then opening afterward;
- roughly half-density FEM runs and half as many bowls during that sparse
  opening; kick and sine floor remain continuous;
- no inherited scratch-buffer or platter-drag gestures.
- dry, undoubled snare pops on 2 and 4 whose tone and velocity breathe over a
  12-hit cycle;
- stone glass, handbell, tubular, bowl, and one restrained church geometry.
- no white-noise splash at 0:00; the kick and sine floor introduce the track.
- one through-composed pentatonic bell line whose melodic cursor never resets
  at bar boundaries; asymmetric turns prevent eight-note loop repetition.

The two-minute form is four seamless mutations of that vocabulary. Density and
stereo placement evolve, but the four-on-floor and offbeat sine gallop never
stop. There are no midsection breakdowns and no alternate mix variants.

```bash
node pop/wattajetta/bin/render-wattajetta.mjs --stone-study
```

The output is `out/wattajetta-stone-canonical.mp3`. The FEM cache in
`out/.wattajetta-bell-cache-v1/` keeps the deterministic rebuild quick; the
revised low-loss glass model uses `-fem2` cache keys so older materials remain
warm without masking the new physical response.
