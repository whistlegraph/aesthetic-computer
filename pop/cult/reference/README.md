# trackdrum-friction-reference

Ground truth for the friction voice, so a port can be **diffed instead of
argued about**.

`trackdrum-friction-reference.swift` is the continuous-friction block and the
`setDrumSkinScratch` position→material mapping lifted verbatim out of
`slab/menuband/Sources/MenuBand/MenuBandPercussion.swift`. No GUI, no audio
engine: it renders the same maths for a simulated gesture straight to raw
float32, so you can measure the real thing.

```bash
cd pop/cult/reference
swiftc -O trackdrum-friction-reference.swift -o ref && ./ref
ffmpeg -f f32le -ar 48000 -ac 1 -i ref.f32 ref.wav
```

## What it settled

Three rounds of tuning by ear went the wrong way — the skids were called
buzzy, then harsh, then still harsh — because every fix was aimed at an
assumption. Measured against this reference, the assumption was backwards:

| | centroid | 85% rolloff | peak |
|---|---|---|---|
| the real voice | **5604 Hz** | **13787 Hz** | **0.039** |
| the port, same gesture | 5679 Hz | 14033 Hz | 0.109 |

The real TrackDrum friction is **bright and tiny**. Bright-and-quiet reads as
a surface you feel; bright-and-loud reads as harsh; dull is mud. The DSP port
was already correct — what was wrong was *where the gestures travelled*: paths
that hug the pad centre sit on skin at 175 Hz cutoff and sound dull by
construction, so they were moved out to snare/rim where the reference drag
goes.

And the distinction that took longest to find: **bright is the cutoff,
metallic is the resonance.** Past surface distance 0.62 the resonance ramp
climbs 185 → 560 Hz and rings. Paths stop short of it, which buys the
brightness without the ring.
