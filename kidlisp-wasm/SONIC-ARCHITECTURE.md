# Hybrid Latent Sonic Architecture

This is the architecture for `kidlisp-wasm` audio export.

## Goal

Let the framebuffer cover the full color spectrum while the sound path covers a much wider synthesis range than a fixed visual-to-bytebeat mapping.

That means two things have to be true at once:

1. The buffer can act like an audio-bearing field.
2. The system does not hear that field the same way every frame.

## Core Model

`visible framebuffer -> hidden latent field -> changing listener -> audio experts -> stereo output`

The visible frame is still the thing we see.
The hidden latent field is the thing we hear.
The listener is a small feedforward network that decides how to scan and decode the latent field over time.

## Layers

### 1. Visible Framebuffer

The rendered KidLisp or fixture frame is read in full every visual frame.
We extract:

- global color and luminance statistics
- spatial balance and centroid
- edge energy
- tile-local color features across the frame

### 2. Hidden Latent Field

The frame is re-encoded into a continuous latent grid.
Each tile contributes to a latent vector, and that vector is blended with its prior value so the sonic field has memory.

This gives us:

- continuity across frames
- local sonic neighborhoods
- enough hidden state for simple visuals to still evolve sonically

### 3. Changing Listener

A feedforward control network reads global frame features plus a persistent latent state.
It outputs:

- scan motion and stereo drift
- pitch and formant drift
- table warp and breath amount
- expert mixture weights
- living-system rules for the petri/bytebeat branch

So the sound is not just “what the image is.”
It is also “how the current listener chooses to hear the image.”

### 4. Decoder Experts

The latent field is decoded by a mixture of experts:

- `tonal`: additive sine-like harmonics
- `vocal`: voiced source plus moving formants
- `table`: raw PCM-style readout from the full RGB buffer
- `living`: petri/bytebeat emergent branch

The listener mixes these experts per frame and per read position.

## Why This Matches The Artistic Goal

A fixed mapping like `red = sine` or `edges = noise` gets boring fast.
The hybrid latent system keeps a strong relation to the image, but it changes the interpretation over time.

That means:

- a pulsing square can drift between tone, table-like grit, and breathy vowel sound
- a gradient can span the whole spectrum without collapsing into one static drone
- a simple orbit can still sound alive because the listener is moving through the latent field

## Current Repo Pieces

- [`sonic-frame.mjs`](./sonic-frame.mjs) implements the hybrid latent engine.
- [`sonic-fixtures.mjs`](./sonic-fixtures.mjs) defines simple visual fixtures like `pulse-square` and `gradient-sweep`.
- [`mp4.mjs`](./mp4.mjs) renders KidLisp pieces or fixtures and muxes the soundtrack into MP4.

## Current Limits

This is a first pass, not a trained EnCodec-style codec yet.
The latent field is continuous and deterministic, but not learned from a corpus.

So this architecture is now in place, and it is ready for the next step:

- trainable latent encoders/decoders
- vector-quantized audio codes
- richer voice modeling
- explicit spectral/STFT buffer modes
