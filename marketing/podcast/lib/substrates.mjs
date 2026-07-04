// substrates.mjs — the MEDIUM each reading is printed on.
//
// Same abstraction as /pop's pop/lib/substrate.mjs (a "substrate" = the
// physical-feeling medium the mix is baked onto), but VOICE-TUNED: no music
// sub/kick EQ, no seasick wow/flutter on speech. Each substrate is an ffmpeg
// -af tone + glue chain applied in the master. Swap per episode to give each
// reading its own character — the sonic counterpart to the video colour theme.

export const SUBSTRATES = {
  // Transparent — just a touch of body + air. The neutral default.
  clean: () => [
    "equalizer=f=220:t=q:w=1.0:g=-0.8",   // de-mud
    "equalizer=f=2600:t=q:w=1.2:g=0.6",   // presence
    "equalizer=f=10000:t=q:w=0.8:g=1.2",  // air
  ],
  // Warm bedroom-tape: gentle warmth, softened top, a whisper of wow, bus glue.
  tape: () => [
    "equalizer=f=150:t=q:w=1.0:g=1.4",
    "equalizer=f=500:t=q:w=1.2:g=-0.8",
    "equalizer=f=3000:t=q:w=1.3:g=0.7",
    "equalizer=f=9000:t=q:w=0.8:g=1.4",
    "vibrato=f=0.5:d=0.04",                // barely-there wow (not seasick)
    "acompressor=threshold=-20dB:ratio=2:attack=40:release=400:makeup=1.5:knee=8",
  ],
  // Vintage broadcast: band-limited, midrange-forward, tight glue.
  radio: () => [
    "highpass=f=120",
    "lowpass=f=9000",
    "equalizer=f=1800:t=q:w=1.0:g=2.4",
    "equalizer=f=3200:t=q:w=1.2:g=1.4",
    "acompressor=threshold=-16dB:ratio=3:attack=8:release=160:makeup=2:knee=6",
  ],
  // Intimate & warm with rounded highs — pairs with June's "night" video theme.
  night: () => [
    "equalizer=f=120:t=q:w=1.0:g=1.6",
    "equalizer=f=420:t=q:w=1.2:g=-0.6",
    "equalizer=f=2800:t=q:w=1.3:g=0.4",
    "equalizer=f=8500:t=q:w=0.9:g=-0.6",  // softened, intimate top
    "acompressor=threshold=-19dB:ratio=2.2:attack=35:release=350:makeup=1.6:knee=8",
  ],
};

export const DEFAULT_SUBSTRATE = "tape";
export const substrateChain = (name) => (SUBSTRATES[name] ?? SUBSTRATES[DEFAULT_SUBSTRATE])();

// Per-episode substrate (like the video colour theme). Falls back to default.
export const EPISODE_SUBSTRATE = {
  "may-26": "tape",
  "june-26": "night",
};
