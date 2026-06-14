// _template.mjs — the faithful marimbaba lullaby (the seed all 15 riff on).
// A variation = (1) build the marimbaba events with some transform, and/or
// (2) splice MOTIFS into a new tune over the same DNA, then renderLullaby().
//
// Run:  node variations/_template.mjs        (from pop/marimba/lullabies)

import { dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { renderLullaby } from "../lib/core.mjs";
import { buildMarimbaba } from "../lib/marimbaba.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));

const events = buildMarimbaba({ bpm: 56, transpose: 0, leadPreset: "rosewood" });

const { mp3, durationSec } = renderLullaby(events, {
  name: "_template",
  here: HERE,
  title: "marimbaba (seed)",
  reverb: { wet: 0.3, decay: 0.82, damp: 0.4 },
  fadeOut: 4.0,
});
console.log(`✓ ${mp3} · ${durationSec.toFixed(1)}s`);
