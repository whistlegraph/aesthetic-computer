// Vignette and Aura, recovered from the Construct sheets of the same names.
// Both lay a soft coloured field over the painting rather than drawing a shape,
// and both take their colours from hslaToRgba with recovered ranges, so they
// share a module. Constants read out of the compiled expression table.

import { canvasFor } from "./nopaint-canvas.mjs";

const frozen = (value) => Object.freeze(value);
const choose = (random, values) => values[Math.floor(random() * values.length)];
const between = (random, [low, high]) => low + random() * (high - low);

export const VIGNETTE = frozen({
  canvas: 256,
  sizes: frozen([16, 32, 64, 96, 128]),
  // hardness = round(random(radius / 2, radius - 2))
  hardness: frozen([.5, 1]),
  hardnessInset: 2,
  dark: frozen([0, 1]),               // choose(0, 1): dark vignette or light
  drift: frozen([-3, 3]),             // choose(-3, 3)
  angle: frozen([0, 360]),
  // hslaToRgba(random(0,1), random(0,1), random(.05,.2) | random(.7,.9), 1)
  darkLightness: frozen([.05, .2]),
  lightLightness: frozen([.7, .9]),
  cue: "vignette - theme",
  tween: frozen(["Radius", "Hardness"]),
});

export const AURA = frozen({
  canvas: 256,
  spray: frozen([90, 110]),           // random(90, 110)
  rate: frozen([2, 4]),               // random(2, 4)
  // The three AdjustHSL parameters the emitter is given at birth.
  hue: frozen([0, 100]),
  saturation: frozen([25, 120]),
  lightness: frozen([60, 180]),
  changeSeconds: 1,                   // Timer "Change"
  changeHue: frozen([-100, 100]),     // and lightness is re-rolled from its range
  changeAngle: frozen([0, 360]),
  cue: "aura - theme",
  cueVolume: frozen([-100, -7.5]),    // faded in over three seconds
  cueFadeSeconds: 3,
});

export function hslaToRgba(hue, saturation, lightness) {
  const chroma = (1 - Math.abs(2 * lightness - 1)) * saturation;
  const sector = ((hue % 1) + 1) % 1 * 6;
  const second = chroma * (1 - Math.abs(sector % 2 - 1));
  const [r, g, b] = [[chroma, second, 0], [second, chroma, 0], [0, chroma, second],
    [0, second, chroma], [second, 0, chroma], [chroma, 0, second]][Math.floor(sector) % 6];
  const base = lightness - chroma / 2;
  return frozen([r, g, b].map((channel) => Math.round((channel + base) * 255)));
}

export const vignetteProposal = frozen({
  version: 1,
  slug: "vignette",
  label: "Vignette",
  compatible: true,
  source: frozen({ ...VIGNETTE, actionSheet: "Vignette",
    // Construct tweened Radius and Hardness; the proposal holds one pose.
    effect: "Vignette",
    vehicle: "VignetteVehicle",
    reconstructed: frozen(["the radius/hardness tween"]) }),
  generate({ random, width, height, base }) {
    const size = choose(random, VIGNETTE.sizes);
    const scale = Math.min(width, height) / VIGNETTE.canvas;
    const radius = Math.max(2, size * scale);
    const hardness = Math.round(between(random,
      [radius / 2, Math.max(radius / 2, radius - VIGNETTE.hardnessInset)]));
    const dark = choose(random, VIGNETTE.dark) === 0;
    const lightness = between(random,
      dark ? VIGNETTE.darkLightness : VIGNETTE.lightLightness);
    return frozen({ ...base, kind: "vignette",
      size, radius, hardness, dark,
      color: hslaToRgba(random(), random(), lightness),
      angle: Math.floor(between(random, VIGNETTE.angle)),
      drift: choose(random, VIGNETTE.drift),
      x: Math.floor(random() * width), y: Math.floor(random() * height),
      width, height,
      brush: frozen({ slug: "vignette", params: frozen([String(size)]),
        colon: frozen([]),
        parameters: frozen({ size, hardness, dark, cue: VIGNETTE.cue }) }) });
  },
  render({ paste }, score) {
    const { canvas } = canvasFor(score, (target) => {
      // Construct's Vignette effect darkens *away* from its centre: the
      // painting stays clear around the vehicle and closes in past the radius.
      // Drawing it as a soft spot, which is what this did first, is the
      // opposite picture.
      target.soft(score.x, score.y, score.radius, score.hardness, score.color,
        { peak: 235, invert: true });
    });
    paste(canvas, 0, 0);
  },
});

export const auraProposal = frozen({
  version: 1,
  slug: "aura",
  label: "Aura",
  compatible: true,
  source: frozen({ ...AURA, actionSheet: "Aura", object: "AuraParticles",
    // The emitter's AdjustHSL numbers are exact; how its particles look is not
    // in the sheet, so the bloom is an AC reading.
    reconstructed: frozen(["the particle bloom"]) }),
  generate({ random, width, height, base }) {
    const scale = Math.min(width, height) / AURA.canvas;
    const spray = between(random, AURA.spray);
    const rate = between(random, AURA.rate);
    const angle = Math.floor(between(random, AURA.changeAngle));
    const hue = between(random, AURA.hue) / 100;
    const saturation = between(random, AURA.saturation) / 120;
    const lightness = between(random, AURA.lightness) / 180;
    const petals = Math.max(2, Math.round(rate * 4));
    return frozen({ ...base, kind: "aura",
      spray, rate, angle, petals,
      // The emitter's own radius range; a bloom this size actually reads.
      radius: Math.max(24, between(random, [25, 120]) * scale),
      color: hslaToRgba(hue, saturation, Math.min(.9, lightness)),
      x: Math.floor(random() * width), y: Math.floor(random() * height),
      width, height,
      brush: frozen({ slug: "aura",
        params: frozen([String(Math.round(spray)), String(angle)]),
        colon: frozen([]),
        parameters: frozen({ spray, rate, angle, cue: AURA.cue }) }) });
  },
  render({ paste }, score) {
    const { canvas } = canvasFor(score, (target) => {
      // The emitter sprays `spray` degrees wide around `angle`. Particles are
      // laid along each ray so the bloom reads as spray rather than as blobs,
      // and each keeps the strongest coverage so it stays one bloom.
      for (let petal = 0; petal < score.petals; petal += 1) {
        const offset = (petal / Math.max(1, score.petals - 1) - .5) * score.spray;
        const radians = (score.angle + offset) * Math.PI / 180;
        for (let along = 1; along <= 5; along += 1) {
          const reach = score.radius * along / 5;
          const size = score.radius / 3 * (1 - along / 8);
          target.soft(score.x + Math.cos(radians) * reach,
            score.y + Math.sin(radians) * reach,
            size, size / 4, score.color,
            { peak: 150 - along * 18, blend: "strongest" });
        }
      }
      target.soft(score.x, score.y, score.radius / 2, score.radius / 6,
        score.color, { peak: 190, blend: "strongest" });
    });
    paste(canvas, 0, 0);
  },
});
