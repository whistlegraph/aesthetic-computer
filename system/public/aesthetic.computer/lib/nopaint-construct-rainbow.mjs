// Rainbow, recovered from the Construct "Rainbow" sheet. It is not a brush at
// all — it draws nothing and adds nothing. It applies the AdjustHSL effect to
// the whole painting and walks its hue parameter, so it belongs with the pixel
// transforms rather than with the shapes.
//
// The sheet, in full: a repeating 0.1s "ShiftHue" timer does `hueShift += 1`
// and sets AdjustHSL parameter 0 to `abs(hueShift)`; when `hueShift` reaches
// 100 it is set to -100. The applied rotation is therefore a triangle wave
// 100 → 0 → 100 over twenty seconds, and `rainbow - theme` loops at -5 dB
// underneath it.
//
// A pixel transform is applied once when the proposal is chosen, so this takes
// one point off that wave rather than animating along it. The wave itself is
// recorded in `source` so the day the conductor can animate a transform, the
// numbers are already here.

const frozen = (value) => Object.freeze(value);

export const RAINBOW = frozen({
  shiftSeconds: .1,       // Timer "ShiftHue"
  step: 1,                // hueShift += 1
  bounds: frozen([-100, 100]),
  cue: "rainbow - theme",
  cueVolume: -5,
  cueTag: "HueShift",
  effect: "AdjustHSL",
});

// Construct's AdjustHSL takes its hue parameter as a percentage of a full
// rotation, so 100 is all the way round.
const TURN = 360 / 100;

function rotateHue(r, g, b, degrees) {
  const max = Math.max(r, g, b), min = Math.min(r, g, b);
  const lightness = (max + min) / 2 / 255;
  if (max === min) return [r, g, b];
  const delta = (max - min) / 255;
  const saturation = delta / (1 - Math.abs(2 * lightness - 1));
  let hue;
  if (max === r) hue = ((g - b) / 255 / delta) % 6;
  else if (max === g) hue = (b - r) / 255 / delta + 2;
  else hue = (r - g) / 255 / delta + 4;
  hue = (((hue * 60 + degrees) % 360) + 360) % 360;

  const chroma = (1 - Math.abs(2 * lightness - 1)) * saturation;
  const sector = hue / 60;
  const second = chroma * (1 - Math.abs(sector % 2 - 1));
  const [nr, ng, nb] = [[chroma, second, 0], [second, chroma, 0], [0, chroma, second],
    [0, second, chroma], [second, 0, chroma], [chroma, 0, second]][Math.floor(sector) % 6];
  const base = lightness - chroma / 2;
  return [nr, ng, nb].map((channel) => Math.round((channel + base) * 255));
}

export const rainbowProposal = frozen({
  version: 1,
  slug: "rainbow",
  label: "Rainbow",
  compatible: true,
  kind: "pixel-transform",
  fidelity: "event-sheet-exact",
  source: frozen({ ...RAINBOW, file: "/nopaint.art/data.json", sheet: "Rainbow",
    cycle: "hueShift += 1 every .1s; at 100 it becomes -100; AdjustHSL hue = abs(hueShift)",
    cycleSeconds: 20,
    animation: "not modeled — a pixel transform is applied once, not per frame" }),
  generate({ random, base }) {
    // Any point on the recovered wave is a legal rotation, so take one.
    const shift = Math.round(random() * RAINBOW.bounds[1]);
    return frozen({ ...base, kind: "rainbow", transform: "rainbow", shift,
      brush: frozen({ slug: "rainbow", params: frozen([String(shift)]),
        colon: frozen([]),
        parameters: frozen({ shift, degrees: shift * TURN, effect: RAINBOW.effect }) }) });
  },
  applyPixels(pixels, width, height, { shift }) {
    const output = new Uint8ClampedArray(pixels);
    const degrees = shift * TURN;
    for (let index = 0; index < output.length; index += 4) {
      if (output[index + 3] === 0) continue;
      const [r, g, b] = rotateHue(output[index], output[index + 1], output[index + 2], degrees);
      output[index] = r;
      output[index + 1] = g;
      output[index + 2] = b;
    }
    return output;
  },
});
