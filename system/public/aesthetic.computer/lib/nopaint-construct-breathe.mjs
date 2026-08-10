// Breathe, recovered from the Construct "Breathe" sheet. Like Rainbow it is not
// a brush: it never draws anything. It puts Construct's Bulge effect over the
// painting and drives the effect's magnitude from a Sine behaviour, so it
// belongs with the pixel transforms.
//
// The sheet: size is picked from the list "64|96|128|196|256" by
// ProcessNumericParameter(3, 1, 5); radius is size / 2; a repeating 0.04s
// "BulgeExpand" timer sets Bulge parameter 1 to abs(Sine.Value); a repeating
// 6s "BulgeCycle" timer restarts the sine and replays the theme, whose volume
// is -5 - (5 - size / 256 * 5), so a bigger breath is a louder one.
//
// A pixel transform applies once, so this takes one point off that sine. The
// 0.04s / 6s pair is recorded in `source` for when a transform can animate.

const frozen = (value) => Object.freeze(value);

export const BREATHE = frozen({
  canvas: 256,
  sizes: frozen([64, 96, 128, 196, 256]),
  expandSeconds: .04,     // Timer "BulgeExpand"
  cycleSeconds: 6,        // Timer "BulgeCycle"
  effect: "Bulge",
  cue: "breathe - theme",
  cueVolume: (size) => -5 - (5 - size / 256 * 5),
});

export const breatheProposal = frozen({
  version: 1,
  slug: "breathe",
  label: "Breathe",
  compatible: true,
  kind: "pixel-transform",
  fidelity: "event-sheet-exact",
  source: frozen({ sizes: BREATHE.sizes, expandSeconds: BREATHE.expandSeconds,
    cycleSeconds: BREATHE.cycleSeconds, effect: BREATHE.effect, cue: BREATHE.cue,
    file: "/nopaint.art/data.json", sheet: "Breathe",
    magnitude: "abs(Sine.Value), refreshed every .04s over a 6s cycle",
    animation: "not modeled — a pixel transform is applied once, not per frame",
    // Construct's Bulge is a shader; this is a radial resample of the same
    // shape rather than a port of it.
    reconstructed: frozen(["the bulge falloff"]) }),
  generate({ random, width, height, base }) {
    const size = BREATHE.sizes[Math.floor(random() * BREATHE.sizes.length)];
    const scale = Math.min(width, height) / BREATHE.canvas;
    // One point off the sine: anywhere in its swing is a legal magnitude.
    const magnitude = random();
    return frozen({ ...base, kind: "breathe", transform: "breathe",
      brush: frozen({ slug: "breathe", params: frozen([String(size)]),
        colon: frozen([]),
        parameters: frozen({ size, radius: size / 2 * scale, magnitude,
          cue: BREATHE.cue, cueVolume: BREATHE.cueVolume(size),
          x: Math.floor(random() * width), y: Math.floor(random() * height) }) }) });
  },
  applyPixels(pixels, width, height, { radius, magnitude, x, y }) {
    const output = new Uint8ClampedArray(pixels);
    const centerX = x ?? width / 2;
    const centerY = y ?? height / 2;
    const left = Math.max(0, Math.floor(centerX - radius));
    const right = Math.min(width - 1, Math.ceil(centerX + radius));
    const top = Math.max(0, Math.floor(centerY - radius));
    const bottom = Math.min(height - 1, Math.ceil(centerY + radius));
    for (let py = top; py <= bottom; py += 1) {
      for (let px = left; px <= right; px += 1) {
        const dx = px - centerX;
        const dy = py - centerY;
        const distance = Math.hypot(dx, dy);
        if (distance >= radius || distance === 0) continue;
        // Pull the sample toward the centre by more the nearer it is, which is
        // what makes the middle swell.
        const pull = 1 - magnitude * (1 - distance / radius) ** 2;
        const sourceX = Math.round(centerX + dx * pull);
        const sourceY = Math.round(centerY + dy * pull);
        if (sourceX < 0 || sourceX >= width || sourceY < 0 || sourceY >= height) continue;
        const to = (py * width + px) * 4;
        const from = (sourceY * width + sourceX) * 4;
        for (let channel = 0; channel < 4; channel += 1) {
          output[to + channel] = pixels[from + channel];
        }
      }
    }
    return output;
  },
});
