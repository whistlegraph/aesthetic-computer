import test from "node:test";
import assert from "node:assert/strict";
import { nopaintProposal } from "../public/aesthetic.computer/disks/line.mjs";
import { makeProposal, seededRandom } from "../public/aesthetic.computer/lib/nopaint-proposals.mjs";
import { color, line, setBuffer } from "../public/aesthetic.computer/lib/graph.mjs";

test("Line's entire raster stays inside the painting with a visible edge margin", () => {
  const guard = 32;
  for (const [width, height] of [[8, 8], [96, 32], [256, 256], [390, 676], [844, 312], [1200, 700]]) {
    const surface = {
      width: width + guard * 2,
      height: height + guard * 2,
      pixels: new Uint8ClampedArray((width + guard * 2) * (height + guard * 2) * 4),
    };
    for (let seed = 0; seed < 16; seed++) {
      const random = seededRandom(seed);
      const base = makeProposal(random, width, height);
      // Include paths that reach both opposite corners before insetting.
      const edgeBase = seed % 2 === 0
        ? { ...base, x: 0, y: 0, w: width - 1, h: height - 1 }
        : base;
      const score = nopaintProposal.generate({ random, width, height, base: edgeBase });
      surface.pixels.fill(0);
      setBuffer(surface);
      nopaintProposal.render({ ink: (rgba) => {
        color(...rgba);
        return { line: (x0, y0, x1, y1, thickness) =>
          line(x0 + guard, y0 + guard, x1 + guard, y1 + guard, thickness) };
      } }, score, 600);
      let count = 0;
      for (let index = 3; index < surface.pixels.length; index += 4) {
        if (!surface.pixels[index]) continue;
        count++;
        const pixel = (index - 3) / 4;
        const x = pixel % surface.width - guard;
        const y = Math.floor(pixel / surface.width) - guard;
        assert.ok(x > 0 && x < width - 1 && y > 0 && y < height - 1,
          `${width}x${height}, seed ${seed}: stroke pixel ${x},${y} touches or crosses an edge`);
      }
      assert.ok(count > 0, "the proposal remains visible");
    }
  }
});
