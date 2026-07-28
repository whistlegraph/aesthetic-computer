import { nopaintProposal as lineProposal } from "../system/public/aesthetic.computer/disks/line.mjs";
import {
  makeProposal,
  seededRandom,
  seedFrom,
} from "../system/public/aesthetic.computer/lib/nopaint-proposals.mjs";

describe("No Paint compatible brush proposals", () => {
  it("lets Line own a deterministic, bounded parameter score", () => {
    const make = () => {
      const random = seededRandom(seedFrom("line-test-3"));
      const base = makeProposal(random, 596, 446);
      expect(base.kind).toBe("line");
      return lineProposal.generate({ random, width: 596, height: 446, base });
    };

    const first = make();
    const second = make();
    expect(first).toEqual(second);
    expect(first.brush.slug).toBe("line");
    expect(first.brush.colon).toEqual([String(first.thickness)]);
    expect(first.thickness).toBeGreaterThanOrEqual(1);
    expect(first.thickness).toBeLessThanOrEqual(50);
    expect(first.color[3]).toBeGreaterThanOrEqual(24);
    expect(first.color[3]).toBeLessThanOrEqual(192);
    expect(first.points.every(({ x, y }) =>
      x >= 0 && x < 596 && y >= 0 && y < 446)).toBeTrue();
  });
});
