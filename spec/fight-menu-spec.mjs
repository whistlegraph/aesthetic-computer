import { menuHit, menuLayout } from "../system/public/aesthetic.computer/lib/fight/menu.mjs";

describe("Menu Fighter popover", () => {
  it("keeps the launcher and centered panel on a small AC screen", () => {
    const layout = menuLayout(174, 128);
    for (const box of Object.values(layout)) {
      expect(box.x).toBeGreaterThanOrEqual(0);
      expect(box.y).toBeGreaterThanOrEqual(0);
      expect(box.x + box.w).toBeLessThanOrEqual(174);
      expect(box.y + box.h).toBeLessThanOrEqual(128);
    }
    expect(layout.panel.x + layout.panel.w / 2).toBe(87);
  });

  it("opens only from the little launcher while closed", () => {
    const layout = menuLayout(256, 160);
    expect(menuHit(null, 0, 0)).toBe(null);
    expect(menuHit(layout, layout.launcher.x + 1, layout.launcher.y + 1)).toBe("launcher");
    expect(menuHit(layout, layout.train.x + 1, layout.train.y + 1)).toBe(null);
  });

  it("distinguishes train, planned find, and outside taps", () => {
    const layout = menuLayout(256, 160);
    expect(menuHit(layout, layout.train.x + 2, layout.train.y + 2, true)).toBe("train");
    expect(menuHit(layout, layout.find.x + 2, layout.find.y + 2, true)).toBe("find");
    expect(menuHit(layout, 0, 0, true)).toBe("outside");
  });
});
