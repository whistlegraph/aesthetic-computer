import { readFileSync } from "node:fs";
describe("UI and cursor interaction defaults", () => {
  const ui = readFileSync("system/public/aesthetic.computer/lib/ui.mjs", "utf8");
  const bios = readFileSync("system/public/aesthetic.computer/bios.mjs", "utf8");
  it("gives text buttons theme-aware rollover defaults", () => {
    expect(ui).toContain("defaultRolloverScheme($");
    expect(ui).toContain("mode?.buttonHover");
  });
  it("restores SVG cursors outside native mode", () => {
    expect(bios).toContain("cursors/precise.svg");
    expect(bios).toContain("classList.remove(\"native-cursor\")");
  });
  it("preserves continuity-frame aspect", () => {
    expect(bios).toContain('freezeFrameCan.style.objectFit = "contain"');
  });
});
