import { readFileSync } from "node:fs";
import {
  REEL_APPS, aestheticCaption, cleanCaption, daySlot, pickUnposted,
  whistlegraphCaption,
} from "../toolchain/instagram/reel-app-config.mjs";

describe("Instagram Reel apps", () => {
  it("keeps account sources and automation gates isolated", () => {
    expect(REEL_APPS.whistlegraph.prefix).toBe("WHISTLEGRAPH");
    expect(REEL_APPS.whistlegraph.source).toBe("archive");
    expect(REEL_APPS.aesthetic.prefix).toBe("AESTHETIC");
    expect(REEL_APPS.aesthetic.source).toBe("av");
    expect(REEL_APPS.aesthetic.recipes.length).toBeGreaterThan(2);
  });

  it("assigns stable daily slots", () => {
    expect(daySlot("2026-01-01", 0, 1)).toBe(0);
    expect(daySlot("2026-01-02", 0, 1)).toBe(1);
    expect(daySlot("2026-01-02", 1, 3)).toBe(4);
    expect(() => daySlot("2026-01-02", 3, 3)).toThrow();
  });

  it("walks forward instead of reposting a source", () => {
    const rows = [{ id: "a" }, { id: "b" }, { id: "c" }];
    expect(pickUnposted(rows, 0, new Set(["a"])).id).toBe("b");
    expect(pickUnposted(rows, 2, new Set(["c", "a"])).id).toBe("b");
    expect(pickUnposted(rows, 0, new Set(["a", "b", "c"]))).toBeNull();
  });

  it("writes short account-specific captions", () => {
    const whistle = whistlegraphCaption({ desc: "hello #old https://example.com", works: ["w0w"] },
      "play", REEL_APPS.whistlegraph.tags);
    const aesthetic = aestheticCaption(REEL_APPS.aesthetic.recipes[0], REEL_APPS.aesthetic.tags);
    expect(cleanCaption("hello #old https://example.com")).toBe("hello");
    expect(whistle).toContain("aesthetic.computer/whistlegraph");
    expect(whistle).not.toContain("#old");
    expect(aesthetic).toContain("aesthetic.computer/notepat");
    expect(whistle.length).toBeLessThan(2200);
    expect(aesthetic.length).toBeLessThan(2200);
  });

  it("exposes both apps through the Instagram MCP", () => {
    const mcp = readFileSync(new URL("../slab/bin/instagram-mcp.mjs", import.meta.url), "utf8");
    expect(mcp).toContain('whistlegraph: resolve(root, "toolchain/instagram/whistlegraph-ig.mjs")');
    expect(mcp).toContain('aesthetic: resolve(root, "toolchain/instagram/aesthetic-ig.mjs")');
    expect(mcp).toContain('name: "instagram_reel_app_build"');
    expect(mcp).toContain('name: "instagram_reel_app_publish"');
    expect(mcp).toContain('confirm:true is required for live publication');
  });
});
