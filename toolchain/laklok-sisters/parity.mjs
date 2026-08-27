// 🎪 laklok sisters — static parity checker for the two Laer Klokken chat
// interfaces: the AC "raster" piece (disks/laklok.mjs, which inherits its
// chat features from disks/chat.mjs) and the "vector" HTML client
// (system/public/html/index.html).
//
// The sisters rule: any mirrored constant or feature edited in one file must
// land in the other. This script extracts those mirrors from the real sources
// and diffs them, so drift fails loudly instead of surfacing as a subtle
// visual difference weeks later.
//
// Usage:
//   node toolchain/laklok-sisters/parity.mjs        # exits 1 on any drift
//
// Companion: sisters.mjs renders both surfaces side by side for the visual
// half of the same promise; PARITY.md holds the feature matrix + workflow.

import { readFileSync } from "node:fs";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";

const repo = join(dirname(fileURLToPath(import.meta.url)), "..", "..");
const read = (p) => readFileSync(join(repo, p), "utf8");

const raster = read("system/public/aesthetic.computer/disks/laklok.mjs");
const vector = read("system/public/html/index.html");
const chatDisk = read("system/public/aesthetic.computer/disks/chat.mjs");
const highlighting = read("system/public/aesthetic.computer/lib/chat-highlighting.mjs");

const checks = [];
const check = (name, ok, detail = "") => checks.push({ name, ok, detail });

// Pull `const NAME = /regex/flags;` (possibly wrapped to the next line) out of
// a source file and return the literal regex text.
function regexLiteral(src, constName) {
  const m = src.match(new RegExp(`const ${constName} =\\s*\\n?\\s*(\\/(?:[^\\/\\\\\\n]|\\\\.)+\\/[a-z]*)`));
  return m ? m[1] : null;
}

// ── 1. Media-link regex — mirrored verbatim ────────────────────────────────
{
  const a = regexLiteral(raster, "LAK_MEDIA_LINK");
  const b = regexLiteral(vector, "MEDIA_LINK");
  check(
    "media-link regex (LAK_MEDIA_LINK ↔ MEDIA_LINK)",
    !!a && a === b,
    a === b ? a : `raster: ${a}\n  vector: ${b}`,
  );
}

// ── 2. Theme roster — LAK_THEMES keys ↔ data-theme blocks + tema chips ────
{
  const themesBlock = raster.match(/const LAK_THEMES = \{([\s\S]*?)\n\};/)?.[1] || "";
  const rasterThemes = [...themesBlock.matchAll(/^ {2}(\w+): \{/gm)].map((m) => m[1]);
  const vectorCss = [...vector.matchAll(/data-theme="(\w+)"/g)].map((m) => m[1]);
  const vectorChips = [...vector.matchAll(/data-tema="(\w+)"/g)].map((m) => m[1]);
  const same = (x, y) => x.length && x.join() === [...new Set(y)].join();
  check(
    "theme roster (LAK_THEMES ↔ CSS data-theme)",
    same(rasterThemes, vectorCss),
    `raster: ${rasterThemes.join(", ")} · vector css: ${[...new Set(vectorCss)].join(", ")}`,
  );
  check(
    "theme chips (LAK_THEMES ↔ data-tema buttons)",
    same(rasterThemes, vectorChips),
    `raster: ${rasterThemes.join(", ")} · vector chips: ${vectorChips.join(", ")}`,
  );
}

// ── 3. Settings pane — mode + filter chips exist on both sides ────────────
{
  const rasterPane =
    /text: "raster"/.test(raster) &&
    /text: "vector"/.test(raster) &&
    /text: "alle"/.test(raster) &&
    /text: "links"/.test(raster);
  const vectorPane =
    /data-mode="raster"/.test(vector) &&
    /data-mode="vector"/.test(vector) &&
    /data-links="0"/.test(vector) &&
    /data-links="1"/.test(vector);
  check("settings pane chips (mode raster/vector, filter alle/links)", rasterPane && vectorPane,
    `raster: ${rasterPane ? "ok" : "MISSING"} · vector: ${vectorPane ? "ok" : "MISSING"}`);
}

// ── 4. YouTube ID regex — vector mirrors lib/chat-highlighting.mjs ────────
{
  const lib = highlighting.match(/const youtubeRegex =\s*(\/(?:[^\/\\\n]|\\.)+\/[a-z]*)/)?.[1];
  const vec = regexLiteral(vector, "YT_LINK");
  check(
    "youtube id regex (chat-highlighting ↔ YT_LINK)",
    !!lib && lib === vec,
    lib === vec ? lib : `lib: ${lib}\n  vector: ${vec}`,
  );
}

// ── 5. Circus marquee — label + per-letter colors ─────────────────────────
{
  const label = raster.includes('"Laer Klokken"') && vector.includes('"Laer Klokken"');
  check('marquee label ("Laer Klokken" in both)', label);

  const colsBlock = raster.match(/const LAK_CIRCUS_COLS = \[([\s\S]*?)\];/)?.[1] || "";
  const rasterCols = [...colsBlock.matchAll(/\[(\d+), (\d+), (\d+)\]/g)].map(
    (m) => "#" + [m[1], m[2], m[3]].map((n) => (+n).toString(16).padStart(2, "0")).join(""),
  );
  const vecColsBlock = vector.match(/const CIRCUS = \[([^\]]*)\]/)?.[1] || "";
  const vectorCols = [...vecColsBlock.matchAll(/#([0-9a-f]{6})/gi)].map((m) => "#" + m[1].toLowerCase());
  check(
    "circus colors (LAK_CIRCUS_COLS ↔ CIRCUS)",
    rasterCols.length > 0 && rasterCols.join() === vectorCols.join(),
    `raster: ${rasterCols.join(" ")}\n  vector: ${vectorCols.join(" ")}`,
  );
}

// ── 6. Inline color-code keywords — chat.mjs ↔ vector ─────────────────────
{
  const a = chatDisk.match(/const CHAT_COLOR_KEYWORDS = \[([^\]]*)\]/)?.[1]?.replace(/\s/g, "");
  const b = vector.match(/const COLOR_KEYWORDS = \[([^\]]*)\]/)?.[1]?.replace(/\s/g, "");
  check("color-code keywords (CHAT_COLOR_KEYWORDS ↔ COLOR_KEYWORDS)", !!a && a === b,
    a === b ? a : `chat.mjs: ${a} · vector: ${b}`);
}

// ── 7. QR — both sisters encode the same URL ──────────────────────────────
{
  const a = /qr\("https:\/\/laklok\.com"/.test(raster);
  const b = /qrcode\("https:\/\/laklok\.com"/.test(vector);
  check("corner QR encodes https://laklok.com in both", a && b,
    `raster: ${a ? "ok" : "MISSING"} · vector: ${b ? "ok" : "MISSING"}`);
}

// ── 8. Media embeds — behavioral anchors shared with chat.mjs ─────────────
// chat.mjs is where the raster's previews actually live; these anchors catch
// one side changing an endpoint or thumb source without the other.
{
  for (const [name, needle] of [
    ["youtube thumbs (i.ytimg.com mqdefault)", "i.ytimg.com/vi/"],
    ["og preview endpoint (/api/og-preview)", "/api/og-preview?url="],
    ["og image proxy (/api/og-image)", "/api/og-image?url="],
    ["painting confirm endpoint (/api/painting-code)", "/api/painting-code?code="],
  ]) {
    const a = chatDisk.includes(needle);
    const b = vector.includes(needle);
    check(`embed anchor: ${name}`, a && b,
      `chat.mjs: ${a ? "ok" : "MISSING"} · vector: ${b ? "ok" : "MISSING"}`);
  }
}

// ── 9. Message length cap ─────────────────────────────────────────────────
{
  const a = /chatMaxChars = 128/.test(chatDisk);
  const b = /maxlength="128"/.test(vector);
  check("128-char message cap (chatMaxChars ↔ maxlength)", a && b,
    `chat.mjs: ${a ? "ok" : "CHANGED"} · vector: ${b ? "ok" : "CHANGED"}`);
}

// ── Report ────────────────────────────────────────────────────────────────
let failed = 0;
for (const { name, ok, detail } of checks) {
  console.log(`${ok ? "✅" : "❌"} ${name}`);
  if (!ok) {
    failed++;
    if (detail) console.log(`  ${detail}`);
  }
}
console.log(
  failed
    ? `\n🎪 ${failed}/${checks.length} parity check${failed === 1 ? "" : "s"} FAILED — the sisters have drifted. Edit both sides, then re-run.`
    : `\n🎪 All ${checks.length} parity checks passed — the sisters are in lockstep.`,
);
process.exit(failed ? 1 : 0);
