#!/usr/bin/env node
// stories.mjs — the three Instagram Story cards for the grokaesthetic
// workshop at CultureHub LA. Builds self-contained HTML (fonts + art
// inlined as data URIs, so Chrome never has to reach across file://)
// and screenshots each at 1080x1920 via toolchain/macos/chrome-shot.mjs.
//
// Usage: node marketing/campaigns/grokaesthetic/bin/stories.mjs [out-dir]
// Default out: ~/Desktop/Shelf/culturehub-2026/stories
//
// Art is the existing campaign painting (macneopolitan `program`), the same
// image already running on X and Bluesky. Nothing here is generated.
//
// Instagram paints its own chrome over the top and bottom 250px, so every
// readable thing lives between SAFE_TOP and H - SAFE_BOT. Line breaks are
// hand-placed rather than left to wrapping — an orphaned "PT" on its own
// line is the thing that reads as sloppy at thumb speed.

import { execFileSync } from "node:child_process";
import { mkdirSync, writeFileSync, readFileSync, rmSync } from "node:fs";
import { dirname, join, resolve } from "node:path";
import { fileURLToPath, pathToFileURL } from "node:url";
import { tmpdir } from "node:os";

const HERE = dirname(fileURLToPath(import.meta.url));
const REPO = resolve(HERE, "../../../..");
const OUT = resolve((process.argv[2] || `${process.env.HOME}/Desktop/Shelf/culturehub-2026/stories`).replace(/^~/, process.env.HOME));
const BUILD = join(tmpdir(), "grok-stories");

const W = 1080, H = 1920;
const SAFE_TOP = 250, SAFE_BOT = 250;

// the event. these are the facts — nothing here is decorative.
const ev = {
  day: "Sat · Sep 19 · 2026",
  time: "2:00 – 3:30 PM PT",
  venue: "CultureHub LA · The Reef",
  street: "1933 S Broadway, Suite 1268",
  city: "Los Angeles, CA 90007",
  bring: "Bring a laptop —<br>loaners available",
  skill: "No programming or music<br>experience needed",
  price: "Pay what you can · suggested $10",
  encore: "Companion performance Thu Sep 24, 7pm · same venue",
  host: "with Jeffrey Alan Scudder · @aesthetic.computer",
};

// warm palette pulled straight off the painting's wood floor.
const c = {
  paper: "#F6E8C9",
  ink: "#191310",
  soft: "#6B4E33",
  rule: "#C0985F",
  pink: "#DC0554",
};

const b64 = (p) => readFileSync(p).toString("base64");
const font = (f) => `data:font/woff2;base64,${b64(join(REPO, "system/public/type/webfonts", f))}`;

// art comes in pre-scaled so the pages stay small and chrome never resamples
// a 4mb png at paint time. `trim` shaves the wordmark's white margins.
function art(src, geom, trim = false) {
  const out = join(BUILD, `${trim ? "t-" : ""}${geom.replace(/\W/g, "")}-${src.split("/").pop()}`);
  execFileSync("magick", [join(REPO, src), ...(trim ? ["-fuzz", "6%", "-trim", "+repage"] : []), "-resize", geom, "-strip", out]);
  const [w, h] = execFileSync("magick", ["identify", "-format", "%w %h", out]).toString().split(" ").map(Number);
  return { url: `data:image/png;base64,${b64(out)}`, w, h };
}

mkdirSync(BUILD, { recursive: true });
mkdirSync(OUT, { recursive: true });

const asset = {
  bold: font("ywft-processing-bold.woff2"),
  reg: font("ywft-processing-regular.woff2"),
  mono: font("BerkeleyMonoVariable-Regular.woff2"),
  tall: art("marketing/campaigns/macneopolitan/gens/program-portrait.png", "1080x"),
  wide: art("marketing/campaigns/macneopolitan/gens/program.png", "1240x"),
  mark: art("marketing/campaigns/grokaesthetic/refs/grokaesthetic-wordmark.png", "900x", true),
};

// the wordmark ships on white; multiply drops it onto the cream cleanly,
// which beats keying the white out and fringing the pixel letterforms.
const mark = (w, extra = "") =>
  `<img src="${asset.mark.url}" style="display:block;mix-blend-mode:multiply;width:${w}px;height:${Math.round(w * asset.mark.h / asset.mark.w)}px;${extra}">`;

// a window onto the painting — explicit offsets, not object-fit, so the
// figure can be placed exactly where the layout wants it.
const pane = (a, top, h, oy = 0) => `
  <div style="position:absolute;top:${top}px;left:0;width:${W}px;height:${h}px;overflow:hidden;background:${c.ink};">
    <img src="${a.url}" style="position:absolute;left:${Math.round((W - a.w) / 2)}px;top:${-oy}px;width:${a.w}px;">
  </div>
  <div style="position:absolute;top:${top + h}px;left:0;width:${W}px;height:10px;background:${c.pink};"></div>
  <div style="position:absolute;top:${top - 10}px;left:0;width:${W}px;height:10px;background:${c.pink};"></div>`;

const shell = (body) => `<!doctype html><html><head><meta charset="utf-8">
<style>
@font-face { font-family: "YWFT"; src: url("${asset.bold}") format("woff2"); font-weight: 700; }
@font-face { font-family: "YWFT"; src: url("${asset.reg}") format("woff2"); font-weight: 400; }
@font-face { font-family: "Berkeley"; src: url("${asset.mono}") format("woff2"); }
* { margin: 0; padding: 0; box-sizing: border-box; }
html, body { width: ${W}px; height: ${H}px; overflow: hidden; }
body { background: ${c.paper}; color: ${c.ink}; font-family: "Berkeley", monospace; -webkit-font-smoothing: antialiased; }
.stage { position: relative; width: ${W}px; height: ${H}px; }
.head { font-family: "YWFT"; font-weight: 700; letter-spacing: -.01em; line-height: .92; }
.rule { height: 5px; background: ${c.rule}; }
.pink { color: ${c.pink}; }
.soft { color: ${c.soft}; }
</style></head><body><div class="stage">${body}</div></body></html>`;

// ─ story 1 — the hook ──────────────────────────────────────────────────────
// painting holds the top of the safe zone, the apron underneath carries the
// four things a thumb can absorb in a second: who, what, when, cost of entry.
const story1 = shell(`
  ${pane(asset.tall, 250, 820, 0)}
  <div style="position:absolute;top:1110px;left:70px;width:940px;">
    ${mark(860)}
    <div class="head" style="font-size:124px;margin-top:4px;">WORKSHOP</div>
    <div class="rule" style="margin:24px 0 0;"></div>
    <div style="font-size:46px;line-height:1.28;margin-top:24px;">
      Make art, music &amp; tiny programs<br>together — <span class="pink">no experience needed.</span>
    </div>
    <div class="soft" style="font-size:35px;margin-top:20px;">Sat Sep 19 · 2–3:30 PM PT · CultureHub LA</div>
  </div>
`);

// ─ story 2 — the details ───────────────────────────────────────────────────
const row = (k, v, sub = "") => `
  <div style="display:flex;gap:22px;align-items:baseline;margin-top:26px;">
    <div class="soft" style="font-size:32px;width:124px;flex:none;letter-spacing:.05em;opacity:.75;">${k}</div>
    <div style="font-size:40px;line-height:1.3;">${v}${sub ? `<div class="soft" style="font-size:36px;line-height:1.3;">${sub}</div>` : ""}</div>
  </div>`;

const story2 = shell(`
  ${pane(asset.wide, 250, 360, 30)}
  <div style="position:absolute;top:650px;left:70px;width:940px;">
    ${mark(520)}
    <div class="head" style="font-size:88px;margin-top:4px;">WORKSHOP</div>
    <div class="rule" style="margin:22px 0 0;"></div>
    ${row("WHEN", `${ev.day}<br>${ev.time}`)}
    ${row("WHERE", ev.venue, `${ev.street}<br>${ev.city}`)}
    ${row("BRING", ev.bring)}
    ${row("SKILL", ev.skill)}
    ${row("COST", `<span class="pink">${ev.price}</span>`)}
    <div class="rule" style="margin:30px 0 0;"></div>
    <div class="soft" style="font-size:30px;line-height:1.42;margin-top:20px;">${ev.encore}<br>${ev.host}</div>
  </div>
`);

// ─ story 3 — the rsvp ──────────────────────────────────────────────────────
// y 1140–1440 is deliberately empty cream. that is the link sticker's seat;
// the url is never typed because story text is not tappable.
const story3 = shell(`
  ${pane(asset.wide, 250, 550, 30)}
  <div style="position:absolute;top:840px;left:70px;width:940px;text-align:center;">
    ${mark(620, "margin:0 auto;")}
    <div class="head pink" style="font-size:112px;margin-top:14px;">TAP TO RSVP</div>
    <div class="soft" style="font-size:36px;margin-top:10px;">use the link sticker below ↓</div>
  </div>
  <div style="position:absolute;top:1430px;left:70px;width:940px;text-align:center;">
    <div class="rule"></div>
    <div style="font-size:38px;line-height:1.34;margin-top:18px;">
      ${ev.day} · ${ev.time}<br>${ev.venue}, Los Angeles
    </div>
    <div class="soft" style="font-size:34px;line-height:1.34;margin-top:6px;">Pay what you can · bring a laptop</div>
    <div class="soft" style="font-size:30px;line-height:1.34;margin-top:6px;">@aesthetic.computer · @whistlegraph · @culturehub</div>
  </div>
`);

const cards = [
  ["story-1-hook", story1],
  ["story-2-details", story2],
  ["story-3-rsvp", story3],
];

for (const [name, html] of cards) {
  const page = join(BUILD, `${name}.html`);
  writeFileSync(page, html);
  const png = join(OUT, `${name}.png`);
  execFileSync("node", [
    join(REPO, "toolchain/macos/chrome-shot.mjs"),
    pathToFileURL(page).href, png, "--size", `${W}x${H}`, "--budget", "4000",
  ], { stdio: ["ignore", "ignore", "ignore"] });
  const dim = execFileSync("magick", ["identify", "-format", "%wx%h", png]).toString();
  if (dim !== `${W}x${H}`) { console.error(`✗ ${name}: ${dim}, wanted ${W}x${H}`); process.exit(1); }
  console.log(`✓ ${name}.png ${dim}`);
}

rmSync(BUILD, { recursive: true, force: true });
console.log(`\nreading stays inside y ${SAFE_TOP}–${H - SAFE_BOT}. → ${OUT}`);
