#!/usr/bin/env node
// menu-pdf.mjs — render the /pop menu (lib/menu.mjs) as a printable,
// color-coded PDF + PNG. A restaurant-style spec sheet of every
// instrument, fx, percussion voice, bed, vocal pipeline, scale, and
// form that lives in /pop, with a QR + URL to the repo directory.
//
// Usage:
//   node pop/bin/menu-pdf.mjs                 # → ~/Desktop/pop-menu.{pdf,png}
//   node pop/bin/menu-pdf.mjs --out path.pdf  # custom destination (.png sibling)

import { writeFileSync, mkdtempSync, rmSync, readdirSync } from "node:fs";
import { resolve, join } from "node:path";
import { tmpdir, homedir } from "node:os";
import { spawnSync } from "node:child_process";
import { MENU } from "../lib/menu.mjs";

// the /pop directory on the public mirror
const REPO_URL = "https://github.com/whistlegraph/aesthetic-computer/tree/main/pop";

// ── args ──────────────────────────────────────────────────────────────
const argv = process.argv.slice(2);
let out = join(homedir(), "Desktop", "pop-menu.pdf");
for (let i = 0; i < argv.length; i++) {
  if (argv[i] === "--out" && argv[i + 1]) out = resolve(argv[++i]);
}
const pngOut = out.replace(/\.pdf$/i, "") + ".png";

const esc = (s) =>
  String(s).replace(/&/g, "&amp;").replace(/</g, "&lt;").replace(/>/g, "&gt;");

// ── per-section accent colors ─────────────────────────────────────────
const SECTION_COLORS = {
  instruments: "#b5331f", // brick red
  fx: "#c0631a", //          burnt orange
  percussion: "#9a7d12", //  ochre
  beds: "#1f7a5a", //        teal-green
  master: "#1d5b8a", //      steel blue
  pitch_time: "#6b3fa0", //  violet
  analysis: "#1d7a8a", //    deep cyan
  vocal: "#a8327d", //       magenta
  scales: "#3f7d2e", //      leaf green
  forms: "#8a5223", //       sienna
  modulation: "#2f6f7a", //  cyan-slate
  score: "#5a6b1f", //       olive
  sample_sources: "#6b5546", // taupe
};

const SECTION_TITLES = {
  instruments: "instruments — tonal voices",
  fx: "fx — buffer transforms",
  percussion: "percussion — AC one-shot kit",
  beds: "beds — continuous textures",
  master: "master — bus & glue chain",
  pitch_time: "pitch & time — offline transforms",
  analysis: "analysis — audio → control signals",
  vocal: "vocal — TTS, alignment, placement",
  scales: "scales — interval sets",
  forms: "forms — arrangement templates",
  modulation: "modulation — envelopes & LFOs",
  score: "score — notation & converters",
  sample_sources: "sourced audio — provenance registry",
};

// ── section renderers ─────────────────────────────────────────────────
function rowsFor(section, items) {
  if (Array.isArray(items)) {
    return `<div class="loose">${items.map(esc).join(" · ")}</div>`;
  }
  if (section === "percussion") {
    return (
      `<div class="loose">${items.voices.map(esc).join(" · ")}</div>` +
      `<div class="note">${esc(items.note)}</div>`
    );
  }
  if (section === "scales") {
    return Object.entries(items)
      .map(
        ([k, v]) =>
          `<div class="item"><span class="name">${esc(k)}</span>` +
          `<span class="blurb">[${v.join(", ")}]</span></div>`,
      )
      .join("");
  }
  if (section === "sample_sources") {
    const block = (label, obj) =>
      `<div class="subhead">${esc(label)}</div>` +
      Object.entries(obj)
        .map(([k, v]) => {
          const meta = [v.lane, v.license].filter(Boolean).map(esc).join(" · ");
          return (
            `<div class="item"><span class="name">${esc(k)}</span>` +
            `<span class="blurb">${esc(v.blurb || "")}` +
            (meta ? ` <span class="meta">${meta}</span>` : "") +
            `</span></div>`
          );
        })
        .join("");
    return block("source repos", items.repos) + block("samples used", items.used);
  }
  // generic object-of-defs
  return Object.entries(items)
    .map(([name, def]) => {
      const presets = def.presets ? ` (${def.presets.join(", ")})` : "";
      const lane = def.lane ? `<span class="lane">${esc(def.lane)}</span>` : "";
      const inlineMark =
        def.inline && !def.file ? `<span class="tag">inline</span>` : "";
      return (
        `<div class="item"><span class="name">${esc(name + presets)}</span>` +
        `<span class="blurb">${esc(def.blurb || "")} ${lane}${inlineMark}</span></div>`
      );
    })
    .join("");
}

const sections = Object.entries(MENU)
  .filter(([section]) => section !== "proposed") // wishlist — see MENU-WISHLIST.md
  .map(([section, items]) => {
    const c = SECTION_COLORS[section] || "#1a1712";
    // the longest section may flow across the column/page break
    const cls = section === "sample_sources" ? ' class="splittable"' : "";
    return (
      `<section${cls} style="--c:${c}"><h2>${esc(SECTION_TITLES[section] || section)}</h2>` +
      rowsFor(section, items) +
      `</section>`
    );
  })
  .join("\n");

// ── QR to the /pop repo directory ─────────────────────────────────────
const qr = spawnSync(
  "qrencode",
  ["-o", "-", "-t", "PNG", "-s", "12", "-m", "1", REPO_URL],
  { maxBuffer: 1 << 20 },
);
if (qr.status !== 0) {
  console.error("qrencode failed — is it installed? (brew install qrencode)");
  process.exit(1);
}
const qrDataUri = `data:image/png;base64,${qr.stdout.toString("base64")}`;

const today = new Date().toISOString().slice(0, 10);

const html = `<!doctype html><html><head><meta charset="utf-8"><style>
  @page { size: letter; margin: 0.5in 0.55in; }
  * { box-sizing: border-box; }
  body {
    font-family: "Berkeley Mono", "Latin Modern Mono", Menlo, monospace;
    color: #1a1712; background: #f4efe2;
    margin: 0; font-size: 7.8px; line-height: 1.33;
  }
  header { position: relative; text-align: center; margin-bottom: 10px; }
  h1 { font-size: 25px; letter-spacing: 0.06em; margin: 0; font-weight: 700; }
  .sub { font-size: 7.8px; letter-spacing: 0.22em; text-transform: uppercase;
         margin-top: 5px; color: #6b6353; }
  .rule { border: none; border-top: 1.5px solid #1a1712; margin: 10px 0 0; }
  .qr { position: absolute; right: 0; top: -2px; width: 86px; text-align: center; }
  .qr img { width: 74px; height: 74px; image-rendering: pixelated; }
  .qr-cap { font-size: 6.2px; letter-spacing: 0.04em; color: #6b6353;
            margin-top: 2px; line-height: 1.35; }
  .columns { column-count: 2; column-gap: 22px; }
  section { break-inside: avoid; margin-bottom: 8px;
            border-left: 3px solid var(--c); padding-left: 8px; }
  section.splittable { break-inside: auto; }
  section.splittable h2 { break-after: avoid; }
  h2 { font-size: 9.6px; text-transform: uppercase; letter-spacing: 0.06em;
       margin: 0 0 4px; padding-bottom: 2.5px; color: var(--c);
       border-bottom: 1px solid var(--c); }
  .item { padding: 1.6px 0; padding-left: 0.9em; text-indent: -0.9em; }
  .name { font-weight: 700; }
  .name::after { content: " — "; font-weight: 400; color: #b3a98f; }
  .blurb { color: #4a443a; }
  .lane { color: #8a7f66; }
  .lane::before { content: "· "; }
  .tag { color: #8a7f66; font-style: italic; }
  .tag::before { content: " ["; } .tag::after { content: "]"; }
  .meta { color: #8a7f66; }
  .loose { color: #4a443a; }
  .note { color: #8a7f66; font-style: italic; margin-top: 3px; }
  .subhead { font-weight: 700; margin: 5px 0 2px; color: #6b6353; }
  footer { text-align: center; margin-top: 14px; padding-top: 8px;
           border-top: 1.5px solid #1a1712;
           font-size: 8px; letter-spacing: 0.13em;
           text-transform: uppercase; color: #6b6353; }
  footer .url { text-transform: none; letter-spacing: 0.02em; color: #4a443a; }
</style></head><body>
  <header>
    <div class="qr">
      <img src="${qrDataUri}" alt="QR to /pop">
      <div class="qr-cap">scan for the<br>/pop source</div>
    </div>
    <h1>the pop menu</h1>
    <div class="sub">pickable building blocks across /pop</div>
    <hr class="rule">
  </header>
  <div class="columns">${sections}</div>
  <footer>
    aesthetic.computer · /pop · ${today}<br>
    <span class="url">${esc(REPO_URL)}</span>
  </footer>
</body></html>`;

// ── render via headless Chrome ────────────────────────────────────────
const work = mkdtempSync(join(tmpdir(), "pop-menu-"));
const htmlPath = join(work, "menu.html");
writeFileSync(htmlPath, html);

const chrome = "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome";
const res = spawnSync(
  chrome,
  [
    "--headless",
    "--disable-gpu",
    "--no-pdf-header-footer",
    `--print-to-pdf=${out}`,
    `file://${htmlPath}`,
  ],
  { stdio: "inherit" },
);
if (res.status !== 0) {
  rmSync(work, { recursive: true, force: true });
  console.error("chrome failed to render the menu PDF");
  process.exit(1);
}
console.log(`pop menu → ${out}`);

// ── PDF → PNG (pages stitched side-by-side into one image) ────────────
const ppm = spawnSync(
  "pdftoppm",
  ["-png", "-r", "200", out, join(work, "pg")],
  { stdio: "inherit" },
);
if (ppm.status === 0) {
  const pages = readdirSync(work)
    .filter((f) => f.startsWith("pg") && f.endsWith(".png"))
    .sort()
    .map((f) => join(work, f));
  // trim each page to its content, then lay panels side-by-side like an
  // open folded menu, tops aligned, with a uniform cream border.
  const groups = pages.flatMap((p) => ["(", p, "-trim", "+repage", ")"]);
  const stitch = spawnSync(
    "magick",
    [
      ...groups,
      "-background", "#f4efe2",
      "-gravity", "north",
      "+smush", "44",
      "-bordercolor", "#f4efe2", "-border", "44",
      pngOut,
    ],
    { stdio: "inherit" },
  );
  if (stitch.status === 0) console.log(`pop menu → ${pngOut}`);
  else console.error("magick failed to stitch the PNG");
} else {
  console.error("pdftoppm failed — skipping PNG");
}

rmSync(work, { recursive: true, force: true });
