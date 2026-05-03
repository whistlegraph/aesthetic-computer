#!/usr/bin/env node
// test-slide.mjs — render ONE audience slide as a 1080x1920 PNG so we
// can sanity-check the prompt + duotone composition without running the
// full recap pipeline (no TTS, no transcribe, no align, no concat).
//
// Generates the per-segment photo via OpenAI gpt-image-2 if it doesn't
// already exist (cached at recap/out/jeffrey-photos/<segment>.png), then
// composes the slide HTML via puppeteer with the same fonts + palette
// slides.mjs uses, and writes the result to --out.
//
// Usage:
//   node bin/test-slide.mjs <audience-name> <segment-name> --out <path>
//   node bin/test-slide.mjs jeffrey-73h-2026-05-02 02_menuband_arc \
//        --out ~/Desktop/test-slide.png
//
// Flags:
//   --force       regen the photo even if cached
//   --skip-photo  use the existing cached photo only; fail if missing
//   --no-photo    render the slide without the photo (background only)

import { readFileSync, writeFileSync, mkdirSync, existsSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { homedir } from "node:os";
import { execSync } from "node:child_process";

// Resolve puppeteer from one of the known node_modules locations.
const __HERE = dirname(fileURLToPath(import.meta.url));
const __PUPPETEER_DIR = [
  resolve(__HERE, "../../oven/node_modules/puppeteer"),
  "/opt/oven/node_modules/puppeteer",
  resolve(__HERE, "../node_modules/puppeteer"),
].find((p) => existsSync(p));
if (!__PUPPETEER_DIR) {
  throw new Error("puppeteer not found in any known node_modules location");
}
const puppeteer = (await import(`${__PUPPETEER_DIR}/lib/esm/puppeteer/puppeteer.js`)).default;

const HERE = dirname(fileURLToPath(import.meta.url));
const ROOT = resolve(HERE, "..");
const REPO = resolve(ROOT, "..");

// ── puppeteer concurrency lock ─────────────────────────────────────────
// 8 GB machine cannot host two Chromium instances at once (the user
// crashed once on 2026-05-02 from parallel test-slide runs). Bail loud
// if anything else is already holding a puppeteer browser open.
{
  const myPid = String(process.pid);
  const sniff = (pat) => {
    try {
      return execSync(`pgrep -f "${pat}" 2>/dev/null || true`, { encoding: "utf8" })
        .split("\n").map(s => s.trim()).filter(s => s && s !== myPid);
    } catch { return []; }
  };
  const others = [
    ...sniff("bin/test-slide\\.mjs"),
    ...sniff("bin/slides\\.mjs"),
    ...sniff("pipeline\\.fish"),
  ];
  if (others.length) {
    console.error(`✗ another puppeteer-using process is running (pids ${others.join(", ")}) — refusing to launch a second Chromium on this 8 GB machine. Wait for it, or kill it.`);
    process.exit(2);
  }
}

// ── parse args ─────────────────────────────────────────────────────────
const argv = process.argv.slice(2);
const flags = {};
const positional = [];
for (let i = 0; i < argv.length; i++) {
  const a = argv[i];
  if (a.startsWith("--")) {
    const next = argv[i + 1];
    if (next !== undefined && !next.startsWith("--")) {
      flags[a.slice(2)] = next;
      i++;
    } else {
      flags[a.slice(2)] = true;
    }
  } else positional.push(a);
}

const audienceName = positional[0];
const segmentName = positional[1];
if (!audienceName || !segmentName) {
  console.error("usage: test-slide.mjs <audience> <segment> [--out PATH] [--force | --skip-photo | --no-photo]");
  process.exit(1);
}
const expandHome = (p) => p?.startsWith("~/") ? resolve(homedir(), p.slice(2)) : p;
const outPath = expandHome(flags.out) || `${ROOT}/out/test-slide.png`;
const force = !!flags.force;
const skipPhoto = !!flags["skip-photo"];
const noPhoto = !!flags["no-photo"];

// ── load audience ─────────────────────────────────────────────────────
const { audience, PALETTE } = await import(`${ROOT}/audience/${audienceName}.mjs`);
const slide = audience.slides[segmentName];
if (!slide) {
  console.error(`segment '${segmentName}' not found in audience '${audienceName}'`);
  console.error(`available: ${Object.keys(audience.slides).join(", ")}`);
  process.exit(1);
}

// ── photo gen / cache ─────────────────────────────────────────────────
const PHOTOS_DIR = `${ROOT}/out/jeffrey-photos`;
mkdirSync(PHOTOS_DIR, { recursive: true });
const photoPath = `${PHOTOS_DIR}/${segmentName}.png`;
let photoForBody = null;

if (noPhoto) {
  console.log("→ photo: skipped (--no-photo)");
} else if (existsSync(photoPath) && !force) {
  console.log(`→ photo cached: ${photoPath.replace(REPO + "/", "")}`);
  photoForBody = photoPath;
} else if (skipPhoto) {
  console.error(`✗ no cached photo at ${photoPath} and --skip-photo set`);
  process.exit(1);
} else {
  if (!slide.metaphor) {
    console.error(`segment '${segmentName}' has no metaphor; cannot generate photo`);
    process.exit(1);
  }
  console.log("→ photo: generating via gpt-image-2");
  photoForBody = await generatePhoto(slide.metaphor, photoPath);
}

// ── load OpenAI key (same path as jeffrey-photos.mjs) ────────────────
function loadOpenAIKey() {
  if (process.env.OPENAI_API_KEY) return process.env.OPENAI_API_KEY;
  const vault = `${REPO}/aesthetic-computer-vault/.devcontainer/envs/devcontainer.env`;
  if (existsSync(vault)) {
    for (const line of readFileSync(vault, "utf8").split("\n")) {
      if (line.startsWith("OPENAI_API_KEY=")) {
        return line.slice("OPENAI_API_KEY=".length).trim().replace(/^['"]|['"]$/g, "");
      }
    }
  }
  throw new Error("OPENAI_API_KEY not found in env or vault");
}

async function generatePhoto(metaphor, outFile) {
  const SHOOT_DIR = `${REPO}/portraits/jeffrey/corpus/shoot`;
  const ARCHIVE_DIR = `${REPO}/portraits/jeffrey/ig-archive/whistlegraph`;
  const refs = [
    `${SHOOT_DIR}/jeffery-av--07.jpg`,
    `${SHOOT_DIR}/jeffery-av--01.jpg`,
    `${SHOOT_DIR}/jeffery-av--04.jpg`,
    `${ARCHIVE_DIR}/2018-12-02_Bq4ckGFFNtW.jpg`,
    `${ARCHIVE_DIR}/2020-09-02_CEpxlO2FOvD.jpg`,
    `${ARCHIVE_DIR}/2021-07-10_CRI095Vl7AO_1.jpg`,
    `${ARCHIVE_DIR}/2025-01-25_DFQ2lHPzN_W.jpg`,
    `${ARCHIVE_DIR}/2017-04-10_BStid5yjTHq.jpg`,
  ].filter((p) => existsSync(p));

  const apiKey = loadOpenAIKey();
  const fd = new FormData();
  fd.append("model", "gpt-image-2");
  fd.append("prompt", metaphor);
  fd.append("size", "1024x1536");
  fd.append("quality", "high");
  fd.append("n", "1");
  for (const ref of refs) {
    const buf = readFileSync(ref);
    const ext = ref.toLowerCase().endsWith(".png") ? "png" : "jpeg";
    fd.append("image[]", new Blob([buf], { type: `image/${ext}` }), ref.split("/").pop());
  }
  console.log(`  refs: ${refs.length}`);
  const t0 = Date.now();
  const res = await fetch("https://api.openai.com/v1/images/edits", {
    method: "POST",
    headers: { Authorization: `Bearer ${apiKey}` },
    body: fd,
  });
  if (!res.ok) {
    const err = await res.text();
    throw new Error(`OpenAI ${res.status}: ${err.slice(0, 500)}`);
  }
  const json = await res.json();
  const b64 = json.data?.[0]?.b64_json;
  if (!b64) throw new Error(`no image returned: ${JSON.stringify(json).slice(0, 200)}`);
  writeFileSync(outFile, Buffer.from(b64, "base64"));
  console.log(`  ✓ ${outFile.replace(REPO + "/", "")} (${((Date.now() - t0) / 1000).toFixed(1)}s)`);
  return outFile;
}

// ── compose slide HTML ────────────────────────────────────────────────
console.log("→ composing slide HTML");

const FONT_BOLD = `${REPO}/system/public/type/webfonts/ywft-processing-bold.ttf`;
const FONT_REG = `${REPO}/system/public/type/webfonts/ywft-processing-regular.ttf`;
const fontBoldB64 = readFileSync(FONT_BOLD).toString("base64");
const fontRegB64 = readFileSync(FONT_REG).toString("base64");

// Resolve every query the slide declares (besides `photo`, already done
// above) into the args object the slide's body() expects. We mini-replicate
// scout.mjs here so the test-slide path doesn't depend on the full pipeline.
function fileToDataUrl(p) {
  const ext = p.toLowerCase().split(".").pop();
  const mime = ext === "svg" ? "image/svg+xml" : ext === "webp" ? "image/webp" : ext === "jpg" || ext === "jpeg" ? "image/jpeg" : "image/png";
  return `data:${mime};base64,${readFileSync(p).toString("base64")}`;
}

const bodyArgs = {};
if (photoForBody) bodyArgs.photo = fileToDataUrl(photoForBody);

if (slide.queries) {
  for (const [qname, q] of Object.entries(slide.queries)) {
    if (qname === "photo") continue;
    if (q.screenshot) {
      const cached = `${ROOT}/out/screenshots/${segmentName}-${qname}.png`;
      if (existsSync(cached)) {
        console.log(`→ screenshot cached: ${qname} (${cached.replace(REPO + "/", "")})`);
        bodyArgs[qname] = fileToDataUrl(cached);
      } else {
        console.log(`→ screenshot not cached: ${qname} — fetching ${q.screenshot}`);
        const { spawnSync } = await import("node:child_process");
        const r = spawnSync("node", [`${ROOT}/bin/screenshots.mjs`, audienceName, "--only", segmentName], { stdio: "inherit" });
        if (r.status !== 0) console.warn(`  ⚠ screenshot fetch failed for ${qname}`);
        if (existsSync(cached)) bodyArgs[qname] = fileToDataUrl(cached);
      }
    } else if (q.glob) {
      // glob → expand via shell, take first match. PDF support: pdftoppm
      // first page → PNG → data URL.
      const { execSync, execFileSync } = await import("node:child_process");
      const abs = q.glob.startsWith("/") ? q.glob : `${REPO}/${q.glob}`;
      try {
        const out = execSync(`ls -1 ${abs} 2>/dev/null | head -n 1`, { encoding: "utf8" }).trim();
        if (!out) continue;
        if (q.pdfPage) {
          const { tmpdir } = await import("node:os");
          const { basename } = await import("node:path");
          const stem = `${tmpdir()}/test-slide-${basename(out, ".pdf")}-p${q.pdfPage}`;
          execFileSync("pdftoppm", [
            "-png", "-r", "150",
            "-f", String(q.pdfPage), "-l", String(q.pdfPage),
            "-scale-to", String(q.pdfWidth || 800),
            out, stem,
          ]);
          const pngs = execSync(`ls -1 ${stem}-*.png 2>/dev/null | head -n 1`, { encoding: "utf8" }).trim();
          if (pngs) bodyArgs[qname] = fileToDataUrl(pngs);
        } else {
          bodyArgs[qname] = fileToDataUrl(out);
        }
      } catch {}
    } else if (q.json) {
      const abs = q.json.startsWith("/") ? q.json : `${REPO}/${q.json}`;
      try { bodyArgs[qname] = JSON.parse(readFileSync(abs, "utf8")); } catch {}
    }
  }
}

const bodyHtml = typeof slide.body === "function"
  ? slide.body(bodyArgs)
  : slide.body || slide;

const html = `<!DOCTYPE html>
<html><head><meta charset="utf-8"><style>
@font-face {
  font-family: 'ProcessingB';
  src: url(data:font/ttf;base64,${fontBoldB64}) format('truetype');
}
@font-face {
  font-family: 'ProcessingR';
  src: url(data:font/ttf;base64,${fontRegB64}) format('truetype');
}
* { box-sizing: border-box; margin: 0; padding: 0; }
html, body { width: 1080px; height: 1920px; font-family: 'ProcessingR'; -webkit-font-smoothing: antialiased; }
body { background: ${PALETTE.bg}; position: relative; overflow: hidden; }
</style></head><body>
${bodyHtml}
</body></html>`;

// ── render ────────────────────────────────────────────────────────────
console.log("→ rendering 1080×1920 PNG via puppeteer");
const browser = await puppeteer.launch({
  headless: true,
  args: ["--no-sandbox", "--allow-file-access-from-files"],
});
const page = await browser.newPage();
await page.setViewport({ width: 1080, height: 1920, deviceScaleFactor: 1 });
await page.setContent(html, { waitUntil: "networkidle0" });
// Give fonts a beat; puppeteer's networkidle doesn't always wait on data: URIs.
await new Promise((r) => setTimeout(r, 200));
mkdirSync(dirname(outPath), { recursive: true });
await page.screenshot({ path: outPath, type: "png", fullPage: false });
await browser.close();
console.log(`✓ ${outPath}`);
