// Render oskiewar's Steam store images straight out of the game.
//
//   node xbox/steam/store-page/render-assets.mjs            # everything
//   node xbox/steam/store-page/render-assets.mjs capsules   # title lockups only
//   node xbox/steam/store-page/render-assets.mjs shots      # 1920x1080 gameplay
//   node xbox/steam/store-page/render-assets.mjs logo       # transparent logotype
//
// Every capsule is a real render at its own viewport — the game lays itself
// out per shape, so nothing is resampled. The shot-list and Valve's sizes are
// in assets-needed.md; this file is that table made executable. Output lands
// in ./assets. Gameplay screenshots land in ./assets/shots as candidates —
// pick five by eye, Valve wants "what your game is actually like to play".

import { spawnSync } from "node:child_process";
import { createServer } from "node:http";
import { mkdir, readFile, rename, rm } from "node:fs/promises";
import { extname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const here = resolve(fileURLToPath(new URL(".", import.meta.url)));
const live = resolve(here, "../../live");
const repo = resolve(live, "../..");
const output = join(here, "assets");
const what = process.argv[2] || "all";
const wants = (name) => what === "all" || what === name;

// Valve's post-2024 store + library sizes. `hud: false` hides the key legend
// and clock; capsules carry "just your game logo and artwork".
const capsules = [
  { name: "main-capsule", width: 1232, height: 706 },
  { name: "header-capsule", width: 920, height: 430 },
  { name: "small-capsule", width: 462, height: 174 },
  { name: "vertical-capsule", width: 748, height: 896 },
  { name: "library-capsule", width: 600, height: 900 },
  { name: "library-header", width: 920, height: 430 },
  { name: "library-hero", width: 3840, height: 1240 },
  { name: "page-background", width: 1438, height: 810, dim: true },
  { name: "event-cover", width: 800, height: 450 },
];

const mime = new Map([
  [".html", "text/html; charset=utf-8"], [".js", "text/javascript; charset=utf-8"],
  [".mjs", "text/javascript; charset=utf-8"], [".ttf", "font/ttf"],
  [".woff2", "font/woff2"], [".svg", "image/svg+xml"],
]);
// Same map as render-social-preview.mjs: the page dies on a 404'd module.
function fileFor(pathname) {
  if (pathname === "/" || pathname === "/mac-test.html") return join(live, "mac-test.html");
  if (/^\/[a-z0-9-]+\.(mjs|js)$/.test(pathname)) return join(live, pathname.slice(1));
  if (pathname === "/aesthetic.computer/lib/auth0-otp.mjs")
    return join(repo, "system/public/aesthetic.computer/lib/auth0-otp.mjs");
  if (pathname.startsWith("/aesthetic.computer/"))
    return join(repo, "system/public", pathname.slice(1));
  if (pathname.startsWith("/ComicRelief-Regular."))
    return join(repo, "system/public/papers.aesthetic.computer/foundry/fonts", pathname.slice(1));
  return "";
}
const server = createServer(async (request, response) => {
  const url = new URL(request.url, "http://127.0.0.1");
  if (url.pathname === "/api/product-analytics-config") {
    response.writeHead(200, { "content-type": "application/json" }); response.end("{}"); return;
  }
  // No QR codes in store screenshots: they encode oskiewar.com links, which
  // are marketing on Valve's terms. An undefined qrcode trips the game's own
  // typeof guards and the four QR sites collapse.
  if (url.pathname.endsWith("/qr/qr.mjs")) {
    response.writeHead(200, { "content-type": "text/javascript" });
    response.end("export const qrcode = undefined;"); return;
  }
  const path = fileFor(url.pathname);
  if (!path) { response.writeHead(404); response.end("not found"); return; }
  try {
    response.writeHead(200, { "content-type": mime.get(extname(path)) ||
      "application/octet-stream", "cache-control": "no-store" });
    response.end(await readFile(path));
  } catch (error) { response.writeHead(500); response.end(error.message); }
});

await mkdir(join(output, "shots"), { recursive: true });
await new Promise((ready) => server.listen(0, "127.0.0.1", ready));
const origin = `http://127.0.0.1:${server.address().port}`;
const chrome = process.env.PUPPETEER_EXECUTABLE_PATH ||
  "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome";
const { default: puppeteer } = await import("puppeteer");
const browser = await puppeteer.launch({ headless: true, executablePath: chrome,
  args: ["--autoplay-policy=no-user-gesture-required", "--use-gl=angle",
    "--use-angle=metal", "--ignore-gpu-blocklist"] });
const sleep = (ms) => new Promise((r) => setTimeout(r, ms));

async function openTitle(page, { width, height, flags, query = "social-preview&opponent=fight" }) {
  await page.setViewport({ width, height, deviceScaleFactor: 1 });
  await page.evaluateOnNewDocument((f) => { globalThis.__oskiewarRenderFlags = f; }, flags);
  await page.goto(`${origin}/?${query}`, { waitUntil: "networkidle2" });
  await page.evaluate(() => document.fonts.ready);
  await sleep(900);
}

try {
  if (wants("capsules")) {
    for (const capsule of capsules) {
      const page = await browser.newPage();
      await page.emulateMediaFeatures([{ name: "prefers-color-scheme", value: "dark" }]);
      await openTitle(page, { ...capsule, flags: { hud: false, keys: false } });
      const file = join(output, `${capsule.name}.png`);
      await page.screenshot({ path: file, type: "png" });
      await page.close();
      if (capsule.dim) {
        // Valve: page backgrounds should be "subtle, not too bright".
        const bright = file.replace(/\.png$/, "-bright.png");
        await rename(file, bright);
        const dimmed = spawnSync("ffmpeg", ["-y", "-i", bright, "-vf",
          "eq=brightness=-0.18:saturation=0.75", "-frames:v", "1", "-update", "1", file],
          { encoding: "utf8" });
        if (dimmed.status !== 0) throw new Error(dimmed.stderr);
        await rm(bright);
      }
      console.log(`🖼  ${capsule.name} ${capsule.width}×${capsule.height}`);
    }
  }

  if (wants("logo")) {
    // The library logo overlays the hero, so it has to be the wordmark alone
    // on transparency. Render the lockup with no fighters, no dust, no HUD on
    // the flat night ground, crop the band, and key the ground out.
    const page = await browser.newPage();
    await page.emulateMediaFeatures([{ name: "prefers-color-scheme", value: "dark" }]);
    await openTitle(page, { width: 1920, height: 1080, flags: { hud: false, keys: false, dust: false, sky: false },
      query: "social-preview" });
    const raw = join(output, "shots", "_logo-raw.png");
    await page.screenshot({ path: raw, type: "png" });
    await page.close();
    const keyed = spawnSync("ffmpeg", ["-y", "-i", raw, "-vf",
      "crop=960:250:410:330,colorkey=0x07081c:0.12:0.1,scale=1280:-1:flags=lanczos",
      "-frames:v", "1", "-update", "1", join(output, "library-logo.png")], { encoding: "utf8" });
    if (keyed.status !== 0) throw new Error(keyed.stderr);
    console.log("🖼  library-logo (transparent, 1280 wide)");
  }

  if (wants("shots")) {
    const page = await browser.newPage();
    await page.emulateMediaFeatures([{ name: "prefers-color-scheme", value: "dark" }]);
    await page.setViewport({ width: 1920, height: 1080, deviceScaleFactor: 1 });
    await page.goto(`${origin}/?self-play&opponent=fight`, { waitUntil: "networkidle2" });
    await page.evaluate(() => document.fonts.ready);
    await sleep(4000);
    for (let index = 0; index < 120; index++) {
      await sleep(750);
      const file = join(output, "shots", `shot-${String(index).padStart(2, "0")}.png`);
      await page.screenshot({ path: file, type: "png" });
    }
    await page.close();
    console.log("📸 120 gameplay candidates in assets/shots — pick five");
  }
} finally {
  await browser.close();
  await new Promise((closed) => server.close(closed));
}
