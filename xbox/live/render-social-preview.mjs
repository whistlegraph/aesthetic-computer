// Burn oskiewar's real title renderer into Open Graph poster/video assets.

import { createHash } from "node:crypto";
import { spawnSync } from "node:child_process";
import { createServer } from "node:http";
import { mkdir, mkdtemp, readFile, rm, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { extname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const live = resolve(fileURLToPath(new URL(".", import.meta.url)));
const repo = resolve(live, "../..");
const output = join(live, "social");
const poster = join(output, "oskiewar-title.jpg");
const video = join(output, "oskiewar-title.mp4");
const manifestPath = join(output, "manifest.json");
const sources = ["hello.js", "mac-test.html", "frame-driver.mjs"];
const sourceHash = createHash("sha256");
for (const name of sources) sourceHash.update(await readFile(join(live, name)));
sourceHash.update(await readFile(new URL(import.meta.url)));
const build = sourceHash.digest("hex").slice(0, 16);

if (process.argv.includes("--check")) {
  const manifest = JSON.parse(await readFile(manifestPath, "utf8"));
  if (manifest.build !== build) throw new Error(
    `social preview is stale (${manifest.build} != ${build}); run npm run xbox:burn:oskiewar-social`);
  console.log(`oskiewar social preview ${build} is current`);
  process.exit(0);
}

const mime = new Map([
  [".html", "text/html; charset=utf-8"], [".js", "text/javascript; charset=utf-8"],
  [".mjs", "text/javascript; charset=utf-8"], [".ttf", "font/ttf"],
  [".svg", "image/svg+xml"],
]);
function fileFor(pathname) {
  if (pathname === "/" || pathname === "/mac-test.html")
    return join(live, "mac-test.html");
  if (["/hello.js", "/oskiewar-sfx.mjs", "/frame-driver.mjs",
      "/round-room.mjs"].includes(pathname)) return join(live, pathname.slice(1));
  if (pathname === "/aesthetic.computer/dep/@akamfoad/qr/qr.mjs")
    return join(repo, "system/public/aesthetic.computer/dep/@akamfoad/qr/qr.mjs");
  if (pathname === "/aesthetic.computer/lib/product-analytics.mjs")
    return join(repo, "system/public/aesthetic.computer/lib/product-analytics.mjs");
  if (pathname === "/aesthetic.computer/lib/oskiewar-analytics.mjs")
    return join(repo, "system/public/aesthetic.computer/lib/oskiewar-analytics.mjs");
  if (pathname === "/aesthetic.computer/cursors/precise.svg" ||
      pathname === "/aesthetic.computer/cursors/active.svg")
    return join(repo, "system/public", pathname.slice(1));
  if (pathname === "/ComicRelief-Regular.ttf") return join(repo,
    "system/public/papers.aesthetic.computer/foundry/fonts/ComicRelief-Regular.ttf");
  return "";
}

const server = createServer(async (request, response) => {
  const url = new URL(request.url, "http://127.0.0.1");
  if (url.pathname === "/api/product-analytics-config") {
    response.writeHead(200, { "content-type": "application/json" });
    response.end("{}");
    return;
  }
  if (url.pathname === "/favicon.ico") {
    response.writeHead(204); response.end(); return;
  }
  const path = fileFor(url.pathname);
  if (!path) { response.writeHead(404); response.end("not found"); return; }
  try {
    const body = await readFile(path);
    response.writeHead(200, { "content-type": mime.get(extname(path)) ||
      "application/octet-stream", "cache-control": "no-store" });
    response.end(body);
  } catch (error) {
    response.writeHead(500); response.end(error.message);
  }
});

await mkdir(output, { recursive: true });
await new Promise((ready) => server.listen(0, "127.0.0.1", ready));
const origin = `http://127.0.0.1:${server.address().port}`;
const temporary = await mkdtemp(join(tmpdir(), "oskiewar-social-"));
const webm = join(temporary, "title.webm");
const chrome = process.env.PUPPETEER_EXECUTABLE_PATH ||
  "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome";
const { default: puppeteer } = await import("puppeteer");
const browser = await puppeteer.launch({ headless: true, executablePath: chrome,
  args: ["--autoplay-policy=no-user-gesture-required"] });
try {
  const page = await browser.newPage();
  await page.setViewport({ width: 1200, height: 630, deviceScaleFactor: 1 });
  await page.emulateMediaFeatures([{ name: "prefers-color-scheme", value: "dark" }]);
  await page.goto(`${origin}/?social-preview`, { waitUntil: "networkidle2" });
  await page.evaluate(() => document.fonts.ready);
  await new Promise((resolveWait) => setTimeout(resolveWait, 900));
  await page.screenshot({ path: poster, type: "jpeg", quality: 94 });
  const recorder = await page.screencast({ path: webm, fps: 30 });
  await new Promise((resolveWait) => setTimeout(resolveWait, 6000));
  await recorder.stop();
  await page.close();
  const encoded = spawnSync("ffmpeg", ["-y", "-i", webm, "-an",
    "-c:v", "libx264", "-profile:v", "main", "-level", "3.1",
    "-pix_fmt", "yuv420p", "-r", "30", "-movflags", "+faststart",
    "-t", "6", video], { encoding: "utf8" });
  if (encoded.status !== 0) throw new Error(encoded.stderr || "ffmpeg failed");
  await writeFile(manifestPath, JSON.stringify({
    format: "ac.oskiewar.social-preview", version: 1, build,
    width: 1200, height: 630, durationSeconds: 6, framesPerSecond: 30,
    image: "oskiewar-title.jpg", video: "oskiewar-title.mp4",
  }, null, 2) + "\n");
  console.log(`burned ${poster}\nburned ${video}\nbuild ${build}`);
} finally {
  await browser.close();
  await new Promise((closed) => server.close(closed));
  await rm(temporary, { recursive: true, force: true });
}
