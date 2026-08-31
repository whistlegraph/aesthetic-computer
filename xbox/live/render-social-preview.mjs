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
const posterSize = { width: 1200, height: 630 };
const videoSize = { width: 720, height: 1280 };
const captureSeconds = 2;
const loopSeconds = captureSeconds * 2;
const sources = ["oskiewar.js", "mac-test.html", "frame-driver.mjs"];
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
  // Every module the shell imports has to be listed, or the page dies on a 404
  // and the capture comes out empty rather than loudly wrong.
  if (["/oskiewar.js", "/oskiewar-sfx.mjs", "/oskiewar-voice.mjs",
      "/oskiewar-midi.mjs", "/frame-driver.mjs",
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
  await page.setViewport({ ...posterSize, deviceScaleFactor: 1 });
  await page.emulateMediaFeatures([{ name: "prefers-color-scheme", value: "light" }]);
  await page.goto(`${origin}/?social-preview`, { waitUntil: "networkidle2" });
  await page.evaluate(() => document.fonts.ready);
  await new Promise((resolveWait) => setTimeout(resolveWait, 900));
  await page.screenshot({ path: poster, type: "jpeg", quality: 94 });

  // The MP4 is a phone-shaped, silent light-mode title loop. Reversing the
  // short capture back to its first frame removes the visible loop seam.
  await page.setViewport({ ...videoSize, deviceScaleFactor: 1 });
  await page.goto(`${origin}/?social-preview`, { waitUntil: "networkidle2" });
  await page.evaluate(() => document.fonts.ready);
  await new Promise((resolveWait) => setTimeout(resolveWait, 900));
  const recorder = await page.screencast({ path: webm, fps: 30 });
  await new Promise((resolveWait) => setTimeout(resolveWait,
    captureSeconds * 1000 + 150));
  await recorder.stop();
  await page.close();
  const loopFilter = `[0:v]trim=duration=${captureSeconds},` +
    "setpts=PTS-STARTPTS,split[forward][reverse];" +
    "[reverse]reverse,setpts=PTS-STARTPTS[backward];" +
    "[forward][backward]concat=n=2:v=1:a=0,fps=30,format=yuv420p[loop]";
  const encoded = spawnSync("ffmpeg", ["-y", "-i", webm, "-an",
    "-filter_complex", loopFilter, "-map", "[loop]",
    "-c:v", "libx264", "-profile:v", "main", "-level", "3.1",
    "-crf", "23", "-movflags", "+faststart", "-t", `${loopSeconds}`,
    video], { encoding: "utf8" });
  if (encoded.status !== 0) throw new Error(encoded.stderr || "ffmpeg failed");
  await writeFile(manifestPath, JSON.stringify({
    format: "ac.oskiewar.social-preview", version: 2, build,
    theme: "light", imageWidth: posterSize.width, imageHeight: posterSize.height,
    videoWidth: videoSize.width, videoHeight: videoSize.height,
    durationSeconds: loopSeconds, framesPerSecond: 30, audio: false,
    image: "oskiewar-title.jpg", video: "oskiewar-title.mp4",
  }, null, 2) + "\n");
  console.log(`burned ${poster}\nburned ${video}\nbuild ${build}`);
} finally {
  await browser.close();
  await new Promise((closed) => server.close(closed));
  await rm(temporary, { recursive: true, force: true });
}
