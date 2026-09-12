// oskiewar netplay browser rig, 26.09.11
// The parts two end-to-end scripts share: the real relay and the real shell on
// a local port, and a browser seat pointed at them. Nothing here is stubbed —
// `OskiewarLiveManager` is the class the session server runs, and the pages are
// served the same `mac-test.html` and `oskiewar.js` that ship.
//
// Used by netplay-browser.mjs (two seats, one fight) and netplay-rejoin.mjs
// (one seat that leaves and comes back).
import { createServer } from "node:http";
import { readFile } from "node:fs/promises";
import { extname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { WebSocketServer } from "ws";
import puppeteer from "puppeteer";
import { OskiewarLiveManager } from "../../../session-server/oskiewar-live-manager.mjs";
import { validateDemo } from "../../../system/netlify/functions/oskiewar-replays.mjs";

const here = resolve(fileURLToPath(new URL(".", import.meta.url)), "..");
const repo = resolve(here, "../..");
const chrome = process.env.PUPPETEER_EXECUTABLE_PATH ||
  "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome";
const mime = new Map([[".html", "text/html; charset=utf-8"],
  [".js", "text/javascript; charset=utf-8"], [".mjs", "text/javascript; charset=utf-8"],
  [".ttf", "font/ttf"], [".svg", "image/svg+xml"]]);

export const wait = (ms) => new Promise((done) => setTimeout(done, ms));

export function fileFor(pathname) {
  if (pathname === "/oskiewar.js") return join(here, "oskiewar.js");
  if (/^\/(oskiewar-(sfx|voice|midi)|frame-driver|round-room|account)\.mjs$/.test(pathname))
    return join(here, pathname.slice(1));
  if (pathname === "/aesthetic.computer/dep/@akamfoad/qr/qr.mjs")
    return join(repo, "system/public/aesthetic.computer/dep/@akamfoad/qr/qr.mjs");
  if (pathname.startsWith("/aesthetic.computer/lib/") ||
      pathname.startsWith("/aesthetic.computer/cursors/"))
    return join(repo, "system/public", pathname.slice(1));
  if (pathname === "/ComicRelief-Regular.ttf")
    return join(repo,
      "system/public/papers.aesthetic.computer/foundry/fonts/ComicRelief-Regular.ttf");
  if (pathname === "/" || /^\/[a-z0-9-]+\/?$/.test(pathname)) return join(here, "mac-test.html");
  return "";
}

// The real relay, in this process, on a local port.
export async function localStack() {
  const manager = new OskiewarLiveManager();
  // Every round the fight files, and every one the store would have refused.
  const filed = { rounds: [], rejected: [] };
  const server = createServer(async (request, response) => {
    const url = new URL(request.url, "http://127.0.0.1");
    if (url.pathname === "/favicon.ico") { response.writeHead(204); response.end(); return; }
    if (url.pathname === "/api/product-analytics-config" ||
        url.pathname === "/api/oskiewar-pops" || url.pathname === "/api/oskiewar-country") {
      response.writeHead(200, { "content-type": "application/json" });
      response.end("{}"); return;
    }
    if (url.pathname === "/api/oskiewar-replays") {
      // Versus rounds are filed now, so this end of the wire has to be real:
      // a 404 on the POST would have the game logging upload errors through a
      // fight the rig is trying to measure. Each demo is validated by the
      // store's OWN reader and kept for the script to inspect.
      if (request.method === "POST") {
        const chunks = [];
        for await (const chunk of request) chunks.push(chunk);
        let demo = null;
        try { demo = JSON.parse(Buffer.concat(chunks).toString()); } catch {}
        const invalid = demo ? validateDemo(demo) : "Invalid JSON";
        if (invalid) {
          filed.rejected.push({ id: demo?.roundId || "?", reason: invalid });
          response.writeHead(400, { "content-type": "application/json" });
          response.end(JSON.stringify({ error: invalid }));
          return;
        }
        filed.rounds.push(demo);
        response.writeHead(201, { "content-type": "application/json" });
        response.end(JSON.stringify({ ok: true, id: demo.matchId, stored: true }));
        return;
      }
      response.writeHead(404, { "content-type": "application/json" });
      response.end('{"replay":null}'); return;
    }
    const path = fileFor(url.pathname);
    if (!path) { response.writeHead(404); response.end("not found"); return; }
    try {
      const body = await readFile(path);
      response.writeHead(200, { "content-type": mime.get(extname(path)) ||
        "application/octet-stream", "cache-control": "no-store" });
      response.end(body);
    } catch (error) { response.writeHead(500); response.end(error.message); }
  });
  const sockets = new WebSocketServer({ server });
  sockets.on("connection", (ws, request) => {
    if (manager.handleConnection(ws, request)) return;
    ws.close(1008, "unknown endpoint");
  });
  await new Promise((ready) => server.listen(0, "127.0.0.1", ready));
  const port = server.address().port;
  return { server, manager, filed, origin: `http://127.0.0.1:${port}`,
    wsOrigin: `ws://127.0.0.1:${port}` };
}

// Everything or nothing. The game makes two kinds of sound and they leave by
// different doors: drums and hits go through WebAudio, which headless Chrome
// renders to no device at all, while "FIGHT", "K.O." and the fighters' names
// go to `speechSynthesis`, which on macOS is the PLATFORM's voice and comes
// out of the real speakers whatever Chrome is doing. So an automated run used
// to announce every round out loud over a fight that was otherwise silent.
// A test that runs in the background is muted on both doors; `--audio` opens
// both, which means a real window, because WebAudio needs a device.
export const launchBrowser = (left, { headful = false, audible = false } = {}) =>
  puppeteer.launch({
    executablePath: chrome, headless: headful || audible ? false : "new",
    args: ["--autoplay-policy=no-user-gesture-required", "--no-first-run",
      ...(audible ? [] : ["--mute-audio"]),
      "--window-size=900,560", `--window-position=${left},60`] });

// Each seat gets its OWN browser, not another tab: a background tab's frame
// driver parks and settles its owed ticks in one-second bursts, so two tabs
// in one window spend most of their ticks holding for each other and read
// like a netcode fault that is really a harness artifact.
//
// The shell hardcodes the production relay, so each page is handed a local
// one before any of its script runs.
export async function openSeat(browser, origin, wsOrigin, path, label,
  { audible = false } = {}) {
  const page = (await browser.pages())[0] || await browser.newPage();
  await page.setViewport({ width: 900, height: 560 });
  page.on("pageerror", (error) => console.log(`  [${label}] page error: ${error.message}`));
  page.on("console", (message) => {
    const text = message.text();
    if (/error|desync|NET_(END|BEGIN)/i.test(text)) console.log(`  [${label}] ${text}`);
  });
  await page.evaluateOnNewDocument((local) => {
    const Native = WebSocket;
    window.WebSocket = function Patched(url, ...rest) {
      return new Native(String(url)
        .replace("wss://session-server.aesthetic.computer", local), ...rest);
    };
    window.WebSocket.prototype = Native.prototype;
    Object.assign(window.WebSocket, { CONNECTING: 0, OPEN: 1, CLOSING: 2, CLOSED: 3 });
    window.__netplayTelemetry = [];
  }, wsOrigin);
  // `--mute-audio` silences Chrome's own graph but not the platform's voice,
  // which is a different service entirely; this is the other half of the
  // switch. The object stays in place so the voice module still finds it.
  if (!audible) await page.evaluateOnNewDocument(() => {
    const quiet = function speak() {};
    try { SpeechSynthesis.prototype.speak = quiet; } catch (_) {}
    try { speechSynthesis.speak = quiet; } catch (_) {}
  });
  await page.goto(origin + path, { waitUntil: "domcontentloaded" });
  return page;
}

export const readStats = (page) => page.evaluate(() => ({
  net: globalThis.__oskiewarNetStats || null,
  room: globalThis.__oskiewarVersusRoom || "",
  touchScreen: globalThis.__oskiewarTouch?.screen || "",
  seat: globalThis.__oskiewarRoundBridge?.seat || "",
  live: globalThis.__oskiewarRoundBridge?.live === true,
  account: globalThis.__oskiewarAccount?.handle || "",
}));

// Hold each key well past one frame: the shell samples at 60 Hz and a
// sub-frame synthetic press is missed (this is documented behaviour).
export async function play(page, script, beat = 180) {
  for (const keys of script) {
    for (const key of keys) await page.keyboard.down(key);
    await wait(beat);
    for (const key of keys) await page.keyboard.up(key);
    await wait(60);
  }
}
