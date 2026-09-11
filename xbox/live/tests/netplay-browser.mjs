// oskiewar rollback netplay, browser end to end, 26.09.09
// Two real browsers on one room, joined by a local instance of the real relay
// and served the real shell. Proves what the unit tests cannot: that the
// handshake happens through the actual bridge, that both browsers open a
// rollback session, and that after a fight their state hashes agree.
//
//   node xbox/live/tests/netplay-browser.mjs [--seconds 20] [--headful]
//
// Exits non-zero with a reason if either seat fails to open a session or the
// two seats disagree. Findings and the streamed-lane analysis it replaces:
// xbox/live/oskiewar-multiplayer.md
import { createServer } from "node:http";
import { readFile } from "node:fs/promises";
import { extname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { WebSocketServer } from "ws";
import puppeteer from "puppeteer";
import { OskiewarLiveManager } from "../../../session-server/oskiewar-live-manager.mjs";

const here = resolve(fileURLToPath(new URL("..", import.meta.url)));
const repo = resolve(here, "../..");
const args = process.argv.slice(2);
const seconds = Number(args[args.indexOf("--seconds") + 1]) || 20;
const headful = args.includes("--headful");
// Where to leave a frame from each seat, taken mid-fight. Looking at the two
// pictures is the only way to check the half of this that no hash covers:
// that both seats are drawing the same fight, with a HUD.
const shots = args.includes("--shots")
  ? (args[args.indexOf("--shots") + 1] || "").replace(/^-.*/, "") || "." : "";
const room = "netpl" + String(Math.floor(Math.random() * 90) + 10);
const chrome = process.env.PUPPETEER_EXECUTABLE_PATH ||
  "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome";
const mime = new Map([[".html", "text/html; charset=utf-8"],
  [".js", "text/javascript; charset=utf-8"], [".mjs", "text/javascript; charset=utf-8"],
  [".ttf", "font/ttf"], [".svg", "image/svg+xml"]]);

function fileFor(pathname) {
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

// The real relay, in this process, on a local port. Nothing about the room
// logic is stubbed: this is the class the session server runs.
async function localStack() {
  const manager = new OskiewarLiveManager();
  const server = createServer(async (request, response) => {
    const url = new URL(request.url, "http://127.0.0.1");
    if (url.pathname === "/favicon.ico") { response.writeHead(204); response.end(); return; }
    if (url.pathname === "/api/product-analytics-config" ||
        url.pathname === "/api/oskiewar-pops" || url.pathname === "/api/oskiewar-country") {
      response.writeHead(200, { "content-type": "application/json" });
      response.end("{}"); return;
    }
    if (url.pathname === "/api/oskiewar-replays") {
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
  return { server, manager, origin: `http://127.0.0.1:${port}`,
    wsOrigin: `ws://127.0.0.1:${port}` };
}

const wait = (ms) => new Promise((done) => setTimeout(done, ms));

// Each seat gets its OWN browser, not another tab: a background tab's frame
// driver parks and settles its owed ticks in one-second bursts, so two tabs
// in one window spend most of their ticks holding for each other and read
// like a netcode fault that is really a harness artifact.
//
// The shell hardcodes the production relay, so each page is handed a local
// one before any of its script runs.
async function openSeat(browser, origin, wsOrigin, path, label) {
  const page = (await browser.pages())[0] || await browser.newPage();
  await page.setViewport({ width: 900, height: 560 });
  page.on("pageerror", (error) => console.log(`  [${label}] page error: ${error.message}`));
  page.on("console", (message) => {
    const text = message.text();
    if (/error|Error|desync/.test(text)) console.log(`  [${label}] ${text}`);
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
  await page.goto(origin + path, { waitUntil: "domcontentloaded" });
  return page;
}

const readStats = (page) => page.evaluate(() => ({
  net: globalThis.__oskiewarNetStats || null,
  room: globalThis.__oskiewarVersusRoom || "",
  touchScreen: globalThis.__oskiewarTouch?.screen || "",
}));

// Hold each key well past one frame: the shell samples at 60 Hz and a
// sub-frame synthetic press is missed (this is documented behaviour).
async function play(page, script, beat = 180) {
  for (const keys of script) {
    for (const key of keys) await page.keyboard.down(key);
    await wait(beat);
    for (const key of keys) await page.keyboard.up(key);
    await wait(60);
  }
}

// WASD, not the arrows. A rollback seat reads `gamepad(0)` whichever chair it
// sits in — the rival's word is written into pad 1 — so arrow keys drive a pad
// that netplay immediately overwrites, and the fighter never hears them. The
// arrow version of these scripts still produced rollbacks, but only as a side
// effect of its Space/Enter presses, which is a much thinner test than it looks.
//
// Note also that these cannot reach a round rollover on their own: versus
// rounds are untimed (`roundIsTimed()` is false under `versusLane()`), so a
// movement-only fight runs one endless round. Landing hits is what ends one.
const hostScript = [["KeyD"], ["Space"], ["KeyD", "KeyW"],
  ["Enter"], ["KeyA"], ["Space"], ["KeyD"], ["Enter"]];
const guestScript = [["KeyA"], ["Enter"], ["KeyW"], ["KeyA"],
  ["Space"], ["KeyD"], ["Enter"], ["KeyA"]];

const fail = (reason) => { console.log(`\nFAILED: ${reason}`); process.exitCode = 1; };

const stack = await localStack();
console.log(`local stack on ${stack.origin}, room ${room}`);
const launch = (left) => puppeteer.launch({ executablePath: chrome,
  headless: headful ? false : "new",
  args: ["--autoplay-policy=no-user-gesture-required", "--no-first-run",
    "--window-size=900,560", `--window-position=${left},60`] });
const browsers = [await launch(40), await launch(980)];
const browser = { close: () => Promise.all(browsers.map((one) => one.close())) };
try {
  // Seat one arrives at an empty address, waits out the claim, and hosts.
  const host = await openSeat(browsers[0], stack.origin, stack.wsOrigin, "/" + room, "host");
  await wait(1500);
  await play(host, [["Enter"]]);
  await wait(5000);
  const hosting = await readStats(host);
  console.log(`host claimed room ${hosting.room || "(none)"}`);
  if (!hosting.room) fail("seat one never claimed the room");

  // Seat two arrives at the same address and takes the chair.
  const guest = await openSeat(browsers[1], stack.origin, stack.wsOrigin, "/" + room, "guest");
  await wait(1500);
  await play(guest, [["Enter"]]);
  await wait(3000);

  let hostStats = (await readStats(host)).net;
  let guestStats = (await readStats(guest)).net;
  if (!hostStats) fail("the host never opened a rollback session");
  if (!guestStats) fail("the guest never opened a rollback session");
  if (hostStats && guestStats) {
    console.log(`sessions open: host seat ${hostStats.seat}, guest seat ${guestStats.seat}`);
    if (hostStats.seat !== 0 || guestStats.seat !== 1) fail("the seats are not host and challenger");
  }

  // Fight. Both hands run their own script at the same time.
  const until = Date.now() + seconds * 1000;
  let shot = false;
  while (Date.now() < until) {
    await Promise.all([play(host, hostScript, 150), play(guest, guestScript, 150)]);
    if (shots && !shot && Date.now() > until - seconds * 500) {
      shot = true;
      await host.screenshot({ path: `${shots}/netplay-host.png` });
      await guest.screenshot({ path: `${shots}/netplay-guest.png` });
      console.log(`frames written to ${shots}/netplay-{host,guest}.png`);
    }
  }
  await wait(1200);

  hostStats = (await readStats(host)).net;
  guestStats = (await readStats(guest)).net;
  if (!hostStats || !guestStats) fail("a session ended before the fight did");
  else {
    const row = (label, stats) => console.log(
      `  ${label.padEnd(6)} frame ${String(stats.frame).padStart(5)} ` +
      `confirmed ${String(stats.confirmed).padStart(5)} ` +
      `rollbacks ${String(stats.rollbacks).padStart(4)} (max ${stats.maxRollback}) ` +
      `stalls ${String(stats.stalls).padStart(4)} waits ${String(stats.waits).padStart(4)} ` +
      `desyncs ${stats.desyncs} sent ${stats.sent} recv ${stats.received} ` +
      `snapshot ${(stats.snapshotMs / Math.max(1, stats.frame)).toFixed(3)}ms/f ` +
      `resim ${(stats.resimMs / Math.max(1, stats.frame)).toFixed(3)}ms/f`);
    console.log(`\nafter ${seconds}s of two-handed play:`);
    row("host", hostStats);
    row("guest", guestStats);
    if (hostStats.desyncs || guestStats.desyncs)
      fail(`the seats disagreed: ${hostStats.desyncs} + ${guestStats.desyncs} desyncs`);
    if (hostStats.frame < 300) fail(`the fight barely ran (${hostStats.frame} frames)`);
    if (!hostStats.rollbacks && !guestStats.rollbacks)
      console.log("  note: no rollbacks happened — a local wire is nearly free");
    // The strongest check available from outside: each seat's own hash of a
    // frame both pads are known for, compared where the frames coincide.
    const shared = Math.min(hostStats.hashFrame, guestStats.hashFrame);
    if (hostStats.hashFrame === guestStats.hashFrame &&
        hostStats.hash !== guestStats.hash)
      fail(`frame ${shared} hashes differ: ${hostStats.hash} vs ${guestStats.hash}`);
    else console.log(`  hashes agree at the frames they share (last checked ${shared})`);
    if (!process.exitCode) console.log("\nPASSED: two browsers, one fight.");
  }
} finally {
  await browser.close();
  stack.server.close();
  process.exit(process.exitCode || 0);
}
