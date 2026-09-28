// check-piece.mjs — run a piece for real and report what its console says.
//
// `node --check` only proves a piece parses. The bumblebee parsed, published,
// and threw in the browser, and nothing in Aesel could see it. This opens the
// piece in headless Chrome on aesthetic.computer, lets it run, taps and presses
// a key so act() and sim() are exercised, and returns every page error and
// console error it printed — the same errors a person would see.
//
// A local file is checked before it is published: the page asks for a scratch
// piece, and the request for that piece's source is answered with the file.
import { existsSync, readFileSync } from "node:fs";
import { createRequire } from "node:module";
import { basename, dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const SITE = "https://aesthetic.computer";
const CHROME = "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome";

// Puppeteer lives in the repository's node_modules; a tarball install has none.
function loadPuppeteer() {
  for (let dir = dirname(fileURLToPath(import.meta.url)); dir !== dirname(dir); dir = dirname(dir)) {
    const candidate = resolve(dir, "node_modules/puppeteer/package.json");
    if (existsSync(candidate)) return createRequire(candidate)("puppeteer");
  }
  return null;
}

export async function checkPiece(target, { seconds = 4, cwd = process.cwd() } = {}) {
  const puppeteer = loadPuppeteer();
  if (!puppeteer) throw new Error("ac check needs the aesthetic-computer checkout (puppeteer); this install has none.");
  const file = /\.(mjs|lisp)$/.test(target) ? resolve(cwd, target) : "";
  if (file && !existsSync(file)) throw new Error(`no such file: ${target}`);
  const scratch = `aesel-check-${Date.now().toString(36)}`;
  const address = file ? `${SITE}/${scratch}` : target.startsWith("http") ? target : `${SITE}/${target.replace(/^\/+/, "")}`;
  const url = `${address}${address.includes("?") ? "&" : "?"}nogap=true&nolabel=true&noauth=true`;

  const browser = await puppeteer.launch({
    headless: "new",
    ...(existsSync(CHROME) ? { executablePath: CHROME } : {}),
    args: ["--no-sandbox", "--mute-audio", "--autoplay-policy=no-user-gesture-required"],
    timeout: 90000,
    protocolTimeout: 90000,
  });
  // A failing paint() or sim() fails every frame; say it once, with a count.
  const seen = new Map(), details = new Map();
  const note = (kind, text) => {
    const line = `${kind}: ${String(text).trim().split("\n").slice(0, 2).join(" · ").slice(0, 400)}`;
    seen.set(line, (seen.get(line) || 0) + 1);
  };
  try {
    const page = await browser.newPage();
    await page.setViewport({ width: 480, height: 360, deviceScaleFactor: 1 });
    if (file) {
      // The site's service worker would fetch the piece itself, out of reach
      // of the interception below.
      await page.setBypassServiceWorker(true);
      const source = readFileSync(file, "utf8");
      const ext = file.endsWith(".lisp") ? ".lisp" : ".mjs";
      await page.setRequestInterception(true);
      page.on("request", (request) => {
        const path = new URL(request.url()).pathname;
        if (path.endsWith(`/${scratch}${ext}`)) {
          request.respond({ status: 200, contentType: ext === ".mjs" ? "text/javascript" : "text/plain", body: source });
        } else request.continue();
      });
    }
    page.on("pageerror", (error) => note("error", error.message));
    page.on("console", async (message) => {
      const type = message.type(), text = message.text();
      // The page's own network chatter (analytics, auth, sockets) is not the piece.
      if (/favicon|net::ERR|Failed to load resource|firebase|auth0|socket|session-server|WebSocket/i.test(text)) return;
      // AC runs the piece in a worker, catches what it throws, and reports it
      // as a warning — "🎨 Paint failure…" with the error object attached.
      const failure = type === "warn" && /failure|error|exception/i.test(text);
      if (type !== "error" && !failure) return;
      // Read the attached error once per distinct warning; the rest just count.
      const key = failure ? text : `error:${text}`;
      if (details.has(key)) { note(details.get(key).kind, details.get(key).detail); return; }
      details.set(key, { kind: failure ? text.replace(/\s*JSHandle@\w+/g, "").replace(/\.\.\.$/, "").trim() : "console.error", detail: text });
      let detail = "";
      for (const arg of message.args()) {
        const value = await arg.evaluate((v) => v instanceof Error ? `${v.name}: ${v.message}\n${(v.stack || "").split("\n").find((l) => /\.mjs|\.lisp|blob:|disks\//.test(l)) || ""}` : "").catch(() => "");
        if (value) { detail = value; break; }
      }
      const entry = details.get(key);
      entry.detail = detail || text;
      note(entry.kind, entry.detail);
    });
    await page.goto(url, { waitUntil: "domcontentloaded", timeout: 30000 });
    await page.waitForFunction(() => window.acBOOTED === true, { timeout: 30000 }).catch(() => note("error", "the piece never finished booting"));
    await new Promise((r) => setTimeout(r, 1200));
    // Exercise input once: a tap in the middle and a space.
    await page.mouse.click(240, 180).catch(() => {});
    await page.keyboard.press("Space").catch(() => {});
    await new Promise((r) => setTimeout(r, Math.max(1000, seconds * 1000 - 1200)));
  } finally {
    await browser.close();
  }
  // A warning whose error was gone before it could be read says nothing the
  // readable one did not; drop it when a readable one exists.
  const lines = [...seen].filter(([line]) => !/JSHandle@/.test(line) || ![...seen.keys()].some((other) => other !== line && !/JSHandle@/.test(other)));
  const problems = lines.map(([line, count]) => count > 1 ? `${line}  (×${count})` : line);
  return { target: file ? basename(file) : address, problems };
}
