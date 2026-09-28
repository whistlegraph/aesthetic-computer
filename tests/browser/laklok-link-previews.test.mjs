// laklok-link-previews.test, 2026.09.28
// End-to-end proof that the vector laklok client (system/public/html/index.html)
// turns the links the room actually shares into cards with a byline, and that
// a SoundCloud card plays in place. The page is the local file, /api/og-preview
// is the local handler (so providers run for real against the upstream
// sites), and the chat socket plus history are fixtures: nothing is posted
// and the live room is never joined. /api/og-image is the local handler too,
// answered as bytes the way lith now sends binary (see lith/server.mjs).
//
//   node tests/browser/laklok-link-previews.test.mjs
//   LAKLOK_CDP_URL=http://127.0.0.1:9333 node tests/browser/laklok-link-previews.test.mjs
//
// LAKLOK_CDP_URL attaches to an already-running Chrome (e.g. Poorslice's,
// forwarded over ssh) instead of launching one here.

import puppeteer from "puppeteer";
import { readFile, mkdir } from "node:fs/promises";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";
import ogPreview from "../../system/netlify/functions/og-preview.mjs";
import ogImage from "../../system/netlify/functions/og-image.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const SHOTS = join(HERE, "__screens__", "laklok-link-previews");
const html = await readFile(join(HERE, "../../system/public/html/index.html"), "utf8");

// The links from the room, and what each card must say.
const LINKS = [
  { url: "https://soundcloud.com/pikblodplus/some-do-others-do-not?si=77852b8017", site: /^SoundCloud · /, title: "some do - others do not", player: true },
  { url: "https://www.discogs.com/release/252760-Car-Skid-And-Crash-Toys-Are-Terrific", site: /^Discogs · Car Skid And Crash · 1986$/, title: "Toys Are Terrific" },
  { url: "https://www.reddit.com/r/laerklokken_goodiepal/s/5zGA2hedTN", site: /^Reddit · r\/laerklokken_goodiepal/, title: /Lær Klokken/ },
  { url: "https://archive.org/details/1.-klap-perker-lady-smita-version", site: /^Internet Archive · DJ HVAD$/, title: "DJ HVAD - 1996hvadcore" },
  { url: "https://we.tl/t-NWhPivK0mkiJWnxh", site: /^WeTransfer$/, title: /./ },
];

const t0 = Date.parse("2026-09-28T12:00:00Z");
const history = LINKS.map((link, i) => ({
  id: `fixture-${i}`, from: "@test", sub: "fixture", text: link.url,
  when: new Date(t0 + i * 60000).toISOString(),
}));

let failures = 0;
const check = (ok, label, detail = "") => {
  console.log(`${ok ? "✅" : "❌"} ${label}${ok ? "" : ` — ${detail}`}`);
  if (!ok) failures++;
};
const matches = (value, want) => (want instanceof RegExp ? want.test(value) : value === want);

const browser = process.env.LAKLOK_CDP_URL
  ? await puppeteer.connect({ browserURL: process.env.LAKLOK_CDP_URL })
  : await puppeteer.launch({ headless: true });
const page = await browser.newPage();
try {
  await page.setViewport({ width: 900, height: 900 });
  await mkdir(SHOTS, { recursive: true });

  // A socket that says hello and nothing else; history comes over REST.
  await page.evaluateOnNewDocument(() => {
    window.WebSocket = class {
      constructor() {
        setTimeout(() => {
          this.onopen?.();
          this.onmessage?.({ data: JSON.stringify({ type: "connected",
            content: JSON.stringify({ chatters: 1, handles: [], messages: [] }) }) });
        }, 50);
      }
      send() {}
      close() {}
    };
  });

  // The page is laklok.com and the API is aesthetic.computer, so every
  // stand-in answer needs the CORS header the real endpoints send.
  const cors = { "Access-Control-Allow-Origin": "*" };
  await page.setRequestInterception(true);
  page.on("request", async (request) => {
    const url = new URL(request.url());
    try {
      if (url.hostname === "laklok.com" && url.pathname === "/html/")
        return request.respond({ contentType: "text/html", body: html });
      if (url.pathname === "/api/chat-messages")
        return request.respond({ contentType: "application/json", headers: cors, body: JSON.stringify({ messages: history }) });
      if (url.hostname === "aesthetic.computer" && url.pathname === "/api/og-preview") {
        const response = await ogPreview(new Request(url.href));
        return request.respond({ status: response.status, contentType: "application/json",
          headers: cors, body: await response.text() });
      }
      if (url.hostname === "aesthetic.computer" && url.pathname === "/api/og-image") {
        const response = await ogImage(new Request(url.href));
        return request.respond({ status: response.status, headers: cors,
          contentType: response.headers.get("content-type") || "application/octet-stream",
          body: Buffer.from(await response.arrayBuffer()) });
      }
      if (url.pathname === "/api/laklok-theme" || url.pathname === "/api/visit-track")
        return request.respond({ status: 204, headers: cors, body: "" });
      return request.continue();
    } catch (error) {
      console.warn("interception failed for", url.href, error.message);
      return request.abort().catch(() => {});
    }
  });

  await page.goto("https://laklok.com/html/?ac-automation=1", { waitUntil: "domcontentloaded" });
  // Cards hydrate as their rows scroll into view; the fixture is short enough
  // to fit, and the handler takes a moment per upstream.
  await page.waitForFunction((n) => document.querySelectorAll(".embed.og.ready").length >= n,
    { timeout: 45000 }, LINKS.length).catch(() => {});

  // Thumbnails settle after the cards; give each a moment to decode.
  await page.waitForFunction(() => [...document.querySelectorAll(".embed.og img")]
    .every((i) => i.complete), { timeout: 20000 }).catch(() => {});
  const cards = await page.$$eval(".embed.og.ready", (els) => els.map((a) => ({
    href: a.href, site: a.querySelector(".site")?.textContent || "",
    title: a.querySelector(".t")?.lastChild?.textContent || "",
    thumb: a.querySelector("img.thumb, img.fav")?.naturalWidth > 0, listen: !!a.querySelector(".listen"),
  })));
  check(cards.length === LINKS.length, `${LINKS.length} cards drawn`, `got ${cards.length}`);
  for (const link of LINKS) {
    const card = cards.find((c) => c.href === new URL(link.url).href);
    if (!card) { check(false, `card for ${link.url}`, "missing"); continue; }
    check(matches(card.site, link.site) && matches(card.title, link.title),
      `${card.site} — ${card.title}`, `site "${card.site}", title "${card.title}"`);
    check(card.thumb, `${new URL(link.url).hostname} thumbnail decoded`);
    check(card.listen === !!link.player, `${new URL(link.url).hostname} ${link.player ? "has" : "has no"} ▶`);
  }
  await page.screenshot({ path: join(SHOTS, "cards.png") });

  // ▶ swaps the SoundCloud card for the widget, and the widget loads.
  await page.click(".embed.og .listen");
  const frame = await page.waitForSelector("iframe.embed.player", { timeout: 10000 }).catch(() => null);
  check(!!frame, "▶ swaps the card for a player");
  if (frame) {
    const src = await frame.evaluate((f) => f.src);
    check(src.startsWith("https://w.soundcloud.com/player/?url="), "player points at the SoundCloud widget", src);
    const loaded = await page.waitForFunction(() => {
      const f = document.querySelector("iframe.embed.player");
      return f && f.getBoundingClientRect().height > 100;
    }, { timeout: 10000 }).then(() => true, () => false);
    check(loaded, "player is laid out at full height");
    const widget = page.frames().find((f) => f.url().startsWith("https://w.soundcloud.com/player"));
    const ready = widget && await widget.waitForSelector(".sc-button-play, .playButton, button", { timeout: 20000 })
      .then(() => true, () => false);
    check(!!ready, "SoundCloud widget rendered its controls");
    await new Promise((r) => setTimeout(r, 1500));
    await page.screenshot({ path: join(SHOTS, "player.png") });
  }
} finally {
  await page.close().catch(() => {});
  if (process.env.LAKLOK_CDP_URL) browser.disconnect(); else await browser.close();
}

console.log(failures ? `\n${failures} failed` : "\nall passed", `— screenshots in ${SHOTS}`);
process.exit(failures ? 1 : 0);
