// Render the laklok demo room (`laklok:demo:en` — invented people, never the real room). With LOCAL=1 the edited piece file is swapped
// in over production's copy; without it, production is captured as served.
import puppeteer from "/home/jas/aesthetic-computer/node_modules/puppeteer/lib/esm/puppeteer/puppeteer.js";
import { readFileSync, mkdirSync } from "node:fs";
const OUT = process.argv[2] || "raw"; mkdirSync(OUT, { recursive: true });
const local = process.env.LOCAL ? readFileSync("laklok.mjs", "utf8") : null;
const sleep = (ms) => new Promise((r) => setTimeout(r, ms));
const browser = await puppeteer.launch({ headless: "new", executablePath: "/usr/bin/google-chrome", args: ["--no-sandbox", "--mute-audio"] });
for (const d of [{ name: "iphone69", width: 440, height: 956, deviceScaleFactor: 3 }, { name: "ipad13", width: 1032, height: 1376, deviceScaleFactor: 2 }]) {
  const ctx = await browser.createBrowserContext(); const page = await ctx.newPage();
  await page.setViewport({ ...d, isMobile: true });
  await page.emulateMediaFeatures([{ name: "prefers-color-scheme", value: "light" }]);
  const sockets = [];
  const cdp = await page.createCDPSession(); await cdp.send("Network.enable");
  cdp.on("Network.webSocketCreated", (e) => sockets.push(e.url));
  page.on("console", (m) => { const t = m.text(); if (/Chat connect|rror/.test(t)) console.log("PAGE:", t.slice(0, 160)); });
  if (local) { // swap only the piece file; every other request goes untouched
    await cdp.send("Fetch.enable", { patterns: [{ urlPattern: "*disks/laklok.mjs*", requestStage: "Request" }] });
    cdp.on("Fetch.requestPaused", async (e) => {
      console.log("swapping", e.request.url.split("/").pop().slice(0, 50));
      await cdp.send("Fetch.fulfillRequest", { requestId: e.requestId, responseCode: 200,
        responseHeaders: [{ name: "content-type", value: "text/javascript; charset=utf-8" }, { name: "access-control-allow-origin", value: "*" }, { name: "cross-origin-resource-policy", value: "cross-origin" }],
        body: Buffer.from(local).toString("base64") });
    });
  }
  await page.goto("https://aesthetic.computer/laklok:demo:en?nogap=true", { waitUntil: "load" });
  await sleep(Number(process.env.WAIT || 30000));
  await page.screenshot({ path: `${OUT}/${d.name}_chat.png` });
  console.log("ok", d.name, "page-level sockets:", JSON.stringify(sockets));
  await ctx.close();
}
await browser.close();
