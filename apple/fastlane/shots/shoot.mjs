// Store screenshots of production aesthetic.computer at App Store sizes.
// Run on a machine with Chrome and puppeteer that is NOT jeffrey's Mac
// (paths below are jasellite's): node shoot.mjs raw [prompt,notepat,line,roz,juke]
// The chat figure has its own script, chat-shoot.mjs. compose.py then lays the
// captions over raw/*.png and writes ../screenshots/ios/en-US/.
import puppeteer from "/home/jas/aesthetic-computer/node_modules/puppeteer/lib/esm/puppeteer/puppeteer.js";
import { mkdirSync } from "node:fs";
const OUT = process.argv[2] || "out"; mkdirSync(OUT, { recursive: true });
const only = process.argv[3] ? process.argv[3].split(",") : null;
const DEVICES = [
  { name: "iphone69", width: 440, height: 956, deviceScaleFactor: 3 },
  { name: "ipad13", width: 1032, height: 1376, deviceScaleFactor: 2 },
];
const sleep = (ms) => new Promise((r) => setTimeout(r, ms));
const drag = async (page, pts) => {
  await page.mouse.move(pts[0][0], pts[0][1]); await sleep(300); await page.mouse.down(); await sleep(400);
  for (const [x, y] of pts.slice(1)) { await page.mouse.move(x, y, { steps: 12 }); await sleep(30); }
  await page.mouse.up(); await sleep(250);
};
const SHOTS = {
  notepat: async (page, d) => {
    await page.goto("https://aesthetic.computer/notepat?nogap=true", { waitUntil: "load" });
    await sleep(9000);
    await page.mouse.click(d.width / 2, d.height * 0.2); await sleep(500);
    for (const k of ["c", "e", "g"]) { await page.keyboard.down(k); await sleep(150); }
    await sleep(700);
  },
  line: async (page, d) => {
    const w = d.width, h = d.height;
    const r = Math.min(w, h) * 0.17;
    const steps = [
      ["rect~skyblue", [[w-3,h*.64],[3,h*.06]]], ["rect~green", [[w-3,h-3],[3,h*.64]]],
      ["oval~yellow", [[w*.72,h*.2],[w*.72+r,h*.2+r]]],
      ["rect~orange", [[w*.22,h*.5],[w*.62,h*.72]]],
      ["shape~red", [[w*.16,h*.5],[w*.42,h*.36],[w*.68,h*.5],[w*.16,h*.5]]],
      ["rect~brown", [[w*.37,h*.6],[w*.47,h*.72]]],
      ["oval~pink", [[w*.78,h*.75],[w*.78+r*.22,h*.75+r*.22]]],
      ["line~magenta", [[w*.78,h*.86],[w*.78,h*.77]]],
      ["line~white", [[w*.1,h*.16],[w*.3,h*.16]]], ["line~white", [[w*.14,h*.19],[w*.36,h*.19]]],
    ];
    for (const [piece, pts] of steps) {
      await page.goto(`https://aesthetic.computer/${piece}?nogap=true`, { waitUntil: "load" });
      await sleep(6000); await drag(page, pts); await sleep(600);
    }
  },
  prompt: async (page) => {
    await page.goto("https://aesthetic.computer/prompt?nogap=true", { waitUntil: "load" });
    await sleep(12000);
  },
  rso: async (page) => {
    await page.goto("https://aesthetic.computer/$rso?nogap=true", { waitUntil: "load" });
    await sleep(14000);
  },
  r8dio: async (page, d) => {
    await page.goto("https://aesthetic.computer/r8dio?nogap=true", { waitUntil: "load" });
    await sleep(10000); await page.mouse.click(d.width / 2, d.height / 2); await sleep(6000);
  },
  roz: async (page) => {
    await page.goto("https://aesthetic.computer/$roz?nogap=true", { waitUntil: "load" });
    await sleep(14000);
  },
  juke: async (page, d) => {
    await page.goto("https://aesthetic.computer/juke?nogap=true", { waitUntil: "load" });
    await sleep(9000);
    await page.mouse.click(d.width / 2, d.height * (d.name === "iphone69" ? 0.2 : 0.14)); await sleep(9000);
  },
};
const browser = await puppeteer.launch({
  headless: "new", executablePath: "/usr/bin/google-chrome",
  args: ["--autoplay-policy=no-user-gesture-required", "--mute-audio", "--no-sandbox"],
});
for (const d of DEVICES) for (const [name, run] of Object.entries(SHOTS)) {
  if (only && !only.includes(name)) continue;
  const ctx = await browser.createBrowserContext(); const page = await ctx.newPage();
  await page.setViewport({ width: d.width, height: d.height, deviceScaleFactor: d.deviceScaleFactor, isMobile: true, hasTouch: false });
  await page.emulateMediaFeatures([{ name: "prefers-color-scheme", value: "light" }]);
  try { await run(page, d); await page.screenshot({ path: `${OUT}/${d.name}_${name}.png` }); console.log("ok", d.name, name); }
  catch (e) { console.log("FAIL", d.name, name, e.message); }
  await ctx.close();
}
await browser.close();
