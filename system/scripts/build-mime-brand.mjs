// Rebuild MIME's tab icons and 1200×630 share card from its inline wordmark.
// CHROME_PATH can select a local Chromium executable.
import { readFile, writeFile, mkdir } from "node:fs/promises";
import { fileURLToPath } from "node:url";
import puppeteer from "puppeteer-core";
import sharp from "sharp";

const root = new URL("../public/mime/", import.meta.url);
const html = await readFile(new URL("index.html", root), "utf8");
const logo = html.match(/<svg class="mime-sculpture"[\s\S]*?<\/svg>/)?.[0];
if (!logo) throw new Error("MIME wordmark not found");
const font = async (name) => (await readFile(new URL(`../public/type/webfonts/${name}`, import.meta.url))).toString("base64");
const [ywft, mono] = await Promise.all([font("ywft-processing-regular.woff2"), font("BerkeleyMonoVariable-Regular.woff2")]);
const browser = await puppeteer.launch({ headless: true, executablePath: process.env.CHROME_PATH ||
  (process.platform === "darwin" ? "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome" : "/usr/bin/chromium") });
try {
  const page = await browser.newPage();
  await page.setViewport({ width: 1200, height: 630, deviceScaleFactor: 1 });
  await page.setContent(`<!doctype html><html><head><style>
    @font-face { font-family: ywft; src: url(data:font/woff2;base64,${ywft}); }
    @font-face { font-family: mono; src: url(data:font/woff2;base64,${mono}); }
    * { box-sizing: border-box; }
    body { margin: 0; width: 1200px; height: 630px; display: grid; place-items: center;
      background: radial-gradient(ellipse at 18% 5%, #403451, transparent 65%), #171526; color: #f5eefa; }
    main { width: 1104px; height: 534px; display: flex; flex-direction: column; align-items: center; justify-content: center;
      border: 2px solid #625271; border-radius: 24px; background: linear-gradient(145deg, #332b45, #211d30);
      box-shadow: inset 2px 2px 0 #796687, 0 10px 0 #0b0915; }
    .brand { display: flex; align-items: flex-start; margin-top: 12px; }
    svg { width: 790px; height: 272px; overflow: visible; }
    sup { font: 66px/1 ywft; color: #7de3d7; margin: 12px 0 0 0; }
    p { margin: 32px 0 0; font: 28px/1.5 mono; color: #eee5f4; }
  </style></head><body><main><div class="brand">${logo}<sup>.ac</sup></div><p>Comments on Aesthetic Computer media.</p></main></body></html>`);
  await page.evaluate(() => document.fonts.ready);
  await mkdir(new URL("social/", root), { recursive: true });
  await page.screenshot({ path: fileURLToPath(new URL("social/mime.jpg", root)), type: "jpeg", quality: 94 });
  const icon = await page.$eval("svg", (svg) => {
    const defs = svg.querySelector("defs").outerHTML;
    const glyph = [...svg.children].find((node) => node.tagName.toLowerCase() === "g").outerHTML;
    return `<svg xmlns="http://www.w3.org/2000/svg" viewBox="-7 0 56 56">${defs}<rect x="-7" width="56" height="56" rx="12" fill="#211d30"/><g transform="translate(0 3)">${glyph}</g></svg>`;
  });
  await writeFile(new URL("favicon.svg", root), icon + "\n");
  for (const [name, size] of [["favicon-32.png", 32], ["apple-touch-icon.png", 180]]) {
    await sharp(Buffer.from(icon), { density: 384 }).resize(size, size).png().toFile(fileURLToPath(new URL(name, root)));
  }
  console.log("Built MIME favicon, touch icon, and 1200×630 social image.");
} finally { await browser.close(); }
