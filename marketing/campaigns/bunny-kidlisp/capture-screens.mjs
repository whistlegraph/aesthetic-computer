// capture-screens.mjs — grab restless-egg-style KidLisp screenshots at laptop
// aspect (1280×800), WITH the code + QR overlay visible (default labeled view —
// that overlay is the restless-egg look). One frame per $code after it blooms.
import { existsSync, mkdirSync } from "node:fs";
import { dirname } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const OUT = `${HERE}/screens`;
mkdirSync(OUT, { recursive: true });
const REPO = `${HERE}/../../..`;

const PUPPETEER_DIR = [`${REPO}/oven/node_modules/puppeteer`, "/opt/oven/node_modules/puppeteer"].find((p) => existsSync(p));
const puppeteer = (await import(`${PUPPETEER_DIR}/lib/esm/puppeteer/puppeteer.js`)).default;
const CHROME = ["/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"].find((p) => existsSync(p));

const CODES = process.argv.slice(2);
if (!CODES.length) { console.error("usage: capture-screens.mjs '$tezz' '$roz' ..."); process.exit(1); }

const browser = await puppeteer.launch({
  headless: "new", ...(CHROME ? { executablePath: CHROME } : {}),
  args: ["--no-sandbox", "--use-gl=swiftshader", "--autoplay-policy=no-user-gesture-required", "--window-size=1280,800"],
});
for (const code of CODES) {
  const slug = code.replace(/^\$/, "");
  const page = await browser.newPage();
  await page.setViewport({ width: 1280, height: 800, deviceScaleFactor: 2 });
  const url = `https://aesthetic.computer/${code}?density=3&tv&spoofaudio`; // labeled view → code + QR
  try { await page.goto(url, { waitUntil: "networkidle2", timeout: 45000 }); } catch {}
  await page.evaluate(() => { const c = document.querySelector("canvas"); if (c) { c.focus(); c.click(); } }).catch(() => {});
  await page.keyboard.press("Space").catch(() => {});
  await new Promise((r) => setTimeout(r, 9000)); // let it bloom + the QR settle
  const out = `${OUT}/${slug}.png`;
  await page.screenshot({ path: out });
  console.log(`✓ ${out}`);
  await page.close();
}
await browser.close();
