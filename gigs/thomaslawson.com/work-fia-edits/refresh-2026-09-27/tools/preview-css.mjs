// preview-css.mjs — load live pages, replace the served #tl-refresh CSS with the
// local refresh.css, report split-vs-logo geometry, and screenshot.
import puppeteer from "puppeteer-core";
import { readFileSync } from "node:fs";
const css = readFileSync(new URL("../refresh.css", import.meta.url), "utf8");
const pages = (process.argv[2] || "bookshelf,about,exhibitions,curatorial-projects,in-the-studio,beyond-the-studio").split(",");
const b = await puppeteer.launch({ executablePath: "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome", headless: "new" });
for (const slug of pages) for (const [vn, vp] of [["desktop", { width: 1440, height: 900 }], ["mobile", { width: 390, height: 844, deviceScaleFactor: 2, isMobile: true }]]) {
  const p = await b.newPage(); await p.setViewport(vp);
  await p.goto(`https://www.thomaslawson.com/${slug}/?nc=${Date.now()}`, { waitUntil: "networkidle2" });
  await p.evaluate((c) => { const el = document.getElementById("tl-refresh"); if (el) el.textContent = c; }, css);
  await new Promise((r) => setTimeout(r, 600));
  const r = await p.evaluate(() => { const bx = (s) => { const e = document.querySelector(s); if (!e) return null; const q = e.getBoundingClientRect(); return [Math.round(q.x), Math.round(q.y), Math.round(q.width), Math.round(q.height)]; };
    const hdr = document.querySelector("[data-elementor-type=header], header"); return { logo: bx("[data-elementor-type=header] img, header img"), headerBottom: hdr && Math.round(hdr.getBoundingClientRect().bottom), media: bx(".tl-split-media"), text: bx(".tl-split-text"), overflowX: document.documentElement.scrollWidth > innerWidth }; });
  console.log(slug, vn, JSON.stringify(r));
  await p.screenshot({ path: `evidence/logo-space/${slug}-${vn}.png` });
  await p.close();
}
await b.close();
