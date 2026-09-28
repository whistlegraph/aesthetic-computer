// gaps.mjs — for every page, find vertical gaps > 100px between consecutive visible
// content (text lines, images) and name the elements on either side. LIVE=1 = no inject.
import { readFile } from "node:fs/promises";
import puppeteer from "puppeteer-core";
const R = "/Users/jas/aesthetic-computer/gigs/thomaslawson.com/work-fia-edits/refresh-2026-09-27";
const inj = await readFile(R + "/inject.js", "utf8");
const pages = JSON.parse(await readFile(R + "/pages.json", "utf8")).filter((p) => !process.env.TL_ONLY || process.env.TL_ONLY.split(",").includes(p.slug));
const vp = +(process.env.W || 1440);
const b = await puppeteer.launch({ executablePath: "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome", headless: true, protocolTimeout: 120000 });
const rows = [];
await Promise.all([0, 1, 2].map(async (w) => {
  for (let i = w; i < pages.length; i += 3) {
    const pg = pages[i]; const p = await b.newPage(); await p.setViewport({ width: vp, height: 900 });
    await p.evaluateOnNewDocument(() => document.addEventListener("DOMContentLoaded", () => { const s = document.createElement("style"); s.textContent = ".elementor-invisible{visibility:visible!important;opacity:1!important;transform:none!important;animation:none!important}"; document.head.appendChild(s); }));
    if (!process.env.LIVE) await p.evaluateOnNewDocument((inj) => document.addEventListener("DOMContentLoaded", () => (0, eval)(inj)), inj);
    try { await p.goto(pg.link, { waitUntil: "networkidle2", timeout: 60000 }); } catch {}
    await new Promise((r) => setTimeout(r, 600));
    const r = await p.evaluate(() => {
      const boxes = [];
      const label = (el) => { const c = el.closest("[class*=elementor-element-], header, footer, .tl-shelf-item, .tl-cap"); return (el.tagName.toLowerCase() + ":" + (el.textContent || el.getAttribute("alt") || "").trim().slice(0, 24)) + " @" + ((c && (c.className.match(/elementor-element-\w+/) || [c.tagName.toLowerCase()])[0]) || ""); };
      document.querySelectorAll("img, h1, h2, h3, h4, h5, h6, p, a, span, li, figcaption, .elementor-divider-separator, .site-footer").forEach((el) => {
        const rc = el.getBoundingClientRect(); const cs = getComputedStyle(el);
        if (rc.width < 4 || rc.height < 4 || cs.visibility === "hidden" || cs.display === "none" || el.closest("#wpadminbar, .tl-shelf-source, .tl-cap-source, .tl-doorway-source, [aria-hidden=true]") ) return;
        if (el.tagName !== "IMG" && !el.classList.contains("elementor-divider-separator") && !el.classList.contains("site-footer") && !Array.from(el.childNodes).some((n) => n.nodeType === 3 && n.textContent.trim())) return;
        boxes.push({ top: rc.top + scrollY, bottom: rc.bottom + scrollY, el: label(el) });
      });
      boxes.sort((a, b) => a.top - b.top);
      const gaps = []; let maxBottom = boxes.length ? boxes[0].bottom : 0; let prev = boxes[0];
      for (const bx of boxes.slice(1)) { if (bx.top - maxBottom > 100) gaps.push({ gap: Math.round(bx.top - maxBottom), after: prev.el, before: bx.el }); if (bx.bottom > maxBottom) { maxBottom = bx.bottom; prev = bx; } }
      return { gaps, height: document.documentElement.scrollHeight };
    });
    rows.push({ slug: pg.slug, ...r }); await p.close();
  }
}));
await b.close();
let total = 0, n = 0; const hot = {};
for (const r of rows.sort((a, b) => a.slug.localeCompare(b.slug))) { for (const g of r.gaps) { total += g.gap; n++; const k = g.after.split(" @")[1] + " → " + g.before.split(" @")[1]; } }
console.log("pages", rows.length, "| gaps>100px:", n, "| total gap px:", total, "| total page height:", rows.reduce((s, r) => s + r.height, 0));
const flat = rows.flatMap((r) => r.gaps.map((g) => ({ slug: r.slug, ...g }))).sort((a, b) => b.gap - a.gap);
for (const g of flat.slice(0, +(process.env.TOP || 25))) console.log(String(g.gap).padStart(5), g.slug.padEnd(34), "|", g.after, "→", g.before);
