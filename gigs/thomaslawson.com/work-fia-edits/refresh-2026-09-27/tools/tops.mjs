// tops.mjs — top-of-page spacing per page: header height, header→first content, title→next.
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
    rows.push({ slug: pg.slug, ...(await p.evaluate(() => {
      const hdr = document.querySelector("#masthead, header.site-header"); const hb = hdr ? hdr.getBoundingClientRect().bottom + scrollY : 0;
      const els = [...document.querySelectorAll("main img, main h1, main h2, main h3, main h4, main h5, main h6, main p, .tl-eyebrow, .tl-shelf-list")].filter((e) => { const r = e.getBoundingClientRect(); const cs = getComputedStyle(e); return r.width > 4 && r.height > 4 && cs.visibility !== "hidden" && !e.closest(".tl-shelf-source,.tl-cap-source,.tl-doorway-source"); });
      const band = document.querySelector(".page-id-140 .elementor-element-71fa6aa"); const bandTop = band ? band.getBoundingClientRect().top + scrollY : Infinity;
      const first = Math.min(bandTop, ...els.map((e) => e.getBoundingClientRect().top + scrollY));
      const title = els.find((e) => /^H[12]$/.test(e.tagName) || e.matches("[data-tl-role=display], .tl-doorway-sign")) || null;
      let after = null;
      if (title) { const tb = title.getBoundingClientRect().bottom + scrollY; const nxt = els.map((e) => e.getBoundingClientRect().top + scrollY).filter((t) => t > tb + 1).sort((a, b) => a - b)[0]; after = nxt != null ? Math.round(nxt - tb) : null; }
      return { header: Math.round(hb), toFirst: Math.round(first - hb), titleTop: title ? Math.round(title.getBoundingClientRect().top + scrollY) : null, titleToNext: after };
    })) }); await p.close();
  }
}));
await b.close();
rows.sort((a, b) => a.slug.localeCompare(b.slug));
for (const r of rows) console.log(r.slug.padEnd(40), "header", String(r.header).padStart(4), "| header→first", String(r.toFirst).padStart(4), "| title at", String(r.titleTop).padStart(5), "| title→next", String(r.titleToNext).padStart(4));
const avg = (k) => Math.round(rows.reduce((s, r) => s + (r[k] || 0), 0) / rows.length);
console.log("AVG header", avg("header"), "| header→first", avg("toFirst"), "| title→next", avg("titleToNext"));
