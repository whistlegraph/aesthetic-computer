// tiny.mjs — list visible text under 14px on each page, with v-next CSS injected.
import { readFile } from "node:fs/promises";
import puppeteer from "puppeteer-core";
const R = "/Users/jas/aesthetic-computer/gigs/thomaslawson.com/work-fia-edits/refresh-2026-09-27";
const inj = await readFile(R + "/inject.js", "utf8");
const pages = JSON.parse(await readFile(R + "/pages.json", "utf8"));
const b = await puppeteer.launch({ executablePath: "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome", headless: true, protocolTimeout: 120000 });
const agg = {};
const vp = +(process.env.W || 1440);
await Promise.all([0, 1].map(async (w) => {
  for (let i = w; i < pages.length; i += 2) {
    const pg = pages[i];
    const p = await b.newPage(); await p.setViewport({ width: vp, height: 900 });
    if (!process.env.LIVE) await p.evaluateOnNewDocument((inj) => document.addEventListener("DOMContentLoaded", () => (0, eval)(inj)), inj);
    try { await p.goto(pg.link, { waitUntil: "networkidle2", timeout: 60000 }); } catch {}
    const r = await p.evaluate(() => {
      const out = {}; const tw = document.createTreeWalker(document.body, NodeFilter.SHOW_TEXT);
      while (tw.nextNode()) { const t = tw.currentNode.textContent.trim(); if (!t) continue; const el = tw.currentNode.parentElement; const rc = el.getBoundingClientRect(); if (!rc.width) continue; const cs = getComputedStyle(el); if (cs.visibility === "hidden" || cs.display === "none") continue; const px = parseFloat(cs.fontSize); if (px < 13.9 && !el.closest("#wpadminbar")) { const k = Math.round(px * 10) / 10 + "px " + cs.fontFamily.split(",")[0].replace(/"/g, "") + " <" + el.tagName.toLowerCase() + "." + (el.className || "").toString().split(" ")[0] + ">"; out[k] = out[k] || t.slice(0, 40); } }
      return out; });
    for (const [k, v] of Object.entries(r)) (agg[k] ||= { sample: v, pages: [] }).pages.push(pg.slug);
    await p.close();
  }
}));
await b.close();
for (const [k, v] of Object.entries(agg).sort((a, b) => b[1].pages.length - a[1].pages.length)) console.log(v.pages.length, k, "::", v.sample, "::", v.pages.slice(0, 3).join(","));
