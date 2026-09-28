// probe.mjs <url> <expr> — load page, inject refresh preview, evaluate expr
import { readFile } from "node:fs/promises";
import puppeteer from "puppeteer-core";
const [url, expr, w = 1440] = process.argv.slice(2);
const R = "/Users/jas/aesthetic-computer/gigs/thomaslawson.com/work-fia-edits/refresh-2026-09-27";
const inj = await readFile(R + "/inject.js", "utf8");
const b = await puppeteer.launch({ executablePath: "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome", headless: true });
const p = await b.newPage(); await p.setViewport({ width: +w, height: 900 });
if (process.env.EARLY) await p.evaluateOnNewDocument((inj) => document.addEventListener("DOMContentLoaded", () => (0, eval)(inj)), inj);
await p.goto(url, { waitUntil: "networkidle2", timeout: 60000 });
if (!process.env.NOINJ && !process.env.EARLY) await p.evaluate(inj);
await new Promise((r) => setTimeout(r, 800));
console.log(JSON.stringify(await p.evaluate(expr), null, 1));
if (process.env.SHOT) await p.screenshot({ path: process.env.SHOT });
await b.close();
