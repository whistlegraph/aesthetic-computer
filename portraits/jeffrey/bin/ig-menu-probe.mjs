#!/usr/bin/env node
// Loads cookies, opens a single post, clicks "More options", and dumps every
// visible menu item + a screenshot. Used to calibrate the archive runner's
// menu selectors when IG ships a UI shift.
//
// Usage:
//   node bin/ig-menu-probe.mjs <shortcode> [--account=whistlegraph]

import { readFileSync, existsSync, writeFileSync } from "fs";
import { homedir } from "os";
import { join } from "path";
import { chromium } from "playwright";

const args = process.argv.slice(2);
const shortcode = args.find((a) => !a.startsWith("--"));
const accountArg = args.find((a) => a.startsWith("--account="));
const account = accountArg ? accountArg.split("=")[1] : "whistlegraph";

if (!shortcode) {
  console.error("usage: ig-menu-probe.mjs <shortcode> [--account=...]");
  process.exit(1);
}

const HERE = new URL(".", import.meta.url).pathname;
const REPO = join(HERE, "..", "..", "..");
const COOKIES_PATH = join(REPO, "portraits/jeffrey/sessions", `${account}.cookies.json`);
const PROFILE_DIR = join(homedir(), ".ac-instagram-profile");
const OUT = join(REPO, "portraits/jeffrey/curated", `menu-probe-${shortcode}`);

const ctx = await chromium.launchPersistentContext(PROFILE_DIR, {
  channel: "chrome",
  headless: false,
  viewport: null,
  args: ["--start-maximized"],
});
if (existsSync(COOKIES_PATH)) {
  await ctx.addCookies(JSON.parse(readFileSync(COOKIES_PATH, "utf8")));
}
const page = ctx.pages()[0] || (await ctx.newPage());

await page.goto(`https://www.instagram.com/p/${shortcode}/`, {
  waitUntil: "domcontentloaded",
});
await page.waitForTimeout(3000);

// Take a baseline screenshot of the post page.
await page.screenshot({ path: `${OUT}-1-post.png`, fullPage: false });

// Click "More options".
const moreBtn = page.getByRole("button", { name: /more options/i }).first();
await moreBtn.click();
await page.waitForTimeout(1500);

await page.screenshot({ path: `${OUT}-2-menu.png`, fullPage: false });

// Dump every visible button/role=button + their accessible names.
const items = await page.evaluate(() => {
  const out = [];
  const seen = new Set();
  for (const el of document.querySelectorAll(
    'button, [role="button"], [role="menuitem"], a',
  )) {
    const r = el.getBoundingClientRect();
    if (r.width === 0 || r.height === 0) continue;
    const text = (el.innerText || el.textContent || "").trim().slice(0, 80);
    const aria = el.getAttribute("aria-label") || "";
    if (!text && !aria) continue;
    const k = text + "|" + aria;
    if (seen.has(k)) continue;
    seen.add(k);
    out.push({
      tag: el.tagName.toLowerCase(),
      role: el.getAttribute("role") || "",
      text,
      aria,
    });
  }
  return out;
});

writeFileSync(`${OUT}-items.json`, JSON.stringify(items, null, 2));
console.log(`wrote ${OUT}-items.json with ${items.length} interactive elements`);
console.log("\n── items mentioning archive / hide / delete / manage ──");
for (const it of items) {
  if (/archive|hide|delete|manage|edit|private/i.test(it.text + " " + it.aria)) {
    console.log(`  ${it.tag}[role=${it.role}]  text=${JSON.stringify(it.text)}  aria=${JSON.stringify(it.aria)}`);
  }
}
console.log("\n── all visible interactive items ──");
for (const it of items.slice(0, 60)) {
  console.log(`  ${it.tag}[role=${it.role}]  ${JSON.stringify(it.text || it.aria)}`);
}

await page.waitForTimeout(8000);
await ctx.close();
