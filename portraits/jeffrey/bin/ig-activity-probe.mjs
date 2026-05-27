#!/usr/bin/env node
// Probe IG's Your Activity → Photos and videos → Posts UI to find the bulk
// archive flow. Loads cookies, navigates to /your_activity_/interactions/
// and tries common deep paths used since IG retired per-post Archive in the
// web menu.

import { readFileSync, existsSync } from "fs";
import { homedir } from "os";
import { join } from "path";
import { chromium } from "playwright";

const account = "whistlegraph";
const HERE = new URL(".", import.meta.url).pathname;
const REPO = join(HERE, "..", "..", "..");
const COOKIES_PATH = join(REPO, "portraits/jeffrey/sessions", `${account}.cookies.json`);
const PROFILE_DIR = join(homedir(), ".ac-instagram-profile");
const OUT = join(REPO, "portraits/jeffrey/curated");

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

const candidates = [
  "https://www.instagram.com/your_activity/",
  "https://www.instagram.com/your_activity/interactions/posts/",
  "https://www.instagram.com/your_activity_/interactions/posts/",
  "https://www.instagram.com/accounts/your_activity/",
  "https://www.instagram.com/accounts/your_activity_/photos_and_videos/posts/",
  "https://www.instagram.com/accounts/edit/photos_and_videos/posts/",
];

for (const url of candidates) {
  console.log("\n▸", url);
  try {
    const resp = await page.goto(url, { waitUntil: "domcontentloaded", timeout: 15000 });
    await page.waitForTimeout(2500);
    console.log("  status:", resp?.status(), "→", page.url());
  } catch (e) {
    console.log("  err:", e.message.split("\n")[0]);
  }
}

// Now try the user's own profile and look for "Manage posts" / "Archive" affordances.
console.log("\n▸ profile page");
await page.goto(`https://www.instagram.com/${account}/`, { waitUntil: "domcontentloaded" });
await page.waitForTimeout(2000);
console.log("  url:", page.url());

await page.screenshot({ path: join(OUT, "activity-probe-profile.png") });

const items = await page.evaluate(() => {
  const out = [];
  for (const el of document.querySelectorAll('button, [role="button"], a')) {
    const r = el.getBoundingClientRect();
    if (r.width === 0 || r.height === 0) continue;
    const text = (el.innerText || el.textContent || "").trim().slice(0, 80);
    const aria = el.getAttribute("aria-label") || "";
    const href = el.getAttribute("href") || "";
    if (!text && !aria && !href) continue;
    if (/archive|manage|activity|select|edit profile|your activity/i.test(text + " " + aria + " " + href)) {
      out.push({ tag: el.tagName.toLowerCase(), text, aria, href });
    }
  }
  return out;
});

console.log("\n── items mentioning archive/manage/activity on profile ──");
for (const it of items) console.log(" ", it);

await page.waitForTimeout(10000);
await ctx.close();
