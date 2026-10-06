// Run against a local preview or the public board after publication.
import { chromium } from "playwright";
import assert from "node:assert/strict";

const base = process.argv[2];
if (!base) throw new Error("Usage: node utilities/deadlines/check-browser.mjs <board-url>");
const browser = await chromium.launch({headless: true, channel: "chrome"});
try {
  const page = await browser.newPage({viewport: {width: 1440, height: 1000}});
  const errors = [];
  page.on("pageerror", (error) => errors.push(error.message));
  const count = () => page.locator("article.opportunity").count();
  const waitCount = (expected) => page.waitForFunction((n) => document.querySelectorAll("article.opportunity").length === n, expected);
  for (const url of [base.replace(/\/$/, ""), base.replace(/\/$/, "") + "/"]) {
    await page.goto(url, {waitUntil: "domcontentloaded"});
    await page.locator("#filters").waitFor({state: "visible", timeout: 10000});
    const all = await count();
    assert(all > 0);
    assert(await page.locator("link[rel=stylesheet]").evaluate((el) => el.sheet !== null));
    await page.getByRole("searchbox").fill("no opportunity will match this text");
    await waitCount(0);
    await page.getByRole("button", {name: "Reset", exact: true}).click();
    await waitCount(all);
    assert.equal(await page.getByRole("searchbox").inputValue(), "");
    await page.getByRole("checkbox", {name: "Free to apply", exact: true}).check();
    assert(await count() <= all);
    await page.getByRole("button", {name: "Reset", exact: true}).click();
    await waitCount(all);
    assert.equal(await page.getByRole("checkbox", {name: "Free to apply", exact: true}).isChecked(), false);
    assert(!new URL(page.url()).searchParams.has("free"));
    await page.getByRole("searchbox").fill("no opportunity will match this text");
    await waitCount(0);
    await page.getByRole("button", {name: "Clear filters", exact: true}).click();
    await waitCount(all);
    assert(!new URL(page.url()).search);
  }
  await page.setViewportSize({width: 390, height: 844});
  assert(await page.evaluate(() => document.documentElement.scrollWidth <= innerWidth));
  assert.deepEqual(errors, []);
  console.log("Browser verified both entry routes, assets, search, reset, clear filters and mobile width.");
} finally {
  await browser.close();
}
