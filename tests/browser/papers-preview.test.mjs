// Local assets only; no accounts, production writes, or automatic PDF downloads.
import test from "node:test";
import assert from "node:assert/strict";
import { readFile, mkdir } from "node:fs/promises";
import { chromium } from "playwright";

test("paper cover preview preserves browser tabs, explicit downloads, and keyboard access", async () => {
  const site = process.env.AC_PAPERS_SITE
    ? new URL(`file://${process.env.AC_PAPERS_SITE.replace(/\/$/, "")}/`)
    : new URL("../../system/public/papers.aesthetic.computer/", import.meta.url);
  const browser = await chromium.launch({ headless: true, executablePath: process.env.AC_BROWSER_EXECUTABLE });
  const context = await browser.newContext({ viewport: { width: 1100, height: 900 } });
  const pdfRequests = [];
  let missingCover = false;
  await context.route("**/*", async route => {
    const url = new URL(route.request().url());
    if (url.hostname !== "papers.aesthetic.computer") return route.abort();
    const path = url.pathname === "/" ? "index.html" : url.pathname.slice(1);
    if (path.endsWith(".pdf")) {
      pdfRequests.push(url.pathname);
      // An HTML stand-in lets headless Chrome verify the new tab destination.
      return route.fulfill({ contentType: "text/html", body: "Paper document" });
    }
    if (missingCover && path.startsWith("previews/")) return route.fulfill({ status: 404, body: "Missing" });
    try {
      const types = { html: "text/html", mjs: "text/javascript", css: "text/css", jpg: "image/jpeg", png: "image/png", otf: "font/otf" };
      return route.fulfill({ body: await readFile(new URL(path, site)), contentType: types[path.split(".").at(-1)] || "application/octet-stream" });
    } catch { return route.fulfill({ status: 404, body: "Missing" }); }
  });
  try {
    const page = await context.newPage();
    await page.goto("https://papers.aesthetic.computer/");
    const paper = page.locator('a[data-base="/ac-native-os-26-arxiv"]');
    await paper.click();
    await page.waitForFunction(() => {
      const img = document.querySelector(".paper-preview img");
      return document.querySelector("dialog").open && !img.hidden && img.naturalWidth;
    });
    assert.equal(pdfRequests.length, 0, "opening a cover does not request or save the PDF");
    assert.ok(await page.locator(".paper-preview img").evaluate(img => img.naturalHeight > img.naturalWidth));
    const shots = process.env.AC_SHOT_DIR || "/tmp/ac-ui-checks";
    await mkdir(shots, { recursive: true });
    await page.screenshot({ path: `${shots}/paper-cover.png` });
    const popupReady = page.waitForEvent("popup");
    await page.getByRole("link", { name: "Open in tab" }).click();
    const popup = await popupReady;
    await popup.waitForLoadState();
    assert.equal(new URL(popup.url()).pathname, "/ac-native-os-26-arxiv.pdf");
    await popup.close();
    const downloadReady = page.waitForEvent("download");
    await page.getByRole("link", { name: "Download PDF" }).click();
    assert.equal((await downloadReady).suggestedFilename(), "ac-native-os-26-arxiv.pdf");
    await page.keyboard.press("Escape");
    assert.equal(await page.locator("dialog").evaluate(d => d.open), false);
    assert.equal(await paper.evaluate(a => a === document.activeElement), true);
    await page.setViewportSize({ width: 390, height: 780 });
    await paper.click();
    await page.screenshot({ path: `${shots}/paper-phone.png` });
    assert.ok(await page.evaluate(() => document.documentElement.scrollWidth <= innerWidth));
    await page.getByRole("button", { name: "Close preview" }).click();
    await page.evaluate(() => document.documentElement.dataset.theme = "dark");
    await paper.click();
    await page.screenshot({ path: `${shots}/paper-dark.png` });
    await page.keyboard.press("Escape");
    missingCover = true;
    await page.locator('a[data-base="/kidlisp-26-joss"]').click();
    await page.getByRole("status").filter({ hasText: "Cover unavailable" }).waitFor();
    assert.ok(await page.getByRole("link", { name: "Open in tab" }).isVisible());
  } finally { await browser.close(); }
});
