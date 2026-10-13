import test from "node:test";
import assert from "node:assert/strict";
import { readFile } from "node:fs/promises";
import { chromium } from "playwright";
import { profilePage } from "../backend/profile-page.mjs";

const library = new URL("../public/aesthetic.computer/lib/", import.meta.url);
const post = (kind, code, when = "2026-10-12") => ({ code: kind + "_" + code, name: "@jeffrey", when,
  board: kind === "painting" ? "image/png" : "text/x-lisp", parent: null, text: "", replies: 0,
  file: { url: "/fixture.svg", name: code + ".png", type: "image/png" },
  media: { kind, code, id: code, author: "@jeffrey" } });
const posts = [post("painting", "f9i"), post("painting", "abc"), post("kidlisp", "hello")];
const profile = { handle: "@jeffrey", mood: { text: "zuck’s well handled" }, colors: null, counts: {painting:410,piece:44,kidlisp:4006,tape:22,whistlegraph:0,mood:288}, latestPainting:posts[0], onlineTimeMs:null };

async function setup(browser, { mime = false, delay = 0 } = {}) {
  const context = await browser.newContext({ viewport: { width: 1000, height: 750 }, colorScheme: "dark" });
  const page = await context.newPage(), requests = [], errors = [];
  page.setDefaultTimeout(7000);
  page.on("pageerror", (error) => errors.push(error.message));
  let fail = false;
  await context.routeWebSocket('**/profile-stream?*', socket => {
    socket.send(JSON.stringify({type:'profile:snapshot',data:{presence:{online:true}}}));
  });
  await context.route("**/*", async (route) => {
    const url = new URL(route.request().url());
    if (url.pathname === "/api/profile-card") {
      requests.push(url.search);
      if (delay) await new Promise((resolve) => setTimeout(resolve, delay));
      if (fail) return route.fulfill({ status: 503, contentType: "application/json", body: "{}" });
      return route.fulfill({ contentType: "application/json", body: JSON.stringify(url.searchParams.has('membership')
        ? {registeredAt:'2023-02-17',loginCount:814} : profile) });
    }
    if (url.pathname.endsWith("/profile-card.mjs") || url.pathname.endsWith("/profile-card.css")) {
      const name = url.pathname.split("/").pop();
      return route.fulfill({ contentType: name.endsWith("css") ? "text/css" : "text/javascript", body: await readFile(new URL(name, library), "utf8") });
    }
    if (url.pathname === "/api/mime") return route.fulfill({ contentType: "application/json", body: JSON.stringify({ recent: posts, hasMore: false, boards: [] }) });
    if (url.pathname === "/fixture.svg") return route.fulfill({ contentType: "image/svg+xml", body: '<svg xmlns="http://www.w3.org/2000/svg" width="200" height="200"><rect width="200" height="200" fill="#afdcb6"/><circle cx="100" cy="100" r="65" fill="#fcb8dd"/></svg>' });
    if (url.pathname === "/" && mime) return route.fulfill({ contentType: "text/html", body: await readFile(new URL("../public/mime/index.html", import.meta.url), "utf8") });
    if (url.pathname === "/@jeffrey") return route.fulfill({ contentType: "text/html", body: profilePage(url.pathname) });
    return route.fulfill({ body: "", contentType: "text/plain" });
  });
  return { context, page, requests, errors, fail: (value) => { fail = value; } };
}

test("MIME opens immediately, isolates keys, restores focus and scroll, and reuses profile data", async () => {
  const browser = await chromium.launch({ headless: true, channel: process.env.PLAYWRIGHT_CHANNEL });
  try {
    const { page, requests, errors } = await setup(browser, { mime: true, delay: 500 });
    await page.goto("https://mime.ac/?ac-automation=1");
    await page.locator(".mime-startup").waitFor({ state: "detached" });
    await page.waitForFunction(() => !!customElements.get("ac-profile"));
    const author = page.locator(".name[data-ac-profile-handle]").first();
    await author.focus();
    const before = await page.evaluate(() => ({ url: location.href, y: scrollY }));
    await author.click();
    const dialog = page.getByRole("dialog", { name: "@jeffrey profile" });
    await dialog.waitFor();
    assert.equal(await dialog.getByRole("heading", { name: "@jeffrey" }).count(), 1);
    assert.equal(await dialog.getByRole("status").textContent(), "Loading…", "show shell before network resolves");
    await dialog.getByText('4,006', {exact:true}).waitFor();
    await dialog.getByText('814', {exact:true}).waitFor();
    assert.equal(await dialog.locator('img').count(), 0, 'painting is wallpaper, not a photo');
    assert.match(await dialog.locator('.wallpaper').evaluate(node => node.style.backgroundImage), /fixture.svg/);
    assert.equal(await dialog.locator('.mood-field dt').textContent(), 'Mood');
    assert.equal(await dialog.locator('.presence-row dd').textContent(), 'Online');
    const loaded = requests.length;
    await page.keyboard.press("ArrowDown");
    await page.waitForTimeout(400);
    assert.equal(await page.evaluate(() => scrollY), before.y);
    for (let i = 0; i < 14; i++) {
      await page.keyboard.press("Tab");
      assert.equal(await page.evaluate(() => !!document.activeElement.closest("dialog")), true, await page.evaluate(() => document.activeElement.outerHTML.slice(0, 100)));
    }
    assert.deepEqual(await page.evaluate(() => ({ url: location.href, y: scrollY })), before, "feed stays still during modal");
    await page.screenshot({ path: "/tmp/ac-profile-modal-desktop.png" });
    await page.keyboard.press("Escape");
    await dialog.waitFor({ state: "detached" });
    assert.equal(await author.evaluate((node) => document.activeElement === node), true);
    assert.deepEqual(await page.evaluate(() => ({ url: location.href, y: scrollY })), before);
    await author.click();
    await dialog.getByText("814", {exact:true}).waitFor();
    assert.equal(requests.length, loaded, "repeat opens use the short-lived cache");
    for (const viewport of [{width:390,height:844}, {width:320,height:640}, {width:704,height:334}, {width:440,height:320}]) {
      await page.setViewportSize(viewport);
      await page.mouse.move(0,0);
      await page.waitForTimeout(350);
      const bounds = await dialog.locator('ac-profile').boundingBox();
      assert.ok(bounds.x >= 0 && bounds.x + bounds.width <= viewport.width, JSON.stringify(bounds));
      assert.ok(bounds.y >= 0 && bounds.y + bounds.height <= viewport.height, JSON.stringify(bounds));
      assert.equal(await dialog.locator('ac-profile').evaluate(node => node.scrollWidth <= node.clientWidth), true, JSON.stringify(viewport));
      await page.screenshot({path:`/tmp/ac-profile-modal-${viewport.width}.png`});
    }
    await page.mouse.click(2,2);
    await dialog.waitFor({ state: "detached" });
    assert.deepEqual(errors, []);
  } finally { await browser.close(); }
});

test("direct profile handles failures, reduced motion and narrow screens", async () => {
  const browser = await chromium.launch({ headless: true, channel: process.env.PLAYWRIGHT_CHANNEL });
  try {
    const { page, requests, errors, fail } = await setup(browser);
    fail(true);
    await page.goto("https://aesthetic.computer/@jeffrey");
    await page.getByText("Couldn’t load this profile.").waitFor();
    fail(false);
    await page.getByRole("button", { name: "Try again" }).click();
    await page.getByText("Feb 17, 2023", {exact:true}).waitFor();
    assert.equal(requests.length, 3);
    for (const width of [1000, 390, 320]) {
      await page.setViewportSize({ width, height: 844 });
      assert.equal(await page.evaluate(() => document.documentElement.scrollWidth <= innerWidth), true);
      await page.screenshot({ path: `/tmp/ac-profile-page-${width}.png` });
    }
    await page.emulateMedia({reducedMotion:'reduce'});
    await page.evaluate(async () => (await import('/aesthetic.computer/lib/profile-card.mjs')).openProfile('@jeffrey'));
    assert.equal(await page.locator('dialog ac-profile').evaluate(node => getComputedStyle(node).transform), 'none');
    await page.keyboard.press('Escape');
    assert.deepEqual(errors, []);
  } finally { await browser.close(); }
});
