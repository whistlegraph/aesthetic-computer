// One browser, isolated account/storage, no provider calls or public paintings.
import assert from "node:assert/strict";
import { createServer } from "node:http";
import { readFile, mkdir } from "node:fs/promises";
import { resolve, extname } from "node:path";
import { fileURLToPath } from "node:url";
import { chromium } from "playwright";
const root = resolve(
  fileURLToPath(new URL("../../system/public/", import.meta.url)),
);
const server = createServer(async (req, res) => {
  try {
    const path = new URL(req.url, "http://local").pathname,
      file = resolve(root, "." + decodeURIComponent(path));
    if (!file.startsWith(root + "/")) throw Error("path");
    const data = await readFile(file);
    res.setHeader(
      "Content-Type",
      {
        ".mjs": "text/javascript",
        ".js": "text/javascript",
        ".json": "application/json",
        ".css": "text/css",
        ".html": "text/html",
        ".png": "image/png",
      }[extname(file)] || "application/octet-stream",
    );
    res.end(data);
  } catch {
    res.writeHead(404);
    res.end();
  }
});
await new Promise((r) => server.listen(0, "127.0.0.1", r));
const base = `http://127.0.0.1:${server.address().port}`;
const browser = await chromium.launch({ channel: "chrome", headless: true });
const context = await browser.newContext({
    viewport: { width: 900, height: 900 },
  }),
  page = await context.newPage();
const errors = [],
  moves = [],
  uploads = [];
let releaseCloud,
  publishedPNG,
  wip = { code: "webtest", revision: 0 },
  failTracking = true;
let notifyCloud;
const cloudSubmitted = new Promise((resolve) => (notifyCloud = resolve));
page.on("pageerror", (e) => errors.push(e.message));
await page.addInitScript(() => {
  globalThis.auth0 = {
    Auth0Client: class {
      async isAuthenticated() {
        return true;
      }
      async getTokenSilently() {
        return "isolated-test-token";
      }
      async logout() {}
    },
  };
});
await context.route("**/*", async (route) => {
  const request = route.request(),
    url = new URL(request.url()),
    path = url.pathname;
  const json = (value) =>
    route.fulfill({
      status: 200,
      contentType: "application/json",
      body: JSON.stringify(value),
    });
  if (path === "/api/nopaint-inference") {
    if (request.method() === "GET")
      return json({
        models: [
          {
            id: "ac-test",
            model: "test/image",
            name: "Test Cloud",
            location: "AC cloud",
            available: true,
            braincells: 8,
            quote: "test-quote",
          },
        ],
        service: { available: true },
      });
    assert.equal(request.headers().authorization, "Bearer isolated-test-token");
    const body = request.postDataJSON();
    moves.push(body);
    await new Promise((r) => {
      releaseCloud = r;
      notifyCloud();
    });
    return json({
      image: body.image,
      billing: { braincells: 8 },
      prompt: "Test move",
    });
  }
  if (path === "/api/easel-credits")
    return json({ handle: "@fixture", remaining: 100, purchased: 0 });
  if (path === "/api/painting-wip") {
    const body = request.postDataJSON();
    if (body.action === "create") {
      wip = { ...wip, id: body.id, key: body.key, revision: 0 };
      return json(wip);
    }
    assert.equal(body.code, wip.code);
    wip.revision++;
    return json({ code: wip.code, revision: wip.revision, status: "wip" });
  }
  if (path.startsWith("/presigned-upload-url/")) {
    const parts = path.split("/"),
      file = parts[3];
    uploads.push(file);
    return json({
      slug: "fixture/" + file,
      uploadURL: "https://upload.example/" + file,
    });
  }
  if (url.hostname === "upload.example") {
    if (path.endsWith(".png")) publishedPNG = request.postDataBuffer();
    return route.fulfill({ status: 200, body: "" });
  }
  if (path === "/api/track-media") {
    if (failTracking) {
      failTracking = false;
      return route.fulfill({
        status: 503,
        contentType: "application/json",
        body: '{"error":"Test interruption"}',
      });
    }
    const body = request.postDataJSON();
    wip.slug = body.slug;
    return json({ code: wip.code, slug: body.slug });
  }
  if (path === "/api/painting-code")
    return json({ code: wip.code, slug: wip.slug, handle: "fixture" });
  if (path === "/media/paintings/webtest.png")
    return route.fulfill({
      status: 200,
      contentType: "image/png",
      body: publishedPNG,
    });
  if (url.origin !== base) return route.abort();
  return route.continue();
});
const paint = () => page.locator(".decisions [data-action=paint]"),
  no = () => page.locator(".decisions [data-action=no]");
async function ready() {
  await page.waitForFunction(
    () =>
      document.querySelector(".decisions [data-action=paint]")?.disabled ===
      false,
  );
}
async function pixels() {
  return page.locator("#canvas").evaluate((c) => c.toDataURL());
}
async function select(name) {
  await page.locator("#status").click();
  await page.locator("#models .model-row").filter({ hasText: name }).click();
  await page.waitForSelector("#models", { state: "hidden" });
  await ready();
  assert.ok((await page.locator("#status").textContent()).includes(name));
}
async function menuAction(menu, id) {
  await page.locator("summary").filter({ hasText: menu }).click();
  await page.locator(`.menu-items [data-action=${id}]`).click();
}
try {
  await page.goto(base + "/nopaint.art/ai/index.html");
  await ready();
  await page.waitForFunction(() =>
    document.querySelector("#status").textContent.includes("@fixture"),
  );
  await select("Classic");
  const initial = await pixels();
  await paint().click();
  await ready();
  const painted = await pixels();
  assert.ok(painted !== initial);
  await no().click();
  await page.mouse.move(0, 0);
  await ready();
  assert.ok((await pixels()) === initial);
  await page.reload();
  await ready();
  assert.ok((await pixels()) === initial, "reload resumes accepted pixels");
  assert.equal(moves.length, 0, "local moves must not call cloud inference");
  // Exact mask preservation through the real browser image pipeline.
  const maskResult = await page.evaluate(async () => {
    const m = await import("/nopaint.art/ai/images.mjs"),
      source = document.querySelector("#canvas").toDataURL(),
      mask = new Uint8Array(65536);
    mask[256 * 40 + 40] = 1;
    const area = await m.region(source, mask),
      generated = m.canvas();
    generated.getContext("2d").fillRect(0, 0, 256, 256);
    const result = await m.read(await area.finish(generated.toDataURL())),
      before = await m.read(source),
      a = before.getContext("2d").getImageData(0, 0, 256, 256).data,
      b = result.getContext("2d").getImageData(0, 0, 256, 256).data;
    return a.every((v, i) => Math.floor(i / 4) === 256 * 40 + 40 || v === b[i]);
  });
  assert.equal(maskResult, true);
  await select("Test Cloud");
  await page.locator("#hint").fill("a red square");
  await paint().click();
  await cloudSubmitted;
  assert.equal(moves.length, 1);
  assert.equal(moves[0].hint, "a red square");
  await no().click();
  await page.mouse.move(0, 0);
  releaseCloud();
  await page.waitForTimeout(600);
  assert.ok((await pixels()) === initial, "No ignores a late cloud result");
  await select("Classic");
  await paint().click();
  await ready();
  const beforeDone = await pixels();
  await menuAction("File", "done");
  await page.waitForSelector("#notice[open]");
  assert.match(
    await page.locator("#notice-message").textContent(),
    /Test interruption/,
  );
  assert.ok((await pixels()) === beforeDone);
  await page.locator("#notice-retry").click();
  await ready();
  assert.ok((await pixels()) !== beforeDone, "successful Done starts fresh");
  assert.equal(uploads.length, 2, "publication retry reuses PNG and ZIP");
  assert.doesNotMatch(
    await page.locator("#status").textContent(),
    /Test interruption/,
  );
  await menuAction("File", "history");
  await page.waitForSelector("#history[open]");
  assert.ok((await page.locator("#history-list img").count()) >= 2);
  await page.locator("#history [data-close]").click();
  const shots = resolve(
    fileURLToPath(new URL("./__screens__/nopaint-ai", import.meta.url)),
  );
  await mkdir(shots, { recursive: true });
  await page.screenshot({ path: shots + "/desktop.png" });
  await page.setViewportSize({ width: 390, height: 844 });
  await page.screenshot({ path: shots + "/phone.png" });
  assert.equal(
    await page.evaluate(
      () => document.documentElement.scrollWidth > innerWidth,
    ),
    false,
  );
  await page.emulateMedia({ colorScheme: "dark" });
  await page.screenshot({ path: shots + "/phone-dark.png" });
  await page.locator("#status").click();
  await page.screenshot({ path: shots + "/models.png" });
  assert.ok(await page.locator("#models").isVisible());
  assert.deepEqual(errors, []);
  console.log(
    JSON.stringify({
      result: "passed",
      checks: [
        "local Paint/No",
        "reload persistence",
        "exact mask preservation",
        "authenticated cloud quote and hint",
        "late cloud cancellation",
        "Done recovery without duplicate uploads",
        "history",
        "phone layout",
        "dark mode",
      ],
      screenshots: shots,
    }),
  );
} finally {
  await browser.close();
  await new Promise((r) => server.close(r));
}
