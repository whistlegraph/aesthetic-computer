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
  uploads = [],
  emails = [],
  pairs = new Map();
let releaseCloud,
  publishedPNG,
  wip = { code: "webtest", revision: 0 },
  failTracking = true;
let notifyCloud;
const cloudSubmitted = new Promise((resolve) => (notifyCloud = resolve));
page.on("pageerror", (e) => errors.push(e.message));
await context.addInitScript(() => {
  globalThis.auth0 = {
    Auth0Client: class {
      async isAuthenticated() {
        return localStorage.getItem("fixture-signed-out") !== "true";
      }
      async getTokenSilently() {
        return "isolated-test-token";
      }
      async logout() {
        localStorage.setItem("fixture-signed-out", "true");
      }
      async loginWithRedirect() {
        location.href = location.pathname + "?code=fixture&state=fixture";
      }
      async handleRedirectCallback() {
        localStorage.removeItem("fixture-signed-out");
      }
      async loginWithPopup() {
        localStorage.removeItem("fixture-signed-out");
      }
    },
  };
});
const routeRequest = async (route) => {
  const request = route.request(),
    url = new URL(request.url()),
    path = url.pathname;
  const json = (value) =>
    route.fulfill({
      status: 200,
      contentType: "application/json",
      body: JSON.stringify(value),
    });
  if (path === "/api/nopaint-signin") {
    const body = request.postDataJSON();
    if (body.action === "create") {
      const code = "AA" + String(pairs.size + 1).padStart(4, "0");
      pairs.set(code, { code, pollSecret: "s".repeat(43), claimed: false });
      return json(pairs.get(code));
    }
    const pair = pairs.get(body.code);
    assert.ok(pair);
    if (body.action === "poll") {
      assert.equal(body.pollSecret, pair.pollSecret);
      return json(
        pair.claimed
          ? {
              status: "claimed",
              session: {
                accessToken: "isolated-test-token",
                account: { id: "fixture", label: "@fixture" },
              },
            }
          : { status: "pending" },
      );
    }
    if (body.action === "claim") {
      assert.equal(
        request.headers().authorization,
        "Bearer isolated-test-token",
      );
      pair.claimed = true;
      return json({ handle: "fixture", kind: "browser" });
    }
    if (body.action === "email")
      return json({
        client_id: "fixture",
        authParams: {
          response_type: "code",
          state: "test-state",
          code_challenge: "test-pkce",
          redirect_uri: "https://aesthetic.computer/api/nopaint-signin",
        },
      });
    throw Error("Unexpected sign-in action");
  }
  if (
    url.hostname === "hi.aesthetic.computer" &&
    path === "/passwordless/start"
  ) {
    emails.push(request.postDataJSON());
    return json({});
  }
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
            provider: "openrouter",
            size: [256, 256],
            generation: {
              resolution: "1K",
              aspect_ratio: "1:1",
              quality: "low",
              n: 1,
            },
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
};
await context.route("**/*", routeRequest);
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
async function studioAction(id) {
  await page.locator("#status").click();
  await page
    .getByRole("button", { name: "Painting actions", exact: true })
    .click();
  await page.locator(`#studio-actions [data-action=${id}]`).click();
}
try {
  await page.goto(base + "/nopaint.art/ai/index.html");
  await ready();
  await page.waitForFunction(() =>
    document.querySelector("#status").textContent.includes("@fixture"),
  );
  assert.match(
    await page.locator("#status").textContent(),
    /^Classic · @fixture/,
  );
  assert.equal(await page.locator("#menus, #before").count(), 0);
  assert.equal(await page.locator("#hint").isVisible(), false);
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
  await page.locator("#status").click();
  const choices = await page.locator(".model-row").allTextContents();
  assert.equal(choices.length, 5, "four CPU modes and the cloud offer only");
  assert.doesNotMatch(choices.join(" "), /Random|Desktop|Fleet/);
  assert.doesNotMatch(choices.join(" "), /\bfree\b/i);
  assert.equal(await page.locator("#studio-actions").isVisible(), false);
  await page
    .getByRole("button", { name: "Details about Classic", exact: true })
    .click();
  assert.match(
    await page.locator("#model-details-list").textContent(),
    /strip shifts/,
  );
  assert.equal(moves.length, 0);
  await page.locator("#model-details [data-close]").click();
  await page.locator("#models [data-close]").click();
  await studioAction("settings");
  await page.locator("#mode").selectOption("inpaint");
  assert.equal(await page.locator("#brush").count(), 0);
  await page.locator("#brush-settings [data-close]").click();
  await studioAction("account");
  assert.equal(await page.locator("#signed-in").isVisible(), true);
  await page.locator("#account [data-close]").click();
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
  assert.equal(await page.locator("#hint").isVisible(), true);
  await page.locator("#hint").fill("a red square");
  await paint().click();
  await cloudSubmitted;
  assert.equal(moves.length, 1);
  assert.equal(moves[0].hint, "a red square");
  const second = await context.newPage();
  await second.goto(base + "/nopaint.art/ai/index.html");
  await second.waitForFunction(() =>
    document
      .querySelector("#status")
      .textContent.includes("Open in another tab"),
  );
  assert.equal(
    await second.locator(".decisions [data-action=paint]").isDisabled(),
    true,
  );
  assert.equal(
    await second.evaluate(async () => {
      const { openStore } = await import("/nopaint.art/ai/storage.mjs");
      const store = await openStore();
      const track = await store.get(await store.setting("current"));
      return (
        Boolean(track.pending) &&
        !track.events.some((e) => e.type === "interrupted")
      );
    }),
    true,
    "second tab must preserve the active cloud request",
  );
  await second.close();
  await no().click();
  await page.mouse.move(0, 0);
  releaseCloud();
  await page.waitForTimeout(600);
  assert.ok((await pixels()) === initial, "No ignores a late cloud result");
  await select("Classic");
  assert.equal(await page.locator("#hint").isVisible(), false);
  await paint().click();
  await ready();
  const beforeDone = await pixels();
  await studioAction("done");
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
  await studioAction("history");
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
  await page
    .getByRole("button", { name: "Details about Test Cloud", exact: true })
    .click();
  const details = await page.locator("#model-details-list").textContent();
  assert.match(details, /1K · 1:1/);
  assert.match(details, /256 × 256 pixels/);
  assert.match(details, /8 Braincells/);
  assert.equal(
    await page.locator("#model-source").getAttribute("href"),
    "https://openrouter.ai/test/image",
  );
  assert.equal(
    await page.locator("#hint").isVisible(),
    false,
    "viewing cloud details must not change the Classic selection",
  );
  assert.equal(moves.length, 1, "viewing details must not request inference");
  await page.screenshot({ path: shots + "/model-details.png" });
  await page.locator("#model-details [data-close]").click();
  await page.locator("#models [data-close]").click();
  await studioAction("account");
  await page.locator("#signout").click();
  await page.waitForSelector("#signin-methods", { state: "visible" });
  await page.locator("#account [data-close]").click();
  await select("Color");
  assert.equal(await page.locator("#account").isVisible(), false);
  await page.reload();
  await ready();
  await page.locator("#status").click();
  await page.locator(".model-row").filter({ hasText: "Test Cloud" }).waitFor();
  assert.doesNotMatch(
    await page.locator("#models").textContent(),
    /Sign in|\bfree\b/i,
  );
  assert.equal(await page.locator("#model-notice").isVisible(), false);
  const beforeSignIn = await pixels();
  await page.locator(".model-row").filter({ hasText: "Test Cloud" }).click();
  await page.waitForSelector("#account[open]");
  assert.equal(
    await page.locator("#account-title").textContent(),
    "Sign in with AC",
  );
  assert.match(
    await page.locator("#account-model").textContent(),
    /Test Cloud/,
  );
  await page.waitForSelector("#signin-qr", { state: "visible" });
  assert.equal(await page.locator("#code, #code-label").count(), 0);
  assert.doesNotMatch(
    await page.locator("#account").textContent(),
    /six-digit|Send code/i,
  );
  assert.match(await page.locator("#status").textContent(), /^Color$/);
  await page.screenshot({ path: shots + "/model-signin.png" });
  await page.keyboard.press("Escape");
  assert.equal(await page.locator("#account").isVisible(), false);
  assert.equal(await page.locator("#models").isVisible(), true);
  assert.match(await page.locator("#status").textContent(), /^Color$/);
  await page.locator("#models [data-close]").click();
  await ready();
  await page.locator("#status").click();
  await page.locator(".model-row").filter({ hasText: "Test Cloud" }).click();
  await page.locator("#redirect-login").click();
  await page.waitForFunction(() => {
    const status = document.querySelector("#status")?.textContent || "";
    return status.startsWith("Test Cloud") && status.includes("@fixture");
  });
  await ready();
  assert.equal(await page.locator("#account").isVisible(), false);
  assert.equal(await page.locator("#models").isVisible(), false);
  assert.equal(
    await page.evaluate(() => sessionStorage.getItem("nopaint-signin-model")),
    null,
  );
  assert.equal(await pixels(), beforeSignIn, "sign-in preserves the painting");
  assert.equal(
    moves.length,
    1,
    "selecting a model and signing in never generate a move",
  );
  // A separate phone browser approves the QR; the editor receives the session.
  await studioAction("account");
  await page.locator("#signout").click();
  await page.waitForSelector("#signin-qr", { state: "visible" });
  await page.locator("#account [data-close]").click();
  await select("Classic");
  await page.locator("#status").click();
  await page.locator(".model-row").filter({ hasText: "Test Cloud" }).click();
  await page.waitForSelector("#signin-qr", { state: "visible" });
  const pair = [...pairs.values()].at(-1);
  const phone = await browser.newContext({
    viewport: { width: 390, height: 844 },
  });
  await phone.route("**/*", routeRequest);
  await phone.addInitScript(() => {
    globalThis.auth0 = {
      Auth0Client: class {
        async isAuthenticated() {
          return true;
        }
        async getTokenSilently() {
          return "isolated-test-token";
        }
      },
    };
  });
  const phonePage = await phone.newPage();
  phonePage.on("pageerror", (e) => errors.push(e.message));
  await phonePage.goto(base + "/nopaint.art/ai/signin.html#code=" + pair.code);
  await phonePage
    .getByRole("button", { name: "Continue as @fixture" })
    .waitFor();
  assert.equal(pair.claimed, false, "scanning alone does not approve sign-in");
  await phonePage.getByRole("button", { name: "Continue as @fixture" }).click();
  await phonePage
    .getByText("Return to No Paint on the screen you scanned.", {
      exact: true,
    })
    .waitFor();
  await page.waitForSelector("#account", { state: "hidden" });
  await ready();
  assert.match(
    await page.locator("#status").textContent(),
    /^Test Cloud.*@fixture/,
  );
  await phonePage.screenshot({ path: shots + "/phone-signin.png" });
  await phone.close();
  // Email requests a link, never an OTP; receiving a session follows the same poll.
  await studioAction("account");
  await page.locator("#signout").click();
  await page.waitForSelector("#signin-qr", { state: "visible" });
  const emailPair = [...pairs.values()].at(-1);
  await page.locator("#email-link summary").click();
  await page.locator("#email").fill("painter@example.test");
  await page.locator("#sign-submit").click();
  await page
    .getByText(
      "Check your email and tap the sign-in link. This screen will sign in automatically.",
      { exact: true },
    )
    .waitFor();
  assert.equal(emails.length, 1);
  assert.equal(emails[0].send, "link");
  assert.equal(emails[0].email, "painter@example.test");
  assert.equal(emails[0].authParams.response_type, "code");
  emailPair.claimed = true;
  await page.waitForSelector("#account", { state: "hidden" });
  assert.equal(moves.length, 1, "QR and email sign-in do not start inference");
  assert.deepEqual(errors, []);
  console.log(
    JSON.stringify({
      result: "passed",
      checks: [
        "local Paint/No",
        "Classic default after sign-in",
        "large model picker with secondary painting actions",
        "no brush size setting",
        "cloud model sign-in prompt, cancellation and redirect selection recovery",
        "phone QR approval and email links without code entry",
        "model details without changing selection or making inference requests",
        "reload persistence",
        "exact mask preservation",
        "authenticated cloud quote and hint",
        "late cloud cancellation",
        "second tab preserves pending work",
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
