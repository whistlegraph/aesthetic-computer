import { createServer } from "node:http";
import { readFile, mkdir } from "node:fs/promises";
import { fileURLToPath } from "node:url";
import assert from "node:assert/strict";
import { chromium } from "playwright";
import { memoryDb } from "./memory-db.mjs";
import { createGameStore } from "../../backend/oskie-store.mjs";
import { createGameAPI } from "../../netlify/functions/oskie.mjs";
import { starterGame } from "../../public/oskie/format.mjs";
const owner = { sub: "browser-test", handle: "@browser-test" },
  store = createGameStore(memoryDb());
const project = await store.create(owner),
  api = createGameAPI({ store: async () => store });
const server = createServer(async (req, res) => {
  try {
    const path = new URL(req.url, "http://localhost").pathname;
    if (path.startsWith("/api/oskie/")) {
      let body = "";
      for await (const part of req) body += part;
      const r = await api({
        path,
        httpMethod: req.method,
        headers: req.headers,
        body,
      });
      res.writeHead(r.statusCode, r.headers);
      res.end(r.body);
      return;
    }
    const filename =
      /^\/oskie\/(?:[a-f0-9]{32})?$/.test(path) || path === "/oskie"
        ? "index.html"
        : path.replace("/oskie/", "");
    if (!/^[a-z.-]+$/.test(filename)) {
      res.writeHead(404).end();
      return;
    }
    res.setHeader(
      "Content-Type",
      filename.endsWith(".mjs")
        ? "text/javascript"
        : filename.endsWith(".css")
          ? "text/css"
          : "text/html",
    );
    res.end(
      await readFile(
        new URL("../../public/oskie/" + filename, import.meta.url),
      ),
    );
  } catch (e) {
    res.writeHead(500).end(e.message);
  }
});
await new Promise((r) => server.listen(0, "127.0.0.1", r));
const base = "http://127.0.0.1:" + server.address().port;
const browser = await chromium.launch({ channel: "chrome", headless: true });
try {
  const page = await browser.newPage({
      viewport: { width: 1280, height: 1000 },
    }),
    errors = [];
  page.on("pageerror", (e) => errors.push(e.message));
  page.on("console", (m) => {
    if (m.type() === "error") console.error(m.text());
  });
  const url =
    base + new URL(project.preview).pathname + new URL(project.preview).hash;
  await page.goto(url);
  console.log("loaded", await page.locator("#status").textContent());
  await page
    .locator("#status")
    .filter({ hasText: "Revision" })
    .waitFor({ timeout: 15000 })
    .catch(async (e) => {
      console.log(
        "failed state",
        await page.locator("body").innerText(),
        errors,
      );
      await page.screenshot({ path: "/tmp/ac-oskie-failed.png" });
      throw e;
    });
  await mkdir("/tmp/ac-oskie-check", { recursive: true });
  await page.screenshot({ path: "/tmp/ac-oskie-check/game.png" });
  const canvas = page.frameLocator("iframe").locator("canvas"),
    box = await canvas.boundingBox();
  await canvas.click({
    position: { x: (180 / 1080) * box.height, y: (220 / 1080) * box.height },
  });
  await page.waitForFunction(() =>
    document.querySelector("#status").textContent.includes("Revision"),
  );
  // Wait for actual browser pixels to reach the same server path Claude reads.
  let feedback;
  for (let i = 0; i < 40; i++) {
    feedback = await store.feedback(owner, project.id);
    if (feedback.frame) break;
    await new Promise((r) => setTimeout(r, 250));
  }
  assert.equal(feedback.status, "running");
  assert.ok(feedback.frame?.startsWith("data:image/png;base64,"));
  await page.screenshot({ path: "/tmp/ac-oskie-check/played.png" });
  // A new revision must replace the running game in the open tab.
  const updated = starterGame("changed-game", "Live update");
  updated.files["game.mjs"].text =
    "export function boot(){throw Error('Intentional preview error')}";
  const next = await store.update(owner, project.id, updated, project.revision);
  await page
    .locator("#status")
    .filter({ hasText: "Intentional preview error" })
    .waitFor({ timeout: 10000 });
  assert.equal(
    (await store.feedback(owner, project.id)).revision,
    next.revision,
  );
  // The game gets no network, cookies, parent window or local storage.
  const probe = starterGame("sandbox-test", "Sandbox test");
  probe.files["game.mjs"].text =
    `export async function boot(api){let blocked=0;try{await fetch('${base}/should-never-load')}catch{blocked++}try{localStorage.getItem('secret')}catch{blocked++}if(typeof document==='undefined'&&typeof parent==='undefined')blocked++;if(blocked!==3)throw Error('Sandbox leak: '+blocked);api.wipe(0,100,50);}`;
  const probed = await store.update(owner, project.id, probe, next.revision);
  await page
    .locator("#title")
    .filter({ hasText: "Sandbox test" })
    .waitFor({ timeout: 10000 });
  await page
    .locator("#status")
    .filter({ hasText: "Revision " + probed.revision.slice(0, 8) })
    .waitFor();
  // A runaway game worker must remain stoppable from the host page.
  const runaway = starterGame("loop-test", "Runaway test");
  runaway.files["game.mjs"].text = "export function boot(){while(true){}}";
  await store.update(owner, project.id, runaway, probed.revision);
  await page
    .locator("#title")
    .filter({ hasText: "Runaway test" })
    .waitFor({ timeout: 10000 });
  await page.getByRole("button", { name: "Stop", exact: true }).click();
  assert.equal(await page.locator("iframe").count(), 0);
  await page.goto(base + "/oskie/");
  await page.setViewportSize({ width: 390, height: 844 });
  await page.screenshot({ path: "/tmp/ac-oskie-check/mobile.png" });
  assert.equal(
    await page.evaluate(
      () => document.documentElement.scrollWidth > innerWidth,
    ),
    false,
  );
  assert.deepEqual(errors, []);
  console.log(
    "PASS: rendering, pointer input, preview pixels, live reload, error reporting, sandbox restrictions, runaway stop, mobile layout. Screenshots: /tmp/ac-oskie-check",
  );
} finally {
  await browser.close();
  server.close();
}
