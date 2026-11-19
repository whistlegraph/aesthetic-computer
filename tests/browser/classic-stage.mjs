// classic-stage, 2026.11.19
// Takes each screen of No Paint classic as a frame and a stage: a screenshot,
// and beside it everything the game has placed on that screen, with its box
// (the bridge's "stage" op). toolchain/nopaint/stage-map.mjs draws the two as
// one picture, so a layout can be looked at before it is changed.
//
// Nothing real is reached: the build is served from a folder and the services
// are played, as in classic-steps.e2e.mjs.
//
//   node classic-stage.mjs            every screen: OUT/<screen>.png and .json
//   HOVER=1 node classic-stage.mjs    and the title screen with the pointer over each button
//
// Beside this file on a fleet machine: book-kit.mjs, ./site (the build),
// nopaint-pieces.mjs and painting-state.mjs.

import { existsSync, readFileSync, writeFileSync } from "node:fs";
import { join } from "node:path";
import { fileURLToPath } from "node:url";
import { launch, openPage, serveFolder, viewport, wait, OUT } from "./book-kit.mjs";

const here = (path) => fileURLToPath(new URL(path, import.meta.url));
const SITE = process.env.SITE || (existsSync(here("./site")) ? here("./site") : here("../../system/public/nopaint.art"));
const LIB = existsSync(here("./nopaint-pieces.mjs")) ? here("./") : here("../../system/public/aesthetic.computer/lib/");
const [W, H] = (process.env.SIZE || "512x704").split("x").map(Number);
const VIEW = viewport(W, H, Number(process.env.DSF || 1));
const site = await serveFolder(SITE);
const browser = await launch(VIEW, (process.env.CHROME_ARGS || "").split(" ").filter(Boolean));

async function open(guest) {
  const context = await browser.createBrowserContext();
  const { page } = await openPage(browser, VIEW, { context, consoleTypes: ["error"] });
  if (!guest) await page.evaluateOnNewDocument(() => localStorage.setItem("nopaint:classic:account", JSON.stringify({ token: "test-token", label: "@tester" })));
  const cors = { "Access-Control-Allow-Origin": "*", "Access-Control-Allow-Headers": "Content-Type, Authorization" };
  await page.setRequestInterception(true);
  page.on("request", (request) => {
    const url = request.url(), json = (value) => request.respond({ status: 200, contentType: "application/json", headers: cors, body: JSON.stringify(value) });
    if (request.method() === "OPTIONS" && !url.startsWith(site.url)) return request.respond({ status: 204, headers: cors });
    if (url.startsWith("https://server.nopaint.art/")) { const at = new URL(url); return json(at.pathname === "/get-code" ? { code: at.searchParams.get("as") || "Classic1" } : { code: "t3st" }); }
    if (url === "https://aesthetic.computer/api/nopaint-wallet") return json({ remaining: 0, purchased: 0, unit: "braincells" });
    if (url === "https://aesthetic.computer/api/nopaint-inference") return json({ models: [] });
    if (url === "https://aesthetic.computer/api/painting-wip") { const body = JSON.parse(request.postData() || "{}"); return json({ code: "t3st", revision: body.action === "save" ? (body.revision || 0) + 1 : 0, steps: 0, status: "wip" }); }
    const lib = /lib\/(nopaint-pieces|painting-state)\.mjs$/.exec(url);
    if (lib) return request.respond({ status: 200, contentType: "text/javascript", headers: cors, body: readFileSync(join(LIB, `${lib[1]}.mjs`)) });
    if (url.startsWith(site.url) || url.startsWith("data:") || url.startsWith("blob:")) return request.continue();
    request.abort();
  });
  await page.goto(site.url + "/", { waitUntil: "domcontentloaded", timeout: 45000 });
  const ask = (op, args) => page.evaluate((o, a) => new Promise((resolve, reject) => {
    const channel = new BroadcastChannel("nopaint-classic"), id = `s${Math.random()}`;
    channel.onmessage = ({ data }) => { if (data?.id === id) { channel.close(); data.error ? reject(new Error(data.error)) : resolve(data.result); } };
    channel.postMessage({ to: "game", id, op: o, ...a });
    setTimeout(() => reject(new Error(`no answer to ${o}`)), 8000);
  }), op, args || {});
  for (let i = 0; i < 200; i++) { if (await ask("state").then((s) => s.layout, () => null)) break; await wait(200); }
  await wait(2500);
  const take = async (name) => {
    await page.screenshot({ path: join(OUT, `${name}.png`) });
    const stage = await ask("stage");
    writeFileSync(join(OUT, `${name}.json`), JSON.stringify(stage));
    console.log(`${name}: ${stage.layout}, ${stage.things.filter((thing) => thing.shown).length} shown of ${stage.things.length}`);
    return stage;
  };
  return { page, ask, take, close: () => context.close() };
}

// A guest: the dark opening screen, the lit one, and the pointer over each thing on it.
let game = await open(true);
await game.take("guest-dark");
await game.page.mouse.click(W / 2, H * 0.45); await wait(2200);
const lit = await game.take("guest-lit");
if (process.env.HOVER) for (const uid of (process.env.HOVER_UIDS || "114,9,44,789,710").split(",").map(Number)) {
  const thing = lit.things.find((t) => t.uid === uid); if (!thing) continue;
  await game.page.mouse.move((thing.css[0][0] + thing.css[1][0]) / 2, (thing.css[0][1] + thing.css[1][1]) / 2); await wait(700);
  await game.take(`guest-lit-over-${uid}`);
}
await game.close();

// A painter: the same two, then painting, the pause screen, and after Save.
game = await open(false);
await game.take("painter-dark");
await game.page.mouse.click(W / 2, H * 0.45); await wait(2200);
const painterLit = await game.take("painter-lit");
if (process.env.HOVER) {
  // Over each sprite and text that could be a button, to see how the game answers a pointer.
  const spots = painterLit.things.filter((t) => t.shown && !/Particle|Stamp|Cursor/.test(t.type) && t.css[1][0] - t.css[0][0] < W * 0.7 && t.css[1][1] - t.css[0][1] < H * 0.4);
  const seen = new Set();
  for (const thing of spots) {
    const x = Math.round((thing.css[0][0] + thing.css[1][0]) / 2), y = Math.round((thing.css[0][1] + thing.css[1][1]) / 2);
    if (seen.has(`${x >> 4},${y >> 4}`) || x < 0 || y < 0 || x > W || y > H) continue; seen.add(`${x >> 4},${y >> 4}`);
    await game.page.mouse.move(x, y); await wait(600);
    await game.take(`painter-lit-over-${thing.type}-${thing.uid}`);
  }
  await game.page.mouse.move(W / 2, H * 0.45);
}
await game.page.mouse.click(W / 2, H * 0.45); await wait(2500);
await game.take("painting");
for (let i = 0; i < 6; i++) { try { await game.ask("prompt", { text: "Box: 128 128 90 60 FF0000" }); break; } catch { await wait(1500); } }
await wait(2200);
await game.ask("call", { name: "Paint" }); await wait(5000);
await game.ask("call", { name: "Pause" }); await wait(1500);
await game.take("pause");
await game.ask("call", { name: "Save" }); await wait(7000);
await game.take("saved");
await game.close();
await browser.close(); site.close();
