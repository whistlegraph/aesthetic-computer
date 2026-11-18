// classic-steps.e2e, 2026.11.18
// Plays a few steps of No Paint classic and checks that the page turns each
// one, Paint or No, into a step of an Aesthetic Computer #painting
// (system/public/nopaint.art/classic-steps.mjs).
//
// Nothing real is reached. The build is served from a folder, the two shared
// libraries are answered from files beside this test, and /api/painting-wip is
// played by this script, which keeps every request it was sent in
// OUT/wip-calls.json so the real service's checks can be run on them later
// (toolchain/nopaint/steps-replay.mjs).
//
//   node classic-steps.e2e.mjs
//
// Beside this file on a fleet machine: book-kit.mjs, ./site (the build),
// nopaint-pieces.mjs and painting-state.mjs (from system/public/aesthetic.computer/lib).

import { existsSync, readFileSync, writeFileSync } from "node:fs";
import { gunzipSync } from "node:zlib";
import { join } from "node:path";
import { fileURLToPath } from "node:url";
import { launch, openPage, serveFolder, viewport, wait, OUT } from "./book-kit.mjs";

const here = (path) => fileURLToPath(new URL(path, import.meta.url));
const SITE = process.env.SITE || (existsSync(here("./site")) ? here("./site") : here("../../system/public/nopaint.art"));
const LIB = existsSync(here("./nopaint-pieces.mjs")) ? here("./") : here("../../system/public/aesthetic.computer/lib/");
const VIEW = viewport(512, 704, 1);
const checks = [];
const check = (name, ok, detail = "") => { checks.push({ name, ok: !!ok, detail }); console.log(`${ok ? "ok  " : "FAIL"} ${name}${detail ? `  (${detail})` : ""}`); };

// The painting service, played: one painting, kept in memory.
const calls = [], signins = [], mailed = [];
let stored = null;
function service(body) {
  calls.push(body);
  if (body.action === "create") { stored ||= { id: body.id, key: body.key, code: "t3st", revision: 0, state: null }; return { code: stored.code, revision: stored.revision, steps: 0, status: "wip" }; }
  if (!stored || body.code !== stored.code || body.key !== stored.key) return { http: 403, error: "This painting belongs to another editor" };
  const steps = () => stored.state ? decode(stored.state).layers.length - 1 : 0;
  if (body.action === "save") {
    // As the real service: the same state again is not a new revision.
    if (body.state === stored.state) return { code: stored.code, revision: stored.revision, steps: steps(), status: "wip" };
    if (body.revision !== stored.revision) return { http: 409, error: "This painting changed in another tab" };
    stored.revision++; stored.state = body.state;
    return { code: stored.code, revision: stored.revision, steps: steps(), status: "wip" };
  }
  if (body.action === "read") return { code: stored.code, revision: stored.revision, steps: steps(), status: "wip", canEdit: true, state: body.state ? stored.state : undefined };
  return { http: 400, error: "Unknown painting action" };
}
const decode = (state) => JSON.parse(gunzipSync(Buffer.from(state, "base64")).toString("utf8")).piece;

const site = await serveFolder(SITE);
const browser = await launch(VIEW);
const context = await browser.createBrowserContext();
async function open() {
  const { page, log } = await openPage(browser, VIEW, { context, consoleTypes: ["error"] });
  if (process.env.DEBUG) page.on("pageerror", (error) => console.log("PAGE ERROR", String(error?.stack || error).slice(0, 600)));
  await page.setRequestInterception(true);
  page.on("request", (request) => {
    const url = request.url();
    const cors = { "Access-Control-Allow-Origin": "*", "Access-Control-Allow-Headers": "Content-Type, Authorization" };
    // Sign-in, played: the link is "opened" on the third poll.
    if (url === site.url + "/api/nopaint-signin") {
      const body = JSON.parse(request.postData() || "{}");
      signins.push(body.action);
      const value = body.action === "create" ? { code: "ABC123", pollSecret: "s".repeat(43) }
        : body.action === "email" ? { client_id: "test", authParams: { state: "x" } }
        : signins.filter((a) => a === "poll").length < 3 ? { status: "pending" }
        : { status: "claimed", session: { accessToken: "test-token", account: { label: "@tester", id: "auth0|test" } } };
      return request.respond({ status: 200, contentType: "application/json", body: JSON.stringify(value) });
    }
    if (url === "https://hi.aesthetic.computer/passwordless/start") {
      if (request.method() === "OPTIONS") return request.respond({ status: 204, headers: cors });
      mailed.push(JSON.parse(request.postData() || "{}").email);
      return request.respond({ status: 200, contentType: "application/json", headers: cors, body: "{}" });
    }
    if (url.startsWith(site.url) || url.startsWith("data:") || url.startsWith("blob:")) return request.continue();
    const lib = /^https:\/\/aesthetic\.computer\/aesthetic\.computer\/lib\/(nopaint-pieces|painting-state)\.mjs$/.exec(url);
    if (lib) return request.respond({ status: 200, contentType: "text/javascript", headers: cors, body: readFileSync(join(LIB, `${lib[1]}.mjs`)) });
    if (url === "https://aesthetic.computer/api/painting-wip") {
      if (request.method() === "OPTIONS") return request.respond({ status: 204, headers: cors });
      const { http: status = 200, ...value } = service({ ...JSON.parse(request.postData() || "{}"), ...(request.headers().authorization ? { _as: request.headers().authorization } : {}) });
      return request.respond({ status, contentType: "application/json", headers: cors, body: JSON.stringify(value) });
    }
    request.abort();
  });
  await page.goto(site.url + "/", { waitUntil: "domcontentloaded", timeout: 45000 });
  const ask = (op, args) => page.evaluate((o, a) => new Promise((resolve, reject) => {
    const channel = new BroadcastChannel("nopaint-classic"), id = `t${Math.random()}`;
    channel.onmessage = ({ data }) => { if (data?.id === id) { channel.close(); data.error ? reject(new Error(data.error)) : resolve(data.result); } };
    channel.postMessage({ to: "game", id, op: o, ...a });
    setTimeout(() => reject(new Error(`no answer to ${o}`)), 8000);
  }), op, args || {});
  for (let i = 0; i < 200; i++) { if (await ask("state").then((s) => s.layout, () => null)) break; await wait(200); }
  await wait(600);
  await ask("goto", { layout: "Index" });
  await wait(900);
  const steps = () => page.evaluate(() => ({ code: window.nopaintSteps?.code, version: window.nopaintSteps?.version, saved: window.nopaintSteps?.saved,
    error: window.nopaintSteps?.error, chip: document.querySelector("#nopaint-steps")?.textContent, who: document.querySelector("#nopaint-who")?.textContent }));
  // Ask for a brush, watch it, answer it, and wait until the game has
  // reported that answer and the page has taken the step.
  const play = async (line, answer) => {
    await ask("prompt", { text: line });
    await wait(2200);
    const mark = await page.evaluate(() => window.nopaintSteps.events.length);
    const before = (await steps()).version || 0;
    await ask("call", { name: answer });
    await page.waitForFunction((from, event) => window.nopaintSteps.events.slice(from).includes(event), { timeout: 20000, polling: 100 }, mark, answer === "Paint" ? "painted" : "refused");
    for (let i = 0; i < 60 && ((await steps()).version || 0) <= before; i++) await wait(100);
  };
  return { page, log, ask, steps, play };
}

try {
  let game = await open();
  await game.play("Box: 128 128 90 60 FF0000", "Paint");
  await game.play("Flip: H", "No");
  await game.play("Softy: 128 128 L ?", "Paint");
  await wait(4500); // the quiet before a save, and the save
  // The game proposes on its own between ours, and those are refused when ours
  // takes over: they are steps too. So count what the record holds.
  let seen = await game.steps();
  if (process.env.DEBUG) console.log(JSON.stringify(seen), calls.map((c) => `${c.action}:${c.revision ?? ""}:${(c.state || "").length}`).join(" "), game.log.errors, game.log.console);
  check("the page shows a #code and a version", seen.code === "t3st" && /^#t3st v\d+$/.test(seen.chip || ""), seen.chip);
  check("every step taken is saved", seen.version >= 3 && seen.saved === seen.version && !seen.error, `v${seen.version}, saved ${seen.saved}${seen.error ? `, ${seen.error}` : ""}`);
  check("one painting was created", calls.filter((c) => c.action === "create").length === 1);
  let piece = decode(stored.state);
  const kinds = piece.layers.slice(1).map((layer) => `${layer.code.score.proposal.kind}:${layer.code.score.proposal.decision}`);
  check("the record holds a base and one layer per step", piece.layers.length === seen.version + 1, kinds.join(" "));
  const ours = ["Box:paint", "Flip:no", "Softy:paint"];
  check("our three answers are in it, in order", ours.every((kind, i) => kinds.indexOf(kind) > (i ? kinds.indexOf(ours[i - 1]) : -1)), kinds.join(" "));
  const box = piece.layers.slice(1).find((layer) => layer.code.score.proposal.kind === "Box" && layer.code.score.proposal.decision === "paint");
  check("a Paint keeps the line that made it and the pixels it changed", box?.code.score.proposal.line === "Box: 128 128 90 60 FF0000" && box.pixels.width > 0 && box.pixels.width < 256, `${box?.pixels.width} x ${box?.pixels.height} at ${box?.pixels.x},${box?.pixels.y}`);
  check("a No keeps its line and no pixels", piece.layers.slice(1).filter((layer) => layer.code.score.proposal.decision === "no").every((layer) => layer.pixels.width === 0 && layer.code.source.length > 0));
  // Lay every layer over the base: it must give the picture the record says it has.
  const canvas = Buffer.from(piece.layers[0].pixels.data.$pixels, "base64");
  for (const layer of piece.layers.slice(1)) {
    const data = Buffer.from(layer.pixels.data.$pixels, "base64"), { x, y, width, height } = layer.pixels;
    for (let row = 0; row < height; row++) for (let col = 0; col < width; col++) {
      const from = (row * width + col) * 4, to = ((y + row) * 256 + x + col) * 4;
      if (data[from + 3]) data.copy(canvas, to, from, from + 4);
    }
  }
  check("the layers laid over the base give the saved picture", canvas.equals(Buffer.from(piece.composite.pixels.$pixels, "base64")));
  const before = seen.version;
  await game.page.close();

  // Come back: the same painting goes on.
  game = await open();
  await game.play("Invert", "Paint");
  await wait(4500);
  seen = await game.steps();
  check("after a reload the same #code goes on", seen.code === "t3st" && seen.version > before && seen.saved === seen.version, `${seen.chip}, was v${before}`);
  check("still one painting", calls.filter((c) => c.action === "create").length === 1);

  // Sign in with a handle: the dialog, the link, and the painting saved as them.
  check("a guest is offered sign-in", seen.who === "sign in", seen.who);
  const prompt = (await game.ask("state")).prompt;
  await game.page.click("#nopaint-who");
  await game.page.type("#nopaint-signin input[name=email]", "tester@example.com", { delay: 15 });
  check("typing in the dialog does not reach the game's prompt", (await game.ask("state")).prompt === prompt);
  await game.page.click("#nopaint-signin button[type=submit]");
  await game.page.waitForFunction(() => window.nopaintSteps.account === "@tester", { timeout: 20000, polling: 200 }).catch(() => {});
  await wait(1500);
  seen = await game.steps();
  check("the link was asked for once, for that address", mailed.length === 1 && mailed[0] === "tester@example.com", mailed.join(","));
  check("the page shows the handle once the link is opened", seen.who === "@tester", `${seen.who}; ${signins.join(" ")}`);
  check("the dialog closed itself", await game.page.evaluate(() => !document.querySelector("#nopaint-signin").open));
  await game.play("Flip: V", "Paint");
  await wait(4500);
  seen = await game.steps();
  const asHandle = calls.filter((c) => c.action === "save" && c._as === "Bearer test-token").length;
  check("steps after sign-in are saved as the handle", asHandle >= 1 && seen.saved === seen.version && !seen.error, `${asHandle} saves as @tester, ${seen.chip}${seen.error ? `, ${seen.error}` : ""}`);
  check("no page errors", game.log.errors.length === 0, game.log.errors.join(" | ").slice(0, 160));
} catch (error) {
  check("the run finished", false, String(error?.message || error).slice(0, 200));
}
writeFileSync(join(OUT, "wip-calls.json"), JSON.stringify(calls));
writeFileSync(join(OUT, "results.json"), JSON.stringify({ at: new Date().toISOString(), checks }, null, 1));
await browser.close();
site.close();
const failed = checks.filter((c) => !c.ok);
console.log(`done: ${checks.length - failed.length} passed, ${failed.length} failed`);
process.exit(failed.length ? 1 : 0);
