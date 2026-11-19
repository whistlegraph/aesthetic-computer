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
import { gunzipSync, deflateSync, crc32 } from "node:zlib";
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
const calls = [], moves = [], saves = [];
// The turnstile's client, played: "signing in" leaves for the hosted page and
// comes straight back with a code, as the real one does after the person signs in.
const TURNSTILE = `self.auth0 = { Auth0Client: class {
  constructor(options) { this.options = options; }
  async loginWithRedirect() { localStorage.setItem("@@auth0spajs@@::played", "{}"); location.assign(this.options.authorizationParams.redirect_uri + "/?code=played&state=played"); }
  async handleRedirectCallback() { localStorage.setItem("played-turnstile", "in"); }
  async isAuthenticated() { return localStorage.getItem("played-turnstile") === "in"; }
  async getUser() { return { sub: "auth0|played" }; }
  async getTokenSilently() { return "test-token"; }
  async logout() { localStorage.removeItem("played-turnstile"); localStorage.removeItem("@@auth0spajs@@::played"); }
} };`;
// The AI service, played: two brains at the painting's size, one of them the
// free one, and a move that answers with a flat magenta picture.
const CATALOG = { creditValueUSD: 0.000005, service: { available: true }, trial: { model: "free-brain@1024", family: "free-brain", turns: 1 },
  models: [
    { id: "free-brain", family: "free-brain", size: [256, 256], braincells: 4000, quote: "quote-free", available: true },
    { id: "free-brain@1024", family: "free-brain", size: [1024, 1024], braincells: 4000, quote: "quote-free-1024", available: true },
    { id: "cheap-brain", family: "cheap-brain", size: [256, 256], braincells: 2000, quote: "quote-cheap", available: true },
  ] };
function flatPng(side, [r, g, b]) {
  const row = Buffer.alloc(1 + side * 3); // filter byte, then RGB
  for (let x = 0; x < side; x++) row.set([r, g, b], 1 + x * 3);
  const chunk = (kind, data) => { const body = Buffer.concat([Buffer.from(kind), data]), out = Buffer.alloc(body.length + 8);
    out.writeUInt32BE(data.length, 0); body.copy(out, 4); out.writeUInt32BE(crc32(body), body.length + 4); return out; };
  const head = Buffer.alloc(13); head.writeUInt32BE(side, 0); head.writeUInt32BE(side, 4); head.set([8, 2, 0, 0, 0], 8);
  return Buffer.concat([Buffer.from("89504e470d0a1a0a", "hex"), chunk("IHDR", head), chunk("IDAT", deflateSync(Buffer.concat(Array(side).fill(row)))), chunk("IEND", Buffer.alloc(0))]);
}
const MAGENTA = flatPng(256, [255, 0, 255]).toString("base64");
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

let last = null; // the page in use, for a look when something goes wrong
const site = await serveFolder(SITE);
const browser = await launch(VIEW);
const context = await browser.createBrowserContext();
async function open({ stay = false } = {}) {
  const { page, log } = await openPage(browser, VIEW, { context, consoleTypes: ["error"] });
  if (process.env.DEBUG) page.on("pageerror", (error) => console.log("PAGE ERROR", String(error?.stack || error).slice(0, 600)));
  await page.setRequestInterception(true);
  page.on("request", (request) => {
    const url = request.url();
    const cors = { "Access-Control-Allow-Origin": "*", "Access-Control-Allow-Headers": "Content-Type, Authorization" };
    // The AC turnstile, played: its client library, and the handle lookup.
    if (url === "https://aesthetic.computer/aesthetic.computer/dep/auth0-spa-js.production.js")
      return request.respond({ status: 200, contentType: "text/javascript", headers: cors, body: TURNSTILE });
    if (url.startsWith("https://aesthetic.computer/api/handle?for="))
      return request.respond({ status: 200, contentType: "application/json", headers: cors, body: JSON.stringify({ handle: "tester" }) });
    // The save server, played: it hands back the #code it is asked for.
    if (url.startsWith("https://server.nopaint.art/")) {
      const at = new URL(url);
      saves.push({ path: at.pathname, as: at.searchParams.get("as"), body: (request.postData() || "").slice(0, 40) });
      if (request.method() === "OPTIONS") return request.respond({ status: 204, headers: cors });
      const code = at.searchParams.get("as") || "Classic1";
      return request.respond({ status: 200, contentType: "application/json", headers: cors,
        body: JSON.stringify(at.pathname === "/get-code" ? { code } : { code: "t3st", url: "https://aesthetic.computer/media/paintings/t3st.png" }) });
    }
    if (url === "https://aesthetic.computer/api/nopaint-inference") {
      if (request.method() === "OPTIONS") return request.respond({ status: 204, headers: cors });
      if (request.method() === "GET") return request.respond({ status: 200, contentType: "application/json", headers: cors, body: JSON.stringify(CATALOG) });
      const body = JSON.parse(request.postData() || "{}");
      moves.push({ ...body, _as: request.headers().authorization || null });
      // As the real service: a guest's free turn is one per browser token.
      const spent = !request.headers().authorization && moves.filter((m) => !m._as && m.trialId === body.trialId).length > 1;
      return request.respond({ status: spent ? 409 : 200, contentType: "application/json", headers: cors,
        body: JSON.stringify(spent ? { error: "This browser has used its free painting." } : { image: MAGENTA, width: 256, height: 256, billing: { braincells: request.headers().authorization ? 2000 : 0 } }) });
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
  // Wait for the game to be up (again, after a reload), on its title screen.
  const wake = async () => { for (let i = 0; i < 200; i++) { if (await ask("state").then((s) => s.layout, () => null)) break; await wait(200); } await wait(600); };
  // Leave the title screen as a player does: taps in the middle of it. The
  // first tap on the dark opening screen only turns the lights on.
  const start = async () => {
    for (let taps = 0; taps < 4 && (await ask("state")).layout === "Splash"; taps++) {
      await page.mouse.click(VIEW.W / 2, VIEW.H * 0.45);
      for (let i = 0; i < 20 && (await ask("state")).layout === "Splash"; i++) await wait(150);
    }
    await wait(900);
  };
  // Turn the title screen's lights on, if they are off.
  const lights = async () => {
    for (let i = 0; i < 20 && (await steps()).title?.lights === null; i++) await wait(150);
    if ((await steps()).title?.lights === false) await page.mouse.click(VIEW.W / 2, VIEW.H * 0.45);
    for (let i = 0; i < 40 && (await steps()).title?.lights !== true; i++) await wait(150);
  };
  await wake();
  if (!stay) await start();
  const steps = () => page.evaluate(() => ({ code: window.nopaintSteps?.code, version: window.nopaintSteps?.version, saved: window.nopaintSteps?.saved,
    error: window.nopaintSteps?.error, chip: window.nopaintSteps?.painting, who: window.nopaintSteps?.account, title: window.nopaintSteps?.title }));
  // Tap a thing the game drew, in the middle of its box on the page.
  const tap = async (uid) => { const { css } = await ask("look", { uid }); await page.mouse.click((css[0][0] + css[1][0]) / 2, (css[0][1] + css[1][1]) / 2); };
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
  // Make the next proposal an AI turn, wait for its picture to show, and answer it.
  const playAi = async (answer) => {
    await ask("force", { name: "AI" });
    const mark = await page.evaluate(() => window.nopaintSteps.events.length);
    const before = (await steps()).version || 0;
    await ask("call", { name: "No" }); // whatever is up; the next one is the AI turn
    await page.waitForFunction((from) => window.nopaintSteps.events.slice(from).includes("ai"), { timeout: 20000, polling: 100 }, mark);
    for (let i = 0; i < 100 && (await ask("state")).layout !== "Load"; i++) await wait(100);
    await wait(Number(process.env.AI_WAIT || 9000)); // the spinner, then the picture flickers in
    const after = await page.evaluate(() => window.nopaintSteps.events.length);
    await ask("call", { name: answer });
    await page.waitForFunction((from, event) => window.nopaintSteps.events.slice(from).includes(event), { timeout: 20000, polling: 100 }, after, answer === "Paint" ? "painted" : "refused");
    for (let i = 0; i < 60 && ((await steps()).version || 0) <= before + 1; i++) await wait(100);
  };
  return (last = { page, log, ask, steps, play, playAi, wake, start, tap, lights });
}

// The tuning and the picker, as node sees them, to compare with the game's.
const beside = (name, repo) => existsSync(here(`./${name}`)) ? here(`./${name}`) : here(repo);
const tuning = JSON.parse(readFileSync(beside("classic-tuning.json", "../../toolchain/nopaint/classic-tuning.json"), "utf8"));
const scope = {};
new Function("self", readFileSync(beside("classic-picker.js", "../../toolchain/nopaint/classic-picker.js"), "utf8"))(scope);
const expected = (flags) => Object.fromEntries(scope.nopaintPicker.table(scope.nopaintPicker.resolve(tuning, flags)).map((row) => [row.name, row.chance * 100]));

try {
  let game = await open();
  // The odds: the game picks by the tuning. (The page raises the ai flag by
  // itself for a guest with a free turn; lower it to see the plain odds.)
  for (let i = 0; i < 50 && !("AI" in (await game.ask("odds")).odds); i++) await wait(200);
  const offered = await game.ask("odds");
  check("a guest with a free turn is offered AI at its share, unasked", Math.abs((offered.odds.AI ?? -1) - expected({ ai: true }).AI) < 0.01, `AI ${offered.odds.AI} in 100`);
  await game.ask("flags", { flags: { ai: false } });
  const plain = await game.ask("odds");
  const want = expected({});
  check("the game picks by the tuning", plain.hooked && Object.keys(want).length === Object.keys(plain.odds).length && Object.entries(want).every(([name, chance]) => Math.abs(plain.odds[name] - chance) < 0.01),
    `${Object.keys(plain.odds || {}).length} brushes, Stamp ${plain.odds?.Stamp} in 100`);
  const drawn = await game.ask("sample", { n: 60000 });
  const worst = Object.entries(want).map(([name, chance]) => [name, Math.abs((drawn.counts[name] || 0) / drawn.n * 100 - chance)]).sort((a, b) => b[1] - a[1])[0];
  check("60,000 proposals drawn in the game come out at those odds", worst[1] < 0.6 && Object.keys(drawn.counts).every((name) => name in want), `furthest: ${worst[0]}, off by ${worst[1].toFixed(2)} in 100`);
  check("the first proposal of a run is from the first list", drawn.first in tuning[tuning.first], drawn.first);
  const lit = await game.ask("flags", { flags: { ai: true } }).then(() => game.ask("odds"));
  const wantAi = expected({ ai: true });
  check("raising the ai flag brings the AI list in at its share", Math.abs((lit.odds.AI ?? -1) - wantAi.AI) < 0.01 && Math.abs(lit.odds.Stamp - wantAi.Stamp) < 0.01, `AI ${lit.odds.AI}, Stamp ${lit.odds.Stamp}`);
  const down = await game.ask("flags", { flags: { ai: false } }).then(() => game.ask("odds"));
  check("lowering it gives the 2021 odds back", !("AI" in down.odds) && Math.abs(down.odds.Stamp - want.Stamp) < 0.01);
  // The flag stays down from here, so the game offers no AI turn of its own
  // and the one free turn is still there when the test asks for it by name.
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
  for (let i = 0; i < 50 && !("AI" in (await game.ask("odds")).odds); i++) await wait(200); // the page raises it again
  await game.ask("flags", { flags: { ai: false } });
  await game.play("Invert", "Paint");
  await wait(4500);
  seen = await game.steps();
  check("after a reload the same #code goes on", seen.code === "t3st" && seen.version > before && seen.saved === seen.version, `${seen.chip}, was v${before}`);
  check("still one painting", calls.filter((c) => c.action === "create").length === 1);

  // An AI turn as a guest: the free one.
  await game.playAi("Paint");
  await wait(4500);
  seen = await game.steps();
  piece = decode(stored.state);
  const aiLayer = piece.layers.at(-1), sent = moves[0] || {};
  check("the game asked for one move, with a note of its own and the painting", moves.length === 1 && tuning.ai.notes.includes(sent.hint) && sent.strength === tuning.ai.strength
    && Buffer.from(sent.image || "", "base64").readUInt32BE(16) === 256 && sent.model === "free-brain" && sent.quote === "quote-free" && /^[0-9a-f-]{36}$/.test(sent.trialId || "") && !sent._as, `"${sent.hint}", ${sent.model}`);
  check("the AI picture was painted and is a step", aiLayer.code.score.proposal.kind === "AI" && aiLayer.code.score.proposal.decision === "paint" && aiLayer.code.score.proposal.line === `AI: ${sent.hint}`
    && aiLayer.code.score.proposal.ai?.model === "free-brain" && aiLayer.pixels.width === 256 && seen.saved === seen.version, `${aiLayer.code.score.proposal.line}, ${aiLayer.pixels.width} x ${aiLayer.pixels.height}`);
  const composite = Buffer.from(piece.composite.pixels.$pixels, "base64");
  check("the painting is now the AI's picture, at full strength", composite[0] === 255 && composite[1] === 0 && composite[2] === 255 && composite[4 * 30000] === 255 && composite[4 * 30000 + 1] === 0,
    `top left ${[...composite.subarray(0, 3)].join(",")}`);
  if (process.env.DEBUG) console.log("AFTER GUEST TURN", JSON.stringify(await game.page.evaluate(() => window.nopaintSteps.log)));
  check("a guest's free turn is spent: AI is offered no more", !("AI" in (await game.ask("odds")).odds));

  // Sign in with a handle. The control is the game's own title label; the
  // sign-in is the AC turnstile; the page draws nothing.
  check("the page lays nothing over the game", await game.page.evaluate(() => [...document.body.children].every((el) => ["CANVAS", "SCRIPT", "STYLE", "NOSCRIPT"].includes(el.tagName) || el.id === "fb-root" || !el.getBoundingClientRect().width)));
  await game.ask("goto", { layout: "Splash" });
  for (let i = 0; i < 40 && (await game.steps()).title?.who !== "Sign In"; i++) await wait(150);
  check("a guest's lit title screen says Sign In in the game's top-left slot", (await game.ask("look", { uid: 114 })).text === "Sign In" && (await game.steps()).title.lights === true && (await game.steps()).title.spots.join() === "in", (await game.ask("look", { uid: 114 })).text);
  await game.page.screenshot({ path: join(OUT, "title-guest.png") });
  await game.tap(114);
  await game.page.waitForFunction(() => window.nopaintSteps?.account === "@tester", { timeout: 30000, polling: 250 }).catch(() => {});
  await game.wake();
  seen = await game.steps();
  check("a tap on it goes to the turnstile and comes back signed in", seen.who === "@tester" && !/[?&]code=/.test(game.page.url()), `${seen.who}; ${game.page.url().replace(site.url, "")}`);
  for (let i = 0; i < 40 && (await game.steps()).title?.who !== "@tester"; i++) await wait(150);
  const opening = await game.ask("look", { uid: 98 });
  check("the dark opening screen shows the handle on its yellow label", opening.text === "@TESTER" && opening.shown && (await game.steps()).title.lights === false && (await game.ask("state")).layout === "Splash", opening.text);
  await game.page.screenshot({ path: join(OUT, "title-opening.png") });
  await game.lights();
  check("with the lights on it is in the top-left slot", (await game.ask("look", { uid: 114 })).text === "@tester" && (await game.ask("state")).layout === "Splash", (await game.ask("look", { uid: 114 })).text);
  // Still on the title screen: a painter's AI turns are a line under the handle.
  for (let i = 0; i < 40 && !(await game.steps()).title?.ai; i++) await wait(150);
  const aiLine = async () => (await game.steps()).title?.ai;
  check("a signed-in painter's AI turns start off, and the title screen says so", (await aiLine()) === "AI Off" && !("AI" in (await game.ask("odds")).odds), await aiLine());
  const lineBox = await game.page.evaluate(() => window.nopaintSteps.title.spots);
  const spotsNow = await game.page.evaluate(() => window.nopaintSteps.act("ai").then(() => window.nopaintSteps.title));
  await wait(800);
  check("turned on, the line shows the cost and AI is offered", (await aiLine()) === "AI On 2000" && (await game.ask("odds")).odds.AI > 0 && lineBox.join() === "out,ai", `${await aiLine()}; taps: ${lineBox.join()}`);
  await game.page.screenshot({ path: join(OUT, "title-painter.png") });
  await game.ask("flags", { flags: { ai: false } }); // so the only AI turn is the one asked for below
  await game.start();
  await game.play("Flip: V", "Paint");
  await wait(4500);
  seen = await game.steps();
  const asHandle = calls.filter((c) => c.action === "save" && c._as === "Bearer test-token").length;
  check("the same painting goes on, and its steps are saved as the handle", seen.code === "t3st" && asHandle >= 1 && seen.saved === seen.version && !seen.error, `${asHandle} saves as @tester, ${seen.chip}${seen.error ? `, ${seen.error}` : ""}`);

  // An AI turn for a signed-in painter who has turned them on: paid for.
  await game.playAi("No");
  await wait(4500);
  seen = await game.steps();
  piece = decode(stored.state);
  const refusedAi = piece.layers.at(-1).code.score.proposal;
  check("a paid move goes out as the handle with the cheapest brain", moves.length === 2 && moves[1]._as === "Bearer test-token" && moves[1].model === "cheap-brain" && !moves[1].trialId, `${moves.length} moves; ${moves[1]?.model}, ${moves[1]?._as}`);
  check("an AI turn answered No is a step with no pixels", refusedAi.kind === "AI" && refusedAi.decision === "no" && refusedAi.ai?.braincells === 2000 && piece.layers.at(-1).pixels.width === 0 && seen.saved === seen.version, `${refusedAi.line}: ${refusedAi.decision}`);

  // Save: the game asks the save server for the painting's own #code.
  const savesBefore = calls.filter((c) => c.action === "save").length;
  await game.ask("call", { name: "Pause" }); // Save is on the pause screen
  await wait(1500);
  await game.ask("call", { name: "Save" });
  for (let i = 0; i < 80 && !saves.some((s) => s.path === "/upload-painting"); i++) await wait(250);
  await wait(1500);
  if (process.env.DEBUG) console.log("AT SAVE", JSON.stringify(await game.ask("state")), JSON.stringify(saves), JSON.stringify(await game.page.evaluate(() => window.nopaintSteps.events.slice(-10))));
  const asked = saves.find((s) => s.path === "/get-code"), sentUp = saves.find((s) => s.path === "/upload-painting");
  check("Save asks the save server for this painting's #code", asked?.as === "t3st", JSON.stringify(asked));
  check("the game takes the #code as its code and saves under it", (await game.ask("state")).code === "t3st" && (sentUp?.body || "").startsWith("code=t3st&painting="), `${(await game.ask("state")).code}; ${sentUp?.body?.slice(0, 24)}`);
  check("pressing Save sends the latest steps to AC at once", calls.filter((c) => c.action === "save").length >= savesBefore && (await game.steps()).saved === (await game.steps()).version);

  // Sign out: two taps on the handle.
  await game.ask("goto", { layout: "Splash" });
  for (let i = 0; i < 40 && (await game.steps()).title?.who !== "@tester"; i++) await wait(150);
  await game.lights();
  await game.tap(114);
  for (let i = 0; i < 20 && (await game.steps()).title?.who !== "Sign Out?"; i++) await wait(150);
  const sure = (await game.steps()).title?.who, stillThere = (await game.ask("state")).layout;
  await game.tap(114);
  for (let i = 0; i < 20 && (await game.steps()).who; i++) await wait(150);
  for (let i = 0; i < 20 && (await game.steps()).title?.who !== "Sign In"; i++) await wait(150);
  check("two taps on the handle sign out, and neither starts the game", sure === "Sign Out?" && stillThere === "Splash" && !(await game.steps()).who && (await game.ask("look", { uid: 114 })).text === "Sign In" && (await game.ask("state")).layout === "Splash",
    `${sure} → ${(await game.ask("look", { uid: 114 })).text}`);
  check("no page errors", game.log.errors.length === 0, game.log.errors.join(" | ").slice(0, 160));
} catch (error) {
  check("the run finished", false, String(error?.message || error).slice(0, 200));
  if (process.env.DEBUG && last) console.log("AT FAILURE", JSON.stringify(await last.page.evaluate(() => ({ events: window.nopaintSteps.events.slice(-14), error: window.nopaintSteps.error, log: window.nopaintSteps.log.slice(-12),
    storage: Object.fromEntries(Object.entries(localStorage).filter(([k]) => k.startsWith("nopaint:classic:") && !k.endsWith("account") && !k.endsWith("steps"))) })).catch((e) => String(e))),
    JSON.stringify(moves.map((m) => [m.model, m._as, (m.hint || "").slice(0, 20)])), JSON.stringify(await last.ask("odds").then((o) => o.flags, () => null)));
}
writeFileSync(join(OUT, "wip-calls.json"), JSON.stringify(calls));
writeFileSync(join(OUT, "results.json"), JSON.stringify({ at: new Date().toISOString(), checks }, null, 1));
await browser.close();
site.close();
const failed = checks.filter((c) => !c.ok);
console.log(`done: ${checks.length - failed.length} passed, ${failed.length} failed`);
process.exit(failed.length ? 1 : 0);
