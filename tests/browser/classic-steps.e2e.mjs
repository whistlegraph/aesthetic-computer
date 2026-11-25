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
  async logout(options) {
    localStorage.removeItem("played-turnstile"); localStorage.removeItem("@@auth0spajs@@::played");
    // With somewhere to return to, the real one leaves to end the turnstile's own session and comes back.
    if (options?.logoutParams?.returnTo) { localStorage.setItem("played-logout", "at the turnstile"); location.assign(options.logoutParams.returnTo + "/"); }
  }
} };`;
// The AI service, played: two brains at the painting's size and a move that
// answers with a flat magenta picture, paid from the wallet's free day.
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
const wallet = { free: 0, asked: 0 }; // the handle's free Braincells today; the test sets what is left
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
const browser = await launch(VIEW, (process.env.CHROME_ARGS || "").split(" ").filter(Boolean));
const context = await browser.createBrowserContext(), guestContext = await browser.createBrowserContext();
async function open({ stay = false, guest = false, late = 0 } = {}) {
  const { page, log } = await openPage(browser, VIEW, { context: guest ? guestContext : context, consoleTypes: ["error"] });
  // Painting takes a handle, so the painting parts run signed in: a session
  // handed to the page, as the live test hands one.
  if (!guest) await page.evaluateOnNewDocument(() => localStorage.setItem("nopaint:classic:account", JSON.stringify({ token: "test-token", label: "@tester" })));
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
      // As the real service: the free day pays first, bought Braincells after.
      const cost = CATALOG.models.find((model) => model.id === body.model)?.braincells || 0, free = Math.min(cost, wallet.free);
      wallet.free -= free;
      moves.push({ ...body, _as: request.headers().authorization || null, _free: free, _paid: cost - free });
      return request.respond({ status: 200, contentType: "application/json", headers: cors,
        body: JSON.stringify({ image: MAGENTA, width: 256, height: 256, billing: { braincells: cost, free, paid: cost - free } }) });
    }
    if (url === "https://aesthetic.computer/api/nopaint-wallet") {
      if (request.method() === "OPTIONS") return request.respond({ status: 204, headers: cors });
      wallet.asked++;
      return request.respond({ status: 200, contentType: "application/json", headers: cors, body: JSON.stringify({ unlimited: false, unit: "braincells", remaining: wallet.free, purchased: 50000 }) });
    }
    // late: the page's module arrives after the game is up, as on a visit with the game in the cache.
    if (late && url.startsWith(site.url) && /classic-steps\.mjs/.test(url)) return void setTimeout(() => request.continue().catch(() => {}), late);
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
    error: window.nopaintSteps?.error, chip: window.nopaintSteps?.painting, who: window.nopaintSteps?.account, title: window.nopaintSteps?.title, ai: window.nopaintSteps?.ai }));
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
  // Returns what the AI brush showed: its layout, its note and the words on screen.
  const playAi = async (answer) => {
    await ask("force", { name: "AI" });
    const mark = await page.evaluate(() => window.nopaintSteps.events.length);
    const before = (await steps()).version || 0;
    await ask("call", { name: "No" }); // whatever is up; the next one is the AI turn
    await page.waitForFunction((from) => window.nopaintSteps.events.slice(from).includes("ai"), { timeout: 20000, polling: 100 }, mark);
    for (let i = 0; i < 100 && (await ask("state")).layout !== "AI"; i++) await wait(100);
    await wait(Number(process.env.AI_WAIT || 9000)); // the spinner, then the picture comes in
    const up = await ask("state"), shown = { layout: up.layout, note: String(up.hint || "").trim(),
      texts: (await ask("texts")).texts.filter((text) => text.shown).map((text) => text.text) };
    await page.screenshot({ path: join(OUT, `ai-turn-${answer.toLowerCase()}.png`) });
    const after = await page.evaluate(() => window.nopaintSteps.events.length);
    await ask("call", { name: answer });
    await page.waitForFunction((from, event) => window.nopaintSteps.events.slice(from).includes(event), { timeout: 20000, polling: 100 }, after, answer === "Paint" ? "painted" : "refused");
    for (let i = 0; i < 60 && ((await steps()).version || 0) <= before + 1; i++) await wait(100);
    return shown;
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
  // The odds: the game picks by the tuning. This painter's free Braincells of
  // the day are spent, so no AI turn is offered and none of the bought ones go.
  for (let i = 0; i < 40 && (await game.steps()).ai?.free !== 0; i++) await wait(150);
  await wait(600);
  const plain = await game.ask("odds");
  check("with the day's free Braincells spent, no AI turn is offered", !("AI" in plain.odds) && (await game.steps()).who === "@tester" && (await game.steps()).ai?.free === 0 && wallet.asked === 1, `${(await game.steps()).who}, free ${(await game.steps()).ai?.free}`);
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

  // Come back: the same painting goes on. It is a new day: the handle's free
  // Braincells cover two turns of the cheapest brain, and the AI brush is on
  // with no switch to throw.
  wallet.free = 4000;
  game = await open();
  for (let i = 0; i < 40 && (await game.steps()).ai?.free !== 4000; i++) await wait(150);
  await wait(600);
  check("a painter's AI turns are on while the free day covers one", Math.abs(((await game.ask("odds")).odds.AI ?? -1) - expected({ ai: true }).AI) < 0.01 && (await game.steps()).ai?.cost === 2000, `free ${(await game.steps()).ai?.free}, a turn ${(await game.steps()).ai?.cost}`);
  await game.ask("flags", { flags: { ai: false } }); // so the only AI turns are the ones asked for below
  await game.play("Invert", "Paint");
  await wait(4500);
  seen = await game.steps();
  check("after a reload the same #code goes on", seen.code === "t3st" && seen.version > before && seen.saved === seen.version, `${seen.chip}, was v${before}`);
  check("still one painting", calls.filter((c) => c.action === "create").length === 1);

  // While painting, the handle sits in the checkerboard left of No and Paint.
  const line = await game.page.evaluate(() => window.nopaintSteps.handleLine);
  const drawn2 = line ? await game.ask("look", { uid: line }) : {};
  check("the painter's handle is drawn left of No and Paint", /@tester/.test(drawn2.text || "") && drawn2.layer === "UI" && drawn2.x < 128 && drawn2.y >= 288 && drawn2.y < 304, `${drawn2.text} at ${drawn2.x},${drawn2.y}`);
  await game.page.screenshot({ path: join(OUT, "painting.png") });

  // An AI turn, paid from the free day.
  const aiShown = await game.playAi("Paint");
  await game.ask("flags", { flags: { ai: false } }); // the page raised it again: one free turn is left
  await wait(4500);
  seen = await game.steps();
  piece = decode(stored.state);
  const aiLayer = piece.layers.at(-1), sent = moves[0] || {};
  check("the game asked for one move, with a note of its own and the painting", moves.length === 1 && tuning.ai.notes.includes(sent.hint) && sent.strength === tuning.ai.strength
    && Buffer.from(sent.image || "", "base64").readUInt32BE(16) === 256 && sent.model === "cheap-brain" && sent.quote === "quote-cheap" && !sent.trialId && sent._as === "Bearer test-token" && sent._free === 2000 && sent._paid === 0, `"${sent.hint}", ${sent.model}, ${sent._as}`);
  check("the AI picture was painted and is a step", aiLayer.code.score.proposal.kind === "AI" && aiLayer.code.score.proposal.decision === "paint" && aiLayer.code.score.proposal.line === `AI: ${sent.hint}`
    && aiLayer.code.score.proposal.ai?.model === "cheap-brain" && aiLayer.code.score.proposal.ai?.braincells === 2000 && aiLayer.pixels.width === 256 && seen.saved === seen.version, `${aiLayer.code.score.proposal.line}, ${aiLayer.pixels.width} x ${aiLayer.pixels.height}`);
  check("an AI turn is the AI brush, labelled with its note: \"AI: <note>\", not Load", aiShown.layout === "AI" && aiShown.note === sent.hint
    && aiShown.texts.includes(`AI: ${aiShown.note}`) && !aiShown.texts.some((text) => /^Load\b/.test(text)), `${aiShown.layout}; ${aiShown.texts.filter((text) => /^(AI|Load)\b/.test(text)).join(" | ")}`);
  const composite = Buffer.from(piece.composite.pixels.$pixels, "base64");
  check("the painting is now the AI's picture, at full strength", composite[0] === 255 && composite[1] === 0 && composite[2] === 255 && composite[4 * 30000] === 255 && composite[4 * 30000 + 1] === 0,
    `top left ${[...composite.subarray(0, 3)].join(",")}`);

  // A second AI turn, answered No.
  await game.playAi("No");
  await wait(4500);
  seen = await game.steps();
  piece = decode(stored.state);
  const refusedAi = piece.layers.at(-1).code.score.proposal;
  check("the second move goes out as the handle with the cheapest brain, free too", moves.length === 2 && moves[1]._as === "Bearer test-token" && moves[1].model === "cheap-brain" && moves[1]._paid === 0, `${moves.length} moves; ${moves[1]?.model}, ${moves[1]?._as}`);
  for (let i = 0; i < 20 && "AI" in (await game.ask("odds")).odds; i++) await wait(150);
  check("the free day is spent: the AI brush is offered no more, and nothing bought was touched", !("AI" in (await game.ask("odds")).odds) && seen.ai?.free === 0 && wallet.free === 0 && moves.every((move) => move._paid === 0), `free ${seen.ai?.free}`);
  check("an AI turn answered No is a step with no pixels", refusedAi.kind === "AI" && refusedAi.decision === "no" && refusedAi.ai?.braincells === 2000 && piece.layers.at(-1).pixels.width === 0 && seen.saved === seen.version, `${refusedAi.line}: ${refusedAi.decision}`);

  // Typed at the prompt, the AI brush takes the typed note. With the free day
  // spent the page has no picture for it: the turn fails, the game says
  // "AI Failed", nothing goes to the AI service, and it is not a step.
  const typedFrom = await game.page.evaluate(() => window.nopaintSteps.events.length), movesBefore = moves.length;
  await game.ask("prompt", { text: "AI: add rain" });
  await game.page.waitForFunction((from) => window.nopaintSteps.events.slice(from).includes("failed"), { timeout: 20000, polling: 100 }, typedFrom).catch(() => {});
  let typedError = false;
  for (let i = 0; i < 60 && !typedError; i++) { typedError = (await game.ask("texts")).texts.some((text) => text.shown && text.text === "AI Failed"); if (!typedError) await wait(50); }
  const typedLog = (await game.page.evaluate(() => window.nopaintSteps.log)).find((line) => /ai turn ai\d+ "add rain"/.test(line));
  await wait(4500); // the quiet before a save, and the save
  const typedStep = decode(stored.state).layers.some((layer) => layer.code?.score?.proposal?.kind === "AI" && layer.code.score.proposal.options === "add rain");
  check("typed \"AI: add rain\", the AI brush asks for that note; with no picture to be had it says AI Failed and is no step", typedLog && typedError && moves.length === movesBefore && !typedStep,
    `${typedLog?.replace(/^.*?ai turn/, "ai turn")}; error shown ${typedError}; moves ${movesBefore}→${moves.length}; a step ${typedStep}`);

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
  // The code is written as an address: prompt.ac/ small, a # (the game's font
  // has none), then the code in the game's own letters.
  const codeLine = await game.ask("look", { uid: 718 }), mark = await game.ask("address");
  check("Save shows the code as prompt.ac/#code", mark.shown && mark.code === "t3st" && mark.ink > 60 && mark.layer === codeLine.layer && Math.round(mark.x + mark.width) === Math.round(codeLine.x + codeLine.width - 4 * 8)
    && codeLine.text.replace(/\[[^\]]*\]/g, "") === "t3st" && codeLine.shown, `${mark.width} x ${mark.height} at ${mark.x},${mark.y}, ${mark.ink} pixels of ink`);
  await game.page.screenshot({ path: join(OUT, "saved.png") });
  check("pressing Save sends the latest steps to AC at once", calls.filter((c) => c.action === "save").length >= savesBefore && (await game.steps()).saved === (await game.steps()).version);

  check("no page errors while painting", game.log.errors.length === 0, game.log.errors.join(" | ").slice(0, 160));
  await game.page.close();

  // The other order: the game is up before the page's module. Steps are still
  // taken and saved, with the AI brush on. (Once, the wallet waited on the
  // sign-in and the sign-in on the wallet, and nothing was ever saved.)
  wallet.free = 4000;
  game = await open({ late: 7000 });
  for (let i = 0; i < 60 && (await game.steps()).ai?.free !== 4000; i++) await wait(150);
  await game.ask("flags", { flags: { ai: false } });
  const lateBefore = (await game.steps()).version || 0;
  await game.play("Invert", "Paint");
  await wait(4500);
  seen = await game.steps();
  check("with the game up before the page's module, steps are taken and saved", seen.code === "t3st" && seen.version > lateBefore && seen.saved === seen.version && seen.ai?.free === 4000, `${seen.chip}, saved ${seen.saved}, free ${seen.ai?.free}`);
  wallet.free = 0;

  // A guest. Painting takes a handle: on the lit title screen Sign In is the
  // one thing that answers a tap.
  const before2 = { calls: calls.length, moves: moves.length };
  game = await open({ guest: true, stay: true });
  check("the page lays nothing over the game", await game.page.evaluate(() => [...document.body.children].every((el) => ["CANVAS", "SCRIPT", "STYLE", "NOSCRIPT"].includes(el.tagName) || el.id === "fb-root" || !el.getBoundingClientRect().width)));
  for (let i = 0; i < 40 && (await game.steps()).title?.who !== "Sign In"; i++) await wait(150);
  check("a guest's dark opening screen says SIGN IN on its yellow label", (await game.ask("look", { uid: 98 })).text === "SIGN IN" && (await game.steps()).title.lights === false, (await game.ask("look", { uid: 98 })).text);
  await game.lights();
  const slot = await game.ask("look", { uid: 114 });
  const plate = await game.ask("look", { uid: 158 });
  const tintOf = async (uid) => (await game.ask("stage")).things.find((thing) => thing.uid === uid)?.tint.map((v) => +v.toFixed(2)).join();
  check("with the lights on, Sign In is a button across the middle: black letters, large, on the game's plate turned yellow", /\[scale=3\]\[color=#000000\]Sign In/.test(slot.text) && Math.abs(slot.x + slot.width / 2 - 128) < 1.5
    && Math.abs(plate.x + plate.width / 2 - 128) < 1 && plate.y > 120 && plate.y + plate.height < 164 && plate.x < slot.x && plate.y < slot.y && (await tintOf(158)) === "1,0.92,0.2"
    && !(await game.ask("look", { uid: 398 })).shown && (await game.steps()).title.spots.join() === "in", `plate ${plate.width} x ${plate.height} at ${plate.x},${plate.y}; letters at ${slot.x},${slot.y}`);
  check("the game's own buttons are reddened", (await tintOf(789)) === "1,0.35,0.35" && (await tintOf(710)) === "1,0.4,0.4" && (await tintOf(44)) === "0.8,0.1,0.1" && (await tintOf(9)) === "0.8,0.1,0.1", `Whistlegraph ${await tintOf(789)}, About ${await tintOf(44)}`);
  await game.page.screenshot({ path: join(OUT, "title-guest.png") });
  // The pointer: the game's hand over Sign In, which turns over; an arrow over everything shut.
  const cursor = async () => (await game.ask("stage")).things.find((thing) => thing.type === "Cursor")?.animation;
  const middleOf = ({ css }) => [(css[0][0] + css[1][0]) / 2, (css[0][1] + css[1][1]) / 2];
  await game.page.mouse.move(...middleOf(plate)); await wait(900);
  const over = { ...(await game.steps()).title, cursor: await cursor(), plate: await tintOf(158), words: (await game.ask("look", { uid: 114 })).text };
  await game.page.screenshot({ path: join(OUT, "title-guest-over.png") });
  await game.page.mouse.move(...middleOf(await game.ask("look", { uid: 44 }))); await wait(900);
  const off = { ...(await game.steps()).title, cursor: await cursor(), plate: await tintOf(158) };
  check("under the pointer Sign In turns over and the cursor is the game's hand", over.hot && over.cursor === "Over Button" && over.plate === "0,0,0" && /color=#ffff00/.test(over.words), `${over.cursor}, plate ${over.plate}`);
  check("over a shut button the cursor stays an arrow, and Sign In turns back", !off.hot && off.cursor === "Normal" && off.plate === "1,0.92,0.2", `${off.cursor}, plate ${off.plate}`);
  // Everything else on the screen is shut: the middle (which starts a painting), About, Community, the corners.
  const middle = async (uid) => { const { css } = await game.ask("look", { uid }); return [(css[0][0] + css[1][0]) / 2, (css[0][1] + css[1][1]) / 2]; };
  for (const [x, y] of [[VIEW.W / 2, VIEW.H * 0.36], [VIEW.W / 2, VIEW.H * 0.6], await middle(44), await middle(9), [VIEW.W * 0.8, VIEW.H * 0.2], [VIEW.W * 0.85, VIEW.H * 0.75], [20, VIEW.H * 0.5]]) { await game.page.mouse.click(x, y); await wait(350); }
  await wait(800);
  check("no other tap on a guest's lit screen does anything", (await game.ask("state")).layout === "Splash" && game.page.url() === site.url + "/" && !(await game.steps()).who && (await game.page.evaluate(() => window.nopaintSteps.log)).every((line) => !/ tap: /.test(line)),
    `${(await game.ask("state")).layout}; ${game.page.url().replace(site.url, "")}`);
  await game.tap(114); // Sign In
  await game.page.waitForFunction(() => window.nopaintSteps?.account === "@tester", { timeout: 30000, polling: 250 }).catch(() => {});
  await game.wake();
  seen = await game.steps();
  check("a tap on Sign In goes to the turnstile, and comes back signed in", seen.who === "@tester" && !/[?&]code=/.test(game.page.url()), `${seen.who}; ${game.page.url().replace(site.url, "")}`);
  check("the guest painted nothing on the way", calls.length === before2.calls && moves.length === before2.moves && !seen.code);
  const said = async () => (await game.page.evaluate(() => window.nopaintSteps.log)).filter((line) => / title: /.test(line)).map((line) => line.split(" title: ")[1]);
  for (let i = 0; i < 40 && (await game.steps()).title?.who !== "@tester"; i++) await wait(150);
  check("back from the turnstile, the title never says Sign In", !(await said()).includes("Sign In") && (await said()).at(-1) === "@tester", (await said()).join(" → "));
  for (let i = 0; i < 40 && (await game.steps()).title?.who !== "@tester"; i++) await wait(150);
  const opening = await game.ask("look", { uid: 98 });
  check("the dark opening screen shows the handle on its yellow label", opening.text === "@TESTER" && opening.shown && (await game.steps()).title.lights === false && (await game.ask("state")).layout === "Splash", opening.text);
  await game.page.screenshot({ path: join(OUT, "title-opening.png") });
  await game.lights();
  const slot2 = await game.ask("look", { uid: 114 });
  check("with the lights on it is flush in the top-left corner, at its usual size", slot2.text === "@tester" && slot2.x === 0 && slot2.y === 0 && slot2.height === 16 && (await game.ask("state")).layout === "Splash", `${slot2.text} at ${slot2.x},${slot2.y}, ${slot2.height} high`);
  const plate2 = await game.ask("look", { uid: 158 });
  check("a painter's screen is the game's own: the plate is back in its corner and nothing is red", plate2.x === 0 && plate2.y === 0 && plate2.width === 101 && !(await game.steps()).title.dressed && (await game.steps()).title.pointing === null
    && (await game.ask("stage")).things.filter((thing) => [789, 710, 44, 9, 158].includes(thing.uid)).every((thing) => !(thing.tint[0] > 0.75 && thing.tint[1] < 0.5)), `plate ${plate2.width} x ${plate2.height} at ${plate2.x},${plate2.y}`);
  check("the handle is the only label: there is no AI switch, and the brush count is back", (await game.steps()).title.spots.join() === "out" && (await game.ask("look", { uid: 398 })).shown
    && !(await game.ask("texts")).texts.some((text) => /^AI /.test(text.text)), (await game.steps()).title.spots.join());
  await game.page.screenshot({ path: join(OUT, "title-painter.png") });

  // A later visit, still signed in: the handle is there from the first frame.
  await game.page.reload({ waitUntil: "domcontentloaded" });
  await game.wake();
  for (let i = 0; i < 40 && (await game.steps()).title?.who !== "@tester"; i++) await wait(150);
  check("on a later visit the title shows the handle at once, never Sign In", (await said()).join() === "@tester", (await said()).join(" → "));
  await game.lights();
  const slot3 = await game.ask("look", { uid: 114 });
  check("and it is flush top left there too", slot3.text === "@tester" && slot3.x === 0 && slot3.y === 0, `${slot3.text} at ${slot3.x},${slot3.y}`);

  // Sign out: two taps on the handle.
  await game.tap(114);
  for (let i = 0; i < 20 && (await game.steps()).title?.who !== "Sign Out?"; i++) await wait(150);
  const sure = (await game.steps()).title?.who, stillThere = (await game.ask("state")).layout;
  await game.tap(114);
  await game.page.waitForFunction(() => localStorage.getItem("played-logout") && window.nopaintSteps && !window.nopaintSteps.account, { timeout: 30000, polling: 250 }).catch(() => {});
  await game.wake();
  for (let i = 0; i < 40 && (await game.steps()).title?.who !== "Sign In"; i++) await wait(150);
  check("two taps on the handle sign out at the turnstile too, and neither starts the game", sure === "Sign Out?" && stillThere === "Splash"
    && (await game.page.evaluate(() => localStorage.getItem("played-logout"))) === "at the turnstile" && !(await game.steps()).who && (await game.steps()).title?.who === "Sign In" && (await game.ask("state")).layout === "Splash",
    `${sure} → ${(await game.steps()).title?.who}; ${await game.page.evaluate(() => localStorage.getItem("played-logout"))}`);
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
