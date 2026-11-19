// classic-live.e2e, 2026.11.18
// The proof run. It makes one real painting on the live site by each route,
// a guest and a signed-in painter, and films both. A guest is stopped at the
// title screen, where only Sign In answers. A painter plays a few brushes, takes an AI
// turn with the real service, and saves. Nothing is played here.
//
// THIS SPENDS REAL THINGS. Each run:
//   makes one real #painting on Aesthetic Computer;
//   spends one AI turn of the signed-in painter's free Braincells of the day.
// It is run by hand, never by a schedule.
//
//   VIDEO=1 node classic-live.e2e.mjs            both routes
//   VIDEO=1 node classic-live.e2e.mjs guest      one of them
//
// Env: SITE_URL (default https://nopaint.art), ACCOUNT (a file holding
//      {"token": "...", "label": "@handle"} for the signed-in route; without
//      it that route is skipped), plus book-kit's CHROME, OUT, VIDEO.
//
// The page is left to reach what it reaches in real use, except the visit
// tracker and analytics, which are blocked so the run is not counted.

import { existsSync, readFileSync, writeFileSync } from "node:fs";
import { join } from "node:path";
import { launch, openPage, startClip, viewport, wait, OUT } from "./book-kit.mjs";

const SITE = (process.env.SITE_URL || "https://nopaint.art").replace(/\/$/, "");
const VIEW = viewport(512, 704, 1);
const only = process.argv[2] || null;
const account = process.env.ACCOUNT && existsSync(process.env.ACCOUNT) ? JSON.parse(readFileSync(process.env.ACCOUNT, "utf8")) : null;
const routes = [{ name: "guest", account: null }, ...(account ? [{ name: "handle", account }] : [])].filter((r) => !only || r.name === only);
const results = [];
const say = (route, text) => console.log(`${route.padEnd(7)} ${text}`);

async function run(browser, route) {
  const r = { route: route.name, as: route.account?.label || "guest", checks: [] };
  const check = (name, ok, detail = "") => { r.checks.push({ name, ok: !!ok, detail }); say(route.name, `${ok ? "ok  " : "FAIL"} ${name}${detail ? `  (${detail})` : ""}`); };
  const context = await browser.createBrowserContext();
  const { page, log } = await openPage(browser, VIEW, { context });
  await page.setRequestInterception(true);
  page.on("request", (request) => /visit-tracker|googletagmanager|google-analytics/.test(request.url()) ? request.abort() : request.continue());
  // The signed-in route starts as a painter. AI turns are on by themselves
  // while the handle's free Braincells of the day cover one.
  if (route.account) await page.evaluateOnNewDocument((value) => {
    localStorage.setItem("nopaint:classic:account", JSON.stringify(value));
  }, { token: route.account.token, label: route.account.label });
  const clip = await startClip(page, `classic-${route.name}`);
  try {
    await page.goto(SITE + "/", { waitUntil: "domcontentloaded", timeout: 60000 });
    const ask = (op, args) => page.evaluate((o, a) => new Promise((resolve, reject) => {
      const channel = new BroadcastChannel("nopaint-classic"), id = `live${Math.random()}`;
      channel.onmessage = ({ data }) => { if (data?.id === id) { channel.close(); data.error ? reject(new Error(data.error)) : resolve(data.result); } };
      channel.postMessage({ to: "game", id, op: o, ...a });
      setTimeout(() => reject(new Error(`no answer to ${o}`)), 8000);
    }), op, args || {});
    const steps = () => page.evaluate(() => ({ code: window.nopaintSteps.code, version: window.nopaintSteps.version, saved: window.nopaintSteps.saved,
      error: window.nopaintSteps.error, who: window.nopaintSteps.account || "guest", title: window.nopaintSteps.title,
      chip: window.nopaintSteps.painting, log: window.nopaintSteps.log }));
    const events = () => page.evaluate(() => window.nopaintSteps.events.length);
    const until = (from, event, timeout = 30000) => page.waitForFunction((f, e) => window.nopaintSteps.events.slice(f).includes(e), { timeout, polling: 100 }, from, event);
    for (let i = 0; i < 300 && !(await ask("state").then((s) => s.layout, () => null)); i++) await wait(200);
    await wait(2500); // the dark opening screen, for the film
    if (!route.account) {
      // A guest cannot paint: the lit screen is shut but for Sign In, which goes to the turnstile.
      await page.mouse.click(VIEW.W / 2, VIEW.H * 0.45); // lights on
      await wait(2500); // the lit title screen with its Sign In, for the film
      const slot = await ask("look", { uid: 114 });
      check("the title screen says Sign In", /Sign In/.test(slot.text), slot.text.replace(/\[[^\]]*\]/g, ""));
      await page.mouse.click(VIEW.W / 2, VIEW.H * 0.45); // a tap to start: nothing
      await wait(1500);
      check("a guest's tap to start does nothing", (await ask("state")).layout === "Splash" && new URL(page.url()).host !== "hi.aesthetic.computer");
      await page.mouse.click((slot.css[0][0] + slot.css[1][0]) / 2, (slot.css[0][1] + slot.css[1][1]) / 2); // Sign In
      await page.waitForFunction(() => location.host === "hi.aesthetic.computer", { timeout: 30000 }).catch(() => {});
      await wait(3500);
      check("a guest's tap on Sign In lands on the AC turnstile", new URL(page.url()).host === "hi.aesthetic.computer", page.url().split("?")[0]);
      Object.assign(r, await clip.stop());
      await context.close().catch(() => {});
      return r;
    }
    // Start as a player does: a tap turns the lights on, another starts.
    for (let taps = 0; taps < 4 && (await ask("state")).layout === "Splash"; taps++) {
      await page.mouse.click(VIEW.W / 2, VIEW.H * 0.45);
      await wait(taps ? 400 : 2500); // the lit title screen, with its sign-in slot, for the film
      for (let i = 0; i < 20 && (await ask("state")).layout === "Splash" && taps; i++) await wait(150);
    }
    await wait(1200);
    // Hold the AI flag down so the one AI turn is the one asked for below.
    for (let i = 0; i < 60 && !("AI" in (await ask("odds")).odds); i++) await wait(250);
    const offered = "AI" in (await ask("odds")).odds;
    check("the page found an AI turn to offer", offered, route.account ? "a painter with free Braincells left today" : "none for a guest");
    await ask("flags", { flags: { ai: false } });
    const play = async (line, answer, look = 2600) => {
      await ask("prompt", { text: line });
      await wait(look);
      const mark = await events();
      await ask("call", { name: answer });
      await until(mark, answer === "Paint" ? "painted" : "refused");
      await wait(700);
    };
    await play("Box: 128 128 120 90 2222FF", "Paint");
    await play("Softy: 90 80 L FFDD00", "Paint");
    await play("Flip: H", "No");
    await play("Stamp", "Paint", 3200);
    // The AI turn: the real service makes the picture.
    await ask("force", { name: "AI" });
    let mark = await events();
    await ask("call", { name: "No" });
    await until(mark, "ai");
    const asked = Date.now();
    await page.waitForFunction(() => window.nopaintSteps.log.some((line) => / handed to the game| failed: /.test(line)), { timeout: 150000, polling: 250 });
    r.aiSeconds = +((Date.now() - asked) / 1000).toFixed(1);
    const made = (await steps()).log.filter((line) => /ai turn/.test(line));
    check("the real service made a picture", made.some((line) => /handed to the game/.test(line)), `${r.aiSeconds} s; ${made.at(-1)}`);
    await wait(9000); // the spinner ends and the picture flickers in
    mark = await events();
    await ask("call", { name: "Paint" });
    await until(mark, "painted");
    await wait(2500);
    await play("Vignette", "Paint", 3000);
    await wait(4000); // the save
    let seen = await steps();
    check("every step is on AC under one #code", !!seen.code && seen.saved === seen.version && !seen.error, `${seen.chip}${seen.error ? `, ${seen.error}` : ""}`);
    check(route.account ? "the painter is signed in" : "the painter is a guest", seen.who === (route.account?.label || "guest"), seen.who);
    // Save, from the pause screen: the game shows the painting's own code.
    await ask("call", { name: "Pause" });
    await wait(1800);
    await ask("call", { name: "Save" });
    for (let i = 0; i < 80 && (await ask("state")).code !== seen.code; i++) await wait(250);
    await wait(4000);
    check("Save shows the painting's #code", (await ask("state")).code === seen.code, `the game's code: ${(await ask("state")).code}`);
    seen = await steps();
    Object.assign(r, { code: seen.code, version: seen.version });
    // What AC says about the painting.
    const about = await page.evaluate(async (code) => {
      const [wip, named] = await Promise.all([`https://aesthetic.computer/api/painting-wip?code=${code}`, `https://aesthetic.computer/api/painting-code?code=${code}`].map((url) => fetch(url).then((x) => x.json())));
      return { steps: wip.steps, status: wip.status, handle: named.handle };
    }, seen.code);
    Object.assign(r, about);
    check("AC has the painting with those steps", about.steps === seen.version && about.status === "wip", `#${seen.code}: ${about.steps} steps, ${about.status}`);
    check(route.account ? "it belongs to the handle" : "it belongs to nobody yet", route.account ? `@${about.handle}`.replace("@@", "@") === route.account.label : about.handle === "anon", `handle: ${about.handle}`);
    check("no page errors", log.errors.length === 0, log.errors.join(" | ").slice(0, 200));
    await wait(1500);
  } catch (error) {
    check("the run finished", false, String(error?.message || error).slice(0, 200));
    r.log = await page.evaluate(() => window.nopaintSteps?.log?.slice(-10)).catch(() => null);
  }
  Object.assign(r, await clip.stop());
  await context.close().catch(() => {});
  return r;
}

const browser = await launch(VIEW);
for (const route of routes) {
  results.push(await run(browser, route));
  writeFileSync(join(OUT, "results.json"), JSON.stringify({ site: SITE, at: new Date().toISOString(), results }, null, 1));
}
await browser.close();
const failed = results.flatMap((r) => r.checks).filter((c) => !c.ok);
for (const r of results) console.log(`${r.route}: #${r.code} v${r.version} as ${r.as}, AI in ${r.aiSeconds} s${r.video ? `, ${r.video}` : ""}`);
console.log(`done: ${results.flatMap((r) => r.checks).length - failed.length} passed, ${failed.length} failed`);
process.exit(failed.length ? 1 : 0);
