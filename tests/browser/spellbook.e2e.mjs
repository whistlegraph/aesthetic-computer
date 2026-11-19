// spellbook.e2e, 2026.11.16
// Casts every spell in "Aesthetic Spells" (papers/arxiv-magicians-manual)
// against production as a signed-out visitor and records what happened:
// where the address went, what the prompt said back, whether a key brought
// it home, and a screenshot. It judges nothing; the report is the evidence
// the book's entries cite.
//
// It needs only puppeteer-core and book-kit.mjs (the parts shared with the
// other book tests), so it can run on a fleet machine that has no checkout:
//   mkdir -p ~/scratch/spells && cd ~/scratch/spells && npm i puppeteer-core
//   copy book-kit.mjs and this file there
//   node spellbook.e2e.mjs            # all cases
//   node spellbook.e2e.mjs notepat    # cases whose id contains "notepat"
//
// Env: AC_TEST_URL (default https://aesthetic.computer), CHROME (path),
//      OUT (default ./out), VIDEO=1 to keep a reference clip of every cast
//      in OUT/video (needs ffmpeg; FFMPEG overrides its path). With VIDEO the
//      page's Web Audio output is also taped to OUT/audio, to be muxed after.
//      W, H, DSF set the viewport (default 660 x 460 at 2x: the site draws at
//      half the CSS width, so this is a 330-pixel-wide screen, close to a
//      phone and legible in a small clip).
//
// Not cast, on purpose: done, publish, print, mug, scream, and
// delete-erase-and-forget-me. They publish, charge, notify or delete.

import { launch, openPage, shot, startClip, trimLog, saveResults, viewport, wait } from "./book-kit.mjs";

const BASE = process.env.AC_TEST_URL || "https://aesthetic.computer";
const only = process.argv[2] || null;
const VIEW = viewport(660, 460, 2);
const { W, H } = VIEW;
const px = (fx) => Math.round(W * fx), py = (fy) => Math.round(H * fy);
const slug = (s) => s.replace(/[^a-z0-9]+/gi, "-").replace(/^-|-$/g, "").slice(0, 40) || "x";

// ---- cases -----------------------------------------------------------------
// cast: type at the prompt and press Enter. back: then press this key.
// hook: false boots without acDEBUG, for the chat-backed rooms (in debug the
// chat library dials localhost and never connects).
const cast = (text, extra = {}) => ({ id: `cast:${text}`, kind: "cast", text, ...extra });
const CASES = [
  // I. The prompt
  cast("list", { back: "Escape" }), cast("prompt"), cast("+"), cast("dark"), cast("light"),
  cast("help", { hook: false }),
  // II. Wrong words
  cast("notepta", { settle: 20000 }), cast("how do i draw", { settle: 20000 }),
  { id: "address:missing", kind: "address", path: "/definitely-not-a-piece-xyz" },
  { id: "address:notepat", kind: "address", path: "/notepat" },
  // IV. Become a handle (signed out: these should ask for a login)
  cast("imnew"), cast("hi"), cast("handle spelltest"), cast("me"), cast("mood"), cast("bye"),
  cast("handles", { back: "Escape" }), cast("@jeffrey", { back: "Escape" }),
  // V. Rooms
  cast("laklok", { hook: false }), cast("laer-klokken", { hook: false }), cast("chat", { hook: false }),
  cast("profile", { back: "Escape" }), cast("commits", { back: "Escape" }),
  cast("r8dio", { back: "Escape" }), cast("juke", { back: "Escape" }), cast("moods", { back: "Escape" }),
  { id: "key:ArrowDown", kind: "key", key: "ArrowDown" },
  { id: "key:Tab", kind: "key", key: "Tab", back: "Escape" },
  // VI. Sigils
  cast("@jeffrey/vili", { back: "Escape" }), cast("$nz7", { back: "Escape" }), cast("#jzg", { back: "Escape" }),
  // VII. Paint
  { id: "flow:paint", kind: "paint" },
  cast("line", { back: "Escape" }), cast("oval", { back: "Escape" }), cast("shape", { back: "Escape" }),
  cast("fill blue"), cast("smear", { back: "Escape" }), cast("blur", { back: "Escape" }),
  cast("sparkle", { back: "Escape" }), cast("word hello", { back: "Escape" }), cast("crop", { back: "Escape" }),
  cast("colors", { back: "Escape" }),
  // VIII. Play
  cast("notepat", { back: "Escape", play: "cdefg" }), cast("notepat", { id: "back:Backspace", back: "Backspace" }),
  cast("notepat", { id: "back:Backquote", back: "Backquote" }),
  cast("bleep", { back: "Escape", play: "a" }), cast("stample", { back: "Escape" }), cast("metronome 120", { back: "Escape", play: "a" }),
  cast("tone 440", { back: "Escape", play: "a" }), cast("tone:sine 440", { back: "Escape", play: "a" }), cast("clock", { back: "Escape", play: "a" }),
  cast("whistle", { back: "Escape" }), cast("bgm", { back: "Escape", play: "a" }),
  cast("danish", { back: "Escape" }), cast("english", { back: "Escape" }), cast("spanish", { back: "Escape" }),
  cast("chinese", { back: "Escape" }), cast("japanese", { back: "Escape" }), cast("russian", { back: "Escape" }),
  // IX. Tape and share
  { id: "flow:tape", kind: "tape" },
  cast("share notepat", { back: "Escape" }), cast("pack notepat", { settle: 9000 }),
  cast("selfie", { back: "Escape" }), cast("cam", { back: "Escape" }),
  // X. Write your own
  cast("(wipe blue) (ink yellow) (circle 64 64 30)", { back: "Escape" }),
  cast("code spelltest"), cast("edit notepat"), cast("source notepat", { settle: 8000 }),
  cast("docs"), cast("merry 2-bleep 2-clock", { settle: 6000 }),
  // XI. Catches
  cast("pp"), cast("give", { back: "Escape" }),
];

// ---- one page per case -----------------------------------------------------
async function open(browser, { hook = true } = {}) {
  const { page, log } = await openPage(browser, VIEW);
  if (hook) await page.evaluateOnNewDocument(() => { window.acDEBUG = true; });
  return { page, log };
}

async function boot(page, path = "/") {
  await page.goto(BASE + path, { waitUntil: "domcontentloaded", timeout: 45000 });
  try {
    await page.waitForFunction(() => typeof window.__acPromptTest === "function" || window.acBOOT_START_TIME, { timeout: 20000 });
  } catch {}
  await wait(3500);
}

const where = (page) => page.evaluate(() => ({
  path: location.pathname + location.search,
  host: location.host,
  title: document.title,
  input: typeof window.__acPromptTest === "function" ? window.__acPromptTest()?.input ?? null : undefined,
  frames: [...document.querySelectorAll("iframe")].map((f) => f.getAttribute("src")).filter(Boolean).slice(0, 4),
})).catch((e) => ({ error: String(e).slice(0, 120) }));

// Wake the prompt (once per page: a second tap on the backdrop puts it back
// to sleep and the next key is spent waking it), then type.
const woken = new WeakSet();
async function typeSpell(page, text) {
  if (!woken.has(page)) { await page.mouse.click(px(0.5), py(0.26)); woken.add(page); await wait(500); }
  await page.keyboard.type(text, { delay: 35 });
  await wait(300);
  const seen = (await where(page)).input;
  return { seen, matched: seen === undefined ? null : seen === text };
}

async function settle(page, before, ms) {
  const end = Date.now() + ms;
  let last = before;
  while (Date.now() < end) {
    await wait(500);
    const now = await where(page);
    if (now.error) return now; // navigated away from the document
    last = now;
    if (ms <= 7000 && now.path !== before.path) { await wait(2200); return await where(page); }
  }
  return last;
}

async function run(browser, c) {
  const { page, log } = await open(browser, c);
  const r = { id: c.id, kind: c.kind, text: c.text ?? c.path ?? c.key ?? null, hook: c.hook !== false };
  const t0 = Date.now();
  const clip = await startClip(page, c.id);
  try {
    if (c.kind === "address") {
      await boot(page, c.path);
      r.after = await where(page);
      r.shot = await shot(page, slug(c.id));
    } else if (c.kind === "key") {
      await boot(page);
      r.before = await where(page);
      await page.mouse.move(px(0.5), py(0.26));
      await wait(c.key === "ArrowDown" ? 7000 : 500); // the feed loads on idle
      await page.keyboard.press(c.key);
      r.after = await settle(page, r.before, 6000);
      r.shot = await shot(page, slug(c.id));
      if (c.back) {
        await page.keyboard.press(c.back); await wait(400);
        await page.keyboard.press(c.back); // rax: the first Escape frees the mouse
        await wait(2500);
        r.back = { key: c.back, ...(await where(page)) };
      }
    } else if (c.kind === "paint") {
      await boot(page);
      const step = async (text, drag) => {
        const typed = await typeSpell(page, text);
        const before = await where(page);
        await page.keyboard.press("Enter");
        const after = await settle(page, before, 5000);
        if (drag) {
          await page.mouse.move(px(0.27), py(0.33)); await page.mouse.down();
          await page.mouse.move(px(0.55), py(0.66), { steps: 12 }); await page.mouse.up();
          await wait(600);
        }
        const s = await shot(page, slug(`paint-${text}`));
        let home = null;
        if (after.path !== before.path) { await page.keyboard.press("Escape"); await wait(2200); home = await where(page); }
        return { text, typed, before: before.path, after: after.path, input: after.input, home: home?.path ?? null, shot: s };
      };
      r.steps = [];
      for (const [text, drag] of [["new", false], ["rect red", true], ["no", false], ["yes", false], ["flip", false], ["flop", false], ["left", false], ["right", false], ["resize 128", false], ["dl", false], ["no!", false]]) {
        r.steps.push(await step(text, drag));
      }
    } else if (c.kind === "tape") {
      await boot(page);
      r.steps = [];
      const note = async (text) => r.steps.push({ text, ...(await where(page)), shot: await shot(page, slug(`tape-${text}`)) });
      await typeSpell(page, "tape"); await page.keyboard.press("Enter"); await wait(3500); await note("tape");
      await typeSpell(page, "notepat"); await page.keyboard.press("Enter"); await wait(5000); await note("notepat");
      await page.keyboard.type("cdefg", { delay: 260 }); await wait(1500); await note("played cdefg");
      await page.keyboard.press("Escape"); await wait(5000); await note("Escape");
      await typeSpell(page, "cut"); await page.keyboard.press("Enter"); await wait(6000); await note("cut");
    } else {
      await boot(page);
      r.typed = await typeSpell(page, c.text);
      r.before = await where(page);
      await page.keyboard.press("Enter");
      r.after = await settle(page, r.before, c.settle || 7000);
      if (c.play && !r.after.error) { await page.mouse.click(px(0.5), py(0.5)); await page.keyboard.type(c.play, { delay: 280 }); await wait(900); }
      r.shot = await shot(page, slug(c.id));
      if (c.back && !r.after.error && r.after.path !== r.before.path) {
        await page.keyboard.press(c.back);
        await wait(2600);
        r.back = { key: c.back, ...(await where(page)) };
      }
    }
  } catch (e) {
    r.error = String(e?.message || e).slice(0, 240);
  }
  Object.assign(r, await clip.stop());
  r.ms = Date.now() - t0;
  r.log = trimLog(log);
  await page.close().catch(() => {});
  return r;
}

const browser = await launch(VIEW);
const results = [];
const list = CASES.filter((c) => !only || c.id.includes(only));
for (const [i, c] of list.entries()) {
  const r = await run(browser, c);
  results.push(r);
  const a = r.after || {};
  console.log(`${String(i + 1).padStart(2)}/${list.length} ${c.id.padEnd(36)} → ${(a.path ?? a.error ?? "(flow)").toString().slice(0, 40).padEnd(40)} ${r.back ? "back:" + (r.back.path ?? r.back.error) : ""} ${r.error ? "ERR " + r.error : ""}`);
  saveResults(BASE, results);
}
await browser.close();
console.log("done", results.length);
