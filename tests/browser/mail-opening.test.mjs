// Run: node tests/browser/mail-opening.test.mjs
// One headless browser, local piece overrides and a synthetic mailbox at the
// piece API boundary. No auth session or production mailbox is used.
import assert from "node:assert/strict";
import { readFile } from "node:fs/promises";
import { fileURLToPath } from "node:url";
import { ACSession } from "./ac-harness.mjs";

const root = new URL("../../", import.meta.url);
let source = await readFile(new URL("system/public/aesthetic.computer/disks/mail.mjs", root), "utf8");
const theme = await readFile(new URL("system/public/aesthetic.computer/disks/common/laklok-tema.mjs", root), "utf8");
const media = await readFile(new URL("system/public/aesthetic.computer/disks/common/mail-media.mjs", root), "utf8");
const letter = { id: "a".repeat(24), from: "@reader", subject: "Opening a letter",
  text: "This is a synthetic letter for the phone layout check. ".repeat(5) + "THE END",
  when: new Date().toISOString(), read: false, attachments: [], media: [] };
const mailbox = { addresses: ["tester@aesthetic.computer"], unread: 2,
  inbox: [letter, { ...letter, id: "b".repeat(24), from: "@other", subject: "Another letter", text: "This letter should stay unread. https://example.invalid/music" }], sent: [] };
// Keep production lifecycle functions intact behind a synthetic networking API.
for (const name of ["boot", "sim", "paint", "act"]) {
  source = source.replace(`function ${name}(api) {`, `function ${name}Original(api) {`);
}
source += `
const fixtureMailbox = ${JSON.stringify(mailbox)};
async function fixtureRequest(method, path, body) {
  if (path !== '/api/mail') return { status: 200 };
  if (method === 'GET') return { ...structuredClone(fixtureMailbox), status: 200 };
  if (body.action === 'read') {
    testChannel.postMessage({ readRequest: body });
    const row = fixtureMailbox.inbox.find(r => r.id === body.id);
    if (row && !row.read) { row.read = true; fixtureMailbox.unread--; }
    return { status: 200, read: row ? 1 : 0 };
  }
  return { status: 404, message: 'Recipient not found' };
}
function fixtureApi(api) {
  api.user = { sub: 'synthetic-mail-test' };
  api.net.userRequest = fixtureRequest;
  return api;
}
function boot(api) { return bootOriginal(fixtureApi(api)); }
function sim(api) { return simOriginal(fixtureApi(api)); }
function act(api) { return actOriginal(fixtureApi(api)); }
function paint(api) {
  paintOriginal(fixtureApi(api));
  testChannel?.postMessage({ layout: { width: api.screen.width, height: api.screen.height,
    expandedId, rows: rows.map(r => ({ id: r.letter.id, y0: r.y0, y1: r.y1, reply: r.reply })),
    links: mediaHits.filter(h => h.item.url).map(({ x, y, w, h }) => ({ x, y, w, h })) } });
}
`;
const ac = await ACSession.open();
const errors = [];
const glyphRequests = new Set();
let lastGlyphRequest = 0;
for (const event of ["requestfinished", "requestfailed"]) ac.page.on(event, (req) => glyphRequests.delete(req));
ac.page.on("console", (message) => { if (message.type() === "error" && errors.length < 10) errors.push(message.text()); });
try {
  await ac.page.setViewport({ width: 390, height: 780, isMobile: true, hasTouch: true, deviceScaleFactor: 1 });
  await ac.page.setBypassServiceWorker(true);
  await ac.page.evaluateOnNewDocument(() => {
    window.acDEBUG = false;
    window.mailOpenedLinks = [];
    window.mailReads = [];
    window.open = (url) => { window.mailOpenedLinks.push(url); return { closed: false }; };
    const channel = new BroadcastChannel("ac-mail-test");
    channel.onmessage = ({ data }) => {
      if (data.layout) window.mailLayout = data.layout;
      if (data.piece === "mail") window.mailState = data;
      if (data.readRequest) window.mailReads.push(data.readRequest);
    };
  });
  await ac.page.setRequestInterception(true);
  ac.page.on("request", async (req) => {
    const url = new URL(req.url());
    const json = (body, status = 200) => req.respond({ status, contentType: "application/json", body: JSON.stringify(body) });
    if (url.pathname.endsWith("/disks/mail.mjs")) return req.respond({ status: 200, contentType: "text/javascript", body: source });
    if (url.pathname.endsWith("/disks/common/laklok-tema.mjs")) return req.respond({ status: 200, contentType: "text/javascript", body: theme });
    if (url.pathname.endsWith("/disks/common/mail-media.mjs")) return req.respond({ status: 200, contentType: "text/javascript", body: media });
    const glyphPath = url.pathname.replace(/^\/aesthetic\.computer\//, "/");
    if (glyphPath.startsWith("/disks/drawings/font_1/") && glyphPath.endsWith(".json")) {
      glyphRequests.add(req);
      lastGlyphRequest = Date.now();
      const body = await readFile(`${fileURLToPath(root)}system/public/aesthetic.computer${decodeURIComponent(glyphPath)}`, "utf8");
      await req.respond({ status: 200, contentType: "application/json", body });
      glyphRequests.delete(req);
      return;
    }
    if (url.pathname === "/api/bdf-glyph" && req.method() === "GET") {
      glyphRequests.add(req);
      lastGlyphRequest = Date.now();
      return req.continue();
    }
    if (url.pathname.startsWith("/api/") || req.method() !== "GET") return json({});
    return req.continue();
  });
  await ac.boot("mail?test=1");
  await ac.page.waitForFunction(() => window.mailState?.status === "loaded" && window.mailLayout?.rows.length, { timeout: 30000 });
  const tap = async (x, y) => {
    const p = await ac.page.evaluate(({ x, y }) => {
      const r = document.querySelector("#aesthetic-computer canvas").getBoundingClientRect();
      return { x: r.x + x / window.mailLayout.width * r.width, y: r.y + y / window.mailLayout.height * r.height };
    }, { x, y });
    await ac.page.touchscreen.tap(p.x, p.y);
  };
  const shot = async (name) => {
    const deadline = Date.now() + 15000;
    while (glyphRequests.size || Date.now() - lastGlyphRequest < 500) {
      assert.ok(Date.now() < deadline, "font requests should settle before the screenshot");
      await ac.wait(100);
    }
    await ac.wait(250);
    return ac.shot(name);
  };
  await shot("mail-opening/01-preview");
  let layout = await ac.page.evaluate(() => window.mailLayout);
  assert.ok(layout.width < 320 || layout.height < 220, "exercise the compact layout");
  await tap(layout.links[0].x + 4, layout.links[0].y + 4);
  await ac.page.waitForFunction(() => window.mailOpenedLinks.length === 1);
  assert.deepEqual(await ac.page.evaluate(() => window.mailOpenedLinks), ["https://example.invalid/music"]);
  assert.equal(await ac.page.evaluate(() => window.mailReads.length), 0, "link tap does not also select the row");
  await tap(25, layout.rows[0].y0 + 20);
  await ac.page.waitForFunction(() => window.mailLayout?.expandedId && window.mailState?.unread === 1);
  assert.deepEqual(await ac.page.evaluate(() => window.mailReads), [{ action: "read", id: letter.id }]);
  assert.equal(await ac.page.evaluate(() => window.mailState.inbox[1].read), false);
  assert.equal(await ac.page.evaluate(() => window.mailState.view), "inbox");
  await shot("mail-opening/02-open");
  layout = await ac.page.evaluate(() => window.mailLayout);
  await tap(layout.rows[0].reply.x + 5, layout.rows[0].reply.y + 5);
  await ac.page.waitForFunction(() => window.mailState?.view === "compose");
  assert.equal(await ac.page.evaluate(() => window.mailState.composeTo), "@reader");
  await shot("mail-opening/03-reply");
  await ac.page.evaluate(() => {
    const c = new BroadcastChannel("ac-mail-test");
    c.postMessage({ type: "compose", to: "@q13", text: "This draft should stay here." });
    c.close();
  });
  await ac.page.waitForFunction(() => window.mailState?.composeNote?.includes("full @handle"));
  await shot("mail-opening/04-recipient-error");
  console.log("mail opening browser test passed: links, full letters, individual read, reply and recipient feedback");
} catch (error) {
  await ac.shot("mail-opening/failure");
  console.error(JSON.stringify({ errors, pendingFonts: [...glyphRequests].map((req) => req.url()), state: await ac.page.evaluate(() => ({ status: window.mailState?.status, layout: window.mailLayout })) }));
  throw error;
} finally { await ac.close(); }
