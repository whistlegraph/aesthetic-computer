// Run: node spec/mail-interaction-spec.mjs
// Exercise the production piece with synthetic letters and pointer events.
import assert from "node:assert/strict";
import { mailLinks, MailMedia } from "../system/public/aesthetic.computer/disks/common/mail-media.mjs";

let sequence = 0;
async function fixture({ width = 240, readStatus = 200 } = {}) {
  const piece = await import(`../system/public/aesthetic.computer/disks/mail.mjs?test=${++sequence}`);
  const letter = { id: "a".repeat(24), from: "@reader", subject: "A long letter", text: "First line. ".repeat(30) + "THE END", read: false, when: new Date().toISOString() };
  const mailbox = { status: 200, addresses: ["tester@aesthetic.computer"], unread: 2,
    inbox: [letter, { ...letter, id: "b".repeat(24), text: "Second letter" }],
    sent: [{ ...letter, to: "@reader" }] };
  const writes = [], requests = [];
  let fields;
  const noop = () => {};
  const painter = { box: noop, write: (text, pos) => { writes.push({ text, ...pos }); } };
  const api = {
    user: {}, query: {}, colon: [], params: [], screen: { width, height: 320 },
    hud: { label: noop }, gizmo: { EllipsisTicker: class {} },
    store: { retrieve: async () => null },
    ui: { TextFields: class {
      constructor() { fields = this; this.values = {}; this.input = {}; }
      focus() {} paint() {} act() {} sync() {}
    } },
    net: { userRequest: async (method, path, body) => {
      requests.push({ method, path, body });
      if (method === "GET") return mailbox;
      return { status: readStatus };
    } },
    needsPaint: noop, send: noop, wipe: noop, mask: noop, unmask: noop,
    ink: () => painter, line: noop,
    text: { box: (text, _pos, width) => ({ box: { height: Math.ceil(text.length / Math.max(1, Math.floor(width / 4))) * 9 } }) },
  };
  await piece.boot(api);
  const paint = () => { writes.length = 0; piece.paint(api); };
  const event = (type, x, y, delta = { y: 0 }) => piece.act({ ...api, event: { x, y, delta, is: (value) => value === type } });
  const tap = async (x, y) => { event("touch", x, y); event("lift", x, y); await new Promise(setImmediate); paint(); };
  paint();
  return { piece, api, letter, mailbox, writes, requests, paint, event, tap, fields };
}

const f = await fixture();
const body = f.writes.find((w) => w.text.startsWith("First line"));
assert.ok(body.text.endsWith("…"), "phone inbox starts with a preview");
await f.tap(body.x + 5, body.y + 2);
assert.ok(f.writes.some((w) => w.text.endsWith("THE END")), "tapping the preview reveals the full letter");
assert.ok(!f.writes.some((w) => w.text === "send"), "opening a letter does not compose a reply");
assert.deepEqual(f.requests.filter((r) => r.body?.action === "read").map((r) => r.body), [{ action: "read", id: f.letter.id }]);
assert.equal(f.mailbox.unread, 1, "only the opened letter clears its unread badge");
assert.equal(f.mailbox.inbox[1].read, false);

// Opening and closing the same letter does not send another read request.
await f.tap(body.x + 5, body.y + 2);
assert.ok(f.writes.some((w) => w.text.startsWith("First line") && w.text.endsWith("…")));
assert.equal(f.requests.filter((r) => r.body?.action === "read").length, 1);

// The explicit reply control still addresses compose and carries the subject.
const reply = f.writes.find((w) => w.text === "reply");
await f.tap(reply.x + 1, reply.y + 1);
assert.equal(f.fields.values.to, "@reader");
assert.equal(f.fields.values.subject, "Re: A long letter");
assert.ok(f.writes.some((w) => w.text === "send"));
f.piece.leave();

const drag = await fixture();
const preview = drag.writes.find((w) => w.text.startsWith("First line"));
drag.event("touch", 20, preview.y);
drag.event("draw", 20, preview.y - 1, { y: -1 });
drag.event("lift", 20, preview.y - 1);
drag.paint();
assert.ok(!drag.writes.some((w) => w.text.endsWith("THE END")), "scrolling does not open a letter");
assert.equal(drag.requests.filter((r) => r.body?.action === "read").length, 0);
drag.piece.leave();

const failed = await fixture({ readStatus: 500 });
const failedBody = failed.writes.find((w) => w.text.startsWith("First line"));
await failed.tap(20, failedBody.y + 1);
assert.ok(failed.writes.some((w) => w.text.endsWith("THE END")), "read acknowledgement failure does not hide the letter");
assert.equal(failed.mailbox.unread, 2, "failed acknowledgement leaves the badge alone");
failed.piece.leave();

const sent = await fixture();
const sentTab = sent.writes.find((w) => w.text === "sent");
await sent.tap(sentTab.x + 1, sentTab.y + 1);
const sentBody = sent.writes.find((w) => w.text.startsWith("First line"));
await sent.tap(20, sentBody.y + 1);
assert.ok(sent.writes.some((w) => w.text.endsWith("THE END")), "sent letters also expand");
assert.equal(sent.requests.filter((r) => r.body?.action === "read").length, 0, "opening sent mail never marks inbox mail read");
sent.piece.leave();

assert.deepEqual(mailLinks("Listen (https://example.invalid/song). https://example.invalid/song javascript:alert(1)"), ["https://example.invalid/song"]);
assert.deepEqual(mailLinks("https://example.invalid/a_(b) https://user:password@example.invalid/private"), ["https://example.invalid/a_(b)"]);
const links = await fixture();
links.letter.text = "Listen: https://example.invalid/music";
const opened = [];
links.api.net.web = (...args) => opened.push(args);
links.paint();
const link = links.writes.find((w) => w.text === "open https://example.invalid/music");
assert.ok(link, "ordinary URLs have an explicit opening control");
await links.tap(link.x + 2, link.y + 2);
assert.deepEqual(opened, [["https://example.invalid/music", true]]);
assert.equal(links.requests.filter((r) => r.body?.action === "read").length, 0, "a link tap does not also open its row");
assert.ok(!links.writes.some((w) => w.text === "send"), "a link tap does not compose a reply");
links.piece.leave();

const media = new MailMedia();
const mediaItems = media.layout(links.api, { text: "https://aesthetic.computer/#abc", media: [{ label: "#abc", url: "https://aesthetic.computer/#abc", path: "painting#abc" }] }, 200, {});
assert.equal(mediaItems.length, 1, "existing AC media cards do not get duplicate link controls");
console.log("mail interaction spec passed: opening, reading, replies, scrolling, failures and sent mail");
