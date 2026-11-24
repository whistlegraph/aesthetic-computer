// Run: node spec/mail-interaction-spec.mjs
// Exercise the production piece with synthetic letters and pointer events:
// the box is one row per person; a person opens your whole conversation.
import assert from "node:assert/strict";
import { mailLinks, MailMedia } from "../system/public/aesthetic.computer/disks/common/mail-media.mjs";

let sequence = 0;
const minutesAgo = (n) => new Date(Date.now() - n * 60000).toISOString();

async function fixture({ width = 240, readStatus = 200, query = {}, params = [] } = {}) {
  const piece = await import(`../system/public/aesthetic.computer/disks/mail.mjs?test=${++sequence}`);
  const letter = { id: "a".repeat(24), from: "@reader", subject: "A long letter", text: "First line. ".repeat(30) + "THE END", read: false, when: minutesAgo(30) };
  const second = { ...letter, id: "b".repeat(24), subject: null, text: "Second letter", when: minutesAgo(5) };
  const other = { ...letter, id: "c".repeat(24), from: "@other", subject: null, text: "hi from other", read: true, when: minutesAgo(60) };
  const mine = { id: "d".repeat(24), to: "@reader", subject: "Re: A long letter", text: "my answer", when: minutesAgo(20) };
  const mailbox = { status: 200, addresses: ["tester@aesthetic.computer"], unread: 2,
    inbox: [second, letter, other], sent: [mine] };
  // The server's view of one conversation, oldest first.
  const thread = (address) => {
    const key = address.toLowerCase();
    const theirs = mailbox.inbox.filter((l) => l.from.toLowerCase() === key).map((l) => ({ ...l, mine: false }));
    const ours = mailbox.sent.filter((l) => l.to.toLowerCase() === key).map((l) => ({ ...l, mine: true, read: true }));
    return [...theirs, ...ours].sort((a, b) => new Date(a.when) - new Date(b.when));
  };
  const writes = [], requests = [], labels = [], jumps = [];
  let fields;
  const noop = () => {};
  const painter = { box: noop, write: (text, pos) => { writes.push({ text, ...pos }); } };
  const api = {
    user: {}, query, colon: [], params, screen: { width, height: 320 },
    hud: { label: (text) => labels.push(text) }, gizmo: { EllipsisTicker: class { text() { return ""; } } },
    help: {}, store: { retrieve: async () => null },
    ui: { TextFields: class {
      constructor() { fields = this; this.values = {}; this.input = {}; }
      focus() {} paint() {} act() {} sync() {} reset() { this.values = {}; }
    } },
    net: { userRequest: async (method, path, body) => {
      requests.push({ method, path, body });
      if (method === "GET" && path.startsWith("/api/mail?with=")) {
        return { status: 200, with: decodeURIComponent(path.split("=")[1]), letters: thread(decodeURIComponent(path.split("=")[1])) };
      }
      if (method === "GET") return mailbox;
      if (body?.action === "read") {
        if (readStatus !== 200) return { status: readStatus };
        let read = 0;
        for (const l of mailbox.inbox) if (l.from.toLowerCase() === body.with.toLowerCase() && !l.read) { l.read = true; read++; }
        return { status: 200, read };
      }
      return { status: 200 };
    } },
    jump: (to) => jumps.push(to),
    needsPaint: noop, send: noop, wipe: noop, mask: noop, unmask: noop,
    ink: () => painter, line: noop,
    text: { box: (text, _pos, width) => ({ box: { height: Math.ceil(text.length / Math.max(1, Math.floor(width / 4))) * 9 } }) },
  };
  await piece.boot(api);
  const settle = () => new Promise(setImmediate);
  const paint = () => { writes.length = 0; piece.paint(api); };
  const event = (type, x, y, delta = { y: 0 }) => piece.act({ ...api, event: { x, y, delta, is: (value) => value === type } });
  const tap = async (x, y) => { event("touch", x, y); event("lift", x, y); await settle(); await settle(); paint(); };
  const find = (prefix) => writes.find((w) => w.text.startsWith(prefix));
  paint();
  return { piece, api, letter, second, mailbox, writes, requests, labels, jumps, paint, event, tap, find, fields: () => fields };
}

// 👥 The box: one row per person, newest first, unread counted per person.
const f = await fixture();
const people = f.writes.filter((w) => w.text.startsWith("@")).map((w) => w.text);
assert.deepEqual(people, ["@reader  2", "@other"], "one row per person, newest first, with their unread count");
assert.ok(f.find("Second letter"), "a row previews that person's newest letter");
assert.ok(f.writes.some((w) => w.text === "mark read"), "unread mail offers mark read");
assert.equal(f.requests.filter((r) => r.body?.action === "read").length, 0, "nothing is marked read just by looking at the box");

// 💬 Tapping a person opens the whole conversation and reads it.
const row = f.find("@reader");
await f.tap(row.x + 2, row.y + 2);
assert.ok(f.requests.some((r) => r.method === "GET" && r.path === "/api/mail?with=%40reader"), "the conversation is fetched in full");
assert.ok(f.writes.some((w) => w.text.endsWith("THE END")), "letters in a conversation show whole");
const order = ["A long letter", "my answer", "Second letter"].map((t) => f.writes.findIndex((w) => w.text.includes(t)));
assert.ok(order.every((n) => n >= 0) && order[0] < order[1] && order[1] < order[2], "oldest at the top, both directions in one column");
assert.deepEqual(f.requests.filter((r) => r.body?.action === "read").map((r) => r.body), [{ action: "read", with: "@reader" }]);
assert.equal(f.mailbox.unread, 0, "opening a conversation clears its unread letters");
assert.equal(f.labels.at(-1), "mail @reader", "the corner label names who you are talking to");
assert.ok(!f.writes.some((w) => w.text === "send"), "opening a conversation does not start composing");
assert.ok(f.writes.some((w) => w.text === "write to @reader…"), "the bottom bar writes to them");

// The bottom bar addresses compose and carries the conversation's subject.
const bar = f.find("write to @reader");
await f.tap(bar.x + 1, bar.y + 1);
assert.equal(f.fields().values.to, "@reader");
assert.ok(f.writes.some((w) => w.text === "send"));
f.event("keyboard:down:escape", 0, 0);
f.paint();

// Back returns to the box; the label follows.
const back = f.writes.find((w) => w.text === "back");
assert.ok(back, "a conversation has a way back");
await f.tap(back.x + 1, back.y + 1);
assert.equal(f.labels.at(-1), "mail");
assert.ok(f.find("@other"), "back on the box");
f.piece.leave();

// Scrolling the box never opens a conversation.
const drag = await fixture();
const dragRow = drag.find("@reader");
drag.event("touch", 20, dragRow.y);
drag.event("draw", 20, dragRow.y - 1, { y: -1 });
drag.event("lift", 20, dragRow.y - 1);
await new Promise(setImmediate);
drag.paint();
assert.ok(!drag.requests.some((r) => r.path.startsWith("/api/mail?with=")), "scrolling does not open a conversation");
assert.equal(drag.requests.filter((r) => r.body?.action === "read").length, 0);
drag.piece.leave();

// A failed read acknowledgement still shows the letters and keeps the badge.
const failed = await fixture({ readStatus: 500 });
const failedRow = failed.find("@reader");
await failed.tap(20, failedRow.y + 1);
assert.ok(failed.writes.some((w) => w.text.endsWith("THE END")), "read acknowledgement failure does not hide the letters");
assert.equal(failed.mailbox.unread, 2, "failed acknowledgement leaves the badge alone");
failed.piece.leave();

// `mail~@someone` with no history goes straight to compose, addressed.
const fresh = await fixture({ params: ["@newfriend"] });
assert.equal(fresh.fields().values.to, "@newfriend");
assert.ok(fresh.writes.some((w) => w.text === "send"), "a first letter starts in compose");
fresh.piece.leave();

// `mail~@reader` with history opens the conversation instead.
const known = await fixture({ params: ["@reader"] });
assert.ok(known.writes.some((w) => w.text === "write to @reader…"));
assert.ok(!known.writes.some((w) => w.text === "send"));
known.piece.leave();

// A notification opens the conversation its letter came from.
const notified = await fixture({ query: { letter: "b".repeat(24) } });
assert.equal(notified.labels.at(-1), "mail @reader");
assert.ok(notified.writes.some((w) => w.text === "Second letter"));
notified.piece.leave();

// Links inside a conversation keep their explicit opening control.
assert.deepEqual(mailLinks("Listen (https://example.invalid/song). https://example.invalid/song javascript:alert(1)"), ["https://example.invalid/song"]);
assert.deepEqual(mailLinks("https://example.invalid/a_(b) https://user:password@example.invalid/private"), ["https://example.invalid/a_(b)"]);
const links = await fixture();
links.second.text = "Listen: https://example.invalid/music";
const opened = [];
links.api.net.web = (...args) => opened.push(args);
const linkRow = links.find("@reader");
await links.tap(20, linkRow.y + 1);
const link = links.writes.find((w) => w.text === "open https://example.invalid/music");
assert.ok(link, "ordinary URLs have an explicit opening control");
await links.tap(link.x + 2, link.y + 2);
assert.deepEqual(opened, [["https://example.invalid/music", true]]);
assert.ok(!links.writes.some((w) => w.text === "send"), "a link tap does not compose a reply");
links.piece.leave();

const media = new MailMedia();
const mediaItems = media.layout(links.api, { text: "https://aesthetic.computer/#abc", media: [{ label: "#abc", url: "https://aesthetic.computer/#abc", path: "painting#abc" }] }, 200, {});
assert.equal(mediaItems.length, 1, "existing AC media cards do not get duplicate link controls");
console.log("mail interaction spec passed: people, conversations, reading, compose, back, scrolling, failures, links");
