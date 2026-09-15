// Real loopback SMTP; no credentials, production mailbox, or external delivery.
import assert from "node:assert/strict";
import nodemailer from "nodemailer";
import { BlockList } from "node:net";
import { mailEvent, mailTrace, recordMailEvent, flushMailEvents, observeMail } from "../system/backend/mail-events.mjs";
import { options, queryFor } from "../system/backend/mail-events-cli.mjs";
import { createInbound, makeLimiter } from "../lith/mail-inbound.mjs";

const canary = "PRIVATE_MAIL_CANARY@example.invalid";
const trace = mailTrace();
const sanitized = mailEvent({ event: "stored", trace, transport: "internal", letterId: "a".repeat(24), subject: canary, text: canary, from: canary, filename: canary, error: canary, reason: canary, status: canary, attachments: 2 });
assert.ok(!JSON.stringify(sanitized).includes(canary));
assert.equal(sanitized.error, "UNKNOWN");
assert.equal(mailEvent({ event: canary }), null);
assert.equal(mailEvent({ event: "failed", trace: canary }).trace, undefined);
const logs = [], saved = [], indexes = [];
const db = { db: { collection(name) {
  assert.equal(name, "mail-events");
  return { createIndex: async (...args) => indexes.push(args), insertOne: async (row) => saved.push(row) };
} } };
for (let i = 0; i < 3; i++) recordMailEvent(db, { event: "stored", trace, subject: canary }, (line) => logs.push(line));
await flushMailEvents();
assert.equal(saved.length, 3);
assert.equal(indexes.length, 3, "concurrent events share index setup");
assert.equal(indexes[0][1].expireAfterSeconds, 2592000);
assert.ok(!JSON.stringify([logs, saved]).includes(canary));
recordMailEvent({ db: { collection() { throw new Error(canary); } } }, { event: "accepted", trace }, (line) => logs.push(line));
await flushMailEvents();
assert.equal(JSON.parse(logs.at(-1)).event, "telemetry_unavailable");
assert.ok(!JSON.stringify(logs).includes(canary));
// Telemetry outages don't delay a successful operation, even with hung storage.
let unblock;
const stuck = new Promise((resolve) => { unblock = resolve; });
const hangingDB = { db: { collection: () => ({ createIndex: () => stuck, insertOne: async () => {} }) } };
const oldLog = console.log; console.log = (line) => logs.push(line);
try {
  assert.equal(await observeMail("internal", { trace }, hangingDB, async (_opts, event) => { event("stored"); return "delivered"; }), "delivered");
} finally { console.log = oldLog; unblock(); await flushMailEvents(); }

const limit = makeLimiter({ perPairPerHour: 1, perMinute: 100 });
assert.equal(limit.take("full").ok, true);
assert.equal(limit.takeMany(["fresh", "full"]).ok, false);
assert.equal(limit.take("fresh").ok, true, "failed batch must not consume fresh quota");
const globalLimit = makeLimiter({ perPairPerHour: 100, perMinute: 1 });
assert.equal(globalLimit.takeMany(["one", "two"]).code, 451);
assert.equal(globalLimit.take("one").ok, true);

const events = [], deliveries = [];
const limiter = makeLimiter({ perPairPerHour: 2, perMinute: 100 });
const server = createInbound({
  open: true, domains: ["example.invalid"], limiter,
  record: (fields) => events.push(mailEvent(fields)),
  lookup: async (local) => ["a", "b", "full", "fresh", "broken"].includes(local) ? local : undefined,
  file: async (letter) => {
    if (letter.sub === "broken") throw new Error(canary);
    deliveries.push({ to: letter.sub, trace: letter.trace }); return {};
  },
});
await new Promise((resolve) => server.listen(0, "127.0.0.1", resolve));
const transport = nodemailer.createTransport({ host: "127.0.0.1", port: server.server.address().port, secure: false, ignoreTLS: true, pool: true, maxConnections: 1, maxMessages: 100 });
const send = (to, extra = {}) => transport.sendMail({ from: canary, to, subject: canary, text: canary, ...extra });
try {
  await send("a@example.invalid");
  await send("b@example.invalid");
  assert.deepEqual(deliveries.map((d) => d.to), ["a", "b"], "reused SMTP socket must not keep previous recipients");
  assert.equal(events.filter((e) => e.event === "connected").length, 1, "test actually reused a connection");
  assert.notEqual(deliveries[0].trace, deliveries[1].trace, "trace is per transaction");
  await send("full@example.invalid"); await send("full@example.invalid");
  const before = deliveries.length;
  await assert.rejects(send(["fresh@example.invalid", "full@example.invalid"]), (err) => err.responseCode === 550);
  assert.equal(deliveries.length, before, "mixed rate-limited delivery must not acknowledge/store partial batch");
  await send("fresh@example.invalid");
  await assert.rejects(send("unknown@example.invalid"), (err) => err.responseCode === 550);
  for (let i = 0; i < 3; i++) await assert.rejects(send("broken@example.invalid"), (err) => err.responseCode === 451, "temporary failures release quota for retry");
  await assert.rejects(send("a@example.invalid", { headers: { "Authentication-Results": "mx; dmarc=fail" } }), (err) => err.responseCode === 550);
  await assert.rejects(send("a@example.invalid", { attachments: Array.from({ length: 11 }, (_, i) => ({ filename: `${i}-${canary}`, content: "x" })) }), (err) => err.responseCode === 552);
  // Over advertised MIME cap without trusting a client's SIZE declaration.
  await assert.rejects(send("a@example.invalid", { text: "x".repeat(13 * 1024 * 1024) }), (err) => err.responseCode === 552);
  for (const reason of ["rate_pair", "recipient", "storage", "dmarc", "attachments", "wire_size"]) assert.ok(events.some((e) => e.reason === reason), reason);
  assert.ok(events.some((e) => e.event === "smtp_response" && e.status === 552));
  assert.ok(events.some((e) => e.event === "accepted" && e.status === 250));
  assert.ok(!JSON.stringify(events).includes(canary), "no SMTP payload in telemetry");
} finally {
  transport.close();
  await new Promise((resolve) => server.close(resolve));
}
// A newly refreshed relay list is consulted on every connection.
let relayList = new BlockList();
const relayEvents = [];
const relayServer = createInbound({ domains: [], lookup: async () => {}, file: async () => {}, getRelays: () => relayList, record: (fields) => relayEvents.push(fields) });
let rejected;
relayServer.options.onConnect({ remoteAddress: "127.0.0.1" }, (err) => { rejected = err; });
assert.equal(rejected.responseCode, 554);
relayList = new BlockList(); relayList.addAddress("127.0.0.1");
relayServer.options.onConnect({ remoteAddress: "127.0.0.1" }, (err) => { rejected = err; });
assert.equal(rejected, undefined);

assert.throws(() => options(["--limit", "99999"]));
assert.throws(() => options(["--trace", canary]));
const opts = options(["--failures", "--since", "60", "--trace", trace, "--json"]);
const query = queryFor(opts, 3600000);
assert.equal(query.trace, trace);
assert.equal(query.when.$gte.getTime(), 0);
assert.ok(query.$or.length);
console.log("mail event spec passed: privacy, retention, outage isolation, atomic throttles, SMTP reuse/rejections, relay refresh and inspection filters");
