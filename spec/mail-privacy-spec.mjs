// Run: node --experimental-vm-modules spec/mail-privacy-spec.mjs
// Execute production modules with fake storage/transports; no credentials/network.
import assert from "node:assert/strict";
import { readFile } from "node:fs/promises";
import vm from "node:vm";
import * as privacy from "../shared/mail-privacy.mjs";

const marker = "PRIVATE_LETTER_CANARY";
const failure = Object.assign(new Error(marker), { code: marker, response: marker });
const logs = [];
const notes = [];
const stored = [];
const log = (...args) => logs.push(args);
const context = vm.createContext({ console: { log, error: log }, URL, process: { argv: [], env: {} } });
async function load(path, mocks) {
  const module = new vm.SourceTextModule(await readFile(new URL(path, import.meta.url), "utf8"), {
    context,
    importModuleDynamically: async (name) => synthetic(name, mocks[name]),
  });
  async function synthetic(name, exports) {
    if (!exports) throw new Error(`Missing mock: ${name}`);
    const m = new vm.SyntheticModule(Object.keys(exports), function () {
      for (const [key, value] of Object.entries(exports)) this.setExport(key, value);
    }, { context });
    await m.link(() => {});
    await m.evaluate();
    return m;
  }
  await module.link((name) => synthetic(name, mocks[name]));
  await module.evaluate();
  return module.namespace;
}
const cleanJSON = (value) => JSON.parse(JSON.stringify(value));
const privateFree = (value) => assert.ok(!JSON.stringify(value).includes(marker));
const collection = {
  createIndex: async () => {}, dropIndex: async () => {},
  insertOne: async (row) => { stored.push(row); return { insertedId: "opaque-id" }; },
  findOne: async () => ({ code: "ac25abcde" }),
};
const database = { db: { collection: () => collection }, disconnect: async () => {} };
let pushThrows = false;
let smtpCalls = 0;
const backend = await load("../system/backend/mail.mjs", {
  "./authorization.mjs": {
    handleFor: async () => marker,
    userIDFromHandleOrEmail: async () => "recipient",
  },
  "./filter.mjs": { filter: (s) => s },
  "./shell.mjs": { shell: { log } },
  "../../shared/mail-privacy.mjs": privacy,
  "../../shared/push.mjs": { sendToUser: async (_db, _to, note, options, diagnostic) => {
    notes.push({ note, options });
    diagnostic(marker, failure);
    if (pushThrows) throw failure;
    return { attempted: 1, succeeded: 0, failed: 1, pruned: 0 };
  } },
  nodemailer: { default: { createTransport: () => ({ sendMail: async () => {
    if (++smtpCalls === 1) throw failure;
    return { messageId: "smtp-id" };
  } }) } },
});

await backend.deliver({ from: "sender", to: "recipient", text: marker, subject: marker, device: "target-device" }, database);
await backend.deliverFromOutside({ to: "recipient", fromEmail: `${marker}@example.invalid`, fromName: marker, text: marker, subject: marker }, database);
assert.equal(notes.length, 2);
for (const { note } of notes) assert.deepEqual(cleanJSON(note), privacy.letterNotification("opaque-id"));
assert.equal(notes[0].options.device, "target-device");
assert.ok(stored.every((row) => row.text === marker && row.subject === marker));
await backend.deliverFromOutside({ to: "recipient", fromEmail: marker, text: marker, quiet: true }, database);
assert.equal(notes.length, 2, "throttled inbound delivery stays quiet");
pushThrows = true;
await backend.deliver({ from: "sender", to: "recipient", text: marker }, database);
await backend.deliverFromOutside({ to: "recipient", fromEmail: marker, text: marker }, database);
await backend.sendOutside({ from: "sender", toEmail: "test@example.invalid", text: marker, subject: marker }, database);
assert.equal(smtpCalls, 2, "SMTP fallback remains functional");
privateFree(logs);
privateFree(notes);
assert.equal(privacy.mailErrorCode({ code: "ETIMEDOUT", message: marker }), "ETIMEDOUT");
assert.equal(privacy.mailErrorCode(failure), "UNKNOWN");

// Both APIs sanitize operation, connect, and cleanup errors. A successful /tell
// still exercises the same generic notification path as /mail.
for (const api of ["mail", "tell"]) {
  for (const stage of ["operation", "connect", "disconnect", "success"]) {
    const handler = await load(`../system/netlify/functions/${api}.mjs`, {
      "../../backend/authorization.mjs": { authorize: async () => ({ sub: "sender" }) },
      "../../backend/database.mjs": { connect: async () => {
        if (stage === "connect") throw failure;
        return { ...database, disconnect: async () => { if (stage === "disconnect") throw failure; } };
      } },
      "../../backend/http.mjs": { respond: (status, body) => ({ status, body }) },
      "../../backend/mail.mjs": {
        ...backend,
        deliver: async (...args) => {
          if (stage === "operation") throw failure;
          return backend.deliver(...args);
        },
      },
      "../../../shared/mail-privacy.mjs": privacy,
      mongodb: { ObjectId: class {} },
    });
    const res = await handler.handler({ httpMethod: "POST", headers: {}, body: JSON.stringify({ to: "@test", text: marker }) });
    assert.equal(res.status, stage === "success" ? 200 : 500);
    if (stage !== "success") privateFree(res);
  }
}
privateFree(logs);

// Exercise inbound callbacks without opening a socket or parsing real mail.
let parsed = { from: { value: [{ address: `${marker}@example.invalid` }] }, headers: new Map() };
const inbound = await load("../lith/mail-inbound.mjs", {
  "smtp-server": { SMTPServer: class { constructor(options) { this.options = options; } } },
  mailparser: { simpleParser: async () => parsed },
  "node:net": { BlockList: class {} },
  "node:dns/promises": { resolveTxt: async () => [] },
  "node:fs": { existsSync: () => false, readFileSync: () => "" },
  "../shared/mail-privacy.mjs": privacy,
});
const session = { remoteAddress: "127.0.0.1", amail: new Map([["recipient", { sub: "recipient", local: marker }]]) };
for (const stage of ["filed", "failure", "stamp", "lookup", "relay", "rate"]) {
  const server = inbound.createInbound({
    domains: ["example.invalid"], log,
    secret: stage === "stamp" ? "stamp" : null,
    lookup: async () => { throw failure; },
    file: async () => { if (stage === "failure") throw failure; return { toHandle: marker }; },
    limiter: { take: () => ({ ok: stage !== "rate", why: "Too many letters", code: 451 }) },
  });
  let result;
  const cb = (error) => { result = error; };
  if (stage === "lookup") await server.options.onRcptTo({ address: `${marker}@example.invalid` }, session, cb);
  else if (stage === "relay") server.options.onConnect(session, cb);
  else await server.options.onData({}, session, cb);
  assert.equal(!!result, stage !== "filed");
  if (result) assert.ok(!result.message.includes(marker));
}
privateFree(logs);

// Run the actual runtime request method without booting the graphics worker.
const disk = await readFile(new URL("../system/public/aesthetic.computer/lib/disk.mjs", import.meta.url), "utf8");
const start = disk.indexOf("    userRequest: async (method, endpoint, body) => {");
const end = disk.indexOf("    // Loosely connect the UDP receiver.", start);
assert.ok(start > 0 && end > start);
const requests = vm.runInNewContext(`({${disk.slice(start, end)}})`, {
  $commonApi: { authorize: async () => "fake-token" },
  fetch: async () => { throw failure; }, console: { error: log },
});
for (const path of ["/api/mail", "/api/mail?count", "/api/tell", "/api/mail-status"]) {
  await requests.userRequest("POST", path, { text: marker });
}
privateFree(logs);
const previous = logs.length;
await requests.userRequest("POST", "/api/unrelated", {});
assert.equal(logs.length, previous + 1);
assert.equal(logs.at(-1)[1], failure, "unrelated diagnostics unchanged");

// A failed send preserves the draft and presents only a fixed local message.
const piece = await readFile(new URL("../system/public/aesthetic.computer/disks/mail.mjs", import.meta.url), "utf8");
const sendStart = piece.indexOf("async function send(api,");
const sendEnd = piece.indexOf("// Put the fields away", sendStart);
assert.ok(sendStart > 0 && sendEnd > sendStart);
const compose = vm.runInNewContext(`let composeNote; ${piece.slice(sendStart, sendEnd)}; ({send, note: () => composeNote})`, {
  S: () => ({ couldntSend: "Could not send letter" }),
});
const draft = { to: "@test", subject: marker, body: marker };
await compose.send({ net: { userRequest: async () => { throw failure; } } }, draft);
assert.equal(compose.note(), "Could not send letter");
assert.equal(draft.body, marker);
console.log("mail privacy spec passed: pushes, storage, SMTP, APIs, inbound and client diagnostics");
