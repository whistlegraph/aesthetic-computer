import test from "node:test";
import assert from "node:assert/strict";
import { randomUUID } from "node:crypto";
import { Query, Aggregator } from "mingo";
import { createNetworkRecognitionHandler } from "../backend/network-recognition-handler.mjs";
import { handler as bridgePage } from "../netlify/functions/network-recognition-bridge.mjs";
import { RECOGNITION_ORIGIN, recognitionPage, validateRecognition } from "../public/aesthetic.computer/lib/network-recognition-model.mjs";
import { cachedAcToken, silentAcToken, startRecognitionBridge } from "../public/aesthetic.computer/lib/network-recognition-bridge.mjs";
import { startNetworkRecognition } from "../public/aesthetic.computer/lib/network-recognition.mjs";
import { startVisitTracker } from "../public/aesthetic.computer/lib/visit-tracker.mjs";
import { networkRecognitionReport, recognitionReportOptions } from "../../toolchain/analytics/network-recognition.mjs";

const settle = () => new Promise(resolve => setImmediate(resolve));
const event = (id, phase = "started", headers = {}) => ({
  httpMethod: "POST", headers: { origin: phase === "started" ? "https://sotce.net" : RECOGNITION_ORIGIN, ...headers },
  body: JSON.stringify({ version: 1, id, phase }),
});
function backendFixture() {
  const records = new Map(), indexes = [], authCalls = [];
  let clock = new Date("2026-10-14T22:00:00Z"), subject = null;
  const collection = {
    createIndex: async (key, options) => indexes.push({ key, options }),
    findOne: async query => [...records.values()].find(row => new Query(query).test(row)),
    async updateOne(query, update, options) {
      let row = await this.findOne(query);
      if (!row && options?.upsert) { row = { _id: query._id, ...update.$setOnInsert }; records.set(row._id, row); }
      if (row && update.$max) for (const [key, value] of Object.entries(update.$max)) row[key] ||= value;
    },
  };
  const handler = createNetworkRecognitionHandler({
    now: () => new Date(clock),
    authorize: async (headers, tenant) => { authCalls.push({ headers, tenant }); return subject; },
    connect: async () => ({ db: { collection: name => { assert.equal(name, "network-recognition"); return collection; } } }),
  });
  return { handler, records, indexes, authCalls, user: value => { subject = value; }, advance: ms => { clock = new Date(+clock + ms); } };
}

test("the server stores only a verified boolean, with idempotent starts and bounded upgrades", async () => {
  const f = backendFixture(), id = randomUUID();
  assert.equal((await f.handler(event(id))).statusCode, 204);
  await f.handler(event(id));
  assert.equal(f.records.size, 1);
  let row = [...f.records.values()][0];
  assert.equal(row.acRecognized, false);
  assert.equal(row.bridgeCompleted, false);
  assert.equal(+row.expiresAt - +row.startedAt, 35 * 86400000);
  f.user({ sub: "secret-account", email: "secret@example.com" });
  const checked = event(id, "checked", { authorization: "Bearer secret-token" });
  assert.equal((await f.handler(checked)).statusCode, 204);
  assert.equal(f.authCalls[0].tenant, "aesthetic");
  assert.equal(row.acRecognized, true);
  assert.equal(row.bridgeCompleted, true);
  await f.handler(event(id));
  f.user(null); await f.handler(checked);
  assert.equal(row.acRecognized, true, "late retries cannot downgrade a verified result");
  assert.deepEqual(Object.keys(row).sort(), ["_id", "acRecognized", "bridgeCompleted", "expiresAt", "property", "startedAt"]);
  assert.doesNotMatch(JSON.stringify([...f.records.values()]), /secret|Bearer/);
  assert.ok(f.indexes.some(i => i.options?.expireAfterSeconds === 0));
  await f.handler(event(randomUUID(), "checked", { authorization: "Bearer secret-token" }));
  assert.equal(f.records.size, 1, "a checked request cannot fabricate a page arrival");
  const old = randomUUID(); await f.handler(event(old)); f.advance(60001);
  f.user({ sub: "verified" }); await f.handler(event(old, "checked", { authorization: "Bearer valid" }));
  assert.equal(f.records.get(`sotce.net:${old}`).acRecognized, false, "stale IDs cannot be upgraded");
});

test("origin, schema, automation and opt-outs guard both phases; invalid credentials remain unknown", async () => {
  const f = backendFixture(), id = randomUUID();
  for (const request of [
    event(id, "started", { origin: RECOGNITION_ORIGIN }),
    event(id, "checked", { origin: "https://sotce.net" }),
    { ...event(id), body: JSON.stringify({ version: 1, id, phase: "started", acRecognized: true }) },
    { ...event(id), body: "{" }, { ...event(id), isBase64Encoded: true },
  ]) assert.equal((await f.handler(request)).statusCode, 400);
  assert.equal((await f.handler(event(id, "started", { origin: "https://evil.example" }))).statusCode, 403);
  for (const headers of [{ dnt: "1" }, { "sec-gpc": "1" }, { "user-agent": "HeadlessChrome" }])
    assert.equal((await f.handler(event(id, "started", headers))).statusCode, 204);
  assert.equal(f.records.size, 0);
  await f.handler(event(id));
  await f.handler(event(id, "checked", { authorization: "Bearer invalid" }));
  assert.equal(f.records.get(`sotce.net:${id}`).acRecognized, false);
  assert.equal(f.records.get(`sotce.net:${id}`).bridgeCompleted, true);
  assert.equal((await f.handler({ ...event(id), httpMethod: "GET" })).statusCode, 405, "no public read API");
  assert.equal(validateRecognition({ version: 1, id, phase: "started" }, "https://www.sotce.net").property, "sotce.net");
});

test("collector rate guard bounds writes and expires without persisting addresses", async () => {
  const f = backendFixture(), id = randomUUID();
  for (let n = 0; n < 120; n++) assert.equal((await f.handler(event(id))).statusCode, 204);
  assert.equal((await f.handler(event(id))).statusCode, 429);
  f.advance(60001);
  assert.equal((await f.handler(event(id))).statusCode, 204);
  assert.equal(f.records.size, 1);
});

function windowFixture(origin = RECOGNITION_ORIGIN) {
  const listeners = new Map(), intervals = new Map(), timeouts = new Map(), messages = [], requests = [];
  let next = 0;
  const win = { location: { origin, pathname: "/", search: "" }, navigator: {}, crypto: globalThis.crypto, btoa,
    parent: { postMessage: (...args) => messages.push(args) },
    addEventListener(name, fn) { (listeners.get(name) || listeners.set(name, new Set()).get(name)).add(fn); },
    removeEventListener(name, fn) { listeners.get(name)?.delete(fn); },
    setTimeout(fn) { timeouts.set(++next, fn); return next; }, clearTimeout(id) { timeouts.delete(id); },
    setInterval(fn) { intervals.set(++next, fn); return next; }, clearInterval(id) { intervals.delete(id); },
    fetch: async (url, options) => { requests.push({ url, options }); return { ok: true }; },
  };
  win.top = win;
  const frames = [];
  const doc = { visibilityState: "visible", ...Object.fromEntries(["addEventListener", "removeEventListener"].map(key => [key, win[key].bind(win)])),
    body: { append: frame => frames.push(frame) },
    createElement: () => {
      const handlers = new Map();
      return { contentWindow: { postMessage: (...args) => messages.push(args) }, setAttribute() {},
        addEventListener: (name, fn) => handlers.set(name, fn),
        load: () => handlers.get("load")?.(), remove() { const index = frames.indexOf(this); if (index >= 0) frames.splice(index, 1); } };
    },
  };
  return { win, doc, messages, requests, frames, intervals, timeouts,
    emit: async (name, event) => { for (const fn of [...listeners.get(name) || []]) await fn(event); },
    tick: () => { for (const fn of intervals.values()) fn(); } };
}

test("bridge verifies parent origin/source, does not send identity outward, and accepts one check", async () => {
  const f = windowFixture(), id = randomUUID(); let tokenReads = 0;
  startRecognitionBridge(f.win, f.doc, async () => { tokenReads++; return "private-token"; });
  const message = { origin: "https://sotce.net", source: f.win.parent, data: { type: "ac-recognition:check", id } };
  await f.emit("message", { ...message, origin: "https://evil.example" });
  await f.emit("message", { ...message, source: {} });
  assert.equal(tokenReads, 0);
  await f.emit("message", message); await f.emit("message", message);
  assert.equal(tokenReads, 1); assert.equal(f.requests.length, 1);
  assert.equal(f.requests[0].url, "/api/network-recognition");
  assert.equal(f.requests[0].options.headers.Authorization, "Bearer private-token");
  assert.deepEqual(f.messages, [[{ type: "ac-recognition:done", id }, "https://sotce.net"]]);
  assert.doesNotMatch(JSON.stringify(f.messages), /private-token|handle|recognized/);
});

test("bridge skips credential reads for opt-outs and cancels late attribution on logout", async () => {
  for (const change of [f => { f.win.navigator.globalPrivacyControl = true; }, f => { f.win.acVisitTrackingDisabled = true; },
    f => f.emit("storage", { key: "session-aesthetic" }), f => f.emit("pagehide")]) {
    const f = windowFixture(); let release;
    startRecognitionBridge(f.win, f.doc, () => new Promise(resolve => { release = resolve; }));
    const running = f.emit("message", { origin: "https://sotce.net", source: f.win.parent, data: { type: "ac-recognition:check", id: randomUUID() } });
    await settle(); await change(f); release("old-token"); await running;
    assert.equal(f.requests.length, 0); assert.equal(f.messages.length, 0);
  }
  const f = windowFixture(); f.win.navigator.doNotTrack = "1";
  assert.equal(startRecognitionBridge(f.win, f.doc, () => { throw Error("must not read"); }), null);
});

test("credential lookup uses only handed credentials or the SDK cache, never a login or refresh", async () => {
  let options, lookup;
  const win = { localStorage: { getItem: () => null }, atob,
    auth0: { Auth0Client: class {
      constructor(value) { options = value; }
      async getTokenSilently(value) { lookup = value; return "cached-token"; }
    } },
  };
  assert.equal(await cachedAcToken(win, {}), "cached-token");
  assert.equal(options.cacheLocation, "localstorage");
  assert.deepEqual(lookup, { cacheMode: "cache-only" });
  win.localStorage.getItem = () => btoa(JSON.stringify({ accessToken: "handed-token", account: { id: "private" } }));
  assert.equal(await cachedAcToken(win, {}), "handed-token");
});

test("silent SSO authenticates the response channel and state and stops if consent or login is needed", async () => {
  for (const fail of ["state", "consent_required", "login_required", "timeout", "abort"]) {
    const f = windowFixture();
    const controller = new AbortController();
    const promise = silentAcToken(f.win, f.doc, controller.signal);
    while (!f.frames.length) await settle();
    const frame = f.frames[0], url = new URL(frame.src);
    const response = { state: url.searchParams.get("state"), code: "private-code" };
    const message = { source: frame.contentWindow, origin: "https://hi.aesthetic.computer", data: { type: "authorization_response", response } };
    await f.emit("message", { ...message, source: {} });
    await f.emit("message", { ...message, origin: "https://evil.example" });
    assert.equal(f.requests.length, 0, "untrusted responses never exchange a code");
    if (fail === "timeout") { for (const fn of f.timeouts.values()) fn(); }
    else if (fail === "abort") controller.abort();
    else {
      if (fail === "state") response.state = "forged";
      else response.error = fail;
      await f.emit("message", message);
    }
    assert.equal(await promise, undefined);
    assert.equal(f.frames.length, 0); assert.equal(f.requests.length, 0);
  }
});

test("parent waits for visibility, counts once, authenticates frame responses and cleans up", async () => {
  const f = windowFixture("https://sotce.net"); f.doc.visibilityState = "hidden";
  const api = startNetworkRecognition(f.win, f.doc);
  assert.equal(f.requests.length, 0);
  f.doc.visibilityState = "visible"; await f.emit("visibilitychange"); await settle();
  assert.equal(f.requests.length, 1); assert.equal(f.frames.length, 1);
  assert.equal(startNetworkRecognition(f.win, f.doc), api);
  const id = JSON.parse(f.requests[0].options.body).id, frame = f.frames[0];
  frame.load();
  assert.deepEqual(f.messages[0], [{ type: "ac-recognition:check", id }, RECOGNITION_ORIGIN]);
  const done = { source: frame.contentWindow, origin: RECOGNITION_ORIGIN, data: { type: "ac-recognition:done", id } };
  await f.emit("message", { ...done, origin: "https://evil.example" });
  await f.emit("message", { ...done, source: {} });
  await f.emit("message", { ...done, data: { ...done.data, id: randomUUID() } });
  assert.equal(f.frames.length, 1);
  await f.emit("message", done);
  assert.equal(f.frames.length, 0); assert.equal(f.timeouts.size, 0); assert.equal(f.intervals.size, 0);
});

test("parent suppresses private/automated visits and cancels on private navigation, opt-out or timeout", async () => {
  for (const path of ["/chat", "/write", "/%77rite", "/ask", "/respond", "/comment", "/account", "/%zz"] ) {
    const f = windowFixture("https://sotce.net"); f.win.location.pathname = path;
    assert.equal(recognitionPage(f.win, f.doc), false);
    startNetworkRecognition(f.win, f.doc); await settle(); assert.equal(f.requests.length, 0);
  }
  for (const change of [f => { f.win.navigator.webdriver = true; }, f => { f.win.navigator.globalPrivacyControl = true; },
    f => { f.win.top = {}; }, f => { f.win.location.origin = "https://evil.example"; }]) {
    const f = windowFixture("https://sotce.net"); change(f);
    assert.equal(startNetworkRecognition(f.win, f.doc), null); assert.equal(f.requests.length, 0);
  }
  for (const change of [f => { f.win.location.pathname = "/write"; f.tick(); },
    f => { f.win.acVisitTrackingDisabled = true; f.tick(); },
    f => { f.doc.visibilityState = "hidden"; f.tick(); },
    f => { for (const fn of f.timeouts.values()) fn(); }]) {
    const f = windowFixture("https://sotce.net"); startNetworkRecognition(f.win, f.doc); await settle();
    change(f); assert.equal(f.frames.length, 0); assert.equal(f.requests[0].options.signal.aborted, true);
  }
});

test("stopping the visit tracker cancels a recognition import or an already running check", async () => {
  for (const stopDuringImport of [true, false]) {
    const f = windowFixture("https://sotce.net");
    f.win.location.hostname = "sotce.net";
    f.win.performance = { now: () => 0 };
    const tracker = startVisitTracker(f.win, f.doc);
    if (!stopDuringImport) { await settle(); await settle(); }
    tracker.stop(); await settle(); await settle();
    assert.equal(f.frames.length, 0);
    if (stopDuringImport) assert.equal(f.requests.filter(r => r.url.endsWith("network-recognition")).length, 0);
    else assert.equal(f.requests.find(r => r.url.endsWith("network-recognition")).options.signal.aborted, true);
  }
});

test("bridge HTML limits framing and network access; aggregate report never outputs check or account IDs", async () => {
  const html = await bridgePage({ httpMethod: "GET" });
  assert.match(html.headers["Content-Security-Policy"], /frame-ancestors https:\/\/sotce\.net https:\/\/www\.sotce\.net/);
  assert.match(html.headers["Content-Security-Policy"], /connect-src 'self'/);
  assert.equal(html.headers["Cache-Control"], "no-store");
  assert.equal((await bridgePage({ httpMethod: "POST" })).statusCode, 405);
  const at = new Date(Date.now() - 1000), rows = [
    { _id: "secret-id", property: "sotce.net", startedAt: at, acRecognized: true, bridgeCompleted: true },
    { property: "sotce.net", startedAt: at, acRecognized: false, bridgeCompleted: true },
    { property: "sotce.net", startedAt: at, acRecognized: false, bridgeCompleted: false },
    { property: "other.example", startedAt: at, acRecognized: true, bridgeCompleted: true },
  ];
  const report = await networkRecognitionReport({ collection: () => ({ aggregate: pipeline => ({ toArray: async () => new Aggregator(pipeline).run(rows) }) }) });
  assert.deepEqual(report.totals, { visitsChecked: 3, acSessionVerified: 1, bridgeCompleted: 2, unknown: 2, bridgeIncomplete: 1 });
  assert.equal(report.daily.length, 1);
  assert.doesNotMatch(JSON.stringify(report), /secret-id|other\.example/);
  for (const hours of [-1, 0, 841, NaN, "24"]) assert.throws(() => recognitionReportOptions({ hours }));
});
