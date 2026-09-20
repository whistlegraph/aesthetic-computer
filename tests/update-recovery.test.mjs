import assert from "node:assert/strict";
import { readFileSync } from "node:fs";
import test from "node:test";
import vm from "node:vm";

const bootSource = readFileSync(new URL("../system/public/aesthetic.computer/boot.mjs", import.meta.url), "utf8");
const recovery = bootSource.slice(bootSource.indexOf("// Bound connection recovery"), bootSource.indexOf("// Global error handler for module load failures"));
const success = bootSource.slice(bootSource.indexOf("let bootCompleted = false;"), bootSource.indexOf('// Hide the boot log overlay.'));

function boot(storage, blocked = false) {
  const timers = new Map(), errors = [];
  let reloads = 0;
  const context = vm.createContext({
    window: { location: { reload() { reloads++; } } },
    sessionStorage: {
      getItem: key => storage.get(key),
      setItem(key, value) { if (blocked) throw Error("blocked"); storage.set(key, value); },
      removeItem: key => storage.delete(key),
    },
    bootLog() {}, bootTelemetry: { error: e => errors.push(e), complete() {} },
    performance, bootStartTime: performance.now(),
    setTimeout: fn => { const id = timers.size + 1; timers.set(id, fn); return id; },
    clearTimeout: id => timers.delete(id),
  });
  vm.runInContext(success + recovery, context);
  return { fail: () => vm.runInContext('showConnectionError(new Error("Failed to fetch"))', context),
    succeed: () => vm.runInContext("markBootSuccess()", context), timers, errors,
    reload: () => { for (const fn of timers.values()) fn(); return reloads; } };
}

test("network failures stop after five reloads across fresh page contexts", () => {
  const storage = new Map();
  for (let i = 1; i <= 7; i++) {
    const page = boot(storage);
    page.fail(); page.fail();
    assert.equal(page.errors.length, 1);
    assert.equal(page.reload(), i <= 5 ? 1 : 0);
  }
});

test("successful boot cancels a pending reload and resets the retry budget", () => {
  const storage = new Map();
  const page = boot(storage);
  page.fail(); page.succeed(); page.fail();
  assert.equal(page.reload(), 0);
  assert.equal(storage.size, 0);
  const next = boot(storage); next.fail();
  assert.equal(next.errors[0].retryCount, 1);
});

test("blocked storage cannot cause unbounded reloads", () => {
  const page = boot(new Map(), true); page.fail();
  assert.equal(page.reload(), 0);
});

const diskSource = readFileSync(new URL("../system/public/aesthetic.computer/lib/disk.mjs", import.meta.url), "utf8");
const pollSource = diskSource.slice(diskSource.indexOf("async function startGlobalVersionPoll()"), diskSource.indexOf("// ***Bootstrap***"));
async function poll(responses) {
  const calls = [], sent = [];
  const context = vm.createContext({
    updatePollStarted: false, globalVersionInfo: null, globalUpdateReady: false,
    globalRecentCommits: [], updatePollController: null, debug: false,
    $commonApi: {}, console: { log() {}, warn() {} }, AbortController,
    setTimeout: fn => { queueMicrotask(fn); },
    send: message => sent.push(message),
    fetch: async (url, options) => {
      calls.push({ url, options });
      if (!responses.length) throw Object.assign(new Error("done"), { name: "AbortError" });
      const data = responses.shift();
      if (data instanceof Error) throw data;
      return { ok: true, json: async () => data };
    },
  });
  await vm.runInContext(pollSource + "\nstartGlobalVersionPoll()", context);
  return { context, calls, sent };
}

test("same-version and unknown responses do not announce an update or clear caches", async () => {
  const result = await poll([{ deployed: "aaaaaaa" }, { deployed: "aaaaaaa" }, { deployed: "unknown" }]);
  assert.equal(result.context.globalUpdateReady, false);
  assert.equal(result.sent.length, 0);
  assert.ok(result.calls.every(c => c.options.cache === "no-store"));
});

test("a new deployed hash announces exactly one update", async () => {
  const result = await poll([{ deployed: "aaaaaaa" }, { changed: false, deployed: "aaaaaaa" }, { deployed: "bbbbbbb" }]);
  assert.equal(result.context.globalUpdateReady, true);
  assert.equal(result.context.globalVersionInfo.deployed, "bbbbbbb");
  assert.equal(result.sent.length, 1);
});

test("a failed initial version fetch is retried", async () => {
  const result = await poll([new Error("offline"), { deployed: "aaaaaaa" }, { deployed: "bbbbbbb" }]);
  assert.equal(result.calls[1].url, "/api/version");
  assert.equal(result.context.globalUpdateReady, true);
});

test("recovering an unknown baseline does not announce a deployment", async () => {
  const result = await poll([{ deployed: "unknown" }, { deployed: "aaaaaaa" }]);
  assert.equal(result.context.globalUpdateReady, false);
  assert.equal(result.context.globalVersionInfo.deployed, "aaaaaaa");
});

const versionSource = readFileSync(new URL("../system/netlify/functions/version.mjs", import.meta.url), "utf8")
  .replace(/^import .*;$/gm, "")
  .replace("const execFileAsync = promisify(execFile);", "")
  .replace("export default async (request) =>", "globalThis.handler = async (request) =>")
  .replace('export const config =', 'const config =');
const { default: path } = await import("node:path");
async function versionResponse({ current = "", fail = false } = {}) {
  const context = vm.createContext({
    fs: { readFileSync: () => "aaaaaaa123456789", existsSync: () => false }, path,
    process: { env: { VERSION_LONG_POLL_MS: "0" }, cwd: () => "/test/system" },
    execFileAsync: async () => {
      if (fail) throw Error("git unavailable");
      return { stdout: "aaaaaaa123456789\trefs/heads/main" };
    },
    URL, Response, setTimeout,
  });
  vm.runInContext(versionSource, context);
  return context.handler(new Request(`https://aesthetic.computer/api/version${current ? `?current=${current}` : ""}`));
}

test("version payloads, unchanged polls, and lookup failures all disable HTTP caching", async () => {
  for (const options of [{}, { current: "aaaaaaa" }, { fail: true }]) {
    const response = await versionResponse(options);
    assert.equal(response.headers.get("Cache-Control"), "no-store");
    const data = await response.json();
    assert.equal(data.deployed, "aaaaaaa");
    if (options.current) assert.equal(data.changed, false);
    if (options.fail) assert.equal(data.status, "unknown");
  }
});
