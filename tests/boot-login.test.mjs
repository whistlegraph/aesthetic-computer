import assert from "node:assert/strict";
import { readFileSync } from "node:fs";
import test from "node:test";
import vm from "node:vm";

// Exercise the boot authentication flow without starting the canvas runtime.
const source = readFileSync(new URL("../system/public/aesthetic.computer/boot.mjs", import.meta.url), "utf8");
const auth = source.slice(source.indexOf("// noauth mode should skip"), source.indexOf("// Incoming window-message responder"));
const tick = () => new Promise(resolve => setImmediate(resolve));

async function boot({ cached = false, session, scriptFails = false, noauth = false, desktop } = {}) {
  const storage = new Map();
  if (cached) storage.set("@@auth0spajs@@::test", "{}");
  if (session) storage.set("session-aesthetic", session);
  const messages = [], redirects = [], fetches = [];
  let failScript = scriptFails;
  const client = {
    isAuthenticated: async () => false,
    loginWithRedirect: async options => { redirects.push(options); return "redirected"; },
  };
  const window = {
    origin: "https://aesthetic.computer", acNOAUTH: noauth,
    location: { origin: "https://aesthetic.computer", href: "https://aesthetic.computer/" },
    acAuthTiming: { durations: {}, computeDurations() {}, summary() {} },
    acDISK_SEND: message => messages.push(message),
    postMessage() {},
    acDESKTOP: desktop,
  };
  window.top = window;
  window.parent = window;
  const context = vm.createContext({
    window, console: { log() {}, error() {} }, performance,
    localStorageBlocked: false, sessionStorageBlocked: false, previewOrIcon: false,
    localStorage: { get length() { return storage.size; }, key: i => [...storage.keys()][i] },
    safeLocalStorageGet: key => storage.get(key),
    safeLocalStorageSet: (key, value) => storage.set(key, value),
    safeLocalStorageRemove: key => storage.delete(key),
    location: { search: "", pathname: "/" },
    document: {
      createElement: () => ({}),
      head: { appendChild(script) {
        queueMicrotask(() => {
          if (failScript) { failScript = false; script.onerror(); }
          else { window.auth0 = { createAuth0Client: async () => client }; script.onload(); }
        });
      } },
    },
    bootLog() {}, extractLegitimateParams: () => new URLSearchParams(),
    atob, fetch: async (url, opts) => { fetches.push({ url, opts }); return { ok: false }; },
  });
  vm.runInContext(auth, context);
  await tick();
  return { window, storage, messages, redirects, fetches };
}

const expiredSession = btoa(JSON.stringify({ accessToken: "expired-test-token", account: { id: "test" } }));

test("expired embedded session still lets the visitor log in", async () => {
  const state = await boot({ cached: true, session: expiredSession });
  assert.equal(state.messages.at(-1).content.user, null);
  assert.equal(await state.window.acLOGIN(), "redirected");
  assert.equal(state.redirects[0].authorizationParams.prompt, "login");
  assert.equal(state.storage.has("session-aesthetic"), false);
});

test("malformed saved session can be replaced by signup", async () => {
  const state = await boot({ cached: true, session: "not base64!" });
  assert.equal(state.messages.at(-1).content.user, null);
  await state.window.acLOGIN("signup");
  assert.equal(state.redirects[0].authorizationParams.screen_hint, "signup");
  assert.equal(state.storage.has("session-aesthetic"), false);
});

test("login retries a failed Auth0 script load", async () => {
  const state = await boot({ cached: true, scriptFails: true });
  assert.equal(state.messages.at(-1).content.user, null);
  await state.window.acLOGIN();
  assert.equal(state.redirects.length, 1);
});

test("anonymous boot keeps Auth0 lazy until login", async () => {
  const state = await boot();
  assert.equal(state.window.auth0Client, undefined);
  await state.window.acLOGIN();
  assert.equal(state.redirects.length, 1);
});

test("noauth embeds do not enable login", async () => {
  const state = await boot({ noauth: true });
  assert.equal(state.window.acLOGIN, undefined);
});

test("a handed-in session is checked even without an Auth0 cache", async () => {
  const session = btoa(JSON.stringify({ accessToken: "desktop-token", account: { id: "test" } }));
  const state = await boot({ session });
  await tick();
  const check = state.fetches.find((f) => f.url === "/api/authorized");
  assert.ok(check, "the session was never validated");
  assert.equal(check.opts.headers.Authorization, "Bearer desktop-token");
});

test("the desktop app takes over login", async () => {
  const asked = [];
  const state = await boot({ desktop: { login: async (mode) => { asked.push(mode); return "desktop"; } } });
  assert.equal(await state.window.acLOGIN("signup"), "desktop");
  assert.deepEqual(asked, ["signup"]);
  assert.equal(state.redirects.length, 0);
});
