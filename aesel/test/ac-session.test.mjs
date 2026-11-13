import assert from "node:assert/strict";
import { mkdtemp, readFile, rm, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";
import test from "node:test";
import { ACSession, CLIENT_ID } from "../src/ac-session.mjs";

const jsonResponse = (body, status = 200) =>
  new Response(JSON.stringify(body), { status, headers: { "content-type": "application/json" } });

async function scratch(context) {
  const root = await mkdtemp(join(tmpdir(), "easel-session-"));
  context.after(() => rm(root, { recursive: true, force: true }));
  return join(root, ".ac-token");
}

test("labels the three sign-in states without exposing email", async (context) => {
  const file = await scratch(context);
  const session = new ACSession({ file });
  assert.equal(session.state, "signed-out");
  assert.equal(session.label(), "not signed in");

  await writeFile(file, JSON.stringify({ access_token: "t", user: { email: "x@y.z", handle: null } }));
  assert.equal(session.state, "no-handle");
  assert.equal(session.label(), "signed in · no handle");

  await writeFile(file, JSON.stringify({ access_token: "t", user: { email: "x@y.z", handle: "tester" } }));
  assert.equal(session.state, "signed-in");
  assert.equal(session.handle, "tester");
  assert.equal(session.label(), "@tester");
  assert.doesNotMatch(session.label(), /x@y\.z/);
});

test("refreshes a stale access token in place", async (context) => {
  const file = await scratch(context);
  await writeFile(
    file,
    JSON.stringify({ access_token: "old", refresh_token: "r1", expires_at: 1_000, user: { handle: "tester" } }),
  );
  const calls = [];
  const session = new ACSession({
    file,
    now: () => 10_000_000,
    fetch: async (url, options) => {
      calls.push({ url, body: JSON.parse(options.body) });
      return jsonResponse({ access_token: "new", refresh_token: "r2", expires_in: 3600 });
    },
  });
  assert.equal(await session.token(), "new");
  assert.equal(calls.length, 1);
  assert.match(calls[0].url, /hi\.aesthetic\.computer\/oauth\/token$/);
  assert.equal(calls[0].body.grant_type, "refresh_token");
  assert.equal(calls[0].body.client_id, CLIENT_ID);
  const written = JSON.parse(await readFile(file, "utf8"));
  assert.equal(written.access_token, "new");
  assert.equal(written.refresh_token, "r2");
  assert.equal(written.expires_at, 10_000_000 + 3_600_000);
  assert.equal(written.user.handle, "tester");
});

test("runs the PKCE sign-in against a loopback callback", async (context) => {
  const file = await scratch(context);
  const seen = {};
  const session = new ACSession({
    file,
    callbackPort: 0,
    site: "https://example.test",
    fetch: async (url, options = {}) => {
      if (url.endsWith("/oauth/token")) {
        // The callback must already be closed while the outgoing exchange runs.
        await assert.rejects(globalThis.fetch(seen.callback, {headers:{Connection:'close'}}));
        seen.exchange = JSON.parse(options.body);
        return jsonResponse({ access_token: "acc", refresh_token: "ref", id_token: "id", expires_in: 86_400 });
      }
      if (url.endsWith("/userinfo")) {
        seen.auth = options.headers.Authorization;
        return jsonResponse({ sub: "auth0|123", email: "x@y.z", name: "x" });
      }
      if (url.startsWith("https://example.test/handle?for=")) {
        seen.lookup = url;
        return jsonResponse({ handle: "tester" });
      }
      throw new Error(`unexpected fetch ${url}`);
    },
    openBrowser: (authUrl) => {
      const url = new URL(authUrl);
      seen.challenge = url.searchParams.get("code_challenge");
      assert.equal(url.searchParams.get("client_id"), CLIENT_ID);
      assert.equal(url.searchParams.get("code_challenge_method"), "S256");
      const callback = new URL(url.searchParams.get("redirect_uri"));
      seen.callback = callback.toString();
      callback.searchParams.set("code", "the-code");
      callback.searchParams.set("state", url.searchParams.get("state"));
      // Play the browser: land on the loopback callback.
      globalThis.fetch(callback).catch(() => {});
    },
  });
  const handle = await session.login({ timeoutMs: 5_000 });
  assert.equal(handle, "tester");
  assert.equal(seen.exchange.grant_type, "authorization_code");
  assert.equal(seen.exchange.code, "the-code");
  assert.ok(seen.exchange.code_verifier.length > 40);
  assert.equal(seen.auth, "Bearer acc");
  assert.equal(seen.lookup, "https://example.test/handle?for=auth0%7C123");
  const written = JSON.parse(await readFile(file, "utf8"));
  assert.equal(written.user.handle, "tester");
  assert.equal(written.user.sub, "auth0|123");
  assert.equal(session.label(), "@tester");
  assert.equal(session.logout(), true);
  assert.equal(session.state, "signed-out");
});

test("emits change when the shared token file is rewritten", async (context) => {
  const file = await scratch(context);
  const session = new ACSession({ file });
  context.after(() => session.unwatch());
  const changed = new Promise((resolve, reject) => {
    const timer = setTimeout(() => reject(new Error("no change event")), 4_000);
    session.once("change", (label) => {
      clearTimeout(timer);
      resolve(label);
    });
  });
  session.watch();
  await new Promise((resolve) => setTimeout(resolve, 100));
  await writeFile(file, JSON.stringify({ access_token: "t", user: { handle: "tester" } }));
  assert.equal(await changed, "@tester");
});

test("claiming a handle saves it and emits the new sign-in state", async context => {
  const file = await scratch(context);
  await writeFile(file, JSON.stringify({access_token:"t",expires_at:Date.now()+3600000,user:{sub:"review",handle:null}}));
  const session = new ACSession({file,fetch:async()=>jsonResponse({handle:"newartist"})});
  let changed;session.on("change",value=>{changed=value;});
  assert.equal(await session.claimHandle("newartist"),"newartist");
  assert.equal(session.handle,"newartist");
  assert.equal(changed,"signed-in");
});

 test("sign-in timeout closes the listener and permits a retry", async context => {
  const file = await scratch(context);
  let callback;
  const session = new ACSession({file, callbackPort:0, openBrowser:url=>{
    callback = new URL(url).searchParams.get('redirect_uri');
  }});
  for (let attempt=0;attempt<2;attempt++) {
    await assert.rejects(session.login({timeoutMs:30}), /sign-in timed out/);
    assert.equal(session.signingIn,false);
    await assert.rejects(globalThis.fetch(callback,{headers:{Connection:'close'}}));
  }
});

test("signs in through the device link, waiting while it is pending", async (context) => {
  const file = await scratch(context);
  const seen = { polls: 0, waits: [] };
  const session = new ACSession({
    file, site: "https://example.test",
    openBrowser: (url) => { seen.opened = url; },
    fetch: async (url, options = {}) => {
      const sent = new URLSearchParams(options.body || "");
      if (url.endsWith("/oauth/device/code")) {
        assert.equal(sent.get("client_id"), CLIENT_ID);
        return jsonResponse({ device_code: "dev", user_code: "WXYZ-1234", verification_uri_complete: "https://hi.example/activate?user_code=WXYZ-1234", interval: 2, expires_in: 600 });
      }
      if (url.endsWith("/oauth/token")) {
        assert.equal(sent.get("device_code"), "dev");
        seen.polls += 1;
        if (seen.polls === 1) return new Response(JSON.stringify({ error: "authorization_pending" }), { status: 403 });
        if (seen.polls === 2) return new Response(JSON.stringify({ error: "slow_down" }), { status: 403 });
        return jsonResponse({ access_token: "acc", refresh_token: "ref", id_token: "id", expires_in: 86_400 });
      }
      if (url.endsWith("/userinfo")) return jsonResponse({ sub: "auth0|123", email: "x@y.z", name: "x" });
      if (url.startsWith("https://example.test/handle?for=")) return jsonResponse({ handle: "tester" });
      throw new Error(`unexpected fetch ${url}`);
    },
  });
  let shown;
  const handle = await session.loginDevice({ onCode: (code) => { shown = code; }, wait: async (ms) => { seen.waits.push(ms); } });
  assert.equal(handle, "tester");
  assert.deepEqual(shown, { url: "https://hi.example/activate?user_code=WXYZ-1234", code: "WXYZ-1234" });
  assert.equal(seen.opened, shown.url);
  assert.deepEqual(seen.waits, [2000, 2000, 7000]);
  assert.equal(session.signedIn, true);
  assert.equal(session.signingIn, false);
});

test("the device link answers undefined where the grant is off, and names a refusal", async (context) => {
  const off = new ACSession({ file: await scratch(context), openBrowser: () => {},
    fetch: async () => new Response(JSON.stringify({ error: "unauthorized_client" }), { status: 403 }) });
  assert.equal(await off.loginDevice(), undefined);
  const declined = new ACSession({ file: await scratch(context), openBrowser: () => {},
    fetch: async (url) => url.endsWith("/device/code") ? jsonResponse({ device_code: "d", user_code: "c", verification_uri: "https://hi.example/activate" })
      : new Response(JSON.stringify({ error: "access_denied" }), { status: 403 }) });
  await assert.rejects(declined.loginDevice({ wait: async () => {} }), /declined/);
});
