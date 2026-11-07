import test from "node:test";
import assert from "node:assert/strict";
import {
  PARAMS,
  parseQuery,
  cleanSearch,
  serialize,
  present,
  legitimate,
  viewParams,
  authParams,
  createParamStore,
  warnUnknown,
  renderDoc,
} from "../public/aesthetic.computer/lib/params.mjs";

test("flags are true when present, whatever the value", () => {
  const q = parseQuery("?tv&nogap=true&solo=1");
  assert.equal(q.tv, true);
  assert.equal(q.nogap, true);
  assert.equal(q.solo, true);
  assert.equal(q.perf, undefined);
});

test("numbers are numbers; bare is true; junk stays a string", () => {
  assert.equal(parseQuery("density=8").density, 8);
  assert.equal(parseQuery("maxfps=60&duration=12.5").duration, 12.5);
  assert.equal(parseQuery("?density").density, true);
  assert.equal(parseQuery("density=high").density, "high");
});

test("size: bare, true and WxH ask for a preview; other values are strings", () => {
  assert.equal(parseQuery("?preview").preview, true);
  assert.equal(parseQuery("?preview=true").preview, true);
  assert.equal(parseQuery("?preview=1200x630").preview, "1200x630");
  assert.equal(parseQuery("?preview=walkieware").preview, "walkieware");
  assert.equal(parseQuery("?icon=128x128").icon, "128x128");
});

test("strings and tokens are kept as written", () => {
  const q = parseQuery("?daw=1&highlight=%23ff0000&code=abc&notice=hello+there");
  assert.equal(q.daw, "1");
  assert.equal(q.highlight, "#ff0000");
  assert.equal(q.code, "abc");
  assert.equal(q.notice, "hello there");
});

test("unknown names stay strings and are listed in __unknown", () => {
  const q = parseQuery("?foo=bar&novec&zip");
  assert.equal(q.foo, "bar");
  assert.equal(q.zip, "");
  assert.equal(q.novec, true);
  assert.deepEqual(q.__unknown, ["foo", "zip"]);
  assert.equal(Object.keys(q).includes("__unknown"), false);
  assert.deepEqual({ ...q }.__unknown, undefined);
});

test("aliases canonicalise: autoScale → auto-scale", () => {
  assert.equal(parseQuery("?autoScale=true")["auto-scale"], true);
  assert.equal(parseQuery("?auto-scale")["auto-scale"], true);
  assert.ok(legitimate().includes("autoScale"));
  assert.ok(legitimate().includes("auto-scale"));
});

test("cleanSearch drops auth names and keeps everything else verbatim", () => {
  const raw = "?code=abc&state=xyz&nolabel&session-aesthetic=tok&preview&foo=b%20r";
  assert.equal(cleanSearch(raw, { drop: ["auth"] }), "?nolabel&preview&foo=b%20r");
  assert.equal(cleanSearch("?code=abc&state=xyz", { drop: ["auth"] }), "");
  assert.equal(cleanSearch("", { drop: ["auth"] }), "");
  assert.equal(cleanSearch("?tv&notice=x&zip=1", { keep: ["view"] }), "?tv");
  assert.equal(cleanSearch("?tv&notice=x&zip=1", { keep: ["view", "unknown"] }), "?tv&zip=1");
});

test("every auth-scope name is a token and every token is auth-scope", () => {
  for (const name of authParams()) assert.equal(PARAMS[name].kind, "token", name);
  for (const [name, e] of Object.entries(PARAMS)) {
    if (e.kind === "token") assert.equal(e.scope, "auth", name);
  }
  assert.deepEqual(
    authParams().sort(),
    ["code", "error", "error_description", "session-aesthetic", "session-sotce", "state"],
  );
});

test("viewParams carry sets match what bios preserves and what jump re-appends today", () => {
  assert.deepEqual(
    viewParams("bar").sort(),
    [
      "autoreload", "daw", "density", "desktop", "device", "duration", "height",
      "highlight", "icon", "maxfps", "mime", "noauth", "nogap", "nolabel",
      "nosource", "preview", "reel", "shellhtml", "solo", "tv", "width", "zoom",
    ],
  );
  assert.deepEqual(
    viewParams("jump").sort(),
    ["auto-scale", "device", "highlight", "nogap", "nolabel", "nosource", "perf", "solo", "spoofaudio", "tv"],
  );
});

test("serialize writes name=true for flags and drops false", () => {
  assert.equal(
    serialize({ tv: true, highlight: "0,0,0", perf: false, maxfps: 60, gone: undefined }),
    "tv=true&highlight=0%2C0%2C0&maxfps=60",
  );
  assert.equal(serialize({}), "");
});

test("present() reports names only", () => {
  assert.deepEqual(present("?code=SECRET&tv&autoScale=1"), ["code", "tv", "auto-scale"]);
});

test("the store remembers view values, refuses tokens, and carries by kind", () => {
  const store = createParamStore();
  store.remember("tv");
  store.remember("autoScale", true);
  store.remember("highlight", "#ff0000");
  store.remember("maxfps", 60);
  store.remember("code", "secret");
  store.remember("perf", false);
  assert.equal(store.has("tv"), true);
  assert.equal(store.get("auto-scale"), true);
  assert.equal(store.has("code"), false);
  assert.equal(store.has("perf"), false);
  assert.equal(serialize(store.carried("jump")), "tv=true&auto-scale=true&highlight=%23ff0000");
  store.forget("tv");
  assert.equal(store.has("tv"), false);
  store.clear();
  assert.deepEqual(store.all(), {});
});

test("warnUnknown warns once per name", () => {
  const seen = [];
  warnUnknown(["zzz-test-a", "zzz-test-a", "zzz-test-b"], { warn: (m) => seen.push(m) });
  warnUnknown(["zzz-test-a"], { warn: (m) => seen.push(m) });
  assert.equal(seen.length, 2);
});

test("every entry is well-formed and the doc renders a row per entry", () => {
  const kinds = new Set(["flag", "number", "size", "string", "token"]);
  const scopes = new Set(["view", "boot", "auth", "piece", "server"]);
  for (const [name, e] of Object.entries(PARAMS)) {
    assert.ok(kinds.has(e.kind), `${name} kind`);
    assert.ok(scopes.has(e.scope), `${name} scope`);
    assert.ok(Array.isArray(e.readers), `${name} readers`);
    assert.ok(["live", "unused"].includes(e.status), `${name} status`);
    assert.equal(e.status === "unused", e.readers.length === 0, `${name} status vs readers`);
    if (e.carry) assert.equal(e.scope, "view", `${name} carry only on view`);
  }
  const rows = renderDoc().split("\n");
  assert.equal(rows.length, Object.keys(PARAMS).length + 2);
});
