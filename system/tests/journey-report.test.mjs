// node --experimental-vm-modules --test system/tests/journey-report.test.mjs
import test from "node:test";
import assert from "node:assert/strict";
import vm from "node:vm";
import { readFile } from "node:fs/promises";
import { createHash } from "node:crypto";
import { Query, Aggregator } from "mingo";
import * as model from "../public/aesthetic.computer/lib/visit-model.mjs";

async function report(mode, args, data) {
  let output, closed = false;
  const db = { collection(name) {
    const rows = data[name] || [];
    return {
      aggregate: pipeline => ({ toArray: async () => new Aggregator(pipeline).run(rows) }),
      find(query, options = {}) {
        let cursor = new Query(query).find(rows, options.projection);
        return { sort(value) { cursor = cursor.sort(value); return this; },
          limit(value) { cursor = cursor.limit(value); return this; },
          toArray: async () => cursor.all() };
      },
    };
  } };
  const context = vm.createContext({ URL, URLSearchParams,
    process: { argv: ["node", "report", mode, JSON.stringify(args)] },
    console: { log: value => { output = JSON.parse(value); } },
  });
  const module = new vm.SourceTextModule(await readFile(new URL("../../toolchain/analytics/journey-report.mjs", import.meta.url), "utf8"), { context });
  await module.link(specifier => {
    const exports = specifier === "node:crypto" ? { createHash }
      : specifier.includes("database.mjs") ? { connect: async () => ({ db }), closePool: async () => { closed = true; } }
      : model;
    return new vm.SyntheticModule(Object.keys(exports), function () {
      for (const [key, value] of Object.entries(exports)) this.setExport(key, value);
    }, { context });
  });
  await module.evaluate();
  assert.equal(closed, true);
  return output;
}
const at = new Date(Date.now() - 1000);
test("private account report counts distinct tenant/accounts, resolves handles and bounds event detail", async () => {
  const data = {
    "account-activity": [
      { user: "auth0|one", tenant: "aesthetic", property: "aesthetic.computer", at, session: "raw-session", sequence: 1, action: "piece_opened", piece: "notepat" },
      { user: "auth0|one", tenant: "aesthetic", property: "aesthetic.computer", at, session: "raw-session", sequence: 2, action: "note_played", piece: "notepat" },
      { user: "auth0|two", tenant: "aesthetic", property: "nopaint.art", at, session: "another-session", sequence: 1, action: "piece_opened", piece: "nopaint" },
    ],
    "@handles": [{ _id: "auth0|one", handle: "painter" }],
  };
  const all = await report("accounts", { limit: 2 }, data);
  assert.deepEqual(all.totals, { accounts: 2, events: 3 });
  assert.equal(all.truncated, true);
  assert.equal(all.events.length, 2);
  assert.doesNotMatch(JSON.stringify(all), /auth0\||raw-session|another-session/);
  const one = await report("accounts", { handle: "@painter" }, data);
  assert.deepEqual(one.totals, { accounts: 1, events: 2 });
  assert.ok(one.events.every(row => row.account === "@painter"));
});
test("referral report separates old boots, excludes automation and unmeasured visits, and strips URLs", async () => {
  const result = await report("referrers", {}, {
    "network-visits": [
      { property: "aesthetic.computer", startedAt: at, automated: false, referrerHost: "example.org", interacted: true, engaged: true },
      { property: "aesthetic.computer", startedAt: at, automated: false },
      { property: "aesthetic.computer", startedAt: at, automated: true, referrerHost: "bot.example" },
    ],
    boots: [
      { createdAt: at, meta: { host: "aesthetic.computer", path: "/notepat", referrer: "https://example.org/path?secret=1", user: { sub: "private" } } },
      { createdAt: at, meta: { host: "aesthetic.computer", path: "/mail", referrer: "https://private.example/" } },
      { createdAt: at, meta: { host: "aesthetic.computer", path: "/", userAgent: "SomeBot" } },
    ],
  });
  assert.deepEqual(result.visits, [{ property: "aesthetic.computer", referrerHost: "example.org", visits: 1, interacted: 1, engaged: 1 }]);
  assert.deepEqual(result.legacyBoots, [{ property: "aesthetic.computer", referrerHost: "example.org", boots: 1 }]);
  assert.doesNotMatch(JSON.stringify(result), /secret|private|bot\.example/);
});
