// node --experimental-vm-modules --test system/tests/sotce-route-fallback.test.mjs
import test from "node:test";
import assert from "node:assert/strict";
import vm from "node:vm";
import crypto from "node:crypto";
import { readFile } from "node:fs/promises";

const unexpected = () => { throw new Error("Unexpected external operation"); };
const mocks = {
  "../../backend/sotce-net-constants.mjs": Object.fromEntries([
    "SOTCE_STRIPE_API_PRIV_KEY", "SOTCE_STRIPE_API_PUB_KEY",
    "SOTCE_STRIPE_API_TEST_PRIV_KEY", "SOTCE_STRIPE_API_TEST_PUB_KEY",
    "SOTCE_STRIPE_ENDPOINT_DEV_SECRET", "SOTCE_STRIPE_ENDPOINT_SECRET",
    "priceId", "productId", "prodProductId",
  ].map((name) => [name, "synthetic"])),
  "../../public/aesthetic.computer/lib/helpers.mjs": {
    defaultTemplateStringProcessor: (strings, ...values) =>
      strings.reduce((text, part, i) => text + part + (values[i] ?? ""), ""),
  },
  "../../backend/http.mjs": {
    respond: (statusCode, body, headers) => ({ statusCode, body, headers }),
  },
  "../../backend/database.mjs": { connect: unexpected, ObjectId: unexpected },
  "../../backend/shell.mjs": { shell: { error: unexpected } },
  "../../backend/authorization.mjs": Object.fromEntries([
    "authorize", "deleteUser", "hasAdmin", "handleFor", "getHandleOrEmail", "userIDFromEmail",
  ].map((name) => [name, unexpected])),
  "../../backend/kv.mjs": { connect: unexpected },
  "../../../shared/push.mjs": { broadcastToTopic: unexpected, sendToUser: unexpected },
  stripe: { default: unexpected },
  "node:crypto": { default: crypto },
  "../../backend/notification-choice.mjs": { notificationChoiceUpdate: unexpected },
};
const context = vm.createContext({ process: { env: {} }, console });
const module = new vm.SourceTextModule(
  await readFile(new URL("../netlify/functions/sotce-net.mjs", import.meta.url), "utf8"),
  { context },
);
await module.link((name) => {
  assert.ok(mocks[name], `missing mock ${name}`);
  return new vm.SyntheticModule(Object.keys(mocks[name]), function () {
    for (const [key, value] of Object.entries(mocks[name])) this.setExport(key, value);
  }, { context });
});
await module.evaluate();

test("unknown paths return 404 without touching external services", async () => {
  for (const path of ["/.git/config", "/.env", "/robots.txt", "/.well-known/security.txt", "/api/sotce-net/missing"]) {
    const response = await module.namespace.handler({ httpMethod: "GET", path, headers: {} });
    assert.equal(response?.statusCode, 404, path);
  }
});

test("known static routes still return their own responses", async () => {
  const response = await module.namespace.handler({ httpMethod: "GET", path: "/sw.js", headers: {} });
  assert.equal(response.statusCode, 200);
  assert.match(response.body, /addEventListener\("push"/);
});


test("unsupported methods also return a response without external services", async () => {
  for (const httpMethod of ["POST", "HEAD", "OPTIONS"]) {
    const response = await module.namespace.handler({ httpMethod, path: "/missing", headers: {} });
    assert.equal(response?.statusCode, 404, httpMethod);
  }
});
