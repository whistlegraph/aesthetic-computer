// Run: node --experimental-vm-modules spec/mail-address-spec.mjs
// Production recipient resolution and send routing, without network or mail.
import assert from "node:assert/strict";
import vm from "node:vm";
import { readFile } from "node:fs/promises";

const context = vm.createContext({ console });
async function load(path, mocks) {
  const module = new vm.SourceTextModule(await readFile(new URL(path, import.meta.url), "utf8"), { context });
  await module.link(async (name) => {
    assert.ok(mocks[name], `missing mock ${name}`);
    const exports = mocks[name];
    const mock = new vm.SyntheticModule(Object.keys(exports), function () {
      for (const [key, value] of Object.entries(exports)) this.setExport(key, value);
    }, { context });
    await mock.link(() => {});
    await mock.evaluate();
    return mock;
  });
  await module.evaluate();
  return module.namespace;
}
const handles = new Map([["q13R608tx", "reader"], ["prutti", "other"], ["same", "lower"], ["SAME", "upper"], ["a.b", "literal"]]);
const database = { disconnect: async () => {}, db: { collection: (name) => {
  if (name === "users") return { findOne: async ({ code }) => code === "ac25abcde" ? { _id: "permanent" } : null };
  if (name === "tells") return { createIndex: async () => {} };
  assert.equal(name, "@handles");
  return { find: ({ handle }, options) => {
    assert.ok(options.maxTimeMS <= 2000);
    const regex = new RegExp(handle.$regex, handle.$options);
    return { limit: (count) => ({ toArray: async () => [...handles.keys()].filter((h) => regex.test(h)).slice(0, count).map((handle) => ({ handle })) }) };
  } };
} } };
const backend = await load("../system/backend/mail.mjs", {
  "./mail-events.mjs": { observeMail: () => {} },
  "./authorization.mjs": { handleFor: () => {}, userIDFromHandleOrEmail: async (h) => handles.get(h) },
  "./filter.mjs": { filter: (s) => s },
  "../../shared/push.mjs": { sendToUser: () => {} },
  "../../shared/mail-privacy.mjs": { letterNotification: () => {}, mailErrorCode: () => {}, quietMailPush: () => {} },
  "./mail-media.mjs": { resolveMailMedia: () => {}, outsideMediaBody: () => {} },
});
for (const address of ["@q13R608tx", " @Q13R608TX ", "q13r608tx", "Q13R608TX@AESTHETIC.COMPUTER", "q13r608tx@mail.aesthetic.computer"]) {
  assert.equal(await backend.subFromAddress(address, database), "reader", address);
}
for (const address of ["ac25abcde", "@AC25ABCDE", "AC25ABCDE@aesthetic.computer"]) {
  assert.equal(await backend.subFromAddress(address, database), "permanent", address);
}
assert.equal(await backend.subFromAddress("@same", database), "lower", "exact case wins");
assert.equal(await backend.subFromAddress("@SAME", database), "upper", "exact case wins");
assert.equal(await backend.subFromAddress("@Same", database), undefined, "ambiguous case never chooses a recipient");
assert.equal(await backend.subFromAddress("@A.B", database), "literal", "regex punctuation is literal");
for (const address of ["@q13", "@tyttebear", "@.*", "@a+b", "outside@example.invalid", "@", "", null, {}]) {
  assert.equal(await backend.subFromAddress(address, database), undefined, String(address));
}

const deliveries = [], outside = [];
const api = await load("../system/netlify/functions/mail.mjs", {
  "../../backend/mail-events.mjs": { mailTrace: () => "test", recordMailEvent: () => {} },
  "../../backend/authorization.mjs": { authorize: async () => ({ sub: "sender" }) },
  "../../backend/database.mjs": { connect: async () => database },
  "../../backend/http.mjs": { respond: (statusCode, body, headers) => ({ statusCode, body, headers }) },
  "../../backend/mail-media.mjs": { attachmentList: () => {}, attachmentThumbnail: () => {}, resolveMailMedia: () => {} },
  "../../backend/mail.mjs": { ...backend,
    deliver: async (options) => { deliveries.push(options); return { toHandle: "@q13R608tx" }; },
    sendOutside: async (options) => { outside.push(options); return {}; },
  },
  "../../../shared/mail-privacy.mjs": { mailErrorCode: () => "UNKNOWN" },
  mongodb: { ObjectId: class {} },
});
const send = (to) => api.handler({ httpMethod: "POST", headers: {}, body: JSON.stringify({ to, text: "Synthetic test letter" }) });
assert.equal((await send("@Q13R608TX")).statusCode, 200);
assert.equal(deliveries[0].to, "reader");
for (const to of ["@q13", "q13@aesthetic.computer", "q13@mail.aesthetic.computer", "@Same"]) {
  assert.equal((await send(to)).statusCode, 404, to);
}
assert.equal(outside.length, 0, "unknown local recipients never fall through to SMTP");
assert.equal((await send("outside@example.invalid")).statusCode, 200);
assert.equal(outside.length, 1);
console.log("mail address spec passed: full handles, case, ambiguity, permahandles and SMTP boundaries");
