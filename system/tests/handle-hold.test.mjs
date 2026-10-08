import test from "node:test";
import assert from "node:assert/strict";
import { createHandleHoldHandler, heldByOther, HOLDS } from "../backend/handle-hold.mjs";
import { validateHandle } from "../public/aesthetic.computer/lib/text.mjs";

// Just enough of a Mongo collection for the queries handle-hold makes.
function memoryDB(seed = {}) {
  const tables = { "@handles": [], [HOLDS]: [], ...seed };
  const matches = (doc, query) => Object.entries(query).every(([key, want]) => {
    if (key === "$or") return want.some((q) => matches(doc, q));
    const have = doc[key];
    if (want instanceof RegExp) return want.test(have);
    if (want && typeof want === "object" && !(want instanceof Date)) {
      if ("$gt" in want) return have > want.$gt;
      if ("$lte" in want) return have <= want.$lte;
      if ("$ne" in want) return have !== want.$ne;
    }
    return have === want;
  });
  const collection = (name) => {
    const rows = (tables[name] ??= []);
    return {
      createIndex: async () => {},
      findOne: async (query) => rows.find((doc) => matches(doc, query)) || null,
      deleteOne: async (query) => { const i = rows.findIndex((doc) => matches(doc, query)); if (i >= 0) rows.splice(i, 1); },
      deleteMany: async (query) => { for (let i = rows.length - 1; i >= 0; i--) if (matches(rows[i], query)) rows.splice(i, 1); },
      updateOne: async (query, { $set }, { upsert } = {}) => {
        const doc = rows.find((d) => matches(d, query));
        if (doc) return Object.assign(doc, $set);
        if (!upsert) return;
        if (rows.some((d) => d._id === query._id)) throw Object.assign(new Error("dup"), { code: 11000 });
        rows.push({ _id: query._id, ...$set });
      },
    };
  };
  return { tables, db: { collection } };
}

const A = "11111111-1111-4111-8111-111111111111";
const B = "22222222-2222-4222-8222-222222222222";

function setup(seed) {
  const store = memoryDB(seed);
  let clock = new Date("2026-10-08T12:00:00Z");
  const handler = createHandleHoldHandler({
    connect: async () => ({ db: store.db, disconnect: async () => {} }),
    validateHandle,
    filter: (text) => text.replace(/heck/gi, "****"),
    handleQuarantined: async (_db, handle) => handle.toLowerCase() === "gone",
    now: () => clock,
  });
  const get = async (handle, attempt) => {
    const res = await handler({ httpMethod: "GET", headers: {}, queryStringParameters: { handle, attempt } });
    return { code: res.statusCode, ...JSON.parse(res.body) };
  };
  const hold = async (handle, attempt) => {
    const res = await handler({ httpMethod: "POST", headers: {}, body: JSON.stringify({ handle, attempt }) });
    return { code: res.statusCode, ...JSON.parse(res.body) };
  };
  return { store, get, hold, later: (ms) => { clock = new Date(+clock + ms); } };
}

test("availability names taken, quarantined, invalid and filtered handles", async () => {
  const { get } = setup({ "@handles": [{ _id: "auth0|1", handle: "Jeffrey" }, { _id: "auth0|2", handle: "axb" }] });
  assert.equal((await get("jeffrey")).status, "taken"); // case-insensitive
  assert.equal((await get("gone")).status, "taken");
  assert.equal((await get("a..b")).status, "invalid");
  assert.equal((await get("oheck")).reason, "naughty");
  assert.equal((await get("pinkfrog")).status, "free");
  // A dot in a handle is literal, not a regex wildcard.
  assert.equal((await get("a.b")).status, "free");
});

test("a hold keeps the name for its attempt only, and lapses after ten minutes", async () => {
  const { get, hold, later, store } = setup();
  assert.equal((await hold("pinkfrog", A)).held, true);
  assert.equal((await get("pinkfrog", A)).status, "yours");
  assert.equal((await get("pinkfrog", B)).status, "held");
  assert.equal((await hold("pinkfrog", B)).code, 409);
  assert.equal(await heldByOther(store.db, "PinkFrog", B, new Date("2026-10-08T12:05:00Z")), true);
  assert.equal(await heldByOther(store.db, "pinkfrog", A, new Date("2026-10-08T12:05:00Z")), false);
  later(10 * 60 * 1000 + 1);
  assert.equal((await get("pinkfrog", B)).status, "free");
  assert.equal((await hold("pinkfrog", B)).held, true);
});

test("changing your mind releases the earlier name; holds need a real attempt id", async () => {
  const { get, hold } = setup();
  await hold("first", A);
  await hold("second", A);
  assert.equal((await get("first", B)).status, "free");
  assert.equal((await get("second", B)).status, "held");
  assert.equal((await hold("third", "not-a-uuid")).code, 400);
});
