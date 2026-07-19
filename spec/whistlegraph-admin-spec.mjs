import {
  curationPayload,
  isWhistlegraphAdmin,
  normalizePostPatch,
  normalizeWorkPatch,
} from "../system/netlify/functions/whistlegraph-admin-lib.mjs";
import { createHandler } from "../system/netlify/functions/whistlegraph-admin.mjs";

const JEFFREY = "auth0|63effeeb2a7d55f8098d62f9";
const MINANIMALS = "auth0|6414a4fb936cc041cfc1f011";

function memoryDatabase() {
  const records = new Map();
  const audit = [];
  const cursor = (rows) => ({
    sort() { return this; },
    limit(n) { rows = rows.slice(0, n); return this; },
    async toArray() { return rows; },
  });
  const collection = (name) => {
    if (name === "whistlegraph-curation-audit") {
      return {
        async createIndex() {},
        async insertOne(row) { audit.push(row); },
        find() { return cursor([...audit].reverse()); },
      };
    }
    return {
      async createIndex() {},
      find() { return cursor([...records.values()]); },
      async findOne({ _id }) { return records.get(_id) || null; },
      async updateOne({ _id }, { $set }) { records.set(_id, { _id, ...$set }); },
      async deleteOne({ _id }) { records.delete(_id); },
    };
  };
  return {
    records,
    audit,
    connection: { db: { collection }, async disconnect() {} },
  };
}

describe("Whistlegraph archive desk", () => {
  it("authorizes immutable AC subjects, not claimed handles", () => {
    const allowed = new Set([JEFFREY, MINANIMALS]);
    expect(isWhistlegraphAdmin({ sub: JEFFREY, email_verified: true }, allowed)).toBeTrue();
    expect(isWhistlegraphAdmin({ sub: "auth0|lookalike", email_verified: true, handle: "jeffrey" }, allowed)).toBeFalse();
    expect(isWhistlegraphAdmin({ sub: MINANIMALS, email_verified: false }, allowed)).toBeFalse();
  });

  it("validates post relationships and work metadata", () => {
    expect(normalizePostPatch({ kind: "talk", works: ["sos", "sos"], plots: ["A plot"], desc: " hello " }, new Set(["sos"])))
      .toEqual({ kind: "talk", works: ["sos"], plots: ["A plot"], desc: "hello" });
    expect(() => normalizePostPatch({ works: ["missing"] }, new Set(["sos"]))).toThrowError(/Unknown Whistlegraph code/);
    expect(normalizeWorkPatch({ title: " Scared of Stairs ", by: "Jeffrey", year: 2021, c: "#B44887" }))
      .toEqual({ title: "Scared of Stairs", by: "Jeffrey", year: 2021, c: "#b44887" });
  });

  it("publishes only patch data and a revision", () => {
    const payload = curationPayload([
      { type: "post", key: "123", patch: { works: ["sos"] }, updatedAt: "2026-07-19T10:00:00Z", updatedBy: JEFFREY },
      { type: "work", key: "sos", patch: { title: "Steps" }, updatedAt: "2026-07-19T11:00:00Z", updatedBy: MINANIMALS },
    ]);
    expect(payload.posts).toEqual({ "123": { works: ["sos"] } });
    expect(payload.works).toEqual({ sos: { title: "Steps" } });
    expect(payload.revision).toBe("2026-07-19T11:00:00.000Z");
    expect(JSON.stringify(payload)).not.toContain("auth0|");
  });

  it("rejects non-admin mutations and accepts a validated admin patch", async () => {
    const memory = memoryDatabase();
    let user = { sub: "auth0|lookalike", email_verified: true };
    const handler = createHandler({
      authorizeFn: async () => user,
      handleForFn: async () => "jeffrey",
      connectFn: async () => memory.connection,
      allowedSubs: new Set([JEFFREY, MINANIMALS]),
      loadModelFn: () => ({ workCodes: new Set(["sos", "imab"]), postIds: new Set(["123"]) }),
    });
    const event = {
      httpMethod: "PATCH",
      headers: { authorization: "Bearer test" },
      queryStringParameters: {},
      body: JSON.stringify({ type: "post", key: "123", patch: { kind: "performance", works: ["sos", "imab"], plots: [] } }),
    };
    expect((await handler(event)).statusCode).toBe(403);

    user = { sub: MINANIMALS, email_verified: true };
    const saved = await handler(event);
    expect(saved.statusCode).toBe(200);
    expect(JSON.parse(saved.body).patch.works).toEqual(["sos", "imab"]);
    expect(memory.records.get("post:123").updatedBy).toBe(MINANIMALS);
    expect(memory.audit.length).toBe(1);

    const publicRead = await handler({ httpMethod: "GET", headers: {}, queryStringParameters: { action: "curation" } });
    expect(publicRead.statusCode).toBe(200);
    expect(JSON.parse(publicRead.body).posts["123"].works).toEqual(["sos", "imab"]);
  });
});
