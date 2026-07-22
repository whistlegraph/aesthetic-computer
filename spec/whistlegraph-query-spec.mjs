import { listCollection } from "../system/netlify/functions/whistlegraph-query.mjs";

function collectionWith(rows) {
  const calls = { count: null, find: null, sort: null, skip: null, limit: null };
  const cursor = {
    sort(value) { calls.sort = value; return this; },
    skip(value) { calls.skip = value; return this; },
    limit(value) { calls.limit = value; return this; },
    async toArray() { return rows.slice(0, calls.limit); },
  };
  return {
    calls,
    async countDocuments(query) { calls.count = query; return 123; },
    find(query) { calls.find = query; return cursor; },
  };
}

describe("Whistlegraph Mongo query pages", () => {
  it("uses the indexed workCount field for unfiled posts", async () => {
    const collection = collectionWith([{ _id: "1", id: "1", date: "2026-01-01" }]);
    const page = await listCollection(collection, { kind: "unfiled", sort: "newest", limit: "60" }, "posts");
    expect(collection.calls.find).toEqual({ workCount: 0 });
    expect(collection.calls.sort).toEqual({ date: -1, _id: 1 });
    expect(page.total).toBe(123);
  });

  it("caps pages and returns an opaque continuation cursor", async () => {
    const rows = Array.from({ length: 101 }, (_, i) => ({ _id: String(i), id: String(i), views: 200 - i }));
    const collection = collectionWith(rows);
    const page = await listCollection(collection, { limit: "500" }, "posts");
    expect(page.items.length).toBe(100);
    expect(page.nextCursor).toEqual(jasmine.any(String));
    expect(collection.calls.limit).toBe(101);
  });

  it("routes taxonomy text through Mongo text search", async () => {
    const collection = collectionWith([]);
    await listCollection(collection, { q: "silo fuzz", limit: "20" }, "posts");
    expect(collection.calls.find).toEqual({ $text: { $search: "silo fuzz" } });
    expect(collection.calls.sort).toEqual({ score: { $meta: "textScore" }, _id: 1 });
  });
});
