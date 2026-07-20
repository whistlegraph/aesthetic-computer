import {
  curationPayload,
  DEFAULT_WHISTLEGRAPH_ADMIN_SUBS,
  deriveVisualTags,
  isWhistlegraphAdmin,
  normalizeDeployRequest,
  normalizePostPatch,
  normalizeWorkCode,
  normalizeWorkPatch,
  validateDeployEvidence,
  visualSearchText,
} from "../system/netlify/functions/whistlegraph-admin-lib.mjs";
import { createHandler } from "../system/netlify/functions/whistlegraph-admin.mjs";

const JEFFREY = "auth0|63effeeb2a7d55f8098d62f9";
const MINANIMALS = "auth0|6414a4fb936cc041cfc1f011";

function memoryDatabase() {
  const records = new Map();
  const audit = [];
  const indexCalls = [];
  const cursor = (rows) => ({
    sort() { return this; },
    limit(n) { rows = rows.slice(0, n); return this; },
    async toArray() { return rows; },
  });
  const collection = (name) => {
    if (name === "whistlegraph-curation-audit") {
      return {
        async createIndex(spec) { indexCalls.push({ name, spec }); },
        async insertOne(row) { audit.push(row); },
        find() { return cursor([...audit].reverse()); },
      };
    }
    return {
      async createIndex(spec) { indexCalls.push({ name, spec }); },
      find() { return cursor([...records.values()]); },
      async findOne({ _id }) { return records.get(_id) || null; },
      async updateOne({ _id }, { $set }) { records.set(_id, { _id, ...$set }); },
      async deleteOne({ _id }) { records.delete(_id); },
    };
  };
  return {
    records,
    audit,
    indexCalls,
    connection: { db: { collection }, async disconnect() {} },
  };
}

describe("Whistlegraph Desk", () => {
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
    expect(normalizeWorkCode(" FEAR ")).toBe("fear");
    expect(() => normalizeWorkCode("not-a-code")).toThrowError(/letters or numbers/);
  });

  it("publishes only patch data and a revision", () => {
    const payload = curationPayload([
      { type: "post", key: "123", patch: { works: ["sos"] }, updatedAt: "2026-07-19T10:00:00Z", updatedBy: JEFFREY },
      { type: "work", key: "sos", patch: { title: "Steps" }, updatedAt: "2026-07-19T11:00:00Z", updatedBy: MINANIMALS },
    ]);
    expect(payload.posts).toEqual({ "123": { works: ["sos"] } });
    expect(payload.works).toEqual({ sos: { title: "Steps" } });
    expect(payload.createdWorks).toEqual({});
    expect(payload.deletedWorks).toEqual([]);
    expect(payload.revision).toBe("2026-07-19T11:00:00.000Z");
    expect(JSON.stringify(payload)).not.toContain("auth0|");
  });

  it("indexes every machine-read visual field and derives stable visual tags", () => {
    const record = {
      visual: {
        summary: "A hand draws a spiral in chalk.",
        sequence: ["The mark grows into an oval."],
        setting: "Outdoor pavement",
        subjects: ["Two hands"],
        drawingSurface: "Sidewalk",
        toolsAndMaterials: ["Blue chalk"],
        marksAndForms: ["Spiral", "oval"],
        visibleText: ["SEASONS"],
        camera: "Overhead handheld view",
        uncertainties: ["The small mark may be a leaf."],
      },
    };
    const text = visualSearchText(record);
    expect(text).toContain("seasons");
    expect(text).toContain("small mark may be a leaf");
    expect(deriveVisualTags(record)).toEqual([
      "drawing", "chalk", "outdoors", "multiple-hands", "botanical",
      "geometric", "spiral", "overhead", "handheld", "visible-text",
    ]);
  });

  it("keeps machine visuals behind immutable-sub authentication", async () => {
    const record = {
      postId: "123",
      visual: { summary: "A hand draws a blue chalk spiral.", visibleText: [] },
      autoTags: ["drawing", "chalk", "spiral"],
    };
    const loadVisualsFn = () => ({
      metadata: { count: 1, promptVersion: "test-v1" },
      searchable: [{ id: "123", text: "a hand draws a blue chalk spiral drawing chalk spiral" }],
      byId: new Map([["123", record]]),
    });
    let user = { sub: "auth0|lookalike", email_verified: true };
    const handler = createHandler({
      authorizeFn: async () => user,
      allowedSubs: new Set([JEFFREY, MINANIMALS]),
      loadVisualsFn,
    });
    const event = (action, params = {}) => ({
      httpMethod: "GET",
      headers: { authorization: "Bearer test" },
      queryStringParameters: { action, ...params },
    });
    expect((await handler(event("visual-search", { q: "chalk blue" }))).statusCode).toBe(403);

    user = { sub: JEFFREY, email_verified: true };
    const search = await handler(event("visual-search", { q: "chalk blue" }));
    expect(search.statusCode).toBe(200);
    expect(JSON.parse(search.body).ids).toEqual(["123"]);
    const detail = await handler(event("visual", { id: "123" }));
    expect(detail.statusCode).toBe(200);
    expect(JSON.parse(detail.body).record.visual.summary).toContain("blue chalk spiral");
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

  it("ensures mutation indexes once per warm handler instead of on every save", async () => {
    const memory = memoryDatabase();
    const handler = createHandler({
      authorizeFn: async () => ({ sub: MINANIMALS, email_verified: true }),
      connectFn: async () => memory.connection,
      allowedSubs: new Set([JEFFREY, MINANIMALS]),
      loadModelFn: () => ({ workCodes: new Set(["sos"]), postIds: new Set(["123"]) }),
    });
    const event = {
      httpMethod: "PATCH",
      headers: { authorization: "Bearer test" },
      queryStringParameters: {},
      body: JSON.stringify({ type: "post", key: "123", patch: { kind: "performance", works: ["sos"], plots: ["Artsy Math"] } }),
    };
    expect((await handler(event)).statusCode).toBe(200);
    expect((await handler(event)).statusCode).toBe(200);
    expect(memory.indexCalls).toEqual([
      { name: "whistlegraph-curation", spec: { type: 1, key: 1 } },
      { name: "whistlegraph-curation-audit", spec: { when: -1 } },
    ]);
  });

  it("creates, soft-deletes, and restores a whistlegraph without deleting archive data", async () => {
    const memory = memoryDatabase();
    const handler = createHandler({
      authorizeFn: async () => ({ sub: MINANIMALS, email_verified: true }),
      connectFn: async () => memory.connection,
      allowedSubs: new Set([JEFFREY, MINANIMALS]),
      loadModelFn: () => ({ workCodes: new Set(["sos"]), workByCode: new Map([["sos", { title: "Stairs" }]]), postIds: new Set(["123"]), postWorks: new Map([["123", ["fear"]]]) }),
    });
    const headers = { authorization: "Bearer test" };
    const created = await handler({ httpMethod: "POST", headers, queryStringParameters: { action: "work" }, body: JSON.stringify({ code: "fear", title: "Inside My Mouth", by: "Alex Freundlich", year: 2026, c: "#b44887" }) });
    expect(created.statusCode).toBe(201);
    expect(memory.records.get("work:fear").created).toBeTrue();

    const removed = await handler({ httpMethod: "DELETE", headers, queryStringParameters: {}, body: JSON.stringify({ type: "work", key: "fear" }) });
    expect(removed.statusCode).toBe(200);
    expect(memory.records.get("work:fear").deleted).toBeTrue();
    let publicData = JSON.parse((await handler({ httpMethod: "GET", headers: {}, queryStringParameters: { action: "curation" } })).body);
    expect(publicData.deletedWorks).toEqual(["fear"]);
    expect(publicData.trashedWorks).toBeUndefined();
    const privateData = JSON.parse((await handler({ httpMethod: "GET", headers, queryStringParameters: { action: "data" } })).body);
    expect(privateData.trashedWorks.fear.title).toBe("Inside My Mouth");

    const restored = await handler({ httpMethod: "PATCH", headers, queryStringParameters: {}, body: JSON.stringify({ type: "work", key: "fear", restore: true }) });
    expect(restored.statusCode).toBe(200);
    publicData = JSON.parse((await handler({ httpMethod: "GET", headers: {}, queryStringParameters: { action: "curation" } })).body);
    expect(publicData.createdWorks.fear.title).toBe("Inside My Mouth");
    expect(publicData.deletedWorks).toEqual([]);
    expect(publicData.activeCodes).toEqual(["fear"]);

    const renamed = await handler({ httpMethod: "POST", headers, queryStringParameters: { action: "work-rename" }, body: JSON.stringify({ from: "fear", code: "mouth", title: "Inside My Mouth", by: "Alex Freundlich", year: 2026, c: "#b44887" }) });
    expect(renamed.statusCode).toBe(200);
    expect(JSON.parse(renamed.body).reassigned).toBe(1);
    expect(memory.records.get("post:123").patch.works).toEqual(["mouth"]);
    expect(memory.records.get("work:fear").deleted).toBeTrue();
    expect(memory.records.get("work:mouth").created).toBeTrue();
    expect(memory.records.get("work:mouth").patch.asset).toBe("fear");

    const edited = await handler({ httpMethod: "PATCH", headers, queryStringParameters: {}, body: JSON.stringify({ type: "work", key: "mouth", patch: { title: "Inside My Mouth!", by: "Alex Freundlich", year: 2026, c: "#b44887" } }) });
    expect(edited.statusCode).toBe(200);
    expect(memory.records.get("work:mouth").patch.asset).toBe("fear");
  });
});

describe("Whistlegraph Desk publishing", () => {
  const commit = "a".repeat(40);
  const changeId = "mabcdef0-12345678";
  const branch = `whistlegraph/minanimals/${changeId}`;

  it("keeps both maintainers in the default immutable-sub allowlist", () => {
    expect(DEFAULT_WHISTLEGRAPH_ADMIN_SUBS).toContain(JEFFREY);
    expect(DEFAULT_WHISTLEGRAPH_ADMIN_SUBS).toContain(MINANIMALS);
  });

  it("normalizes an exact-SHA deployment request", () => {
    expect(normalizeDeployRequest({ commit: commit.toUpperCase(), changeId, branch })).toEqual({ commit, changeId, branch });
    expect(() => normalizeDeployRequest({ commit: "main", changeId, branch })).toThrowError(/40-character/);
    expect(() => normalizeDeployRequest({ commit, changeId, branch: "main" })).toThrowError(/branch/);
    expect(() => normalizeDeployRequest({ commit, changeId, branch: `whistlegraph/jeffrey/not-${changeId}` })).toThrowError(/branch/);
  });

  it("independently verifies commit scope and Auth0 attribution", () => {
    const evidence = {
      commit,
      changeId,
      actorSub: MINANIMALS,
      originHead: commit,
      branchHead: commit,
      ancestry: `${commit} ${"b".repeat(40)}`,
      changed: ["system/public/whistlegraph.org/index.html"],
      treeEntries: [`100644 blob ${"c".repeat(40)}\tsystem/public/whistlegraph.org/index.html`],
      message: `Update the index\n\nWhistlegraph-Actor-Sub: ${MINANIMALS}\nWhistlegraph-Change: ${changeId}\n`,
    };
    expect(validateDeployEvidence(evidence).changed).toEqual(evidence.changed);
    expect(() => validateDeployEvidence({ ...evidence, changed: ["lith/server.mjs"] })).toThrowError(/allowlist/);
    expect(() => validateDeployEvidence({ ...evidence, treeEntries: [`120000 blob ${"c".repeat(40)}\tsystem/public/whistlegraph.org/link`] })).toThrowError(/Symlinks/);
    expect(() => validateDeployEvidence({ ...evidence, actorSub: JEFFREY })).toThrowError(/attribution/);
  });

  it("queues a validated deployment and audits the Auth0 sub", async () => {
    const inserted = [];
    const deploys = [];
    const collection = {
      createIndex: async () => {},
      insertOne: async (document) => { inserted.push(document); },
      updateOne: async ({ _id }, { $set }) => Object.assign(inserted.find((document) => document._id === _id), $set),
    };
    const handler = createHandler({
      authorizeFn: async () => ({ sub: MINANIMALS, email_verified: true }),
      connectFn: async () => ({ db: { collection: () => collection }, disconnect: async () => {} }),
      deployFn: async (request) => { deploys.push(request); return { queued: true, commit: request.commit }; },
    });
    const result = await handler({
      httpMethod: "POST",
      headers: { authorization: "Bearer test" },
      queryStringParameters: { action: "deploy" },
      body: JSON.stringify({ commit, changeId, branch }),
    });
    const body = JSON.parse(result.body);
    expect(result.statusCode).toBe(202);
    expect(body.status).toBe("queued");
    expect(deploys.length).toBe(1);
    expect(inserted.length).toBe(1);
    expect(inserted[0].actorSub).toBe(MINANIMALS);
    expect(inserted[0].commit).toBe(commit);
  });

  it("rejects a lookalike handle before invoking deployment", async () => {
    let called = false;
    const handler = createHandler({
      authorizeFn: async () => ({ sub: "auth0|other", handle: "minanimals", email_verified: true }),
      deployFn: async () => { called = true; },
    });
    const result = await handler({ httpMethod: "POST", headers: {}, queryStringParameters: { action: "deploy" }, body: JSON.stringify({ commit, changeId, branch }) });
    expect(result.statusCode).toBe(403);
    expect(called).toBeFalse();
  });
});
