// The consent wall's reading of a player's answer.
//
// The wall is the only door to a generation capability, so the interesting
// tests are the refusals: what a player did NOT tick must never survive into
// the frozen fields, and an ask the desk has no registered term for must fail
// rather than pass through. See the working proposal (vault:
// regarde/proposals/oskiewar-regarde) for the grant these fields become.
import assert from "node:assert/strict";
import test, { mock } from "node:test";
mock.module('../backend/authorization.mjs', { exports: { authorize: async () => ({ sub: 'auth0|fixture' }) } });
const { readScope, frozenFields, SEPARATE, handler } = await import("../netlify/functions/oskiewar-consent.mjs");

// The narrowest answer the wall can send: look at me, make a picture, show
// nobody. Every test below is this with one thing changed.
const minimal = {
  source: ["appearance"],
  outputs: ["portrait"],
  distribution: ["private_preview"],
  retention: "bound_to_purpose_scope",
};

test("the minimal answer is a grant", () => {
  const { scope, error } = readScope(minimal);
  assert.equal(error, undefined);
  assert.deepEqual(scope.source, ["appearance"]);
});

test('explicit consent choices retry identically but a new choice gets a new receipt key', async () => {
  const saved = {...process.env}, originalFetch = globalThis.fetch, sent = [];
  Object.assign(process.env, {REGARDE_GATEWAY_URL:'https://gate.invalid/v0/gateway', REGARDE_SUBJECT_SALT:'fixture', REGARDE_GATEWAY_TOKEN:'fixture'});
  globalThis.fetch = async (url, options) => { sent.push(JSON.parse(options.body)); return Response.json({outcome:'refuse'}); };
  const ask = requestId => handler({httpMethod:'POST',headers:{},body:JSON.stringify({...minimal, requestId})});
  try {
    await ask('11111111-1111-1111-1111-111111111111');
    await ask('11111111-1111-1111-1111-111111111111');
    await ask('22222222-2222-2222-2222-222222222222');
    assert.equal(sent[0].idempotency_key, sent[1].idempotency_key);
    assert.notEqual(sent[0].idempotency_key, sent[2].idempotency_key);
    assert.equal((await ask('bad')).statusCode, 400);
    assert.equal(sent.length, 3);
    assert.ok(!JSON.stringify(sent).includes('auth0|fixture'));
  } finally {
    globalThis.fetch = originalFetch;
    for (const key of ['REGARDE_GATEWAY_URL','REGARDE_SUBJECT_SALT','REGARDE_GATEWAY_TOKEN']) {
      if (saved[key] === undefined) delete process.env[key]; else process.env[key] = saved[key];
    }
  }
});

test("nothing defaults on", () => {
  const { scope } = readScope(minimal);
  // Marketing, merchandise and model training are the three a player is most
  // likely to agree to by momentum. Silence is not agreement.
  for (const key of SEPARATE) assert.equal(scope[key], false, key);
  const frozen = frozenFields(scope).purpose_scope;
  for (const key of SEPARATE) assert.equal(frozen[key], false, key);
});

test("an empty category is refused rather than read as permission", () => {
  for (const key of ["source", "outputs", "distribution"])
    assert.ok(readScope({ ...minimal, [key]: [] }).error, key);
});

test("a term the schema has not registered fails the ask", () => {
  // Not merely dropped: a word nobody has decided the meaning of must not
  // travel to the desk under a grant that appears to cover it.
  assert.ok(readScope({ ...minimal, outputs: ["portrait", "hologram"] }).error);
  assert.ok(readScope({ ...minimal, source: ["dna"] }).error);
  assert.ok(readScope({ ...minimal, retention: "forever" }).error);
});

test("retention is chosen, never inherited", () => {
  assert.ok(readScope({ ...minimal, retention: undefined }).error);
});

test("appearance does not imply voice", () => {
  // The coupling the proposal names by hand. A player who offered a face has
  // not offered a throat, so match audio without a voice sample is an ask for
  // something nobody put on the table.
  const { error } = readScope({ ...minimal, outputs: ["portrait", "match_audio"] });
  assert.match(error, /voice/);
  const allowed = readScope({ ...minimal, source: ["appearance", "voice"],
    outputs: ["portrait", "match_audio"] });
  assert.equal(allowed.error, undefined);
});

test("a body needs appearance references", () => {
  assert.ok(readScope({ ...minimal, source: ["biography"],
    outputs: ["fighter_mesh"] }).error);
});

test("the frozen fields are the grant the proposal describes", () => {
  const { scope } = readScope({ ...minimal, source: ["appearance", "movement"],
    outputs: ["portrait", "fighter_animation"],
    distribution: ["private_preview", "local_gameplay"] });
  const frozen = frozenFields(scope);
  assert.equal(frozen.data_class, "digital_replica_source");
  assert.equal(frozen.operation_kind, "GRANT_CONSENT");
  assert.equal(frozen.purpose_scope.purpose, "oskiewar_fighter_generation");
  assert.equal(frozen.purpose_scope.transform, "stylized");
  assert.equal(frozen.retention_constraint, "bound_to_purpose_scope");
  assert.equal(frozen.schema_version, 1);
});

test("duplicate ticks cannot inflate a category", () => {
  const { scope } = readScope({ ...minimal, source: ["appearance", "appearance"] });
  assert.deepEqual(scope.source, ["appearance"]);
});

test("photo and voice map to narrow, valid REGARDE asks", async () => {
  const { scopeForMedia } = await import("../../xbox/live/oskiewar-wizard.mjs");
  for (const [photo, voice] of [[true, false], [false, true], [true, true]]) {
    const answer = scopeForMedia({ photo, voice });
    assert.equal(readScope(answer).error, undefined);
    assert.deepEqual(answer.source, [...(photo ? ["appearance"] : []), ...(voice ? ["voice"] : [])]);
    assert.deepEqual(answer.outputs, [...(photo ? ["fighter_mesh"] : []), ...(voice ? ["match_audio"] : [])]);
    assert.deepEqual(answer.distribution, ["private_preview", "local_gameplay"]);
    assert.equal(answer.retention, "bound_to_purpose_scope");
    for (const key of SEPARATE) assert.equal(answer[key], false);
  }
  assert.ok(readScope(scopeForMedia({ photo: false, voice: false })).error);
});
