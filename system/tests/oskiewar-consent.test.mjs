// The consent wall's reading of a player's answer.
//
// The wall is the only door to a generation capability, so the interesting
// tests are the refusals: what a player did NOT tick must never survive into
// the frozen fields, and an ask the desk has no registered term for must fail
// rather than pass through. See the working proposal (vault:
// regarde/proposals/oskiewar-regarde) for the grant these fields become.
import assert from "node:assert/strict";
import test from "node:test";
import { readScope, frozenFields, SOURCES, OUTPUTS, DISTRIBUTION, SEPARATE }
  from "../netlify/functions/oskiewar-consent.mjs";

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

test("the wall and the endpoint agree on the vocabulary", async () => {
  // The card the player reads is built from its own copy of these lists. If
  // the two drift, a box a player can tick becomes an unregistered term and
  // the ask fails at the desk with nothing on screen to explain why.
  const { readFile } = await import("node:fs/promises");
  const card = await readFile(
    new URL("../../xbox/live/oskiewar-wizard.mjs", import.meta.url), "utf8");
  for (const value of [...SOURCES, ...OUTPUTS, ...DISTRIBUTION, ...SEPARATE])
    assert.ok(card.includes(`"${value}"`), `wall is missing ${value}`);
});
