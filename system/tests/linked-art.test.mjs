// linked-art.test.mjs — validate the Linked Art serializers (crm/SCORE.md Stage 1).
// Run: node --test system/tests/linked-art.test.mjs
import { test } from "node:test";
import assert from "node:assert/strict";
import {
  personToLinkedArt,
  paintingToLinkedArt,
  pieceToLinkedArt,
  moodToLinkedArt,
  LICENSES,
  isLicense,
  LA_CONTEXT,
  DATA_BASE,
  WEB_BASE,
} from "../backend/linked-art.mjs";

test("license registry", () => {
  assert.ok(isLicense("CC-BY-4.0"));
  assert.ok(isLicense("CC0-1.0"));
  assert.ok(!isLicense("All Rights Reserved"));
  assert.equal(LICENSES["CC-BY-4.0"][0], "https://creativecommons.org/licenses/by/4.0/");
});

test("person → E21 Person with seeAlso back to apex", () => {
  const doc = personToLinkedArt({ handle: "@sat", latestMood: { mood: "generative" } });
  assert.equal(doc["@context"], LA_CONTEXT);
  assert.equal(doc.id, `${DATA_BASE}/@sat`);
  assert.equal(doc.type, "Person");
  assert.equal(doc.identified_by[0].content, "@sat");
  const link = doc.subject_of.find((s) => s._label === "Aesthetic Computer profile");
  assert.equal(link.digitally_carried_by[0].access_point[0].id, `${WEB_BASE}/@sat`);
  assert.ok(doc.subject_of.some((s) => s.content === "generative"));
});

test("painting → DigitalObject with Production, author, rights, image", () => {
  const when = new Date("2026-03-15T12:00:00Z");
  const doc = paintingToLinkedArt({
    code: "Abc123",
    handle: "sat",
    when,
    imageUrl: `${WEB_BASE}/media/@sat/painting/2026.3.15.png`,
    license: "CC-BY-4.0",
  });
  assert.equal(doc.id, `${DATA_BASE}/painting/Abc123`);
  assert.equal(doc.type, "DigitalObject");
  assert.equal(doc.produced_by.type, "Production");
  assert.equal(doc.produced_by.carried_out_by[0].id, `${DATA_BASE}/@sat`);
  assert.equal(doc.produced_by.timespan.begin_of_the_begin, when.toISOString());
  assert.equal(doc.subject_to[0].classified_as[0].id, "https://creativecommons.org/licenses/by/4.0/");
  assert.equal(doc.digitally_shown_by[0].format, "image/png");
  // Every Getty AAT classification must resolve to a real AAT URI.
  for (const c of doc.classified_as) assert.match(c.id, /vocab\.getty\.edu\/aat\/\d+$/);
  // Verified Getty AAT ids (vocab.getty.edu, 2026-06-29) — lock them in so a
  // future careless edit can't silently reintroduce a wrong term.
  const ids = doc.classified_as.map((c) => c.id);
  assert.ok(ids.includes("http://vocab.getty.edu/aat/300033618"), "paintings (visual works)");
  assert.ok(ids.includes("http://vocab.getty.edu/aat/300386810"), "digital art (visual works)");
});

test("piece → E73 with source LinguisticObject + checksum identifier", () => {
  const doc = pieceToLinkedArt({
    code: "cow",
    handle: "sat",
    when: new Date("2026-03-15T12:00:00Z"),
    source: "(wipe blue)",
    hash: "deadbeef",
    license: "CC0-1.0",
  });
  assert.equal(doc.id, `${DATA_BASE}/piece/cow`);
  assert.equal(doc.created_by.type, "Creation");
  assert.equal(doc.identified_by[0].content, "deadbeef");
  assert.equal(doc.carries[0].content, "(wipe blue)");
  assert.equal(doc.subject_to[0].classified_as[0].id, "https://creativecommons.org/publicdomain/zero/1.0/");
  assert.equal(doc.subject_of[0].digitally_carried_by[0].access_point[0].id, `${WEB_BASE}/cow`);
});

test("mood → E33 LinguisticObject, Bluesky cross-ref, apex permalink", () => {
  const doc = moodToLinkedArt({
    handle: "sat",
    rkey: "3kabc",
    mood: "feeling generative",
    when: new Date("2026-03-15T12:00:00Z"),
    blueskyUri: "at://did:plc:xyz/app.bsky.feed.post/3kabc",
    license: "CC-BY-4.0",
  });
  assert.equal(doc.id, `${DATA_BASE}/mood/sat/3kabc`);
  assert.equal(doc.type, "LinguisticObject");
  assert.equal(doc.content, "feeling generative");
  assert.equal(doc.equivalent[0].id, "at://did:plc:xyz/app.bsky.feed.post/3kabc");
  assert.equal(doc.subject_of[0].digitally_carried_by[0].access_point[0].id, `${WEB_BASE}/moods~sat~3kabc`);
});

test("no license → no rights statement (gate is upstream, serializer stays honest)", () => {
  const doc = paintingToLinkedArt({ code: "x", handle: "sat", when: new Date(), license: undefined });
  assert.deepEqual(doc.subject_to, []);
});
