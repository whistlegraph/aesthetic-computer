import test from "node:test";
import assert from "node:assert/strict";
import {
  actionableTarget,
  canonicalHandle,
  duplicateReport,
  ledgerFreshness,
  resolveRocks,
} from "../lib/prox-resolver.mjs";

const now = Date.parse("2026-07-24T02:41:15.000Z");
const rock = (overrides = {}) => ({
  id: "aaaaaaaa-1111-2222-3333-444444444444",
  host: "neo",
  name: "pegatu",
  subject: "old shapedown work",
  status: "complete",
  self: true,
  ledgerUpdatedAt: now - 1_000,
  ...overrides,
});

test("canonical handles remain readable while session ids own identity", () => {
  assert.equal(canonicalHandle(rock()), "neo:pegatu#aaaaaaaa");
  const result = resolveRocks([rock()], "neo:pegatu#aaaaaaaa");
  assert.equal(result.matchType, "canonical-handle");
  assert.equal(result.hits[0].id, rock().id);
});

test("a wrong canonical id suffix never falls back to the pet-name alias", () => {
  const result = resolveRocks([rock()], "neo:pegatu#deadbeef");
  assert.equal(result.matchType, "canonical-handle");
  assert.equal(result.hits.length, 0);
});

test("duplicate pet names are reported and never silently selected", () => {
  const rocks = [rock(), rock({ id: "bbbbbbbb-1111", host: "panda", status: "working" })];
  const result = resolveRocks(rocks, "pegatu");
  assert.equal(result.matchType, "exact-name");
  assert.equal(result.hits.length, 2);
  assert.equal(duplicateReport(rocks).names.length, 1);
  assert.throws(() => actionableTarget(result, { now, verb: "poke" }), /ambiguous/);
});

test("subject matches are labeled discovery-only", () => {
  const result = resolveRocks([rock({ name: "fotos", subject: "working on jastow" })], "jastow");
  assert.equal(result.matchType, "subject-substring");
  assert.throws(() => actionableTarget(result, { now, verb: "wake" }), /discovery-only/);
});

test("actions reject stale ledgers even for exact handles", () => {
  const stale = rock({ ledgerUpdatedAt: now - 121_000, status: "working" });
  assert.equal(ledgerFreshness(stale.ledgerUpdatedAt, now).state, "stale");
  const result = resolveRocks([stale], "neo:pegatu");
  assert.throws(() => actionableTarget(result, { now, verb: "poke" }), /stale ledger/);
});

test("a unique fresh exact pet name remains a safe shorthand", () => {
  const result = resolveRocks([rock({ status: "working" })], "pegatu");
  assert.equal(actionableTarget(result, { now, verb: "poke" }).id, rock().id);
});
