import assert from "node:assert/strict";
import test from "node:test";
import {
  countryFromHeaders,
  normalizeCountry,
  trustedFighterNations,
} from "../oskiewar-country.mjs";
import { handler } from "../../netlify/functions/oskiewar-country.mjs";
import { normalizeNationRows } from
  "../../netlify/functions/oskiewar-replays.mjs";

test("accepts real ISO countries and rejects proxy sentinels", () => {
  assert.equal(normalizeCountry(" us "), "US");
  assert.equal(normalizeCountry("GB"), "GB");
  assert.equal(normalizeCountry("XX"), null);
  assert.equal(normalizeCountry("T1"), null);
  assert.equal(normalizeCountry("USA"), null);
});

test("country comes only from trusted infrastructure headers", () => {
  assert.equal(countryFromHeaders({ "cf-ipcountry": "ca" }), "CA");
  assert.equal(countryFromHeaders({ "x-nf-country": "JP" }), "JP");
  assert.equal(countryFromHeaders({ country: "US" }), null);
});

test("trusted country is assigned per human seat, never to CPU seats", () => {
  assert.deepEqual(trustedFighterNations(["@JEFFREY", "DUMMY"], "us"),
    ["US", null]);
  assert.deepEqual(trustedFighterNations(["BOT", "SPIDERDUMMY"], "JP"),
    [null, null]);
  assert.deepEqual(trustedFighterNations(["@A", "@B"], "NL"),
    ["NL", "NL"]);
  assert.deepEqual(trustedFighterNations(["@A", "DUMMY"], "XX"),
    [null, null]);
});

test("country endpoint is private, cache-safe, and coarse", async () => {
  const response = await handler({ httpMethod: "GET", headers: {
    "cf-ipcountry": "NL", "cf-connecting-ip": "203.0.113.2",
  } });
  assert.equal(response.statusCode, 200);
  assert.equal(response.headers["Cache-Control"], "private, no-store");
  assert.deepEqual(JSON.parse(response.body), { country: "NL" });
  assert.doesNotMatch(response.body, /203\.0\.113\.2/);
});

test("nation aggregation preserves old country-less demos", () => {
  assert.deepEqual(normalizeNationRows([
    { _id: "US", games: 4 }, { _id: "jp", games: 2 },
    { _id: "XX", games: 9 },
  ], 10), {
    matchesPlayed: 10,
    knownGames: 6,
    unknownGames: 4,
    nations: [{ country: "US", games: 4 }, { country: "JP", games: 2 }],
  });
});
