// The oven must not turn a failed render into a piece's cached thumbnail.
import test from "node:test";
import assert from "node:assert/strict";
import { pieceSourceFailure, isStaleDud, isSuspectDud, DUD_CACHE_TTL_MS, captureUrl, cdnHref } from "../oven/grab-duds.mjs";

const api = "https://aesthetic.computer/api/store-kidlisp";

test("a $code piece whose own source read fails aborts the capture", () => {
  assert.match(pieceSourceFailure("$pld", `${api}?code=pld`, 500), /failed to load \(HTTP 500/);
  assert.match(pieceSourceFailure("$pld", `${api}?code=pld`, 404), /not found/);
  assert.equal(pieceSourceFailure("$pld", `${api}?code=pld`, 200), null);
  assert.equal(pieceSourceFailure("$pld", `${api}?code=cow`, 500), null, "an embedded piece may fail");
  assert.equal(pieceSourceFailure("$pld", "https://aesthetic.computer/api/piece-log", 502), null);
  assert.equal(pieceSourceFailure("prompt", `${api}?code=prompt`, 500), null, "only $code pieces");
  assert.equal(pieceSourceFailure("$pld", "not a url", 500), null);
});

test("a dud capture is cached briefly, a real one for good", () => {
  const now = Date.UTC(2026, 10, 18);
  const ago = ms => new Date(now - ms);
  assert.equal(isSuspectDud({ dud: true, size: 9000 }), true);
  assert.equal(isSuspectDud({ size: 528 }), true, "pre-flag records are recognized by size");
  assert.equal(isSuspectDud({ size: 4096 }), false);
  assert.equal(isStaleDud({ size: 528, completedAt: ago(DUD_CACHE_TTL_MS - 1000) }, now), false);
  assert.equal(isStaleDud({ size: 528, completedAt: ago(DUD_CACHE_TTL_MS + 1000) }, now), true);
  assert.equal(isStaleDud({ dud: true, size: 528 }, now), true, "no timestamp: do not trust it");
  assert.equal(isStaleDud({ size: 4096, completedAt: ago(30 * 86400e3) }, now), false);
  assert.equal(isStaleDud(null, now), false);
});

test("a painting keeps its capture flags and its CDN address", () => {
  const base = "https://aesthetic.computer";
  assert.equal(captureUrl(base, "$pld", "tv=true"), `${base}/$pld?tv=true`);
  const painting = new URL(captureUrl(base, "#hf2", "tv=true&noboot=true"));
  assert.equal(painting.searchParams.get("tv"), "true");
  assert.equal(painting.hash, "#hf2");
  const href = cdnHref("https://art.aesthetic.computer/oven/grabs/#hf2-a0792dea.webp");
  assert.equal(new URL(href).pathname, "/oven/grabs/%23hf2-a0792dea.webp");
  assert.equal(cdnHref(href), href, "idempotent");
  assert.equal(cdnHref(undefined), undefined);
});
