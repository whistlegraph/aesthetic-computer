import { test } from "node:test";
import assert from "node:assert/strict";
import { ago, cell, clip, shortPath, shortWhen, toon } from "./toon.mjs";

test("toon header, rows, cap and note", () => {
  const out = toon("papers", [{ id: "a", title: "Hello, world", n: 2 }, { id: "b", title: null, n: false }],
    ["id", "title", "n"], { total: 40, note: "more: full:true" });
  assert.equal(out, 'papers[2 of 40]{id,title,n}:\n  a,"Hello, world",2\n  b,,false\nmore: full:true');
});

test("toon empty list is definitive", () => {
  assert.equal(toon("rocks", [], ["host", "name"]), "rocks[0]{host,name}:");
});

test("cell quotes only when it must", () => {
  assert.equal(cell("plain"), "plain");
  assert.equal(cell('say "hi"'), '"say ""hi"""');
  assert.equal(cell(" edge"), '" edge"');
  assert.equal(cell("a\nb"), '"a\nb"');
  assert.equal(cell(undefined), "");
  assert.equal(cell(0), "0");
});

test("clip keeps short text and hints at the cut", () => {
  assert.equal(clip("short", 10), "short");
  assert.equal(clip("abcdefghijkl", 8), "abcdefg…(+5ch)");
  assert.equal(clip("  a   b  ", 10), "a b");
});

test("shortPath picks the longest root", () => {
  const roots = { $AC: "/u/ac", $VAULT: "/u/ac/vault" };
  assert.equal(shortPath("/u/ac/vault/p.tex", roots), "$VAULT/p.tex");
  assert.equal(shortPath("/u/ac/system/x.mjs", roots), "$AC/system/x.mjs");
  assert.equal(shortPath("/elsewhere/x", roots), "/elsewhere/x");
  assert.equal(shortPath("/u/acme/x", roots), "/u/acme/x");
});

test("ago and shortWhen", () => {
  const now = Date.parse("2026-09-20T12:00:00Z");
  assert.equal(ago(now - 30_000, now), "30s");
  assert.equal(ago(now - 5 * 60_000, now), "5m");
  assert.equal(ago(now - 3 * 3_600_000, now), "3h");
  assert.equal(ago(now - 5 * 86_400_000, now), "5d");
  assert.equal(ago("garbage", now), "?");
  assert.equal(shortWhen("2026-09-20T14:05:33Z"), "09-20 14:05");
  assert.equal(shortWhen("2026-09-20"), "09-20");
  assert.equal(shortWhen(""), "");
});
