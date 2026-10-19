import { test } from "node:test";
import assert from "node:assert/strict";
import { bootPiece, foldPieceLandings } from "../../toolchain/analytics/piece-landings.mjs";

test("a URL path becomes its public piece name", () => {
  assert.equal(bootPiece("/"), "prompt");
  assert.equal(bootPiece("/laer-klokken"), "laer-klokken");
  assert.equal(bootPiece("/line:5:red"), "line");
  assert.equal(bootPiece("/wand~blue"), "wand");
  assert.equal(bootPiece("/@jeffrey/whistle:2"), "@jeffrey/whistle");
  assert.equal(bootPiece("/$cow"), "$cow");
  assert.equal(bootPiece("/notepat/extra/parts"), "notepat");
});

test("private and malformed paths never become piece names", () => {
  for (const path of ["/mail", "/chat~room", "/wallet", "/account/reset", "/%61dmin"]) assert.equal(bootPiece(path), null, path);
  assert.equal(bootPiece("/%E0%A4%A"), null);
  assert.equal(bootPiece("/<script>"), "(other)");
});

test("landings fold per property and piece, leaving out bots, embeds and other sites", () => {
  const ua = "Mozilla/5.0 (Macintosh) Safari/605";
  const report = foldPieceLandings([
    { status: "success", meta: { host: "aesthetic.computer", path: "/laklok", userAgent: ua, referrer: "https://www.google.com/" } },
    { status: "success", meta: { host: "aesthetic.computer", path: "/laklok:mail", userAgent: ua, user: "sub" } },
    { status: "started", meta: { host: "aesthetic.computer", path: "/notepat", userAgent: ua } },
    { status: "success", meta: { host: "aesthetic.computer", path: "/laklok", userAgent: "Googlebot/2.1" } },
    { status: "success", meta: { host: "aesthetic.computer", path: "/laklok", userAgent: ua, embedded: true } },
    { status: "success", meta: { host: "aesthetic.computer", path: "/mail", userAgent: ua } },
    { status: "success", meta: { host: "regarde.io", path: "/", userAgent: ua } },
  ], { properties: ["aesthetic.computer"] });
  assert.equal(report.boots, 3);
  assert.equal(report.excluded, 4);
  assert.deepEqual(report.pieces[0], { property: "aesthetic.computer", piece: "laklok", boots: 2, anonymous: 1, signedIn: 1, completed: 2,
    referrers: [{ host: "google.com", boots: 1 }] });
  assert.equal(report.pieces[1].piece, "notepat");
  assert.equal(report.pieces[1].completed, 0);
});
