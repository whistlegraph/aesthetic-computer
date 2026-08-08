import assert from "node:assert/strict";
import test from "node:test";

import { decodeDump, dumpRows, handler } from
  "../netlify/functions/oskiewar-dump.mjs";

const encode = (value) => Buffer.from(JSON.stringify(value), "utf8")
  .toString("base64").replace(/\+/g, "-").replace(/\//g, "_").replace(/=+$/, "");

const sample = {
  v: 1, p: "paint", n: "RangeError", m: "invalid 3d triangle coordinates",
  src: { file: "live", line: 1162, column: 16 },
  k: "at screenTriangle (live:1162:16)",
  s: {
    build: "2026.08.07.1336 PDT", shell: "GAME",
    round: { id: "local", result: "@JEFFREY WINS ROUND", elapsedMs: 23941 },
    camera: { x: 6594, y: 11798, width: 930, aspect: 2.255 },
    players: [{ handle: "@JEFFREY", stance: "ADVANCE", alive: true,
      x: 6779, y: 12000, z: 0, vx: 0, vy: 0, removed: ["LEFT ARM"] }],
    balls: [{ type: "beach", x: 1517, y: 11904, vx: 2082, vy: -87 }],
  },
};

const get = (query) => handler({ httpMethod: "GET", queryStringParameters: query });

test("a dump link decodes back into the console's own state", () => {
  const dump = decodeDump(encode(sample));
  assert.equal(dump.n, "RangeError");
  assert.equal(dump.s.players[0].handle, "@JEFFREY");
  const rows = Object.fromEntries(dumpRows(dump));
  assert.equal(rows.source, "live:1162:16");
  assert.match(rows.round, /@JEFFREY WINS ROUND {2}23941ms/);
  assert.match(rows["@JEFFREY"], /ADVANCE alive {2}pos 6779,12000,0/);
  assert.match(rows["@JEFFREY"], /lost LEFT ARM/);
  assert.match(rows.balls, /beach@1517,11904 v2082,-87/);
});

test("junk, oversized, and unversioned payloads decode to nothing", () => {
  assert.equal(decodeDump(""), null);
  assert.equal(decodeDump("not base64!"), null);
  assert.equal(decodeDump("a".repeat(8193)), null);
  assert.equal(decodeDump(encode([1, 2, 3])), null);
  assert.equal(decodeDump(encode({ v: 2, s: {} })), null);
  assert.equal(decodeDump(Buffer.from("nonsense").toString("base64url")), null);
});

test("the page renders a dump and escapes what the console sent", async () => {
  const response = await get({ d: encode(sample) });
  assert.equal(response.statusCode, 200);
  assert.match(response.headers["Content-Type"], /text\/html/);
  assert.match(response.headers["Cache-Control"], /immutable/);
  assert.match(response.body, /aesthetic\.computer error/);
  assert.match(response.body, /invalid 3d triangle coordinates/);

  const hostile = await get({ d: encode({ ...sample,
    m: `<script>alert("x")</script>` }) });
  assert.ok(!hostile.body.includes("<script>alert"));
  assert.match(hostile.body, /&lt;script&gt;/);
});

test("a link without a readable dump says so instead of pretending", async () => {
  const response = await get({});
  assert.equal(response.statusCode, 400);
  assert.match(response.body, /No dump in this link/);
  assert.equal(response.headers["Cache-Control"], "no-store");

  const json = await get({ d: encode(sample), format: "json" });
  assert.equal(JSON.parse(json.body).dump.src.line, 1162);
  assert.equal((await get({ d: "!!", format: "json" })).statusCode, 400);
  assert.equal((await handler({ httpMethod: "POST" })).statusCode, 405);
});
