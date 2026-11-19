import test from "node:test";
import assert from "node:assert/strict";
import {createHmac} from "node:crypto";
import {createHandler, deviceTokenMatches} from "../netlify/functions/native-piece.mjs";
import {nativeNoticeModule, nativePiecePath} from "../../lith/media-path.mjs";

const SECRET = "test-secret";
const SUB = "auth0|oskie";
const mint = (sub, ts = "abc123") => `${createHmac("sha256", SECRET).update(`${sub}:${ts}`).digest("hex").slice(0, 32)}.${ts}`;

function world() {
  const pieces = [];
  const stored = [];
  const collections = {
    "@handles": {findOne: async (q) => q.handle === "oskie" ? {_id: SUB} : null},
    pieces: {
      findOne: async (q) => pieces.find((r) => q._id ? String(r._id) === String(q._id) : r.user === q.user && q.slug.$in.includes(r.slug)) || null,
      updateOne: async (f, u) => {
        const row = pieces.find((r) => String(r._id) === String(f._id));
        if (row) Object.assign(row, u.$set); else pieces.push({_id: f._id, ...u.$setOnInsert, ...u.$set});
      },
    },
  };
  const handler = createHandler({
    database: async () => ({db: {collection: (name) => collections[name]}, disconnect: async () => {}}),
    authenticate: async ({authorization}) => authorization === "Bearer signed-in" ? {sub: SUB} : null,
    store: async (object) => stored.push(object),
    makeCode: async () => "abc",
    secret: () => SECRET,
  });
  const post = (body) => handler({httpMethod: "POST", headers: {}, body: JSON.stringify(body)});
  return {handler, post, pieces, stored};
}
const piece = {handle: "oskie", slug: "acid", ext: "mjs", source: "export function paint() {}\n"};

test("a machine's device token publishes its owner's piece, flagged ac-native", async () => {
  const {post, pieces, stored, handler} = world();
  const reply = await post({...piece, token: mint(SUB)});
  assert.equal(reply.statusCode, 200);
  assert.equal(JSON.parse(reply.body).route, "https://aesthetic.computer/@oskie/acid");
  assert.equal(stored[0].key, `${SUB}/piece/acid.mjs`);
  assert.deepEqual([pieces[0].slug, pieces[0].origin, pieces[0].platform, pieces[0].user], ["piece/acid", "ac-native", "ac-native", SUB]);
  const flag = await handler({httpMethod: "GET", queryStringParameters: {sub: SUB, slug: "acid"}});
  assert.equal(JSON.parse(flag.body).platform, "ac-native");
  const other = await handler({httpMethod: "GET", queryStringParameters: {sub: SUB, slug: "nope"}});
  assert.equal(JSON.parse(other.body).platform, null);
});

test("a linked machine's sign-in token publishes too", async () => {
  const {post, stored} = world();
  assert.equal((await post({...piece, token: "signed-in"})).statusCode, 200);
  assert.equal(stored.length, 1);
});

test("a wrong token, another handle's token, or a bad name stores nothing", async () => {
  const {post, stored} = world();
  assert.equal((await post({...piece, token: mint("auth0|someone-else")})).statusCode, 401);
  assert.equal((await post({...piece, token: "0".repeat(32) + ".abc123"})).statusCode, 401);
  assert.equal((await post({...piece, handle: "nobody", token: mint(SUB)})).statusCode, 401);
  assert.equal((await post({...piece, slug: "../x", token: mint(SUB)})).statusCode, 400);
  assert.equal((await post({...piece, ext: "sh", token: mint(SUB)})).statusCode, 400);
  assert.equal((await post({...piece, source: "x".repeat(400001), token: mint(SUB)})).statusCode, 413);
  assert.equal(stored.length, 0);
  assert.equal(deviceTokenMatches(SUB, mint(SUB), ""), false);
});

test("the web notice is a module that names the piece and its author", async () => {
  assert.deepEqual(nativePiecePath("piece/acid.mjs"), {slug: "acid"});
  assert.equal(nativePiecePath("piece/acid.lisp"), null);
  assert.equal(nativePiecePath("painting/acid.png"), null);
  const source = nativeNoticeModule({handle: "@oskie", slug: "acid"});
  const module = await import("data:text/javascript," + encodeURIComponent(source));
  assert.equal(module.platform, "ac-native");
  const written = [];
  module.paint({wipe() {}, ink() {}, write: (text) => written.push(text), screen: {height: 200}});
  assert.deepEqual(written.slice(0, 2), ["acid", "by @oskie"]);
});
