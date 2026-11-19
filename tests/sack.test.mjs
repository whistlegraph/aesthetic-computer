// sack.test, 2026.11.18
// The sack API against an in-memory database and storage: nothing is uploaded.
//   node --test tests/sack.test.mjs

import test from "node:test";
import assert from "node:assert/strict";
import { execFileSync } from "node:child_process";
import { mkdtempSync, writeFileSync, readFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { createHandler } from "../system/netlify/functions/sack.mjs";
import {
  cleanName, uniqueName, cleanType, parseDrop, partCount, storageFor, zipStream,
  PART_BYTES, HANDLE_LIMIT, FILE_LIMIT, GB,
} from "../system/backend/sack.mjs";

// ── Fakes ───────────────────────────────────────────────────────────────────
const matches = (doc, query) => Object.entries(query).every(([key, want]) =>
  want && typeof want === "object" && "$ne" in want ? doc[key] !== want.$ne : doc[key] === want);

function fakeDb() {
  const docs = [];
  const collection = {
    docs,
    createIndex: async () => {},
    findOne: async (query) => docs.find((doc) => matches(doc, query)) || null,
    find: (query) => {
      let found = docs.filter((doc) => matches(doc, query));
      const cursor = { sort: () => cursor, limit: () => cursor, toArray: async () => found };
      return cursor;
    },
    insertOne: async (doc) => { docs.push({ _id: docs.length + 1, ...structuredClone(doc) }); },
    updateOne: async (query, change) => {
      const doc = docs.find((each) => matches(each, query));
      if (!doc) return;
      Object.assign(doc, structuredClone(change.$set || {}));
      for (const [key, value] of Object.entries(change.$push || {})) doc[key].push(...structuredClone(value.$each));
    },
    findOneAndUpdate: async (query, change) => {
      await collection.updateOne(query, change);
      return docs.find((doc) => matches(doc, query)) || null;
    },
  };
  return { collection: () => collection, docs };
}

// Storage that remembers what was begun and lets a test say what "arrived".
function fakeStore() {
  const store = {
    begun: new Map(), objects: new Map(), aborted: [], removed: [],
    begin: async (key) => { store.begun.set(key, "up-" + store.begun.size); return store.begun.get(key); },
    partUrl: async (key, uploadId, n) => `https://storage.test/${key}?uploadId=${uploadId}&partNumber=${n}`,
    complete: async (key) => {
      if (!store.objects.has(key)) throw new Error("nothing uploaded");
      return store.objects.get(key).length;
    },
    abort: async (key) => { store.aborted.push(key); },
    remove: async (keys) => { store.removed.push(...keys); for (const key of keys) store.objects.delete(key); },
    readUrl: async (key, name, inline) => `https://storage.test/${key}?as=${inline ? "inline" : "attachment"}&name=${encodeURIComponent(name)}`,
    read: async (key) => (async function* () { yield store.objects.get(key); })(),
  };
  return store;
}

function rig({ handle = "elias" } = {}) {
  const db = fakeDb(), store = fakeStore();
  let n = 0;
  const handler = createHandler({
    connectDatabase: async () => ({ db, disconnect: async () => {} }),
    openStorage: () => store,
    authorize: async (headers) => headers.authorization ? { sub: headers.authorization.replace("Bearer ", "") } : undefined,
    handleFor: async (sub) => sub === "auth0|nohandle" ? null : handle,
    newCode: async () => "sk" + ++n,
  });
  const call = async (method, { as, query, body } = {}) => {
    const res = await handler({ httpMethod: method, headers: as ? { authorization: "Bearer " + as } : {},
      queryStringParameters: query || {}, body: body ? JSON.stringify(body) : null });
    return { status: res.statusCode, headers: res.headers, raw: res.body,
      json: typeof res.body === "string" && res.body ? JSON.parse(res.body) : null };
  };
  return { db, store, call };
}

const ME = "auth0|me", OTHER = "auth0|other";

// ── Names and limits ────────────────────────────────────────────────────────
test("names lose folders and control characters but keep their letters", () => {
  assert.equal(cleanName("../../etc/passwd"), "passwd");
  assert.equal(cleanName("C:\\music\\GG4m4l4n.wav"), "GG4m4l4n.wav");
  assert.equal(cleanName("lær\u0000 klokken.mp3"), "lær klokken.mp3");
  assert.equal(cleanName(".."), "file");
  assert.equal(cleanName(""), "file");
});

test("a repeated name is numbered", () => {
  assert.equal(uniqueName("a.wav", new Set(["a.wav"])), "a (2).wav");
  assert.equal(uniqueName("a.wav", new Set(["a.wav", "a (2).wav"])), "a (3).wav");
  assert.equal(uniqueName("notes", new Set(["notes"])), "notes (2)");
});

test("types are kept only when they look like types", () => {
  assert.equal(cleanType("audio/wav"), "audio/wav");
  assert.equal(cleanType("text/html; charset=utf-8"), "application/octet-stream");
  assert.equal(cleanType(""), "application/octet-stream");
});

test("a drop is refused when empty, zero-sized or over 2 GB", () => {
  assert.throws(() => parseDrop([]), /at least one/);
  assert.throws(() => parseDrop([{ name: "a", bytes: 0 }]), /above zero/);
  assert.throws(() => parseDrop([{ name: "big.mov", bytes: FILE_LIMIT + 1 }]), /2 GB/);
  assert.equal(parseDrop([{ name: "a.wav", bytes: 5 }, { name: "a.wav", bytes: 6 }])[1].name, "a (2).wav");
});

test("parts cover the file", () => {
  assert.equal(partCount(1), 1);
  assert.equal(partCount(PART_BYTES), 1);
  assert.equal(partCount(PART_BYTES + 1), 2);
  assert.equal(partCount(123694700), 8); // GG4m4l4n.wav
});

// ── The API ─────────────────────────────────────────────────────────────────
test("a drop becomes a sack anyone can open, download from and zip", async () => {
  const { store, call, db } = rig();
  const begun = await call("POST", { as: ME, body: { action: "begin",
    files: [{ name: "GG4m4l4n.wav", bytes: 123694700, type: "audio/wav" }, { name: "note.html", bytes: 12, type: "text/html" }] } });
  assert.equal(begun.status, 200);
  assert.equal(begun.json.code, "sk1");
  assert.equal(begun.json.uploads[0].parts.length, 8);
  assert.equal(begun.json.uploads[1].parts.length, 1);

  // Nothing shows until it is committed.
  assert.deepEqual((await call("GET", { query: { code: "sk1" } })).json.sack.files, []);

  const [wav, note] = db.docs[0].files;
  store.objects.set(wav.key, Buffer.from("RIFFfake"));
  store.objects.set(note.key, Buffer.from("<b>hi</b>"));
  const done = await call("POST", { as: ME, body: { action: "commit", code: "sk1", ids: [wav.id, note.id] } });
  assert.deepEqual(done.json.failed, []);

  const { sack } = (await call("GET", { query: { code: "sk1" } })).json; // no login
  assert.equal(sack.handle, "@elias");
  assert.equal(sack.name, "GG4m4l4n.wav");
  assert.equal(sack.bytes, 8 + 9); // what arrived, not what was declared
  assert.ok(sack.files[0].inline, "audio plays in the page");
  assert.equal(sack.files[1].inline, undefined, "html never runs from storage");
  assert.equal(JSON.stringify(sack).includes(ME), false, "no account id leaves");

  const file = await call("GET", { query: { code: "sk1", file: note.id, inline: "1" } });
  assert.equal(file.status, 302);
  assert.match(file.headers.Location, /as=attachment/);

  const zip = await call("GET", { query: { code: "sk1", zip: "1" } });
  assert.equal(zip.headers["Content-Type"], "application/zip");
  const chunks = [];
  for await (const chunk of zip.raw) chunks.push(chunk);
  const path = join(mkdtempSync(join(tmpdir(), "sack-")), "out.zip");
  writeFileSync(path, Buffer.concat(chunks));
  assert.match(execFileSync("unzip", ["-t", path]).toString(), /No errors detected/);
  assert.equal(execFileSync("unzip", ["-p", path, "note.html"]).toString(), "<b>hi</b>");
});

test("an upload that never arrived is dropped, not kept", async () => {
  const { call, db, store } = rig();
  await call("POST", { as: ME, body: { action: "begin", files: [{ name: "a.wav", bytes: 10 }] } });
  const [file] = db.docs[0].files;
  const done = await call("POST", { as: ME, body: { action: "commit", code: "sk1", ids: [file.id] } });
  assert.deepEqual(done.json.failed, [file.id]);
  assert.deepEqual(db.docs[0].files, []);
  assert.deepEqual(store.aborted, [file.key]);
});

test("a sack belongs to its handle: others cannot add, rename or delete", async () => {
  const { call } = rig();
  await call("POST", { as: ME, body: { action: "begin", files: [{ name: "a.wav", bytes: 10 }] } });
  assert.equal((await call("POST", { as: OTHER, body: { action: "begin", code: "sk1", files: [{ name: "b", bytes: 1 }] } })).status, 404);
  assert.equal((await call("POST", { as: OTHER, body: { action: "rename", code: "sk1", name: "mine" } })).status, 404);
  assert.equal((await call("DELETE", { as: OTHER, query: { code: "sk1" } })).status, 404);
  assert.equal((await call("DELETE", { query: { code: "sk1" } })).status, 401);
  assert.equal((await call("POST", { as: "auth0|nohandle", body: { action: "begin", files: [{ name: "a", bytes: 1 }] } })).status, 403);
});

test("space is counted per handle and a full handle is refused", async () => {
  const { call, db } = rig();
  const half = { name: "half.mov", bytes: FILE_LIMIT };
  for (let i = 0; i < 5; i++)
    assert.equal((await call("POST", { as: ME, body: { action: "begin", files: [half] } })).status, 200);
  const mine = await call("GET", { as: ME });
  assert.equal(mine.json.used, 10 * GB);
  assert.equal(mine.json.limit, HANDLE_LIMIT);
  const over = await call("POST", { as: ME, body: { action: "begin", files: [{ name: "one-more", bytes: 1 }] } });
  assert.equal(over.status, 413);
  assert.equal((await call("POST", { as: OTHER, body: { action: "begin", files: [half] } })).status, 200);

  // A day-old unfinished upload stops holding space.
  for (const doc of db.docs) for (const file of doc.files) file.at = new Date(Date.now() - 25 * 3600 * 1000);
  assert.equal((await storageFor(db, ME)).bytes, 0);
});

test("deleting a file or a sack removes the stored bytes", async () => {
  const { call, db, store } = rig();
  await call("POST", { as: ME, body: { action: "begin", files: [{ name: "a", bytes: 3 }, { name: "b", bytes: 3 }] } });
  const [a, b] = db.docs[0].files;
  store.objects.set(a.key, Buffer.from("aaa")); store.objects.set(b.key, Buffer.from("bbb"));
  await call("POST", { as: ME, body: { action: "commit", code: "sk1", ids: [a.id, b.id] } });
  const less = await call("DELETE", { as: ME, query: { code: "sk1", file: a.id } });
  assert.deepEqual(less.json.sack.files.map((file) => file.name), ["b"]);
  assert.deepEqual(store.removed, [a.key]);
  assert.equal((await call("DELETE", { as: ME, query: { code: "sk1" } })).json.deleted, true);
  assert.equal((await call("GET", { query: { code: "sk1" } })).status, 404);
  assert.deepEqual((await storageFor(db, ME)), { bytes: 0, sacks: 0, files: 0 });
});

test("a zip past 4 GB keeps valid offsets", async () => {
  // Sparse entries: 3 files of 1.5 GB of zeros, never held in memory at once.
  const block = Buffer.alloc(8 * 1024 * 1024);
  const big = (name) => ({ name, open: async function* () { for (let i = 0; i < 192; i++) yield block; } });
  let total = 0, tail = Buffer.alloc(0);
  for await (const chunk of zipStream([big("a.bin"), big("b.bin"), big("c.bin"), { name: "end.txt", open: async function* () { yield Buffer.from("end"); } }])) {
    total += chunk.length;
    tail = Buffer.concat([tail, chunk]).subarray(-400);
  }
  assert.ok(total > 2 ** 32);
  assert.ok(tail.includes(Buffer.from([0x50, 0x4b, 0x06, 0x06])), "zip64 end record");
  assert.ok(tail.includes(Buffer.from([0x50, 0x4b, 0x06, 0x07])), "zip64 locator");
  assert.equal(tail.readUInt32LE(tail.length - 22), 0x06054b50);
});
