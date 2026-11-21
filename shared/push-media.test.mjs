// push-media.test, 2026.11.18
// Which picture a notification carries.  Run: node --test shared/push-media.test.mjs

import test from "node:test";
import assert from "node:assert/strict";
import { mediaInText, faceFor, mediaFor } from "./push-media.mjs";
import { apnsPayload } from "./push.mjs";

test("a message's own media is its picture", () => {
  assert.equal(mediaInText("look #k3d nice"), "https://aesthetic.computer/media/paintings/k3d.png");
  assert.equal(mediaInText("$pld"), "https://oven.aesthetic.computer/grab/png/512/512/%24pld");
  assert.equal(mediaInText("https://www.youtube.com/watch?v=THqGaWXPzQo"), "https://i.ytimg.com/vi/THqGaWXPzQo/hqdefault.jpg");
  assert.equal(mediaInText("https://youtu.be/fiU3ZHYNYeQ?t=3"), "https://i.ytimg.com/vi/fiU3ZHYNYeQ/hqdefault.jpg");
  assert.equal(mediaInText("see https://example.com/a/cat.JPG?x=1 lol"), "https://example.com/a/cat.JPG?x=1");
});

test("plain talk, prices and other links carry none", () => {
  for (const text of ["sov godt klokkers", "it costs $5", "kl 16.30 #1", "https://we.tl/t-1k4bdOg3vSyGPqSZ",
    "http://example.com/cat.png", "mail me a@b.co", "", null])
    assert.equal(mediaInText(text), null, String(text));
});

// One handle with two paintings, the newer hidden; one handle with none.
function fakeDb({ slow = 0 } = {}) {
  const handles = [{ _id: "sub|a", handle: "Elias_fra_iceage" }, { _id: "sub|b", handle: "alki" }];
  const paintings = [
    { user: "sub|a", code: "old", when: 1 }, { user: "sub|a", code: "new", when: 2 },
    { user: "sub|a", code: "sec", when: 3, hidden: true },
  ];
  const ok = (doc, q) => Object.entries(q).every(([k, want]) =>
    want?.$regex ? new RegExp(want.$regex, want.$options).test(doc[k])
      : want?.$in ? want.$in.includes(doc[k] ?? null)
        : want?.$type ? typeof doc[k] === "string" && doc[k] !== want.$ne
          : want && typeof want === "object" && "$ne" in want ? doc[k] !== want.$ne : doc[k] === want);
  return { collection: (name) => name === "@handles"
    ? { findOne: async (q) => { if (slow) await new Promise((r) => setTimeout(r, slow)); return handles.find((h) => ok(h, q)) || null; } }
    : { find: (q) => ({ sort: () => ({ limit: () => ({ toArray: async () =>
      paintings.filter((p) => ok(p, q)).sort((a, b) => b.when - a.when).slice(0, 1) }) }) }) } };
}

test("with no media the sender's newest public painting stands in", async () => {
  const db = fakeDb();
  assert.equal(await faceFor(db, "@Elias_fra_iceage"), "https://aesthetic.computer/media/paintings/new.png");
  assert.equal(await faceFor(db, "@elias_fra_iceage"), "https://aesthetic.computer/media/paintings/new.png", "any case");
  assert.equal(await faceFor(db, "@alki"), null, "no paintings");
  assert.equal(await faceFor(db, "log 🪵"), null, "not a handle");
  assert.equal(await mediaFor(db, "@Elias_fra_iceage", "hej #k3d"), "https://aesthetic.computer/media/paintings/k3d.png", "the message wins");
  assert.equal(await mediaFor(db, "@Elias_fra_iceage", "hej"), "https://aesthetic.computer/media/paintings/new.png");
});

test("a slow or broken lookup never holds the message", async () => {
  const t0 = Date.now();
  assert.equal(await mediaFor(fakeDb({ slow: 400 }), "@alki", "hej", { wait: 50 }), null);
  assert.ok(Date.now() - t0 < 300);
  assert.equal(await mediaFor({ collection: () => { throw new Error("down"); } }, "@alki", "hej"), null);
});

test("the APNs payload carries subtitle, thread and image for the extension", () => {
  const sent = JSON.parse(apnsPayload({ title: "@alki 🕐", subtitle: "laklok", thread: "laklok", body: "hej",
    image: "https://aesthetic.computer/media/paintings/new.png", data: { piece: "laklok" } }));
  assert.deepEqual(sent.aps.alert, { title: "@alki 🕐", subtitle: "laklok", body: "hej" });
  assert.equal(sent.aps["mutable-content"], 1);
  assert.equal(sent.aps["thread-id"], "laklok");
  assert.equal(sent.image, "https://aesthetic.computer/media/paintings/new.png");
  assert.equal(sent.piece, "laklok");
  assert.equal("subtitle" in JSON.parse(apnsPayload({ title: "t", body: "b" })).aps.alert, false);
});
