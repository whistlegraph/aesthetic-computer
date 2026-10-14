import test from "node:test";
import assert from "node:assert/strict";
import {
  Game,
  modelChoices,
  paintCapacity,
} from "../public/nopaint.art/ai/game.mjs";
const deferred = () => {
  let resolve;
  const promise = new Promise((r) => (resolve = r));
  return { promise, resolve };
};
function fixture() {
  const records = new Map(),
    settings = new Map(),
    requests = [];
  let n = 0;
  const store = {
    get: async (id) => structuredClone(records.get(id)),
    save: async (track) => records.set(track.id, structuredClone(track)),
    setting: async (key, value) =>
      value === undefined ? settings.get(key) : settings.set(key, value),
  };
  const account = {
    models: [
      {
        id: "ac-test",
        name: "Test",
        quote: "server-quote",
        braincells: 8,
        available: true,
      },
    ],
    value: { connected: true, remaining: 20, purchased: 0 },
    refresh: async () => {},
    move: async (input) => {
      requests.push(input);
      return { image: "cloud", billing: { braincells: 8 } };
    },
  };
  const images = {
    noise: () => "data:image/png;base64,start",
    region: async (input) => ({ input, finish: async (image) => image }),
    classic: async () => ({ image: "local-result" }),
    crop: async () => "cropped",
  };
  const game = new Game({
    store,
    account,
    images,
    uuid: () => `id-${++n}`,
    seed: () => 3,
  });
  return { game, store, account, images, records, requests };
}
test("selection and refresh are read-only; Paint submits one exact quote and duplicate clicks are ignored", async () => {
  const f = fixture();
  await f.game.boot();
  await f.game.select("ac-test");
  f.game.prepare();
  assert.equal(f.requests.length, 0);
  const pending = deferred();
  f.account.move = async (input) => {
    f.requests.push(input);
    return pending.promise;
  };
  const quote = f.game.quote.id,
    first = f.game.paint(quote),
    second = await f.game.paint(quote);
  assert.equal(second, false);
  await new Promise((r) => setImmediate(r));
  assert.equal(f.requests.length, 1);
  assert.equal(f.requests[0].quote, "server-quote");
  assert.equal(f.requests[0].image, "start");
  pending.resolve({ image: "paid", billing: { braincells: 8 } });
  await first;
  assert.equal(f.game.track.steps.length, 2);
  assert.equal(f.game.track.steps[1].receipt.billing.braincells, 8);
  assert.equal(await f.game.paint(quote), false);
});
test("No discards a late paid image and preserves the settled receipt without blocking local painting", async () => {
  const f = fixture();
  await f.game.boot();
  await f.game.select("ac-test");
  const pending = deferred();
  f.account.move = () => pending.promise;
  const cloud = f.game.paint();
  await new Promise((r) => setImmediate(r));
  await f.game.no();
  assert.equal(f.game.accepted, "data:image/png;base64,start");
  assert.equal(f.game.quote, null);
  await f.game.select("classic");
  await f.game.paint();
  assert.equal(f.game.accepted, "local-result");
  pending.resolve({ image: "late", billing: { braincells: 8 } });
  await cloud;
  assert.equal(f.game.accepted, "local-result");
  assert.equal(f.game.track.steps.length, 2);
  assert.equal(
    f.game.track.events.find((e) => e.type === "cancelled-result").step.receipt
      .billing.braincells,
    8,
  );
  assert.equal(
    f.records.get(f.game.track.id).steps.at(-1).image,
    "local-result",
  );
});
test("Fresh keeps old painting and late receipt; reload never replays an interrupted paid request", async () => {
  const f = fixture();
  await f.game.boot();
  await f.game.select("ac-test");
  const pending = deferred();
  f.account.move = () => pending.promise;
  const old = f.game.track,
    work = f.game.paint();
  await new Promise((r) => setImmediate(r));
  await f.game.no();
  await f.game.fresh();
  const fresh = f.game.track.id;
  pending.resolve({ image: "late" });
  await work;
  assert.equal(f.game.track.id, fresh);
  assert.ok(
    f.records.get(old.id).events.some((e) => e.type === "cancelled-result"),
  );
  f.game.track.pending = { id: "interrupted", model: "ac-test" };
  await f.game.persist();
  await f.game.boot();
  assert.match(f.game.error, /interrupted/);
  assert.equal(f.requests.length, 0);
});
test("unaffordable, stale and disabled cloud models never get a quote; local remains available", async () => {
  const f = fixture();
  await f.game.boot();
  await f.game.select("ac-test");
  for (const change of [
    { remaining: 0 },
    { remaining: 20, stale: true },
    { remaining: 20, stale: false, connected: false },
  ]) {
    Object.assign(f.account.value, change);
    f.game.prepare();
    assert.equal(f.game.quote, null);
    assert.equal(await f.game.paint(), false);
  }
  await f.game.select("classic");
  assert.equal(f.game.quote.braincells, 0);
  assert.equal(paintCapacity(0, {}), "∞");
  assert.equal(f.requests.length, 0);
  assert.equal(
    modelChoices(f.account.models, f.account.value).some(
      (m) => m.id === "evolve",
    ),
    false,
  );
});

test("Classic stays free across sign-in, refresh and old Random settings until a cloud model is chosen", async () => {
  for (const saved of [undefined, "random", "fleet-evolve", "classic-pixels"]) {
    const f = fixture();
    if (saved) await f.store.setting("model", saved);
    f.account.value = { connected: false };
    await f.game.boot();
    assert.equal(f.game.engine, saved === "classic-pixels" ? saved : "classic");
    f.account.value = { connected: true, remaining: 100, purchased: 0 };
    for (let i = 0; i < 4; i++) {
      f.game.prepare();
      assert.equal(f.game.quote.braincells, 0);
      await f.game.paint();
    }
    assert.equal(f.requests.length, 0);
    await f.game.select("ac-test");
    await f.game.paint();
    assert.equal(f.requests.length, 1);
  }
});
test("a failed write to local storage prevents a paid request", async () => {
  const f = fixture();
  await f.game.boot();
  await f.game.select("ac-test");
  f.store.save = async () => {
    throw Error("Storage full");
  };
  assert.equal(await f.game.paint(), false);
  assert.equal(f.requests.length, 0);
  assert.match(f.game.error, /Storage full/);
});
test("failed cloud submission does not auto-retry on recovery or selection", async () => {
  const f = fixture();
  await f.game.boot();
  await f.game.select("ac-test");
  let calls = 0;
  f.account.move = async () => {
    calls++;
    throw Error("Unavailable");
  };
  await f.game.paint();
  f.game.retry();
  f.game.prepare();
  await f.game.select("ac-test");
  assert.equal(calls, 1);
  assert.equal(f.game.track.steps.length, 1);
});

test("No during image preparation prevents an unsent paid request", async () => {
  const f = fixture();
  await f.game.boot();
  await f.game.select("ac-test");
  const pending = deferred();
  f.images.region = () => pending.promise;
  const work = f.game.paint();
  await new Promise((r) => setImmediate(r));
  await f.game.no();
  pending.resolve({
    input: "data:image/png;base64,prepared",
    finish: async (image) => image,
  });
  await work;
  assert.equal(f.requests.length, 0);
  assert.equal(f.records.get(f.game.track.id).pending, null);
});

test("a read-only tab leaves an active pending request and stored settings untouched", async () => {
  const f = fixture();
  await f.game.boot();
  f.game.track.pending = { id: "active", model: "ac-test" };
  await f.game.persist();
  const original = structuredClone(f.records.get(f.game.track.id));
  const reader = new Game({
    store: f.store,
    images: f.images,
    account: f.account,
  });
  await reader.boot({ readOnly: true });
  assert.equal(reader.canPaint, false);
  assert.equal(reader.error, null);
  assert.equal(reader.track.pending.id, "active");
  await reader.select("classic");
  await reader.persist();
  assert.deepEqual(f.records.get(f.game.track.id), original);
  assert.equal(f.requests.length, 0);
  const empty = fixture();
  await empty.game.boot({ readOnly: true });
  assert.ok(empty.game.accepted);
  assert.equal(empty.records.size, 0);
  assert.equal(await empty.store.setting("current"), undefined);
});
