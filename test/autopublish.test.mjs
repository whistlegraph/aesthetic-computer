import assert from "node:assert/strict";
import test from "node:test";
import { AutoPublisher } from "../src/autopublish.mjs";

// The publisher is all timing, so the tests run it at millisecond scale and
// with a clock they control. Nothing here touches the network: `publish` is a
// counter, which is exactly what the coalescing is about.
function publisher(options = {}) {
  const calls = [];
  const auto = new AutoPublisher({
    enabled: true,
    settle: 5,
    minGap: 0,
    publish: async () => {
      calls.push(Date.now());
      return { route: `publish-${calls.length}` };
    },
    ...options,
  });
  return { auto, calls };
}

const settled = (auto) =>
  new Promise((resolve) => {
    auto.once("published", resolve);
    auto.once("failed", resolve);
  });

test("a burst of saves publishes once, with the last bytes", async () => {
  const seen = [];
  const auto = new AutoPublisher({
    enabled: true,
    settle: 5,
    minGap: 0,
    publish: async () => {
      seen.push(auto.published);
      return { route: "once" };
    },
  });
  for (const source of ["one", "two", "three"]) auto.note(source);
  await settled(auto);
  assert.equal(seen.length, 1, "three saves in a burst are one publish");
  assert.equal(auto.published, "three", "the last save is the one that went out");
  assert.equal(auto.pending, false);
});

test("republishing identical bytes is not a publish", async () => {
  const { auto, calls } = publisher();
  auto.note("same");
  await settled(auto);
  assert.equal(calls.length, 1);
  assert.equal(auto.note("same"), false, "unchanged source is refused");
  assert.equal(auto.pending, false);
});

test("a save during a publish goes out after it, not alongside it", async () => {
  let inFlight = 0;
  let peak = 0;
  const auto = new AutoPublisher({
    enabled: true,
    settle: 1,
    minGap: 0,
    publish: async () => {
      inFlight += 1;
      peak = Math.max(peak, inFlight);
      await new Promise((resolve) => setTimeout(resolve, 10));
      inFlight -= 1;
      return { route: "serial" };
    },
  });
  auto.note("first");
  await new Promise((resolve) => auto.once("start", resolve));
  auto.note("second"); // Mid-flight.
  assert.equal(auto.pending, true);
  await settled(auto); // first
  await settled(auto); // second, armed by the first one finishing
  assert.equal(peak, 1, "publishes never overlap");
  assert.equal(auto.published, "second");
});

test("minGap holds the next publish back", async () => {
  const { auto, calls } = publisher({ settle: 1, minGap: 40 });
  auto.note("a");
  await settled(auto);
  auto.note("b");
  await settled(auto);
  assert.ok(
    calls[1] - calls[0] >= 35,
    `expected the second publish to wait out the gap, waited ${calls[1] - calls[0]}ms`,
  );
});

test("flush publishes the pending save immediately", async () => {
  const { auto, calls } = publisher({ settle: 60_000, minGap: 60_000 });
  auto.note("quitting");
  const result = await auto.flush();
  assert.equal(calls.length, 1, "flush does not wait for the settle timer");
  assert.equal(result.route, "publish-1");
  assert.equal(await auto.flush(), null, "a second flush has nothing to do");
});

test("disabled is disabled, and enabling does not replay old saves", async () => {
  const { auto, calls } = publisher({ enabled: false });
  assert.equal(auto.note("ignored"), false);
  assert.equal(auto.pending, false);
  auto.set(true);
  assert.equal(auto.pending, false, "the save made while off is not queued");
  auto.note("wanted");
  await settled(auto);
  assert.equal(calls.length, 1);
});

test("turning it off cancels what was armed", async () => {
  const { auto, calls } = publisher({ settle: 5 });
  auto.note("armed");
  auto.set(false);
  await new Promise((resolve) => setTimeout(resolve, 25));
  assert.equal(calls.length, 0);
  assert.equal(auto.pending, false);
});

test("a failure leaves the piece unpublished so the next save retries", async () => {
  let attempts = 0;
  const auto = new AutoPublisher({
    enabled: true,
    settle: 1,
    minGap: 0,
    publish: async () => {
      attempts += 1;
      if (attempts === 1) throw new Error("no handle");
      return { route: "second-try" };
    },
  });
  const failure = await new Promise((resolve) => {
    auto.once("failed", resolve);
    auto.note("v1");
  });
  assert.match(failure.message, /no handle/);
  assert.equal(auto.published, null, "a failed publish is not remembered as published");
  assert.equal(auto.note("v1"), true, "the same bytes are still worth retrying");
  await settled(auto);
  assert.equal(attempts, 2);
});

test("a failure does not retry on its own", async () => {
  let attempts = 0;
  const auto = new AutoPublisher({
    enabled: true,
    settle: 1,
    minGap: 0,
    publish: async () => {
      attempts += 1;
      throw new Error("rejected");
    },
  });
  await new Promise((resolve) => {
    auto.once("failed", resolve);
    auto.note("v1");
  });
  await new Promise((resolve) => setTimeout(resolve, 30));
  assert.equal(attempts, 1, "a broken publish would otherwise spin all session");
});
