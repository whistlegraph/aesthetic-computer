import test from "node:test";
import assert from "node:assert/strict";
import { promises as fs } from "node:fs";
import os from "node:os";
import path from "node:path";
import { wallpaperBackoff } from "../bin/wallpaper-backoff.mjs";

test("wallpaper cooldown persists across invocations and expires", async () => {
  const directory = await fs.mkdtemp(
    path.join(os.tmpdir(), "wallpaper-backoff-"),
  );
  try {
    let now = 1000000;
    const first = wallpaperBackoff(directory, () => now);
    await first.defer("proxy", "120");
    const next = wallpaperBackoff(directory, () => now);
    assert.equal(await next.active("proxy"), true);
    assert.equal(await next.active("direct"), false);
    now += 119999;
    assert.equal(await next.active("proxy"), true);
    now++;
    assert.equal(await next.active("proxy"), false);
    await next.defer("proxy", new Date(now + 60000).toUTCString());
    assert.equal(await next.active("proxy"), true);
    now += 60000;
    assert.equal(await next.active("proxy"), false);
    await next.defer("proxy", "invalid");
    now += 60000;
    assert.equal(await next.active("proxy"), false);
  } finally {
    await fs.rm(directory, { recursive: true, force: true });
  }
});

test("wallpaper opts out of paid generations and honors proxy Retry-After", async (t) => {
  const directory = await fs.mkdtemp(
    path.join(os.tmpdir(), "wallpaper-caller-"),
  );
  const originalHome = process.env.SLAB_HOME;
  const originalKey = process.env.NVIDIA_API_KEY;
  process.env.SLAB_HOME = directory;
  process.env.NVIDIA_API_KEY = "test-nvidia";
  let calls = [];
  t.mock.method(globalThis, "fetch", async (url, options) => {
    calls.push({ url, options });
    return new Response("unavailable", {
      status: 503,
      headers: { "Retry-After": "120" },
    });
  });
  try {
    const { generate } = await import("../bin/slab-wallpaper.mjs");
    assert.equal(await generate("synthetic test"), null);
    assert.equal(calls.length, 2);
    assert.equal(JSON.parse(calls[1].options.body).allow_fallback, false);
    assert.equal(calls[1].options.headers["User-Agent"], "slab-wallpaper/1");
    assert.equal(await generate("another synthetic test"), null);
    assert.equal(
      calls.length,
      2,
      "different subjects still share the provider cooldown",
    );
  } finally {
    if (originalHome === undefined) delete process.env.SLAB_HOME;
    else process.env.SLAB_HOME = originalHome;
    if (originalKey === undefined) delete process.env.NVIDIA_API_KEY;
    else process.env.NVIDIA_API_KEY = originalKey;
    await fs.rm(directory, { recursive: true, force: true });
  }
});

test("wallpaper timeout includes the image response body and defers later subjects", async (t) => {
  const directory = await fs.mkdtemp(path.join(os.tmpdir(), "wallpaper-body-"));
  const originalHome = process.env.SLAB_HOME;
  const originalKey = process.env.NVIDIA_API_KEY;
  process.env.SLAB_HOME = directory;
  process.env.NVIDIA_API_KEY = "test-nvidia";
  const originalSetTimeout = globalThis.setTimeout;
  t.mock.method(globalThis, "setTimeout", (callback, ms) =>
    originalSetTimeout(callback, ms === 35000 ? 5 : ms),
  );
  let calls = 0;
  t.mock.method(globalThis, "fetch", async (url, { signal }) => {
    calls++;
    return {
      ok: true,
      json: () =>
        new Promise((resolve, reject) => {
          signal.addEventListener(
            "abort",
            () => reject(new DOMException("timeout", "AbortError")),
            { once: true },
          );
        }),
    };
  });
  try {
    const { generate } = await import(
      "../bin/slab-wallpaper.mjs?body-timeout-test"
    );
    assert.equal(await generate("synthetic test"), null);
    assert.equal(calls, 2);
    assert.equal(await generate("another synthetic test"), null);
    assert.equal(calls, 2, "body timeouts persist cooldown for both providers");
  } finally {
    if (originalHome === undefined) delete process.env.SLAB_HOME;
    else process.env.SLAB_HOME = originalHome;
    if (originalKey === undefined) delete process.env.NVIDIA_API_KEY;
    else process.env.NVIDIA_API_KEY = originalKey;
    await fs.rm(directory, { recursive: true, force: true });
  }
});
