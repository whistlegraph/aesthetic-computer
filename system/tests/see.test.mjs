import test from "node:test";
import assert from "node:assert/strict";
import {
  boot,
  act,
  paint,
  leave,
} from "../public/aesthetic.computer/disks/see.mjs";

test("see blocks retry taps until the server cooldown expires", async (t) => {
  let now = 1000000;
  let requests = 0;
  t.mock.method(Date, "now", () => now);
  t.mock.method(globalThis, "fetch", async () => {
    requests++;
    return Response.json(
      { ok: false, reason: "image_budget_exhausted", retry_after: 120 },
      {
        status: 429,
        headers: { "Retry-After": "60" },
      },
    );
  });
  const settle = () => new Promise((resolve) => setImmediate(resolve));
  const tap = () => act({ event: { is: (name) => name === "touch" } });
  boot({ params: ["synthetic", "square"], colon: [], hud: { label() {} } });
  await settle();
  tap();
  await settle();
  assert.equal(requests, 1);
  const labels = [];
  const write = (text) => labels.push(text);
  const ink = () => ({ write });
  paint({ wipe() {}, ink, write, screen: { width: 320, height: 240 } });
  assert.ok(labels.includes("retry in 60s"));
  assert.ok(labels.includes("image allowance is used up"));
  assert.ok(!labels.includes("tap to retry"));
  now += 60000;
  tap();
  await settle();
  assert.equal(requests, 2);
  leave();
});

test("see decodes worker images without DOM APIs and fits the full image on small screens", async (t) => {
  const piece = await import(
    "../public/aesthetic.computer/disks/see.mjs?worker-decode-test"
  );
  const originalFetch = globalThis.fetch;
  let closed = false,
    drawn = false;
  const decoded = {
    width: 1024,
    height: 1024,
    close() {
      closed = true;
    },
  };
  const pixels = new Uint8ClampedArray(1024 * 1024 * 4).fill(255);
  const originalBitmap = globalThis.createImageBitmap;
  const originalCanvas = globalThis.OffscreenCanvas;
  globalThis.createImageBitmap = async (blob) => {
    assert.equal(blob.type, "image/jpeg");
    assert.ok(blob.size > 0);
    return decoded;
  };
  globalThis.OffscreenCanvas = class {
    constructor(width, height) {
      assert.equal(width, 1024);
      assert.equal(height, 1024);
    }
    getContext(kind) {
      assert.equal(kind, "2d");
      return {
        drawImage(image) {
          assert.equal(image, decoded);
          drawn = true;
        },
        getImageData() {
          assert.equal(drawn, true);
          return { data: pixels };
        },
      };
    }
  };
  t.after(() => {
    if (originalBitmap === undefined) delete globalThis.createImageBitmap;
    else globalThis.createImageBitmap = originalBitmap;
    if (originalCanvas === undefined) delete globalThis.OffscreenCanvas;
    else globalThis.OffscreenCanvas = originalCanvas;
    piece.leave();
  });
  t.mock.method(globalThis, "fetch", async (url, options) => {
    if (url === "/api/flux")
      return Response.json({
        ok: true,
        png: "data:image/jpeg;base64,/9j/2Q==",
        seed: null,
        provider: "cloudflare",
        elapsed_ms: 1500,
      });
    assert.match(url, /^data:image\/jpeg;base64,/);
    return originalFetch(url, options);
  });
  piece.boot({ params: ["synthetic"], colon: [], hud: { label() {} } });
  for (let i = 0; i < 20 && !closed; i++) await new Promise(setImmediate);
  assert.equal(closed, true, "decoded ImageBitmap is released");
  const pastes = [],
    labels = [];
  const write = (text) => labels.push(text);
  const render = (width, height) =>
    piece.paint({
      wipe() {},
      ink: () => ({ write }),
      write,
      screen: { width, height },
      paste: (...args) => pastes.push(args),
    });
  render(320, 640);
  assert.equal(pastes.length, 1);
  assert.equal(pastes[0][0].pixels, pixels);
  assert.deepEqual(pastes[0].slice(1), [0, 160, 0.3125]);
  assert.ok(labels.some((label) => label.includes("cloudflare")));
  render(2048, 3072);
  assert.deepEqual(pastes[1].slice(1), [0, 512, 2]);
});
