import assert from "node:assert/strict";
import { readFile } from "node:fs/promises";
import test from "node:test";

import {
  boot,
  makeMeta,
  paint,
} from "../public/aesthetic.computer/lib/nom.mjs";

const manifestUrl = new URL(
  "../public/aesthetic.computer/assets/artnom/manifest.json",
  import.meta.url,
);

async function manifest() {
  return JSON.parse(await readFile(manifestUrl, "utf8"));
}

test("artnom manifest carries AAT identity, source records, IIIF, and rights", async () => {
  const data = await manifest();
  assert.equal(data.categories.length, 5);
  assert.equal(data.categories.flatMap((category) => category.artworks).length, 50);
  for (const category of data.categories) {
    assert.match(category.aat, /^http:\/\/vocab\.getty\.edu\/aat\/\d+$/);
    assert.ok(category.artworks.length >= 10);
    for (const artwork of category.artworks) {
      assert.ok(artwork.aat.includes(category.aat));
      assert.match(artwork.image, /\/full\/200,\/0\/default\.jpg$/);
      assert.match(artwork.url, /^https:\/\/www\.artic\.edu\/artworks\/\d+$/);
      assert.ok(artwork.rights);
    }
  }
});

test("artnom boots the shared nom engine and paints image answers", async () => {
  const data = await manifest();
  const pixels = new Uint8ClampedArray(8 * 6 * 4).fill(255);
  let preloadCount = 0;
  boot({
    params: ["art"],
    artnom: data,
    hud: { label() {} },
    clock: { resync() {} },
    net: {
      preload: async () => {
        preloadCount += 1;
        return { img: { width: 8, height: 6, pixels } };
      },
    },
    num: { randInt: () => 0 },
  });
  await new Promise((resolve) => setImmediate(resolve));

  const calls = { paste: 0, write: 0 };
  const chain = {
    box() {},
    line() {},
    write() { calls.write += 1; },
  };
  paint({
    dark: true,
    screen: { width: 900, height: 700 },
    wipe() {},
    ink() { return chain; },
    box() {},
    line() {},
    write() { calls.write += 1; },
    paste(bitmap, x, y, transform) {
      calls.paste += 1;
      assert.equal(bitmap.width, 64);
      assert.equal(bitmap.height, 64);
      assert.ok(Number.isFinite(x) && Number.isFinite(y));
      assert.ok(transform.width > 0 && transform.height > 0);
    },
  });

  assert.equal(preloadCount, 25);
  assert.equal(calls.paste, 25);
  assert.ok(calls.write > 0);
  assert.deepEqual(makeMeta(["art"]), {
    title: "Artnom",
    desc: "Munch museum thumbnails by art-historical style — Getty AAT categories, IIIF images, and canonical object records.",
  });
});
