// node --test tezos/test/hen-paintings.test.mjs
import { test } from "node:test";
import assert from "node:assert/strict";
import { slugKey, paintingsFromCollection, upscaleFactor, nearestNeighbour, tokenMetadata, paintingTag, SIGNER } from "../hen-paintings.mjs";

test("slugs sort as timestamps even when old months are not zero-padded", () => {
  const files = [
    "https://aesthetic.computer/media/@jeffrey/painting/2023.5.27.23.59.39.png",
    "https://aesthetic.computer/media/@jeffrey/painting/2023.10.01.15.57.19.989.png",
    "https://aesthetic.computer/media/@jeffrey/painting/2026.08.06.17.12.20.837.png",
    "https://aesthetic.computer/media/@jeffrey/auth0|63effeeb2a7d55f8098d62f9/chat/1782187678513.png",
  ];
  const paintings = paintingsFromCollection(files);
  assert.deepEqual(paintings.map((p) => p.slug), ["2026.08.06.17.12.20.837", "2023.10.01.15.57.19.989", "2023.5.27.23.59.39"]);
  assert.ok(slugKey("2023.10.01.15.57.19.989") > slugKey("2023.5.27.23.59.39"));
  assert.equal(slugKey("auth0|abc/painting/2026.08.06.17.12.20.837"), slugKey("2026.08.06.17.12.20.837"), "a sub-prefixed slug compares by its timestamp");
});

test("upscale is an integer factor that clears the target, never below 1×", () => {
  assert.equal(upscaleFactor(128, 128, 1024), 8);
  assert.equal(upscaleFactor(447, 326, 1024), 3);
  assert.equal(upscaleFactor(128, 128, 350), 3);
  assert.equal(upscaleFactor(2048, 100, 1024), 1);
});

test("nearest neighbour keeps every pixel crisp", () => {
  const src = Buffer.from([255, 0, 0, 255, 0, 0, 255, 255]); // 2×1: red, blue
  const out = nearestNeighbour(src, 2, 1, 2); // 4×2
  assert.equal(out.length, 4 * 2 * 4);
  const px = (x, y) => [...out.subarray((y * 4 + x) * 4, (y * 4 + x) * 4 + 4)];
  assert.deepEqual(px(0, 0), [255, 0, 0, 255]);
  assert.deepEqual(px(1, 1), [255, 0, 0, 255]);
  assert.deepEqual(px(2, 0), [0, 0, 255, 255]);
  assert.deepEqual(px(3, 1), [0, 0, 255, 255]);
});

test("token metadata is TZIP-21 with the painting as artifact and the code as a tag", () => {
  const uris = { painting: "ipfs://a", display: "ipfs://b", thumb: "ipfs://c" };
  const m = tokenMetadata({ code: "hf2", handle: "jeffrey", when: "2026-08-07T00:12:22.000Z", width: 447, height: 326, uris, display: [1341, 978], thumb: [447, 326] });
  assert.equal(m.name, "#hf2");
  assert.equal(m.artifactUri, "ipfs://a");
  assert.equal(m.displayUri, "ipfs://b");
  assert.equal(m.thumbnailUri, "ipfs://c");
  assert.deepEqual(m.creators, [SIGNER]);
  assert.ok(m.tags.includes(paintingTag("hf2")));
  assert.match(m.description, /aesthetic\.computer\/painting~hf2/);
  assert.match(m.description, /finished 2026-08-07/);
  assert.equal(m.formats[0].dimensions.value, "447x326");
  assert.equal(m.formats[1].dimensions.value, "1341x978");
  assert.equal(m.date, "2026-08-07T00:12:22.000Z");
  assert.equal(m.decimals, 0);
});
