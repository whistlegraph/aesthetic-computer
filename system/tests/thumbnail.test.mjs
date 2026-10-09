// node --experimental-vm-modules --test system/tests/thumbnail.test.mjs
import test from "node:test";
import assert from "node:assert/strict";
import vm from "node:vm";
import { readFile } from "node:fs/promises";
import sharp from "sharp";

const source = await readFile(new URL("../backend/thumbnail.mjs", import.meta.url), "utf8");
const original = await sharp({ create: { width: 2, height: 1, channels: 3, background: "red" } }).png().toBuffer();

async function fixture({ fallback = false } = {}) {
  const urls = [];
  const context = vm.createContext({ Buffer, process: { env: {}, argv: [] }, console: { log() {}, warn() {} } });
  const got = async url => {
    urls.push(url);
    if (fallback && urls.length === 1) throw Error("Pixel temporarily unavailable");
    return { body: original };
  };
  const gotModule = new vm.SyntheticModule(["got"], function () { this.setExport("got", got); }, { context });
  await gotModule.link(() => { throw Error("Unexpected import"); });
  await gotModule.evaluate();
  const module = new vm.SourceTextModule(source, { context, importModuleDynamically: () => gotModule });
  await module.link(name => {
    assert.equal(name, "sharp");
    return new vm.SyntheticModule(["default"], function () { this.setExport("default", sharp); }, { context });
  });
  await module.evaluate();
  return { urls, thumbnail: module.namespace.getThumbnailFromSlug };
}

test("bare and owner-qualified painting slugs request the same thumbnail", async () => {
  const slug = "2026.10.08.14.06.40.966";
  for (const key of [slug, `${slug}.png`, `auth0|fixture/painting/${slug}`, `auth0|fixture/painting/${slug}.png`]) {
    const f = await fixture();
    const image = await f.thumbnail(key, "@fixture", { userId: "auth0|fixture" });
    assert.equal(f.urls[0], `https://aesthetic.computer/api/pixel/512:contain/@fixture/painting/${slug}.png`);
    assert.equal(f.urls.length, 1);
    assert.deepEqual(image, original);
  }
});

test("thumbnail fallback uses the same normalized painting key and resizes the image", async () => {
  const f = await fixture({ fallback: true });
  const image = await f.thumbnail("auth0|fixture/painting/example.png", "fixture", { userId: "auth0|fixture", size: 16 });
  assert.deepEqual(f.urls, [
    "https://aesthetic.computer/api/pixel/16:contain/@fixture/painting/example.png",
    "https://aesthetic.computer/media/@fixture/painting/example.png",
  ]);
  const metadata = await sharp(image).metadata();
  assert.equal(metadata.width, 16);
  assert.equal(metadata.height, 16);
});

test("guest paintings keep their public bucket and unrelated owner prefixes are not stripped", async () => {
  const guest = await fixture();
  await guest.thumbnail("guest.png", null, { size: 16 });
  assert.deepEqual(guest.urls, ["https://art-aesthetic-computer.sfo3.digitaloceanspaces.com/guest.png"]);
  const other = await fixture();
  await other.thumbnail("auth0|other/painting/example", "fixture", { userId: "auth0|fixture" });
  assert.equal(other.urls[0], "https://aesthetic.computer/api/pixel/512:contain/@fixture/painting/auth0|other/painting/example.png");
});
