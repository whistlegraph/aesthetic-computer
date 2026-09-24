import test from "node:test";
import assert from "node:assert/strict";
import { mkdtempSync, writeFileSync, utimesSync, mkdirSync } from "node:fs";
import { tmpdir } from "node:os";
import path from "node:path";
import { mediaPaths, itemText, mediaChanged, MEDIA_TYPES } from "../src/media.mjs";

const root = mkdtempSync(path.join(tmpdir(), "easel-media-"));
const png = path.join(root, "frame.png");
const mov = path.join(root, "reel.mov");
const wav = path.join(root, "voice.wav");
mkdirSync(path.join(root, "out"));
writeFileSync(png, Buffer.from([0x89, 0x50]));
writeFileSync(mov, Buffer.from([0, 0, 0, 1]));
writeFileSync(wav, Buffer.from("RIFF"));
writeFileSync(path.join(root, "empty.png"), "");
utimesSync(png, new Date(2000), new Date(2000));
utimesSync(mov, new Date(9000), new Date(9000));
utimesSync(wav, new Date(5000), new Date(5000));

test("finds the media files a tool names, freshest first, whatever the quoting", () => {
  const text = `ffmpeg -i "${png}" '${wav}' -o ${mov}; echo {"file_path":"${png}"} frame.png:12 https://example.com/a.png`;
  const found = mediaPaths(text, root);
  assert.deepEqual(found.map((f) => f.name), ["reel.mov", "voice.wav", "frame.png"]);
  assert.equal(found[0].kind, "video");
  assert.equal(found[0].mime, "video/quicktime");
  assert.equal(found[2].mime, "image/png");
});

test("resolves relative and home paths against the workspace, and skips what is not a file", () => {
  const found = mediaPaths("open ./frame.png and out/missing.png and empty.png and ~/nowhere.png", root, { home: root });
  assert.deepEqual(found.map((f) => f.name), ["frame.png"]);
  assert.equal(found[0].path, png);
  assert.deepEqual(mediaPaths("nothing here", root), []);
  assert.deepEqual(mediaPaths("", root), []);
});

test("reads a tool item's command, changes, input and output", () => {
  const text = itemText({ command: "ls", tool: "Read · a.png", changes: [{ path: "b.mov" }], input: { file_path: "c.wav" }, aggregatedOutput: "wrote d.pdf" });
  for (const name of ["ls", "a.png", "b.mov", "c.wav", "d.pdf"]) assert.ok(text.includes(name), name);
  assert.equal(itemText(null), "");
});

test("a sighting replaces the current one only when the file or its write time differs", () => {
  const a = { path: png, mtimeMs: 1 };
  assert.equal(mediaChanged(null, a), true);
  assert.equal(mediaChanged(a, { path: png, mtimeMs: 1 }), false);
  assert.equal(mediaChanged(a, { path: png, mtimeMs: 2 }), true);
  assert.equal(mediaChanged(a, { path: mov, mtimeMs: 1 }), true);
  assert.equal(mediaChanged(a, undefined), false);
});

test("every kind the card can show has a glyph and a mime", () => {
  for (const [ext, type] of MEDIA_TYPES) {
    assert.ok(["picture", "sound", "paper", "video"].includes(type.kind), ext);
    assert.ok(type.mime.includes("/") && type.glyph, ext);
  }
});
