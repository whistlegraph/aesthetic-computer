import test from "node:test";
import assert from "node:assert/strict";
import { mkdtempSync, writeFileSync, utimesSync, mkdirSync, renameSync } from "node:fs";
import { tmpdir } from "node:os";
import path from "node:path";
import { mediaPaths, mediaFile, watchMedia, itemText, mediaChanged, MEDIA_TYPES } from "../src/media.mjs";

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

test("reads explicit tool targets without treating printed paths as selections", () => {
  const text = itemText({ command: "ls", tool: "Read · a.png", changes: [{ path: "b.mov" }], input: { file_path: "c.wav" }, aggregatedOutput: "wrote d.pdf" });
  for (const name of ["ls", "a.png", "b.mov", "c.wav"]) assert.ok(text.includes(name), name);
  assert.ok(!text.includes("d.pdf"));
  assert.equal(itemText(null), "");
});

test("repository listings and source reads cannot switch to an unrelated media file", () => {
  for (const command of ["git status --short", "rg --files", "cat source.mjs"]) {
    const item = { type: "commandExecution", command, exitCode: 0,
      aggregatedOutput: `?? ${png}\n?? ${mov}\n${wav}\n` };
    assert.deepEqual(mediaPaths(itemText(item), root), []);
  }
  const item = { type: "commandExecution", command: `open ${png}`, aggregatedOutput: mov };
  assert.deepEqual(mediaPaths(itemText(item), root).map(file => file.path), [png]);
  const view = { type: "dynamicToolCall", tool: "view_image", input: { path: png } };
  assert.deepEqual(mediaPaths(itemText(view), root).map(file => file.path), [png]);
});

test("a sighting replaces the current one only when the file or its write time differs", () => {
  const a = { path: png, mtimeMs: 1 };
  assert.equal(mediaChanged(null, a), true);
  assert.equal(mediaChanged(a, { path: png, mtimeMs: 1 }), false);
  assert.equal(mediaChanged(a, { path: png, mtimeMs: 2 }), true);
  assert.equal(mediaChanged(a, { path: mov, mtimeMs: 1 }), true);
  assert.equal(mediaChanged(a, undefined), false);
});

test("takes one path whole, spaces included", () => {
  const spaced = path.join(root, "my track.mp3");
  writeFileSync(spaced, "ID3");
  assert.equal(mediaFile("my track.mp3", root)?.mime, "audio/mpeg");
  assert.equal(mediaFile("~/frame.png", "/", { home: root })?.path, png);
  assert.equal(mediaFile("empty.png", root), null);
  assert.equal(mediaFile("notes.txt", root), null);
  assert.equal(mediaFile("", root), null);
});

test("follows a file that is rewritten in place or replaced by a rename", async () => {
  const dir = mkdtempSync(path.join(tmpdir(), "easel-watch-"));
  const mp3 = path.join(dir, "now.mp3");
  writeFileSync(mp3, "one");
  utimesSync(mp3, new Date(1000), new Date(1000));
  const seen = [];
  const stop = watchMedia(mediaFile(mp3, dir), (next) => seen.push(next.size), { settleMs: 30 });
  const settle = () => new Promise((resolve) => setTimeout(resolve, 250));
  writeFileSync(path.join(dir, "other.mp3"), "ignored");
  await settle();
  assert.deepEqual(seen, []);
  writeFileSync(mp3, "two!");
  await settle();
  writeFileSync(path.join(dir, ".tmp.mp3"), "three");
  renameSync(path.join(dir, ".tmp.mp3"), mp3);
  await settle();
  stop();
  writeFileSync(mp3, "four!!");
  await settle();
  assert.deepEqual(seen, [4, 5]);
});

test("every kind the card can show has a glyph and a mime", () => {
  for (const [ext, type] of MEDIA_TYPES) {
    assert.ok(["picture", "sound", "paper", "video"].includes(type.kind), ext);
    assert.ok(type.mime.includes("/") && type.glyph, ext);
  }
});
