import assert from "node:assert/strict";
import { readFile } from "node:fs/promises";
import test from "node:test";

const source = await readFile(new URL("../macos-native/main.swift", import.meta.url), "utf8");
const icon = await readFile(new URL("../macos-native/NoPaintIcon.swift", import.meta.url), "utf8");
const build = await readFile(new URL("../tools/build-macos-app.sh", import.meta.url), "utf8");

test("the app is a focused native No Paint host", () => {
  assert.match(source, /import AppKit/);
  assert.match(source, /import WebKit/);
  assert.match(source, /https:\/\/nopaint\.art\//);
  assert.match(source, /customUserAgent = "NoPaintMac\/1/);
  assert.match(source, /host == "nopaint\.art"/);
  assert.match(source, /NSWorkspace\.shared\.open\(url\)/);
});

test("New Painting requests a fresh full-stage No Paint canvas", () => {
  assert.match(source, /withTitle: "New Painting"/);
  assert.match(source, /URLQueryItem\(name: "fresh", value: "1"\)/);
});

test("the icon is the miniature No Paint interface in TrackDrum material", () => {
  assert.match(icon, /let canvasBox/);
  assert.match(icon, /label: "No"/);
  assert.match(icon, /label: "Paint"/);
  assert.match(icon, /bodyShadow/);
  assert.match(icon, /NSGradient/);
});

test("the Oskiewar-style builder emits a signed installable app", () => {
  assert.match(build, /xcrun swiftc/);
  assert.match(build, /iconutil -c icns/);
  assert.match(build, /codesign --force --deep --sign -/);
  assert.match(build, /\/Applications\/No Paint\.app/);
  assert.match(build, /open "\$destination"/);
});
