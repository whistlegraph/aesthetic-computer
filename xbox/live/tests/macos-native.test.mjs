import assert from "node:assert/strict";
import { readFile } from "node:fs/promises";
import test from "node:test";

const source = await readFile(new URL("../../macos-native/main.swift", import.meta.url), "utf8");
const build = await readFile(new URL("../../tools/build-macos-app.sh", import.meta.url), "utf8");

test("the macOS Dock app uses a native host rather than a WebView", () => {
  assert.match(source, /import AppKit/);
  assert.match(source, /import JavaScriptCore/);
  assert.match(source, /CVDisplayLink/);
  assert.match(source, /import GameController/);
  assert.match(source, /import AVFoundation/);
  assert.doesNotMatch(source, /WebKit|WKWebView/);
  assert.match(source, /titlebarAppearsTransparent = false/);
  assert.doesNotMatch(source, /\.fullSizeContentView/);
  assert.match(source, /viewDidChangeBackingProperties/);
  assert.match(source, /backingScaleFactor/);
});

test("native title clicks start only inside the shared button", () => {
  assert.match(source, /if titleButtonContains\(point\) \{ input\.pulse\("A"\) \}/);
  assert.match(source, /else \{ audio\.drum\("block", velocity: 0\.32, pan: 0\) \}/);
  assert.match(source, /NSCursor\.pointingHand\.set\(\)/);
});

test("the macOS app runs the shared Xbox game source and native host APIs", () => {
  assert.match(build, /xbox\/live\/hello\.js/);
  for (const binding of ["wipe", "box", "line", "triangle3d", "comicWrite",
    "gamepad", "runtime", "gameSignal", "saveReplay", "publishLive"])
    assert.match(source, new RegExp(`forKeyedSubscript: "${binding}"`));
});

test("the macOS installer creates and pins the canonical oskiewar app", () => {
  assert.match(build, /\/Applications\/oskiewar\.app/);
  assert.match(build, /com\.apple\.dock persistent-apps/);
  assert.match(build, /open "\$destination"/);
});
