import assert from "node:assert/strict";
import test from "node:test";
import { classifySeverity, newRelease, sha256 } from "../oskiewar-release.mjs";

test("release fingerprint is stable", () => {
  assert.equal(sha256("oskiewar"), sha256(Buffer.from("oskiewar")));
});

test("native changes escalate the release", () => {
  assert.equal(classifySeverity(["xbox/live/oskiewar.js"]), "live");
  assert.equal(classifySeverity(["apple/oskiewar/Sources/App.swift"]), "ios-native");
  assert.equal(classifySeverity(["xbox/native-bios/App.cpp"]), "xbox-native");
  assert.equal(classifySeverity(["apple/oskiewar/a", "xbox/native-bios/b"]), "multi-native");
});

test("a new fingerprint leaves every channel with a durable obligation", () => {
  const receipt = newRelease("next", "commit", "live", {
    channels: { web: { hash: "old" }, ios: { hash: "next" }, xbox: { hash: "old" } },
  });
  assert.equal(receipt.channels.web.status, "pending");
  assert.equal(receipt.channels.ios.status, "current");
  assert.equal(receipt.channels.xbox.status, "pending");
});
