import assert from "node:assert/strict";
import { existsSync, mkdtempSync, readFileSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";
import test from "node:test";

import { publishToOutbox } from "../lib/outbox.mjs";

test("publishes media before a task-bound completion manifest", () => {
  const root = mkdtempSync(join(tmpdir(), "captutor-outbox-"));
  const video = join(root, "take.mp4");
  const captions = join(root, "take.vtt");
  writeFileSync(video, "video bytes");
  writeFileSync(captions, "WEBVTT\n");

  const result = publishToOutbox({
    outbox: join(root, "outbox"),
    video,
    captions,
    screenplay: "smoke",
    locale: "en",
    format: "docs",
    taskGid: "asana-123",
    now: new Date("2026-07-20T20:00:00Z"),
  });

  const manifest = JSON.parse(readFileSync(result.manifest, "utf8"));
  assert.equal(manifest.schema, "captutor-outbox/v1");
  assert.equal(manifest.status, "complete");
  assert.equal(manifest.taskGid, "asana-123");
  assert.equal(manifest.bytes, 11);
  assert.match(manifest.sha256, /^[a-f0-9]{64}$/);
  assert.ok(existsSync(result.video));
  assert.ok(existsSync(result.captions));
});
