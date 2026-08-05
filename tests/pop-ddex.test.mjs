import assert from "node:assert/strict";
import { mkdtemp, readFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";
import test from "node:test";

import {
  buildErnXml,
  collectIssues,
  exportPacket,
  formatDuration,
  loadRelease,
} from "../pop/bin/ddex.mjs";

test("formats DDEX durations", () => {
  assert.equal(formatDuration(83.5714), "PT1M23.571S");
  assert.equal(formatDuration(12), "PT12S");
});

test("Marimbaba names all private production blockers", async () => {
  const { record } = await loadRelease("marimbaba", "/definitely/missing/release.private.json");
  const issues = await collectIssues(record);
  const blockerPaths = issues.filter((issue) => issue.severity === "blocker").map((issue) => issue.path);
  assert.deepEqual(blockerPaths, [
    "message.sender.dpid",
    "message.sender.fullName",
    "message.recipient.dpid",
    "message.recipient.fullName",
    "identifiers.isrc",
    "identifiers.icpn",
    "rights.pLine.year",
    "rights.pLine.text",
    "rights.cLine.year",
    "rights.cLine.text",
    "assets.audio.localPath",
    "assets.cover.localPath",
  ]);
});

test("draft XML is a namespaced ERN 4.3.2 TestMessage", async () => {
  const { record } = await loadRelease("marimbaba", "/definitely/missing/release.private.json");
  const output = buildErnXml(record, {
    draft: true,
    createdAt: new Date("2026-08-04T12:00:00.000Z"),
    messageId: "AC-TEST-MARIMBABA",
  });
  assert.match(output, /xmlns:ern="http:\/\/ddex.net\/xml\/ern\/432"/);
  assert.match(output, /ReleaseProfileVersionId="SimpleAudioSingle"/);
  assert.match(output, /AvsVersionId="11"/);
  assert.match(output, /<MessageControlType>TestMessage<\/MessageControlType>/);
  assert.match(output, /EVALUATION ONLY/);
  assert.match(output, /<HasVocalPerformance>false<\/HasVocalPerformance>/);
  assert.match(output, /<ContainsAI>All<\/ContainsAI>/);
});

test("draft packet writes XML and an evaluation receipt without copying assets", async () => {
  const { record } = await loadRelease("marimbaba", "/definitely/missing/release.private.json");
  const out = await mkdtemp(join(tmpdir(), "ac-ddex-test-"));
  const result = await exportPacket(record, { draft: true, out });
  const receipt = JSON.parse(await readFile(join(out, "receipt.json"), "utf8"));
  assert.equal(result.outputDirectory, out);
  assert.equal(receipt.mode, "evaluation");
  assert.equal(receipt.deliveryAuthorized, false);
  assert.match(await readFile(join(out, "NewReleaseMessage.xml"), "utf8"), /TestMessage/);
});
