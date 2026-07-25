#!/usr/bin/env node

import { readFile, rename, stat, writeFile } from "node:fs/promises";
import { createHash } from "node:crypto";
import path from "node:path";

function usage() {
  console.error(
    "Usage: accept-storyboard-review.mjs --storyboard <json> --reviewer <name> --frames <comma-separated paths> --inference-report <json> [--video <mp4>] [--note <text>]",
  );
  process.exit(2);
}

const args = process.argv.slice(2);
const valueFor = (flag) => {
  const index = args.indexOf(flag);
  return index === -1 ? undefined : args[index + 1];
};

const storyboardPath = valueFor("--storyboard");
const reviewer = valueFor("--reviewer");
const videoPath = valueFor("--video");
const inferenceReportPath = valueFor("--inference-report");
const frames = valueFor("--frames")?.split(",").filter(Boolean) ?? [];
const note = valueFor("--note") ?? "Opening, workflow, generated output, branding, captions, and closing reviewed clean.";

if (!storyboardPath || !reviewer || frames.length === 0 || !inferenceReportPath) usage();

const storyboard = JSON.parse(await readFile(storyboardPath, "utf8"));
const inference = JSON.parse(await readFile(inferenceReportPath, "utf8"));
if (inference.schema !== "captutor-visual-consistency/v1" || inference.pass !== true ||
    inference.result?.consistent_hidpi !== true) {
  throw new Error(`visual consistency inference did not pass: ${inferenceReportPath}`);
}
if (videoPath) {
  const videoBytes = await readFile(videoPath);
  const videoSha256 = createHash("sha256").update(videoBytes).digest("hex");
  if (inference.videoSha256 !== videoSha256) {
    throw new Error("visual consistency inference belongs to a different video");
  }
}
storyboard.events ??= [];
storyboard.acceptance ??= {};
storyboard.acceptance.requiredChecks ??= [];

if (videoPath) {
  const videoStat = await stat(videoPath);
  storyboard.media ??= {};
  storyboard.media.file = path.basename(videoPath);
  storyboard.media.bytes = videoStat.size;
}

storyboard.events = storyboard.events.filter(
  (event) => !(event.kind === "check" &&
    ["visual_review_clean", "visual_inference_consistent"].includes(event.name)),
);
storyboard.events.push({
  kind:"check",
  name:"visual_inference_consistent",
  atSec:storyboard.media?.durationSec ?? null,
  evidence:{
    pass:true, schema:inference.schema, model:inference.model,
    responseId:inference.responseId, report:path.resolve(inferenceReportPath),
    samples:inference.samples, confidence:inference.result?.confidence,
    note:inference.result?.evidence?.join(" ") || "All required live-browser series match the HiDPI reference.",
  },
});
storyboard.events.push({
  kind: "check",
  name: "visual_review_clean",
  atSec: storyboard.media?.durationSec ?? null,
  evidence: {
    pass: true,
    reviewer,
    reviewedAt: new Date().toISOString(),
    videoWatched: true,
    frames,
    note,
  },
});

if (!storyboard.acceptance.requiredChecks.includes("visual_review_clean")) {
  storyboard.acceptance.requiredChecks.push("visual_review_clean");
}
if (!storyboard.acceptance.requiredChecks.includes("visual_inference_consistent")) {
  storyboard.acceptance.requiredChecks.push("visual_inference_consistent");
}

const temporaryPath = path.join(
  path.dirname(storyboardPath),
  `.${path.basename(storyboardPath)}.${process.pid}.tmp`,
);
await writeFile(temporaryPath, `${JSON.stringify(storyboard, null, 2)}\n`);
await rename(temporaryPath, storyboardPath);
console.log(JSON.stringify({
  storyboard:storyboardPath,
  checks:["visual_inference_consistent", "visual_review_clean"],
  inferenceReport:path.resolve(inferenceReportPath), frames,
}, null, 2));
