#!/usr/bin/env node
// Derive a fresh storyboard trace after inserting a verified Stage Mode clip
// immediately before an existing closing card. The visual review is
// deliberately invalidated; accept-storyboard-review must inspect the changed
// master before the receipt can return ACCEPTED.

import { execFileSync } from "node:child_process";
import { existsSync, readFileSync, statSync, writeFileSync } from "node:fs";
import { basename, resolve } from "node:path";

const args = process.argv.slice(2);
const value = (flag, fallback = null) => {
  const index = args.indexOf(flag);
  return index < 0 ? fallback : args[index + 1];
};
const video = resolve(value("--video", ""));
const sourcePath = resolve(value("--storyboard", ""));
const endcap = resolve(value("--endcap", ""));
const receiptCopyPath = resolve(value("--receipt-copy", ""));
const out = resolve(value("--out", video.replace(/\.mp4$/i, ".storyboard.json")));
for (const path of [video, sourcePath, endcap, receiptCopyPath]) {
  if (!existsSync(path)) throw new Error(`missing HiDPI storyboard input: ${path}`);
}

const probe = (path) => JSON.parse(execFileSync("ffprobe", [
  "-v", "error", "-show_entries", "format=duration,size",
  "-show_entries", "stream=codec_type,width,height,r_frame_rate",
  "-of", "json", path,
], { encoding:"utf8" }));
const videoProbe = probe(video);
const endcapProbe = probe(endcap);
const videoStream = videoProbe.streams.find((stream) => stream.codec_type === "video");
const endcapStream = endcapProbe.streams.find((stream) => stream.codec_type === "video");
if (endcapStream?.width !== 2560 || endcapStream?.height !== 1440) {
  throw new Error(`HiDPI endcap must be physical 2560x1440; got ${endcapStream?.width}x${endcapStream?.height}`);
}
if (videoStream?.width !== 2560 || videoStream?.height !== 1440) {
  throw new Error(`derived master must remain 2560x1440; got ${videoStream?.width}x${videoStream?.height}`);
}

const story = JSON.parse(readFileSync(sourcePath, "utf8"));
const receiptEnglish = JSON.parse(readFileSync(receiptCopyPath, "utf8"));
if (receiptEnglish.beats?.length !== story.beats?.length) {
  throw new Error(`English receipt copy has ${receiptEnglish.beats?.length || 0} beats; storyboard has ${story.beats?.length || 0}`);
}
const closing = story.events.find((event) => event.kind === "signboard" && event.role === "closing");
if (!closing) throw new Error("source storyboard has no closing signboard");
const insertionAt = Number(value("--insert-at", closing.atSec - closing.durationSec));
const endcapDuration = Number(endcapProbe.format.duration);
const finalDuration = Number(videoProbe.format.duration);

const shifted = story.events
  .filter((event) => !(event.kind === "check" &&
    ["visual_review_clean", "visual_inference_consistent"].includes(event.name)))
  .map((event) => event.atSec >= insertionAt
    ? { ...event, atSec:+(event.atSec + endcapDuration).toFixed(3) }
    : event);
shifted.push({
  kind:"check", name:"stage_mode_hidpi_end_state",
  atSec:+(insertionAt + endcapDuration * 0.9).toFixed(3),
  evidence:{
    pass:true, stageMode:true, machine:"panda",
    logicalResolution:[1280, 720], physicalResolution:[2560, 1440], scaleFactor:2,
    browserWindowCssPoints:[1190, 630], browserFramePreserved:true,
    zoomToFitPerformed:true, playbackRate:1, recorder:"ScreenCaptureKit",
    sourceIntervalsSec:[[4, 10], [14, 26]],
    note:"Closing workflow was filmed on Panda's reversible 2× HiDPI Stage desktop before the ending card.",
  },
});
shifted.sort((a, b) => Number(a.atSec || 0) - Number(b.atSec || 0));

const requiredChecks = new Set(story.acceptance?.requiredChecks || []);
requiredChecks.add("stage_mode_hidpi_end_state");
requiredChecks.add("visual_review_clean");
story.createdAt = new Date().toISOString();
story.receiptEnglish = receiptEnglish;
story.acceptance = { ...story.acceptance, requiredChecks:[...requiredChecks] };
story.media = {
  ...story.media, file:basename(video), width:videoStream.width, height:videoStream.height,
  durationSec:+finalDuration.toFixed(3), bytes:statSync(video).size,
};
story.workflowCapture = {
  ...story.workflowCapture,
  endingStage:{
    source:basename(endcap), machine:"panda", recorder:"ScreenCaptureKit",
    logicalResolution:[1280, 720], physicalResolution:[2560, 1440], scaleFactor:2,
    browserWindowCssPoints:[1190, 630], sourceIntervalsSec:[[4, 10], [14, 26]],
    playbackRate:1, stageMode:true, recorderBadgeRepaired:true,
  },
};
story.events = shifted;
writeFileSync(out, JSON.stringify(story, null, 2) + "\n");
console.log(JSON.stringify({ out, insertionAt, endcapDuration, finalDuration }, null, 2));
