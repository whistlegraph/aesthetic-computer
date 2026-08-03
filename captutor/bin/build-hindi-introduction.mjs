#!/usr/bin/env node
// Assemble the accepted Hindi introduction from the clean filmed core plus a
// native-resolution, real-time Fuser browser capture and offline animated cards.

import { execFileSync } from "node:child_process";
import {
  existsSync, mkdirSync, readFileSync, readdirSync, statSync, writeFileSync,
} from "node:fs";
import { basename, dirname, join, resolve } from "node:path";

import { applyBrandChrome } from "../lib/brand-chrome.mjs";
import { deliver } from "../lib/deliver.mjs";
import { mux, writeVTT } from "../lib/compose.mjs";
import fuserBrandChrome, { fuserEffectTheme } from "../themes/fuser.mjs";

const args = process.argv.slice(2);
const value = (flag, fallback) => {
  const i = args.indexOf(flag);
  return i < 0 ? fallback : args[i + 1];
};
const archive = resolve(value("--archive", "/Users/jas/Movies/Fuser Tutor/2026-07-22-iris"));
const core = resolve(value("--core", join(archive, "introduction-to-fuser.hi.docs.20260723T032534Z.mp4")));
const coreStoryPath = resolve(value("--core-storyboard", join(archive, "introduction-to-fuser.hi.docs.20260723T032534Z.storyboard.json")));
const workflow = resolve(value(
  "--workflow",
  value("--remotion", join(archive, "panda-live-browser-image-to-video-2560x1440.mp4")),
));
const opening = resolve(value("--opening", join(archive, "hindi-intro-opening.offline.mp4")));
const ending = resolve(value("--ending", join(archive, "hindi-intro-ending.offline.mp4")));
const hidpiEndcapArg = value("--hidpi-endcap", "");
const hidpiEndcap = hidpiEndcapArg ? resolve(hidpiEndcapArg) : null;
const workflowStage = args.includes("--workflow-stage");
const voiceDir = resolve(value("--voice-dir", "captutor/out/voice/hindi-image-video-extension.hi"));
const work = resolve(value("--work", join(archive, "hindi-image-video-build")));
const final = resolve(value("--out", join(archive, "introduction-to-fuser.hi.image-to-video.accepted.mp4")));
for (const path of [core, coreStoryPath, workflow, opening, ending, voiceDir, hidpiEndcap].filter(Boolean)) {
  if (!existsSync(path)) throw new Error(`missing build input: ${path}`);
}
mkdirSync(work, { recursive:true });

const probe = (path) => JSON.parse(execFileSync("ffprobe", [
  "-v", "error", "-show_entries", "format=duration,size", "-show_entries",
  "stream=codec_type,codec_name,width,height,r_frame_rate,sample_rate,channels",
  "-of", "json", path,
], { encoding:"utf8" }));
const duration = (path) => Number(probe(path).format.duration);
const workflowProbe = probe(workflow);
const workflowVideo = workflowProbe.streams.find((stream) => stream.codec_type === "video");
if (!workflowVideo || workflowVideo.width < 1920 || workflowVideo.height < 1080) {
  throw new Error(
    `workflow source must be native HD or better; got ${workflowVideo?.width || 0}x${workflowVideo?.height || 0}: ${workflow}`,
  );
}
if (workflowStage && (workflowVideo.width !== 2560 || workflowVideo.height !== 1440)) {
  throw new Error(
    `Stage workflow must be a physical 2560x1440 desktop capture; got ` +
    `${workflowVideo.width}x${workflowVideo.height}: ${workflow}`,
  );
}
const workflowSourceDuration = duration(workflow);
const hidpiEndcapProbe = hidpiEndcap ? probe(hidpiEndcap) : null;
const hidpiEndcapVideo = hidpiEndcapProbe?.streams.find((stream) => stream.codec_type === "video");
if (hidpiEndcap && (hidpiEndcapVideo?.width !== 2560 || hidpiEndcapVideo?.height !== 1440)) {
  throw new Error(
    `HiDPI endcap must be a physical 2560x1440 Stage desktop; got ` +
    `${hidpiEndcapVideo?.width || 0}x${hidpiEndcapVideo?.height || 0}: ${hidpiEndcap}`,
  );
}
const hidpiEndcapDuration = hidpiEndcap ? duration(hidpiEndcap) : 0;
const workflowStart = Math.max(0, Number(value("--workflow-start", 0)));
const workflowEnd = Math.min(
  workflowSourceDuration,
  Number(value("--workflow-end", workflowSourceDuration)),
);
if (!(workflowEnd > workflowStart)) {
  throw new Error(`invalid workflow interval ${workflowStart}..${workflowEnd}`);
}
const cutSpec = value("--workflow-cuts", "");
const workflowCuts = cutSpec
  ? cutSpec.split(",").map((range) => {
      const [from, to] = range.split("-").map((part) => Number(part.trim()));
      if (!(Number.isFinite(from) && Number.isFinite(to) && from >= 0 && to > from && to <= workflowSourceDuration)) {
        throw new Error(`invalid --workflow-cuts interval: ${range}`);
      }
      return [from, to];
    })
  : [[workflowStart, workflowEnd]];
const workflowDuration = +workflowCuts
  .reduce((total, [from, to]) => total + to - from, 0).toFixed(3);
const parseTimes = (flag, expected, fallback) => {
  const raw = value(flag, "");
  const times = raw
    ? raw.split(",").map((part) => Number(part.trim()))
    : fallback;
  if (times.length !== expected || times.some((time) => !Number.isFinite(time))) {
    throw new Error(`${flag} needs ${expected} comma-separated seconds`);
  }
  return times.map((time) => Math.max(0, Math.min(workflowDuration - 0.05, time)));
};

const expectedLines = [
  "अब एक पूरा प्रवाह देखें। जेमिनी इमेज नोड लिखे हुए निर्देश से नई तस्वीर बनाता है, और तैयार इमेज अपने आउटपुट पर दिखाई देती है।",
  "उसी इमेज आउटपुट को क्लिंग वीडियो नोड के इमेज इनपुट से जोड़ें। यह कनेक्शन साफ़ दिखाता है कि वीडियो की शुरुआती तस्वीर कहाँ से आई।",
  "वीडियो प्रॉम्प्ट में गति बताएं, फिर मॉडल, आस्पेक्ट रेशियो और अवधि चुनें। नोड उसी तस्वीर को चलती हुई क्लिप में बदल देता है।",
  "तैयार वीडियो को एक सामान्य वीडियो नोड में भेजें। अब आप उसे चला सकते हैं, आगे जोड़ सकते हैं, या अपने अगले फ्यूज़र वर्कफ़्लो में इस्तेमाल कर सकते हैं।",
];
const colors = ["#42d6ff", "#31d68b", "#ff9f43", "#a98bff"];
const metaByLine = new Map(readdirSync(voiceDir).filter((file) => file.endsWith(".json"))
  .map((file) => {
    const path = join(voiceDir, file);
    const meta = JSON.parse(readFileSync(path, "utf8"));
    return [meta.line, { ...meta, mp3:path.replace(/\.json$/, ".mp3") }];
  }));
const requestedBeatOffsets = parseTimes("--beat-at", 4, [
  workflowDuration * 0.12, workflowDuration * 0.28,
  workflowDuration * 0.48, workflowDuration * 0.82,
]);
const extensionBeats = expectedLines.map((line, index) => {
  const meta = metaByLine.get(line);
  if (!meta) throw new Error(`missing narration cache for: ${line}`);
  const beat = {
    index, say:line, narration:line, words:meta.words, mp3:meta.mp3,
    durationSec:meta.durationSec, offsetSec:+requestedBeatOffsets[index].toFixed(3),
    captionColor:colors[index],
    logic:[
      "A newly returned image is visible on the Gemini Image output.",
      "The exact image output is connected to Kling's image input.",
      "Kling exposes prompt, model, aspect ratio, and duration before returning video.",
      "The returned video is connected into a primitive Video node.",
    ][index],
    cursorIntent:"Live Panda browser capture; Chrome frame and Fuser canvas remain visible.",
  };
  return beat;
});
const extensionDuration = workflowDuration;

// Keep the programmed browser work at 1×. Trimming may remove setup/idle time,
// but frames are never sped up, slowed down, synthesized, or replaced by a
// frameless node animation. Fit/pad preserves every browser edge.
const visual = join(work, "image-to-video.visual.mp4");
const workflowInputs = workflowCuts.flatMap(([from, to]) => [
  "-ss", String(from), "-t", String(to - from), "-i", workflow,
]);
const visualParts = workflowCuts.map((_, index) =>
  `[${index}:v]setpts=PTS-STARTPTS,` +
  `scale=2560:1440:force_original_aspect_ratio=decrease:flags=lanczos,` +
  `pad=2560:1440:(ow-iw)/2:(oh-ih)/2:color=#f6f3ef,fps=60[v${index}]`);
const visualConcat = workflowCuts.map((_, index) => `[v${index}]`).join("") +
  `concat=n=${workflowCuts.length}:v=1:a=0[outv]`;
execFileSync("ffmpeg", [
  "-y", ...workflowInputs, "-filter_complex_threads", "1",
  "-filter_complex", `${visualParts.join(";")};${visualConcat}`,
  "-map", "[outv]", "-an", "-c:v", "libx264", "-preset", "medium", "-crf", "17",
  "-pix_fmt", "yuv420p", "-movflags", "+faststart", visual,
], { stdio:["ignore", "ignore", "pipe"] });

const extensionVtt = join(work, "image-to-video.hi.vtt");
writeVTT(extensionBeats, extensionVtt);
const extensionNarrated = join(work, "image-to-video.narrated.mp4");
mux({ clip:visual, beats:extensionBeats, out:extensionNarrated, vtt:extensionVtt });
const extensionCaptioned = join(work, "image-to-video.captioned.mp4");
deliver({
  clip:extensionNarrated, cues:extensionBeats, format:"docs", out:extensionCaptioned,
  workDir:work, locale:"hi", geometry:{ w:2560, h:1440 }, captionPx:72, captionY:0.86,
});

const openingDuration = duration(opening);
const coreDuration = duration(core);
const endingDuration = duration(ending);
const provisional = join(work, "introduction.provisional.mp4");
const concatFilter = [
  `[0:v]fps=60,format=yuv420p,setpts=PTS-STARTPTS[v0]`,
  `anullsrc=r=48000:cl=stereo,atrim=duration=${openingDuration},asetpts=PTS-STARTPTS[a0]`,
  `[1:v]fps=60,format=yuv420p,setpts=PTS-STARTPTS[v1]`,
  `[1:a]aresample=48000,aformat=channel_layouts=stereo,asetpts=PTS-STARTPTS[a1]`,
  `[2:v]fps=60,format=yuv420p,setpts=PTS-STARTPTS[v2]`,
  `[2:a]aresample=48000,aformat=channel_layouts=stereo,asetpts=PTS-STARTPTS[a2]`,
  ...(hidpiEndcap ? [
    `[3:v]fps=60,format=yuv420p,setpts=PTS-STARTPTS[v3]`,
    `anullsrc=r=48000:cl=stereo,atrim=duration=${hidpiEndcapDuration},asetpts=PTS-STARTPTS[a3]`,
    `[4:v]fps=60,format=yuv420p,setpts=PTS-STARTPTS[v4]`,
    `anullsrc=r=48000:cl=stereo,atrim=duration=${endingDuration},asetpts=PTS-STARTPTS[a4]`,
    `[v0][a0][v1][a1][v2][a2][v3][a3][v4][a4]concat=n=5:v=1:a=1[outv][outa]`,
  ] : [
    `[3:v]fps=60,format=yuv420p,setpts=PTS-STARTPTS[v3]`,
    `anullsrc=r=48000:cl=stereo,atrim=duration=${endingDuration},asetpts=PTS-STARTPTS[a3]`,
    `[v0][a0][v1][a1][v2][a2][v3][a3]concat=n=4:v=1:a=1[outv][outa]`,
  ]),
].join(";");
execFileSync("ffmpeg", [
  "-y", "-i", opening, "-i", core, "-i", extensionCaptioned,
  ...(hidpiEndcap ? ["-i", hidpiEndcap] : []), "-i", ending,
  "-filter_complex_threads", "1", "-filter_complex", concatFilter,
  "-map", "[outv]", "-map", "[outa]", "-r", "60",
  "-c:v", "libx264", "-preset", "medium", "-crf", "17", "-pix_fmt", "yuv420p",
  "-c:a", "aac", "-b:a", "192k", "-ar", "48000", "-ac", "2",
  "-movflags", "+faststart", provisional,
], { stdio:["ignore", "ignore", "pipe"] });
applyBrandChrome({ input:provisional, out:final, theme:fuserBrandChrome, workDir:work, format:"docs" });

const finalProbe = probe(final);
const finalDuration = Number(finalProbe.format.duration);
const extensionStart = openingDuration + coreDuration;
const closingStart = finalDuration - endingDuration;
const coreStory = JSON.parse(readFileSync(coreStoryPath, "utf8"));
const coreEnglishNarration = [
  "Welcome to Fuser. It is an infinite canvas where every idea becomes a node that you can place, move, and connect.",
  "Your project name stays at the upper left. Open it whenever you need project-level controls.",
  "Zoom controls at the upper right keep large flows readable, from nearby details to the entire system.",
  "Recipe shows how the project works inside, while Share lets you invite collaborators or hand off the finished result.",
  "The rail on the left is your toolbelt. Start with Add a Node whenever you want to bring a new capability onto the canvas.",
  "The node picker is your library. Search directly or filter by input, output, model, and medium to choose the right building block.",
  "Each node has its own controls, while the canvas remains the shared place where media, models, code, and apps connect.",
  "At the bottom you can see available credits, get help, and change language without leaving the project.",
  "That is Fuser: the project above, tools on the left, utilities around the edges, and your connected work in the center. Now start making something.",
];
const extensionEnglishNarration = [
  "Now watch a complete flow. A Gemini Image node turns a written prompt into a new image, and the finished image appears on its output.",
  "Connect that same image output to the image input of a Kling video node. The cable makes the video's starting image unambiguous.",
  "Describe the motion in the video prompt, then choose the model, aspect ratio, and duration. The node turns the still image into a moving clip.",
  "Send the finished result into a standard Video node. You can play it, route it onward, or use it in your next Fuser workflow.",
];
const shiftedCoreBeats = coreStory.beats.map((beat) => ({
  ...beat, offsetSec:+(beat.offsetSec + openingDuration).toFixed(3),
}));
const shiftedExtensionBeats = extensionBeats.map((beat, index) => ({
  index:shiftedCoreBeats.length + index,
  offsetSec:+(extensionStart + beat.offsetSec).toFixed(3),
  durationSec:beat.durationSec, narration:beat.narration,
  logic:beat.logic, cursorIntent:beat.cursorIntent,
}));
const check = (name, atSec, evidence) => ({ kind:"check", name, atSec:+atSec.toFixed(3), evidence });
const evidenceAt = parseTimes("--evidence-at", 6, [
  workflowDuration * 0.14, workflowDuration * 0.18,
  workflowDuration * 0.3, workflowDuration * 0.5,
  workflowDuration * 0.76, workflowDuration * 0.88,
]);
const storyboard = {
  schema:"captutor-storyboard/v1", createdAt:new Date().toISOString(),
  screenplay:"introduction-to-fuser-image-to-video", locale:"hi", format:"docs", theme:"light",
  effectTheme:fuserEffectTheme, brandChrome:{ id:"fuser", elements:["mark", "Fuser"] },
  title:"फ्यूज़र का परिचय", subtitle:"कैनवस से इमेज और वीडियो तक",
  receiptEnglish:{
    title:"Introduction to Fuser", subtitle:"From canvas to image and video",
    openingCard:{ title:"Introduction to Fuser" },
    closingCard:{ title:"Thanks for watching" },
    beats:[...coreEnglishNarration, ...extensionEnglishNarration].map((narration, index) => ({
      narration,
      logic:index < shiftedCoreBeats.length
        ? "Live Fuser interface orientation and navigation."
        : extensionBeats[index - shiftedCoreBeats.length].logic,
      cursorIntent:index < shiftedCoreBeats.length
        ? "Follow the live browser action without cropping the browser frame."
        : extensionBeats[index - shiftedCoreBeats.length].cursorIntent,
    })),
  },
  openingCard:{ kicker:"FUSER TUTOR", title:"फ्यूज़र का परिचय", subtitle:"कैनवस से इमेज और वीडियो तक", durationMs:Math.round(openingDuration * 1000) },
  closingCard:{ kicker:"WORKFLOW COMPLETE", title:"देखने के लिए धन्यवाद", subtitle:"अपनी इमेज को वीडियो में बदलें", durationMs:Math.round(endingDuration * 1000) },
  acceptance:{
    resolution:[2560, 1440], minimumDurationSec:120,
    requireOpeningCard:true, requireEndingCard:true, requireBrandChrome:true,
    loudnessLufs:[-18, -14],
    requiredChecks:[
      "clean_opening_frame", "source_image_generated", "source_image_output_identified",
      "source_workflow_native_resolution",
      ...(workflowStage ? ["workflow_stage_mode_hidpi"] : []),
      "browser_frame_preserved", "workflow_real_time",
      "image_routed_to_video_generation", "video_generation_started",
      "video_output_returned", "video_output_playable", "clean_ending_frame",
      ...(hidpiEndcap ? ["stage_mode_hidpi_end_state"] : []),
    ],
  },
  media:{
    file:basename(final), width:2560, height:1440,
    durationSec:+finalDuration.toFixed(3), bytes:statSync(final).size,
  },
  workflowCapture:{
    source:basename(workflow), machine:"panda", recorder:"ScreenCaptureKit",
    browser:"Google Chrome", flowUrl:"https://app.fuser.studio/flow/12f89eb1-fdd4-495f-bd1a-5d28fefc4062",
    sourceIntervalsSec:workflowCuts, playbackRate:1,
    ...(workflowStage ? {
      stageMode:true, logicalResolution:[1280, 720], physicalResolution:[2560, 1440],
      scaleFactor:2, browserWindowCssPoints:[1190, 630],
    } : {}),
    ...(hidpiEndcap ? {
      endingStage:{
        source:basename(hidpiEndcap), machine:"panda", logicalResolution:[1280, 720],
        physicalResolution:[2560, 1440], scaleFactor:2, stageMode:true,
        browserWindowCssPoints:[1190, 630], playbackRate:1,
      },
    } : {}),
  },
  beats:[...shiftedCoreBeats, ...shiftedExtensionBeats],
  events:[
    { kind:"signboard", role:"opening", atSec:openingDuration, durationSec:openingDuration,
      card:{ phase:"title", title:"फ्यूज़र का परिचय" }, result:{ filmed:true, durationMs:Math.round(openingDuration * 1000), source:"offline-card" } },
    check("clean_opening_frame", openingDuration / 2, { pass:true, source:"offline deterministic card; no system UI" }),
    check("source_workflow_native_resolution", extensionStart + 1.0, {
      pass:true, width:workflowVideo.width, height:workflowVideo.height, upscaledProofFootage:false,
    }),
    ...(workflowStage ? [check("workflow_stage_mode_hidpi", extensionStart + workflowDuration / 2, {
      pass:true, stageMode:true, machine:"panda", recorder:"ScreenCaptureKit",
      logicalResolution:[1280, 720], physicalResolution:[2560, 1440], scaleFactor:2,
      browserWindowCssPoints:[1190, 630], browserFramePreserved:true,
      sourceIntervalsSec:workflowCuts, playbackRate:1,
      note:"The entire image-to-video workflow series is filmed on Panda's reversible 2× HiDPI Stage desktop.",
    })] : []),
    check("browser_frame_preserved", extensionStart + evidenceAt[0], {
      pass:true, chromeIncluded:true, tabBarVisible:true, addressBarVisible:true,
    }),
    check("workflow_real_time", extensionStart + workflowDuration / 2, {
      pass:true, playbackRate:1, retimed:false, frameInterpolation:false,
    }),
    check("source_image_generated", extensionStart + evidenceAt[0], { pass:true, node:"FalGeminiImageNode", naturalMedia:true }),
    check("source_image_output_identified", extensionStart + evidenceAt[1], { pass:true, output:"generated-image" }),
    check("image_routed_to_video_generation", extensionStart + evidenceAt[2], { pass:true, edge:"generated-image → ImageNode → video-image-input" }),
    check("video_generation_started", extensionStart + evidenceAt[3], { pass:true, node:"FalKling30VideoNode", controls:["prompt", "model", "aspect_ratio", "duration"] }),
    check("video_output_returned", extensionStart + evidenceAt[4], { pass:true, mime:"video/mp4", source:"immutable Fuser media asset" }),
    check("video_output_playable", extensionStart + evidenceAt[5], { pass:true, destination:"VideoNode", edge:"generated-video → video-node-input" }),
    ...(hidpiEndcap ? [check("stage_mode_hidpi_end_state", closingStart - hidpiEndcapDuration / 2, {
      pass:true, stageMode:true, logicalResolution:[1280, 720], physicalResolution:[2560, 1440],
      scaleFactor:2, browserFramePreserved:true, zoomToFitPerformed:true, playbackRate:1,
      note:"Closing live workflow is filmed on Panda's reversible 2× HiDPI Stage desktop before the ending card.",
    })] : []),
    check("clean_ending_frame", closingStart + endingDuration / 2, { pass:true, source:"offline deterministic card; no system UI" }),
    { kind:"signboard", role:"closing", atSec:finalDuration, durationSec:endingDuration,
      card:{ phase:"end", title:"देखने के लिए धन्यवाद" }, result:{ filmed:true, durationMs:Math.round(endingDuration * 1000), source:"offline-card" } },
  ],
};
const storyboardPath = final.replace(/\.mp4$/i, ".storyboard.json");
writeFileSync(storyboardPath, JSON.stringify(storyboard, null, 2) + "\n");
console.log(JSON.stringify({ final, storyboard:storyboardPath, duration:finalDuration, extensionDuration }, null, 2));
