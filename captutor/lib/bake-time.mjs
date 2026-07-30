// bake-time — canonize the long model wait as a short editorial time fold.
//
// Screenplays mark the real asynchronous boundary with `ctx.bakeTime(...)`.
// Captutor keeps a few honest seconds of the live wait, folds out the inert
// middle, and returns on the first result frame. The source recording remains
// untouched; only the composed negative is condensed.

import { execFileSync } from "node:child_process";

const FFMPEG = process.env.FFMPEG || "ffmpeg";

export const BAKE_TIME_PRESET = Object.freeze({
  name:"bake-time-fold",
  liveLeadSec:4,
  resultLeadSec:0,
  minimumFoldSec:8,
  transitionSec:0.64,
});

const finite = (value, fallback) => Number.isFinite(Number(value)) ? Number(value) : fallback;
const clamp = (value, min, max) => Math.max(min, Math.min(max, value));

export function bakeTimeSpans(events = []) {
  const starts = new Map();
  const spans = [];
  for (const event of events) {
    if (event.kind === "bake-time-start") starts.set(event.id, event);
    if (event.kind !== "bake-time-end") continue;
    const start = starts.get(event.id);
    if (!start) continue;
    spans.push({
      id:event.id,
      label:start.label || event.label || "Model is generating",
      startSec:finite(start.atSec, 0),
      endSec:finite(event.atSec, 0),
      liveLeadSec:finite(start.liveLeadSec, BAKE_TIME_PRESET.liveLeadSec),
      resultLeadSec:finite(start.resultLeadSec, BAKE_TIME_PRESET.resultLeadSec),
      minimumFoldSec:finite(start.minimumFoldSec, BAKE_TIME_PRESET.minimumFoldSec),
      transitionSec:finite(start.transitionSec, BAKE_TIME_PRESET.transitionSec),
    });
  }
  return spans.sort((a, b) => a.startSec - b.startSec);
}

export function planBakeTime({ events = [], spans = null, durationSec }) {
  const duration = finite(durationSec, 0);
  if (!(duration > 0)) throw new Error("bake-time needs a positive source duration");
  const source = spans || bakeTimeSpans(events);
  const edits = [];
  let previousEnd = 0;
  for (const span of source) {
    const startSec = clamp(finite(span.startSec, 0), previousEnd, duration);
    const endSec = clamp(finite(span.endSec, startSec), startSec, duration);
    const cutFromSec = clamp(
      startSec + Math.max(0, finite(span.liveLeadSec, BAKE_TIME_PRESET.liveLeadSec)),
      startSec,
      endSec,
    );
    const cutToSec = clamp(
      endSec - Math.max(0, finite(span.resultLeadSec, BAKE_TIME_PRESET.resultLeadSec)),
      cutFromSec,
      endSec,
    );
    const removedSec = cutToSec - cutFromSec;
    const minimumFoldSec = Math.max(
      0,
      finite(span.minimumFoldSec, BAKE_TIME_PRESET.minimumFoldSec),
    );
    if (removedSec >= minimumFoldSec) {
      edits.push({
        id:span.id || `bake-${edits.length + 1}`,
        label:span.label || "Model is generating",
        preset:BAKE_TIME_PRESET.name,
        startSec,
        endSec,
        cutFromSec,
        cutToSec,
        removedSec,
        transitionSec:clamp(
          finite(span.transitionSec, BAKE_TIME_PRESET.transitionSec), 0.1, 1.5,
        ),
      });
    }
    previousEnd = endSec;
  }

  const mapTime = (sourceSec) => {
    const time = clamp(finite(sourceSec, 0), 0, duration);
    let removed = 0;
    for (const edit of edits) {
      if (time >= edit.cutToSec) removed += edit.removedSec;
      else if (time > edit.cutFromSec) return edit.cutFromSec - removed;
    }
    return time - removed;
  };

  const segments = [];
  let cursor = 0;
  for (const edit of edits) {
    if (edit.cutFromSec > cursor) segments.push({ startSec:cursor, endSec:edit.cutFromSec });
    cursor = edit.cutToSec;
  }
  if (cursor < duration) segments.push({ startSec:cursor, endSec:duration });

  return {
    preset:BAKE_TIME_PRESET.name,
    sourceDurationSec:duration,
    outputDurationSec:duration - edits.reduce((sum, edit) => sum + edit.removedSec, 0),
    edits,
    segments,
    mapTime,
  };
}

// The fold is deliberately quiet: a fast dip through black in the real Stage
// geometry. It reads as elapsed bake time without introducing a fake progress
// percentage or a DOM overlay that could leak into product capture.
export function condenseBakeTimeVideo({ input, output, plan, fps = 60 }) {
  if (!plan?.edits?.length) return input;
  const filters = [];
  const labels = [];
  plan.segments.forEach((segment, index) => {
    const duration = segment.endSec - segment.startSec;
    const leftEdit = index > 0 ? plan.edits[index - 1] : null;
    const rightEdit = index < plan.edits.length ? plan.edits[index] : null;
    const fadeIn = leftEdit ? leftEdit.transitionSec / 2 : 0;
    const fadeOut = rightEdit ? rightEdit.transitionSec / 2 : 0;
    const chain = [
      `[0:v]trim=start=${segment.startSec.toFixed(6)}:end=${segment.endSec.toFixed(6)}`,
      "setpts=PTS-STARTPTS",
    ];
    if (fadeIn > 0) chain.push(`fade=t=in:st=0:d=${Math.min(fadeIn, duration / 2).toFixed(3)}`);
    if (fadeOut > 0) {
      const d = Math.min(fadeOut, duration / 2);
      chain.push(`fade=t=out:st=${Math.max(0, duration - d).toFixed(3)}:d=${d.toFixed(3)}`);
    }
    chain.push(`fps=${fps}`, "format=yuv420p");
    const label = `v${index}`;
    filters.push(`${chain.join(",")}[${label}]`);
    labels.push(`[${label}]`);
  });
  filters.push(`${labels.join("")}concat=n=${labels.length}:v=1:a=0[v]`);

  const encoder = process.env.CAPTUTOR_VIDEO_ENCODER ||
    (process.platform === "darwin" ? "h264_videotoolbox" : "libx264");
  const encoderArgs = encoder === "h264_videotoolbox"
    ? ["-b:v", "24M", "-maxrate", "32M", "-bufsize", "48M"]
    : ["-crf", "17", "-preset", "medium"];
  execFileSync(FFMPEG, [
    "-y", "-i", input,
    "-filter_complex", filters.join(";"),
    "-map", "[v]", "-an",
    "-c:v", encoder, ...encoderArgs,
    "-movflags", "+faststart", output,
  ], { stdio:["ignore", "ignore", "pipe"] });
  return output;
}
