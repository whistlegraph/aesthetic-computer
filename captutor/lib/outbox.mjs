// outbox — atomically publish a finished Captutor artifact for another person
// (or Iris's orchestrator) to collect.
//
// Captutor's `out/` directory is a working negative: partial recordings, cached
// narration, cues, and multiple encodes all live there. The Desktop outbox is
// different. Every visible MP4 in it is complete, accompanied by captions and a
// small manifest that says exactly what made it. Files are copied under hidden
// temporary names first and renamed only after the bytes are safely in place,
// so a watcher can never pick up a half-written video.

import {
  copyFileSync,
  existsSync,
  mkdirSync,
  readFileSync,
  renameSync,
  statSync,
  writeFileSync,
} from "node:fs";
import { createHash } from "node:crypto";
import { basename, dirname, join, resolve } from "node:path";

function sha256(path) {
  return createHash("sha256").update(readFileSync(path)).digest("hex");
}

function stamp(now = new Date()) {
  return now.toISOString().replace(/[-:]/g, "").replace(/\.\d{3}Z$/, "Z");
}

function atomicCopy(from, to) {
  const tmp = join(dirname(to), `.${basename(to)}.${process.pid}.tmp`);
  copyFileSync(from, tmp);
  renameSync(tmp, to);
}

export function publishToOutbox({
  outbox,
  video,
  captions,
  screenplay,
  locale,
  format,
  taskGid = null,
  now = new Date(),
}) {
  if (!existsSync(video)) throw new Error(`outbox video does not exist: ${video}`);
  if (!existsSync(captions)) throw new Error(`outbox captions do not exist: ${captions}`);

  const dir = resolve(outbox);
  mkdirSync(dir, { recursive: true });
  const stem = `${screenplay}.${locale}.${format}.${stamp(now)}`;
  const videoOut = join(dir, `${stem}.mp4`);
  const captionsOut = join(dir, `${stem}.vtt`);
  const manifestOut = join(dir, `${stem}.json`);

  atomicCopy(video, videoOut);
  atomicCopy(captions, captionsOut);

  const manifest = {
    schema: "captutor-outbox/v1",
    status: "complete",
    createdAt: now.toISOString(),
    taskGid,
    screenplay,
    locale,
    format,
    video: basename(videoOut),
    captions: basename(captionsOut),
    bytes: statSync(videoOut).size,
    sha256: sha256(videoOut),
  };
  const manifestTmp = join(dir, `.${basename(manifestOut)}.${process.pid}.tmp`);
  writeFileSync(manifestTmp, JSON.stringify(manifest, null, 2) + "\n");
  renameSync(manifestTmp, manifestOut);

  return { video: videoOut, captions: captionsOut, manifest: manifestOut, metadata: manifest };
}
