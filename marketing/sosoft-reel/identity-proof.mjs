#!/usr/bin/env node
import { spawn, spawnSync } from "node:child_process";
import { once } from "node:events";
import { mkdirSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { createCanvas } from "canvas";
import { makeSosoftSideIdentity } from "../lib/sosoft-side-identity.mjs";

const ROOT = dirname(fileURLToPath(import.meta.url));
const input = resolve(ROOT, "delivery/scores-for-social-software-master-vertical.mp4");
const output = resolve(ROOT, "out/sosoft-side-identity-proof.mp4");
const assetsDir = resolve(ROOT, "out/sosoft-identity-assets");
mkdirSync(assetsDir, { recursive: true });
const W = 1080, H = 1920, FPS = 30, SECONDS = Number(process.argv[2] || 8);
const FRAMES = Math.ceil(SECONDS * FPS), BYTES = W * H * 4;
const canvas = createCanvas(W, H), ctx = canvas.getContext("2d");
const image = ctx.createImageData(W, H), frame = Buffer.alloc(BYTES);
const identity = await makeSosoftSideIdentity({ w: W, h: H, fps: FPS, frames: FRAMES, assetsDir });
const dec = spawn("ffmpeg", ["-v", "error", "-t", String(SECONDS), "-i", input, "-f", "rawvideo", "-pix_fmt", "rgba", "-"], { stdio: ["ignore", "pipe", "inherit"] });
const enc = spawn("ffmpeg", ["-y", "-v", "error", "-f", "rawvideo", "-pix_fmt", "bgra", "-s", `${W}x${H}`, "-r", String(FPS), "-i", "-", "-t", String(SECONDS), "-i", input, "-map", "0:v", "-map", "1:a?", "-c:v", "libx264", "-crf", "18", "-preset", "veryfast", "-pix_fmt", "yuv420p", "-c:a", "aac", "-b:a", "192k", "-shortest", "-movflags", "+faststart", output], { stdio: ["pipe", "inherit", "inherit"] });
let off = 0, fi = 0;
for await (const chunk of dec.stdout) {
  let at = 0;
  while (at < chunk.length && fi < FRAMES) {
    const n = Math.min(BYTES - off, chunk.length - at);
    chunk.copy(frame, off, at, at + n); off += n; at += n;
    if (off === BYTES) {
      off = 0; image.data.set(frame); ctx.putImageData(image, 0, 0);
      const t = fi / FPS;
      const env = Math.max(0, Math.sin(t * Math.PI * 2 * 1.8)) ** 5;
      identity.draw(ctx, t, env);
      if (!enc.stdin.write(canvas.toBuffer("raw"))) await once(enc.stdin, "drain");
      fi++;
    }
  }
}
enc.stdin.end();
await new Promise((ok, fail) => enc.on("close", (code) => code === 0 ? ok() : fail(new Error(`encode ${code}`))));
const probe = spawnSync("ffprobe", ["-v", "error", "-show_entries", "format=duration", "-of", "default=nw=1:nk=1", output], { encoding: "utf8" });
console.log(`${output} (${probe.stdout.trim()}s)`);
