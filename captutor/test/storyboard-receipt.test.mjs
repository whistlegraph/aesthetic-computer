import assert from "node:assert/strict";
import { execFileSync } from "node:child_process";
import { mkdtempSync, readFileSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";
import test from "node:test";

test("builds an accepted PDF from screenplay-owned QA checks", () => {
  const work = mkdtempSync(join(tmpdir(), "captutor-receipt-test-"));
  const video = join(work, "take.mp4");
  const storyboard = join(work, "storyboard.json");
  const pdf = join(work, "receipt.pdf");
  execFileSync("ffmpeg", [
    "-hide_banner", "-loglevel", "error",
    "-f", "lavfi", "-i", "color=c=#202128:s=320x180:r=30:d=1.5",
    "-f", "lavfi", "-i", "sine=frequency=220:sample_rate=48000:duration=1.5",
    "-c:v", "libx264", "-pix_fmt", "yuv420p", "-c:a", "aac", video,
  ]);
  writeFileSync(storyboard, JSON.stringify({
    schema:"captutor-storyboard/v1",
    createdAt:"2026-07-22T20:00:00.000Z",
    screenplay:"receipt-smoke", locale:"en", format:"docs", theme:"light",
    title:"Receipt smoke", subtitle:"Trace-derived frame evidence",
    media:{ file:"take.mp4", width:320, height:180, durationSec:1.5, bytes:1 },
    acceptance:{ resolution:[320, 180], minimumDurationSec:1, requiredChecks:["route_complete"] },
    beats:[{
      index:0, offsetSec:0, durationSec:1.2, narration:"Route the output.",
      logic:"The exact upstream output reaches the downstream node.", cursorIntent:"clear",
    }],
    events:[{ kind:"check", name:"route_complete", atSec:0.7, evidence:{ pass:true, edgeCount:1 } }],
  }));
  const result = JSON.parse(execFileSync(process.execPath, [
    "captutor/bin/storyboard-receipt.mjs", "--video", video,
    "--storyboard", storyboard, "--out", pdf,
  ], { encoding:"utf8" }));
  assert.equal(result.accepted, true);
  assert.equal(readFileSync(pdf).subarray(0, 4).toString(), "%PDF");
});
