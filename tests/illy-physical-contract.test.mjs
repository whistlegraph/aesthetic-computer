import assert from "node:assert/strict";
import { mkdtempSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import { resolve } from "node:path";
import { spawnSync } from "node:child_process";
import test from "node:test";

const MCP = resolve("plugins/illy/scripts/illy-mcp.mjs");
// Placeholder credentials: plans are checked before any provider is called.
const placeholder = "contract-test-key";

function call(name, args, env = {}) {
  const request = JSON.stringify({
    jsonrpc: "2.0", id: 1, method: "tools/call",
    params: { name, arguments: args },
  });
  const run = spawnSync(process.execPath, [MCP], {
    input: `${request}\n`, encoding: "utf8",
    env: { ...process.env, OPENAI_API_KEY: placeholder, ...env },
  });
  assert.equal(run.status, 0, run.stderr);
  const response = JSON.parse(run.stdout.trim());
  if (response.result.isError) throw new Error(response.result.content[0].text);
  return JSON.parse(response.result.content[0].text);
}

test("Illy injects physical accuracy into every built-in pipeline", () => {
  const targetDir = mkdtempSync(`${tmpdir()}/illy-contract-`);
  try {
    const plan = call("illy_plan", {
      pipeline: "marketing", targetDir, prompt: "one bunny on a skateboard",
      provider: "openai",
    });
    assert.deepEqual(plan.contracts, ["physical-accuracy"]);
    assert.equal(plan.creativePrompt, "one bunny on a skateboard");
    assert.match(plan.prompt, /PHYSICAL ACCURACY CONTRACT — REQUIRED/);
    assert.match(plan.prompt, /wheels meet the ground/);
  } finally {
    rmSync(targetDir, { recursive: true, force: true });
  }
});

test("motion-ready pop panels inherit the extreme physical contract", () => {
  const targetDir = mkdtempSync(`${tmpdir()}/illy-contract-`);
  try {
    const plan = call("illy_plan", {
      pipeline: "pop-panel", targetDir, prompt: "target-side impact keyframe",
      provider: "openai",
    });
    assert.deepEqual(plan.contracts, ["physical-accuracy", "extreme-physical-beats"]);
    assert.match(plan.prompt, /EXTREME PHYSICAL BEAT CONTRACT — MOTION-READY SOURCE FRAME/);
    assert.match(plan.prompt, /Cause must precede effect/);
  } finally {
    rmSync(targetDir, { recursive: true, force: true });
  }
});

test("Illy rejects unknown contracts before provider execution", () => {
  const targetDir = mkdtempSync(`${tmpdir()}/illy-contract-`);
  try {
    assert.throws(() => call("illy_plan", {
      pipeline: "marketing", targetDir, prompt: "test",
      provider: "openai", contracts: ["missing-contract"],
    }), /unknown contract: missing-contract/);
  } finally {
    rmSync(targetDir, { recursive: true, force: true });
  }
});

test("fal routes generate and edit to the newest registered endpoints", () => {
  const targetDir = mkdtempSync(`${tmpdir()}/illy-contract-`);
  try {
    const env = { FAL_KEY: placeholder };
    const draw = call("illy_plan", { pipeline: "marketing", targetDir, prompt: "a donkey", provider: "fal" }, env);
    assert.equal(draw.model, "google/nano-banana-2.1");
    const edit = call("illy_plan", {
      pipeline: "marketing", targetDir, prompt: "a donkey", provider: "fal",
      references: [resolve("aesel/art/aesel-pencil/refs/aesel-pixel-1024.png")],
    }, env);
    assert.equal(edit.mode, "edit");
    assert.equal(edit.model, "google/nano-banana-2.1/edit");
    const flux = call("illy_plan", { pipeline: "marketing", targetDir, prompt: "a donkey", provider: "fal", model: "blackforestlabs/flux-3/text-to-image" }, env);
    assert.equal(flux.model, "blackforestlabs/flux-3/text-to-image");
  } finally {
    rmSync(targetDir, { recursive: true, force: true });
  }
});
