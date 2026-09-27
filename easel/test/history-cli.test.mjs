import assert from "node:assert/strict";
import { execFileSync } from "node:child_process";
import { mkdtempSync, realpathSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";
import test from "node:test";
import { launchArguments, sessions } from "../src/history-cli.mjs";
import { Transcript } from "../src/transcript.mjs";

const EASEL = join(dirname(fileURLToPath(import.meta.url)), "..", "bin", "easel");

test("a resumed session is launched the way the launcher reads: options, then the directory", (context) => {
  const root = mkdtempSync(join(tmpdir(), "history-"));
  context.after(() => rmSync(root, { recursive: true, force: true }));
  const transcript = new Transcript({ sessionId: "s1", root });
  transcript.meta({ cwd: root, pro: true, subject: "the diff" });
  transcript.event("engine", { status: "started", engine: "claude", model: "claude-sonnet-5", thread: "11111111-2222-4333-8444-555555555555" });
  transcript.event("engine", { status: "restarted", engine: "codex", model: "gpt-6-astra", thread: "aaaaaaaa-bbbb-4ccc-8ddd-eeeeeeeeeeee" });
  transcript.close();
  const [meta] = sessions(root);
  assert.equal(meta.session_id, "s1");
  const args = launchArguments(meta);
  assert.deepEqual(args, ["pro", "--backend", "codex", "--resume", "aaaaaaaa-bbbb-4ccc-8ddd-eeeeeeeeeeee", "--model", "gpt-6-astra", root], "the engine it ended on, the directory last");
  // The launcher itself accepts exactly this shape.
  const dry = execFileSync(EASEL, args, { encoding: "utf8", env: { ...process.env, EASEL_DRY_RUN: "1" } });
  assert.match(dry, /pro=on/);
  assert.match(dry, /backend=codex/);
  assert.match(dry, /resume=yes/);
  assert.ok(dry.includes(`directory=${realpathSync(root)}`), "the directory reaches the launcher");
});
