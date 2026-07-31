import test from "node:test";
import assert from "node:assert/strict";
import { execFile } from "node:child_process";
import { promisify } from "node:util";
import { mkdtemp, mkdir, readFile, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";

const execFileAsync = promisify(execFile);
const here = dirname(fileURLToPath(import.meta.url));
const zzz = join(here, "..", "bin", "zzz");
const providerId = "019f8087-6e5b-7a92-aebc-288cf603bdae";

async function fixture() {
  const root = await mkdtemp(join(tmpdir(), "slab-zzz-test-"));
  const state = join(root, "state");
  await mkdir(join(state, "zzz"), { recursive: true });
  await mkdir(join(state, "active-prompts"), { recursive: true });
  await mkdir(join(state, "awaiting-prompts"), { recursive: true });
  await mkdir(join(state, "running-tools"), { recursive: true });
  return { root, state };
}

function env(root) {
  return {
    ...process.env,
    SLAB_HOME: root,
    SLAB_BIN: "/opt/slab/bin",
    ZZZ_DRY_RUN: "1",
  };
}

test("lists sleeping prompts and dry-run wake preserves their record", async () => {
  const { root, state } = await fixture();
  const entry = {
    version: 1,
    id: "rock-one",
    providerSessionId: providerId,
    agentType: "codex",
    cwd: "/tmp/project",
    subject: "measure the prompt wall",
    summary: "measure prompt wall",
    tty: "ttys099",
    parkedAt: "2026-07-20T20:00:00Z",
    lastActiveAt: "2026-07-20T19:30:00Z",
    reason: "idle-20m",
  };
  const path = join(state, "zzz", "rock-one.json");
  await writeFile(path, JSON.stringify(entry));

  const listed = await execFileAsync(process.execPath, [zzz, "list", "--json"], { env: env(root) });
  assert.deepEqual(JSON.parse(listed.stdout).map((row) => row.id), ["rock-one"]);

  const humanList = await execFileAsync(process.execPath, [zzz, "list"], { env: env(root) });
  assert.match(humanList.stdout, /resume: zzz resume 1/);
  assert.match(humanList.stdout, /resume the newest: zzz resume/);

  const woke = await execFileAsync(process.execPath, [zzz, "resume"], { env: env(root) });
  assert.match(woke.stdout, /woke codex/);
  assert.equal(JSON.parse(await readFile(path, "utf8")).providerSessionId, providerId);
});

test("park refuses a working prompt unless explicitly forced", async () => {
  const { root, state } = await fixture();
  const marker = {
    session_id: "working-rock",
    codex_session_id: providerId,
    agent_type: "codex",
    agent_pid: 999999,
    tty: "ttys098",
    cwd: "/tmp/project",
    subject: "still running",
    updated: new Date().toISOString(),
    state: "working",
  };
  await writeFile(join(state, "active-prompts", marker.session_id), JSON.stringify(marker));

  await assert.rejects(
    execFileAsync(process.execPath, [zzz, "park", marker.session_id], { env: env(root) }),
    /refusing to zzz working prompt/,
  );
});

test("park writes a resumable cold-tier record for a completed prompt", async () => {
  const { root, state } = await fixture();
  const marker = {
    session_id: "complete-rock",
    codex_session_id: providerId,
    agent_type: "codex",
    agent_pid: 999999,
    tty: "ttys097",
    cwd: "/tmp/project",
    subject: "finished turn",
    summary: "finished turn",
    updated: "2026-07-20T19:30:00Z",
    state: "working",
  };
  await writeFile(join(state, "active-prompts", marker.session_id), JSON.stringify(marker));
  await writeFile(join(state, "awaiting-prompts", marker.session_id), "turn complete\n");

  const parked = await execFileAsync(
    process.execPath, [zzz, "park", marker.session_id], { env: env(root) },
  );
  assert.match(parked.stdout, /zzz'd codex/);
  const entry = JSON.parse(await readFile(join(state, "zzz", "complete-rock.json"), "utf8"));
  assert.equal(entry.providerSessionId, providerId);
  assert.equal(entry.reason, "manual");
});

test("park keeps the target TTY and writes a colored stable resume receipt", async () => {
  const { root, state } = await fixture();
  const marker = {
    session_id: "12345678-receipt-rock",
    codex_session_id: providerId,
    agent_type: "codex",
    agent_pid: 999999,
    tty: "ttys096",
    cwd: "/tmp/project",
    subject: "leave a visible receipt",
    summary: "visible receipt",
    updated: "2026-07-20T19:30:00Z",
    state: "working",
  };
  await writeFile(join(state, "active-prompts", marker.session_id), JSON.stringify(marker));
  await writeFile(join(state, "awaiting-prompts", marker.session_id), "turn complete\n");
  const ttyPath = join(root, marker.tty);
  await writeFile(ttyPath, "existing terminal output\n");

  const receiptEnv = {
    ...env(root),
    ZZZ_DRY_RUN: "0",
    ZZZ_TTY_ROOT: root,
  };
  const parked = await execFileAsync(
    process.execPath, [zzz, "park", marker.session_id], { env: receiptEnv },
  );
  assert.match(parked.stdout, /zzz resume 12345678/);
  const tty = await readFile(ttyPath, "utf8");
  assert.match(tty, /\u001b\[1;38;5;213m💤  ZZZ parked this prompt/);
  assert.match(tty, /\u001b\[1;38;5;51mResume: zzz resume 12345678/);
  assert.match(tty, /^existing terminal output/m);
});
