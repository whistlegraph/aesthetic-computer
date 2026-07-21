import test from "node:test";
import assert from "node:assert/strict";
import { spawn } from "node:child_process";
import {
  chmod, mkdtemp, mkdir, readFile, readdir, writeFile,
} from "node:fs/promises";
import { tmpdir } from "node:os";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";

const here = dirname(fileURLToPath(import.meta.url));
const wrapper = join(here, "..", "bin", "codex-slab");
const sleep = (ms) => new Promise((resolve) => setTimeout(resolve, ms));

async function waitFor(fn, timeoutMs = 5000) {
  const deadline = Date.now() + timeoutMs;
  while (Date.now() < deadline) {
    const value = await fn();
    if (value) return value;
    await sleep(50);
  }
  throw new Error("timed out waiting for Codex wrapper state");
}

test("SIGTERM reaches Codex while its terminal remains available", async () => {
  const root = await mkdtemp(join(tmpdir(), "codex-slab-test-"));
  const bin = join(root, "bin");
  const active = join(root, "state", "active-prompts");
  const signalPath = join(root, "codex-signal");
  const readyPath = join(root, "codex-ready");
  const transcript = join(root, "terminal.txt");
  await mkdir(bin, { recursive: true });
  await writeFile(join(bin, "codex"), `#!/bin/bash
trap 'printf term > "$FAKE_CODEX_SIGNAL"; exit 0' TERM INT
printf ready > "$FAKE_CODEX_READY"
while :; do sleep 0.05; done
`);
  await chmod(join(bin, "codex"), 0o755);

  const child = spawn("/usr/bin/script", ["-q", transcript, "/bin/bash", wrapper], {
    env: {
      ...process.env,
      PATH: `${bin}:${process.env.PATH}`,
      SLAB_HOME: root,
      SLAB_BIN: bin,
      FAKE_CODEX_SIGNAL: signalPath,
      FAKE_CODEX_READY: readyPath,
    },
    stdio: "ignore",
  });
  const marker = await waitFor(async () => {
    const names = await readdir(active).catch(() => []);
    if (!names.length) return null;
    let value;
    try { value = JSON.parse(await readFile(join(active, names[0]), "utf8")); }
    catch { return null; }
    return value.wrapper_pid && value.agent_pid !== value.wrapper_pid ? value : null;
  });
  await waitFor(async () => (await readFile(readyPath, "utf8").catch(() => "")) === "ready");
  process.kill(marker.agent_pid, "SIGTERM");
  await waitFor(async () => (await readFile(signalPath, "utf8").catch(() => "")) === "term");
  if (child.exitCode === null && child.signalCode === null) {
    await new Promise((resolve, reject) => {
      const timer = setTimeout(() => reject(new Error("PTY did not close after wrapper exit")), 5000);
      child.once("exit", () => { clearTimeout(timer); resolve(); });
    });
  }
  assert.equal(await readFile(signalPath, "utf8"), "term");
});
