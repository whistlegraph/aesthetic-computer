import test from "node:test";
import assert from "node:assert/strict";
import { execFile } from "node:child_process";
import { promisify } from "node:util";
import { mkdtemp, mkdir, readFile, writeFile, symlink, rm, access } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { fileURLToPath } from "node:url";

const exec = promisify(execFile);
const helper = fileURLToPath(new URL("../bin/claude-sleep", import.meta.url));

async function fixture(t) {
  const root = await mkdtemp(join(tmpdir(), "slab sleep test "));
  t.after(() => rm(root, { recursive: true, force: true }));
  const bin = join(root, "bin");
  await mkdir(bin);
  const script = (name, body) => writeFile(join(bin, name), `#!/bin/bash\nset -eu\n${body}\n`, { mode: 0o755 });
  await script("sudo", '[[ "$1" == -n ]] && shift; exec "$@"');
  await script("pmset", `
if [[ "$1" == -g ]]; then
  printf 'SleepDisabled %s\\n' "$(cat "$SLAB_HOME/actual" 2>/dev/null || echo 0)"
  exit 0
fi
echo "$*" >> "$SLAB_HOME/calls"
if [[ "$1" == -a ]]; then
  if [[ "\${TEST_FAIL_DISABLE:-}" == "$3" ]]; then echo "permission denied" >&2; exit 1; fi
  if [[ "$3" == 1 && "\${TEST_PAUSE_ENABLE:-}" == 1 ]]; then
    touch "$SLAB_HOME/entered"
    while [[ ! -f "$SLAB_HOME/release" ]]; do sleep 0.01; done
  fi
  echo "$3" > "$SLAB_HOME/actual"
fi`);
  await script("osascript", 'echo osascript >> "$SLAB_HOME/calls"');
  await script("ioreg", 'echo "AppleClamshellState = Yes"');
  for (const name of ["slab-cancel-pending", "slab-fade-ambient"]) await script(name, ":");
  await script("slab-afplay", 'echo sound >> "$SLAB_HOME/calls"');
  await symlink(helper, join(bin, "claude-sleep"));
  const env = {
    ...process.env,
    PATH: `${bin}:${process.env.PATH}`,
    SLAB_HOME: root,
    SLAB_BIN: bin,
    SLAB_PMSET: join(bin, "pmset"),
    CLAUDE_STOP_LOG: join(root, "stop.log"),
    AESEL_SESSION_ID: "",
    EASEL_SESSION_ID: "",
  };
  const run = (command, extra = {}) => exec(helper, [command], { env: { ...env, ...extra }, timeout: 10000 });
  const calls = async () => (await readFile(join(root, "calls"), "utf8").catch(() => "")).trim().split("\n").filter(Boolean);
  const held = async () => access(join(root, "state/stay-awake")).then(() => true, () => false);
  return { root, env, run, calls, held };
}

test("manual stay-awake survives turns and automatic sleep attempts", async (t) => {
  const f = await fixture(t);
  await f.run("awake");
  await f.run("work");
  assert.match((await f.run("idle")).stdout, /staying awake/);
  assert.equal(await f.held(), true);
  assert.equal((await f.run("status")).stdout, "SleepDisabled=1\nStayAwake=1\n");
  assert.deepEqual(await f.calls(), ["-a disablesleep 1", "-a disablesleep 1"]);
});

test("turn protection is temporary and manual release allows automatic sleep", async (t) => {
  const f = await fixture(t);
  await f.run("awake");
  await f.run("auto");
  await f.run("work");
  assert.equal((await f.run("status")).stdout, "SleepDisabled=1\nStayAwake=0\n");
  await f.run("idle");
  assert.equal(await f.held(), false);
  assert.deepEqual((await f.calls()).slice(-2), ["-a disablesleep 0", "sleepnow"]);
});

test("explicit Sleep now releases the hold and sleeps", async (t) => {
  const f = await fixture(t);
  await f.run("awake");
  await f.run("now");
  assert.equal(await f.held(), false);
  assert.deepEqual((await f.calls()).slice(-2), ["-a disablesleep 0", "sleepnow"]);
});

test("failed enable reports the error without saving a false preference", async (t) => {
  const f = await fixture(t);
  await assert.rejects(f.run("awake", { TEST_FAIL_DISABLE: "1" }), (error) => {
    assert.match(error.stdout, /permission denied/);
    return true;
  });
  assert.equal(await f.held(), false);
});

test("failed release or explicit sleep preserves the hold and never sleeps", async (t) => {
  const f = await fixture(t);
  await f.run("awake");
  for (const command of ["auto", "now"]) {
    await assert.rejects(f.run(command, { TEST_FAIL_DISABLE: "0" }));
    assert.equal(await f.held(), true);
  }
  assert.equal((await f.calls()).some((call) => /sleepnow|osascript/.test(call)), false);
});

test("startup/wake restore reapplies only a saved manual preference", async (t) => {
  const f = await fixture(t);
  await f.run("restore");
  assert.deepEqual(await f.calls(), []);
  await f.run("awake");
  await writeFile(join(f.root, "actual"), "0");
  await f.run("restore");
  assert.equal((await f.run("status")).stdout, "SleepDisabled=1\nStayAwake=1\n");
  await assert.rejects(f.run("restore", { TEST_FAIL_DISABLE: "1" }));
  assert.equal(await f.held(), true);
});

test("automatic sleep cannot interleave with a manual enable", async (t) => {
  const f = await fixture(t);
  const awake = f.run("awake", { TEST_PAUSE_ENABLE: "1" });
  // Wait until the fake power command is inside the serialized transition.
  for (let i = 0; ; i++) {
    if (await access(join(f.root, "entered")).then(() => true, () => false)) break;
    assert.ok(i < 200, "enable did not reach pmset");
    await new Promise((resolve) => setTimeout(resolve, 10));
  }
  const idle = f.run("idle");
  await writeFile(join(f.root, "release"), "");
  await Promise.all([awake, idle]);
  assert.deepEqual(await f.calls(), ["-a disablesleep 1"]);
  assert.equal(await f.held(), true);
});

for (const muted of [false, true]) {
  test(`closed-lid completion respects manual hold (muted=${muted})`, async (t) => {
    const f = await fixture(t);
    await f.run("awake");
    if (muted) await writeFile(join(f.root, "state/muted"), "");
    const stop = fileURLToPath(new URL("../bin/claude-stop.sh", import.meta.url));
    // execFile closes stdin via the returned ChildProcess so the hook sees EOF.
    const child = exec("/bin/bash", [stop], { env: f.env, timeout: 10000 });
    child.child.stdin.end("{}");
    await child;
    assert.deepEqual(await f.calls(), ["-a disablesleep 1"]);
    assert.equal(await f.held(), true);
  });
}

test("delayed closed-lid sleep respects manual hold without a sleep announcement", async (t) => {
  const f = await fixture(t);
  await f.run("awake");
  const schedule = fileURLToPath(new URL("../bin/claude-sleep-schedule.sh", import.meta.url));
  await exec("/bin/bash", [schedule, "0"], { env: f.env, timeout: 10000 });
  assert.deepEqual(await f.calls(), ["-a disablesleep 1"]);
  assert.equal(await f.held(), true);
});
