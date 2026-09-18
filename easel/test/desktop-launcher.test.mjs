import test from "node:test";
import assert from "node:assert/strict";
import {
  mkdtemp,
  realpath,
  mkdir,
  writeFile,
  readFile,
  chmod,
  rm,
} from "node:fs/promises";
import { tmpdir } from "node:os";
import { join, resolve } from "node:path";
import { spawnSync } from "node:child_process";
const launcher = new URL("../bin/easel-desktop", import.meta.url).pathname;
async function setup(t) {
  const root = await mkdtemp(join(tmpdir(), "easel-launcher-"));
  t.after(() => rm(root, { recursive: true, force: true }));
  const bin = join(root, "bin"),
    app = join(root, "aesel.app"),
    workspace = join(root, "project with spaces");
  for (const p of [bin, app, workspace]) await mkdir(p);
  await writeFile(
    join(bin, "open"),
    '#!/bin/sh\nprintf "%s\\0" "$@" > "$EASEL_TEST_ARGS"\n',
  );
  await chmod(join(bin, "open"), 0o755);
  await writeFile(join(bin, "uname"), "#!/bin/sh\nprintf Darwin\n");
  await chmod(join(bin, "uname"), 0o755);
  const output = join(root, "argv");
  const env = {
    ...process.env,
    PATH: bin + ":" + process.env.PATH,
    EASEL_DESKTOP_APP: app,
    EASEL_TEST_ARGS: output,
  };
  return {
    root,
    app,
    workspace,
    output,
    env,
    run: (args) =>
      spawnSync("bash", [launcher, ...args], { env, encoding: "utf8" }),
  };
}
test("desktop launcher forwards whitespace and shell metacharacters literally", async (t) => {
  const f = await setup(t),
    model = "model $(touch should-not-exist) `echo nope`";
  const out = f.run([
    await realpath(f.workspace),
    "--backend",
    "ac",
    "--model",
    model,
    "--resume",
    "thread with spaces",
    "--no-autopublish",
  ]);
  assert.equal(out.status, 0, out.stderr);
  const argv = (await readFile(f.output)).toString().split("\0").slice(0, -1);
  assert.deepEqual(argv, [
    "-a",
    f.app,
    "--args",
    "--cwd",
    await realpath(f.workspace),
    "--backend",
    "ac",
    "--model",
    model,
    "--resume",
    "thread with spaces",
    "--no-autopublish",
  ]);
  assert(!argv.includes("-n"));
});
test("no arguments activates existing app and leaves last-workspace selection to it", async (t) => {
  const f = await setup(t);
  assert.equal(f.run([]).status, 0);
  assert.deepEqual(
    (await readFile(f.output)).toString().split("\0").slice(0, -1),
    ["-a", f.app],
  );
});
test("help and invalid invocations never call open", async (t) => {
  const f = await setup(t);
  assert.match(f.run(["--help"]).stdout, /standalone aesel/);
  for (const args of [
    ["--cwd"],
    ["--model"],
    ["--backend", "bad"],
    ["--unknown"],
    [f.workspace, f.workspace],
    ["/missing-workspace"],
  ])
    assert.notEqual(f.run(args).status, 0);
  await assert.rejects(readFile(f.output), { code: "ENOENT" });
});
test("missing desktop install has a meaningful terminal fallback", async (t) => {
  const f = await setup(t);
  f.env.EASEL_DESKTOP_APP = join(f.root, "missing.app");
  const result = f.run([]);
  assert.notEqual(result.status, 0);
  assert.match(result.stderr, /Install the standalone app or run ac/);
});
