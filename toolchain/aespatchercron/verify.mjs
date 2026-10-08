import { mkdtemp, readFile, writeFile, rm, realpath } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join, dirname } from "node:path";
import { git, hash, run, relativeFile } from "./io.mjs";
import { snapshot } from "./hand.mjs";
import { eligiblePiece } from "./config.mjs";

export function patchBounds(state, task, limits) {
  const piece = `system/public/aesthetic.computer/disks/${task.route}.mjs`;
  if (!eligiblePiece(task.route)) throw new Error("Sensitive pieces require separate authorization");
  if (!state.files.length || state.files.length > limits.maxFiles || state.files.some(row => row.deleted ||
      (row.file !== piece && !/^tests\/[a-z0-9][a-z0-9_/-]*\.test\.mjs$/.test(row.file)))) throw new Error("Patch exceeds allowed leaf/test scope");
  if (!state.files.some(row => row.file === piece)) throw new Error("No change to the implicated piece");
}

export async function testCommand(repo, file, timeout = 120000) {
  relativeFile(file);
  if (!/^tests\/[a-z0-9][a-z0-9_/-]*\.test\.mjs$/.test(file)) throw new Error("Use a Node test file under tests/");
  if (process.platform !== "darwin") throw new Error("Test isolation currently requires macOS sandbox-exec; do not run generated tests unsandboxed");
  repo = await realpath(repo);
  const scratch = await realpath(await mkdtemp(join(tmpdir(), "aespatcher-test-")));
  const quote = s => JSON.stringify(s);
  const readable = [repo, scratch, dirname(dirname(process.execPath)), "/System", "/Library", "/usr", "/bin", "/sbin", "/opt/homebrew", "/private/etc", "/dev"];
  const profile = `(version 1) (deny default) (allow process*) (allow sysctl-read) (allow mach-lookup) (allow file-read-metadata) (allow file-read* (literal "/") ${readable.map(s => `(subpath ${quote(s)})`).join(" ")}) (allow file-write* (subpath ${quote(scratch)}) (literal "/dev/null"))`;
  try {
    return await run(["/usr/bin/sandbox-exec", "-p", profile, process.execPath, "--test", file], {
      cwd: repo, timeout, cleanEnv: true, env: { PATH: `${dirname(process.execPath)}:/usr/bin:/bin`, HOME: scratch, TMPDIR: scratch, AC_NO_AUTO_DEPLOY: "1" },
    });
  } finally { await rm(scratch, { recursive: true, force: true }); }
}

export async function verifyPatch(repo, task, report, limits, execute = testCommand) {
  const before = await snapshot(repo, task.base);
  if (before.head !== task.base) throw new Error("Worker committed or moved HEAD");
  if (await git(repo, "diff", "--cached", "--name-only")) throw new Error("Unexpected staged changes; unstage and review them first");
  patchBounds(before, task, limits);
  const tracked = await git(repo, "diff", "--numstat", task.base, "--");
  let lines = tracked.split("\n").filter(Boolean).reduce((n, row) => {
    const [add, del] = row.split("\t");
    if (!/^\d+$/.test(add) || !/^\d+$/.test(del)) throw new Error("Binary patch rejected");
    return n + Number(add) + Number(del);
  }, 0);
  const untracked = (await git(repo, "ls-files", "--others", "--exclude-standard")).split("\n").filter(Boolean);
  for (const file of untracked) lines += (await readFile(join(repo, file), "utf8")).split("\n").length;
  if (lines > limits.maxLines) throw new Error("Patch exceeds changed-line budget");
  if (report.status !== "patched" || typeof report.title !== "string" || !/^[a-z][^\n]{9,100}$/.test(report.title) ||
      typeof report.problem !== "string" || report.problem.length < 20 || typeof report.cause !== "string" || report.cause.length < 20 ||
      !Array.isArray(report.tests) || !report.tests.length || report.tests.length > 4 || !report.tests.includes(report.reproduction) ||
      report.tests.some(file => !before.files.some(row => row.file === file))) throw new Error("Worker report needs a concrete cause and changed regression test");
  const receipts = [];
  for (const file of report.tests) {
    const result = await execute(repo, file);
    receipts.push({ file, code: result.code, outputHash: hash(result.stdout + result.stderr) });
    if (result.code !== 0) throw new Error("Patched regression test failed");
  }
  // Run the same new assertion with the old piece; infrastructure failures do not count as reproduction.
  const piece = `system/public/aesthetic.computer/disks/${task.route}.mjs`, path = join(repo, piece);
  const patched = await readFile(path), original = await run(["git", "show", `${task.base}:${piece}`], { cwd: repo });
  if (original.code !== 0) throw new Error("Original piece unavailable");
  let baseline;
  try {
    await writeFile(path, original.stdout);
    baseline = await execute(repo, report.reproduction);
  } finally { await writeFile(path, patched); }
  const output = baseline.stdout + baseline.stderr;
  if (!baseline.code || !/ERR_ASSERTION|AssertionError/.test(output) || !output.includes(`AES_REPRO:${task.id}`)) throw new Error("Original piece did not fail the named regression assertion");
  const after = await snapshot(repo, task.base);
  if (after.digest !== before.digest || after.head !== before.head) throw new Error("Tests changed the patch");
  return { digest: after.digest, base: task.base, lines, tests: receipts,
    reproduction: { file: report.reproduction, baselineCode: baseline.code, outputHash: hash(output), marker: `AES_REPRO:${task.id}` } };
}
