import assert from "node:assert/strict";
import { execFile } from "node:child_process";
import { mkdir, mkdtemp, readFile, rm, unlink, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { promisify } from "node:util";
import test from "node:test";

import {
  admissionVerdict,
  capacityVerdict,
  chooseProvider,
  configFromEnv,
  createIsolatedCheckout,
  run,
  slugify,
  validateCandidateBoundary,
  validateTopic,
} from "../bin/mediascholar.mjs";

const pexec = promisify(execFile);
const GiB = 1024 ** 3;

function thresholds(overrides = {}) {
  return {
    maxLoadPerCpu: 0.55,
    minAvailableMemoryBytes: 4 * GiB,
    minFreeDiskBytes: 32 * GiB,
    ...overrides,
  };
}

test("resource admission yields before competing with the host", () => {
  const config = thresholds();
  const verdict = admissionVerdict(config, {
    loadPerCpu: 0.8,
    availableMemoryBytes: 3 * GiB,
    diskFreeBytes: 20 * GiB,
  });
  assert.equal(verdict.accepted, false);
  assert.equal(verdict.reasons.length, 3);
  assert.equal(admissionVerdict(config, {
    loadPerCpu: 0.2,
    availableMemoryBytes: 8 * GiB,
    diskFreeBytes: 80 * GiB,
  }).accepted, true);
});

test("provider choice is explicit and deterministic", () => {
  const inventory = { claude: { ready: true }, openai: { ready: true } };
  assert.equal(chooseProvider({ provider: "auto", providerPreference: ["openai", "claude"] }, inventory), "openai");
  assert.equal(chooseProvider({ provider: "claude", providerPreference: [] }, inventory), "claude");
  assert.throws(
    () => chooseProvider({ provider: "openai", providerPreference: [] }, { openai: { ready: false } }),
    /not ready/,
  );
});

test("topic validation requires three independent source hosts", () => {
  const topic = {
    title: "A useful synthesized question",
    question: "What changes when a network becomes part of an artwork?",
    claim: "Network maintenance is a compositional medium rather than mere support.",
    whyNow: "Long-running network works are crossing infrastructure generations.",
    signals: [
      { url: "https://example.org/a", accessedAt: "2026-09-05T12:00:00Z" },
      { url: "https://example.net/b", accessedAt: "2026-09-05T12:01:00Z" },
      { url: "https://example.edu/c", accessedAt: "2026-09-05T12:02:00Z" },
    ],
  };
  assert.equal(validateTopic(topic), topic);
  assert.throws(() => validateTopic({
    ...topic,
    signals: topic.signals.map((signal, index) => ({ ...signal, url: `https://example.org/${index}` })),
  }), /three source hosts/);
  assert.equal(slugify("Signal / Medium: 2026"), "signal-medium-2026");
});

test("retained checkout cap stops unattended accumulation", async (t) => {
  const root = await mkdtemp(join(tmpdir(), "mediascholar-capacity-"));
  t.after(() => rm(root, { recursive: true, force: true }));
  await Promise.all([0, 1].map((index) => mkdir(join(root, String(index)), { recursive: true })));
  assert.deepEqual(await capacityVerdict({ worktreesDir: root, maxRetainedCandidates: 2 }), {
    accepted: false,
    retained: 2,
    limit: 2,
    reason: "2 retained Mediascholar checkouts require review",
  });
});

test("candidate checkout is remote-less and rejects edits outside its paper directory", async (t) => {
  const root = await mkdtemp(join(tmpdir(), "mediascholar-boundary-"));
  t.after(() => rm(root, { recursive: true, force: true }));
  const repo = join(root, "repo");
  await mkdir(repo);
  await pexec("git", ["init", "-q", repo]);
  await pexec("git", ["-C", repo, "config", "user.name", "Mediascholar Test"]);
  await pexec("git", ["-C", repo, "config", "user.email", "mediascholar@example.invalid"]);
  await writeFile(join(repo, "README.md"), "source checkout\n");
  await pexec("git", ["-C", repo, "add", "README.md"]);
  await pexec("git", ["-C", repo, "commit", "-qm", "fixture"]);

  const config = { repo, worktreesDir: join(root, "checkouts") };
  const workspace = await createIsolatedCheckout(config, "candidate");
  const remotes = await pexec("git", ["-C", workspace.path, "remote"]);
  assert.equal(remotes.stdout.trim(), "");

  const paperDir = join(workspace.path, "papers", "arxiv-botted-test");
  await mkdir(paperDir, { recursive: true });
  await writeFile(join(paperDir, "test.tex"), "paper\n");
  await validateCandidateBoundary(workspace.path, paperDir, workspace.baseCommit);

  const outside = join(workspace.path, "README.md");
  await writeFile(outside, "changed outside\n");
  await assert.rejects(
    validateCandidateBoundary(workspace.path, paperDir, workspace.baseCommit),
    /outside its paper directory/,
  );
  await unlink(outside);
  await pexec("git", ["-C", workspace.path, "checkout", "--", "README.md"]);
});

test("dry run records admission and advertises without invoking a provider", async (t) => {
  const root = await mkdtemp(join(tmpdir(), "mediascholar-dry-"));
  t.after(() => rm(root, { recursive: true, force: true }));
  const repo = join(root, "repo");
  await mkdir(repo);
  await pexec("git", ["init", "-q", repo]);
  const config = configFromEnv({
    MEDIASCHOLAR_HOME: join(root, "state"),
    MEDIASCHOLAR_REPO: repo,
    MEDIASCHOLAR_ADVERTISE_PATH: join(root, "ledger", "mediascholar.json"),
    MEDIASCHOLAR_MAX_LOAD_PER_CPU: "999",
    MEDIASCHOLAR_MIN_AVAILABLE_MEMORY_MIB: "0",
    MEDIASCHOLAR_MIN_FREE_DISK_GIB: "0",
    MEDIASCHOLAR_MAX_RETAINED_CANDIDATES: "1",
  });
  const result = await run(config, { dryRun: true });
  assert.equal(result.status, "dry-run-complete");
  assert.equal(result.provider, null);
  const advertised = JSON.parse(await readFile(config.advertisePath, "utf8"));
  assert.equal(advertised.name, "mediascholar");
  assert.equal(advertised.status, "complete");
});
