import { test } from "node:test";
import assert from "node:assert/strict";
import { mkdtemp, mkdir, writeFile, readFile, rm, stat } from "node:fs/promises";
import { join } from "node:path";
import { tmpdir } from "node:os";
import { git, save, json, locked, privateDir, statePath } from "../io.mjs";
import { collectDatabase, validateReport, collectorOptions } from "../collect.mjs";
import { opportunities, admit, tasks } from "../queue.mjs";
import { verifyFinding, inspectMap } from "../reuse.mjs";
import { snapshot, committedSnapshot, handCheck, handQuestions, validateReview } from "../hand.mjs";
import { patchBounds, testCommand, verifyPatch } from "../verify.mjs";
import { prepare } from "../worker.mjs";
import { publish, hosting, findDraft } from "../publish.mjs";
import { defaults } from "../config.mjs";
import { ACCOUNT_ACTIONS } from "../../../system/public/aesthetic.computer/lib/account-activity-model.mjs";

const piece = "system/public/aesthetic.computer/disks/fixture.mjs";
const report = () => ({ format: "aespatcher.signals.v1", start: "2026-10-07T00:00:00.000Z", end: "2026-10-08T00:00:00.000Z", minimum: 3,
  runs: [{ route: "fixture", runs: 12, errors: 4 }], transitions: [{ from: "prompt", to: "fixture", count: 8, source: "piece-runs" }],
  visits: [{ property: "aesthetic.computer", surface: "play", visits: 20, interacted: 15, engaged: 10 }], unavailable: [], truncated: [] });

async function fixture(t) {
  const repo = await mkdtemp(join(tmpdir(), "aespatcher-fixture-"));
  t.after(() => rm(repo, { recursive: true, force: true }));
  await mkdir(join(repo, "system/public/aesthetic.computer/disks"), { recursive: true });
  await mkdir(join(repo, "tests"));
  await writeFile(join(repo, piece), "export const add = (a, b) => a - b;\n");
  await writeFile(join(repo, "other.mjs"), "export const original = true;\n");
  await git(repo, "init", "-q");
  await git(repo, "config", "user.email", "fixture@example.invalid");
  await git(repo, "config", "user.name", "Fixture");
  await git(repo, "config", "commit.gpgsign", "false");
  await git(repo, "add", ".");
  await git(repo, "commit", "-qm", "fixture");
  const base = await git(repo, "rev-parse", "HEAD"), home = await privateDir(join(repo, ".state"));
  await writeFile(join(repo, ".git/info/exclude"), ".state/\n");
  const task = { id: "a".repeat(20), route: "fixture", base, worktree: repo, branch: "aespatcher-fixture-aaaaaaaa", status: "prepared", claim: "Repeated failures", paths: [] };
  await privateDir(statePath(home, task.id));
  await save(join(statePath(home, task.id), "evidence.json"), { report: report() });
  return { repo, home, task, base };
}

async function patch(f) {
  await writeFile(join(f.repo, piece), "export const add = (a, b) => a + b;\n");
  await writeFile(join(f.repo, "tests/fixture.test.mjs"), `import { test } from 'node:test';
import assert from 'node:assert/strict';
import { add } from '../${piece}';
test('adds values', () => assert.equal(add(2, 3), 5, 'AES_REPRO:${f.task.id}'));
`);
  return { status: "patched", title: "fix fixture addition", problem: "Adding two values returns their difference.", cause: "The addition function uses the subtraction operator.", tests: ["tests/fixture.test.mjs"], reproduction: "tests/fixture.test.mjs" };
}

test("privacy gate rejects identities, unknown routes and client context", () => {
  assert.equal(validateReport(report(), ["fixture", "prompt"]).runs[0].errors, 4);
  for (const edit of [r => { r.user = "secret"; }, r => { r.runs[0].error = "private message"; },
    r => { r.transitions[0].from = "@person/private"; }, r => { r.visits[0].property = "false.work"; }, r => { r.runs[0].errors = 15; }]) {
    const r = report(); edit(r); assert.throws(() => validateReport(r, ["fixture", "prompt"]), /non-minimized/);
  }
  assert.throws(() => collectorOptions({ hours: 1000, routes: ["fixture"] }), /bounds/);
});

test("server pipelines use actual account event and preserve unknown-route barriers", async () => {
  const pipelines = [];
  const db = { collection: collection => ({ aggregate: pipeline => ({ toArray: async () => { pipelines.push({ collection, pipeline }); return []; } }) }) };
  await collectDatabase(db, { hours: 24, minimum: 3, limit: 5, routes: ["fixture"], end: "2026-10-08T00:00:00Z", properties: ["aesthetic.computer"], hosts: ["aesthetic.computer"] });
  const account = pipelines.find(p => p.collection === "account-activity").pipeline;
  assert.equal(account[0].$match.action, "piece_opened");
  assert.ok(ACCOUNT_ACTIONS.includes(account[0].$match.action));
  assert.equal(account[1].$project.route.$cond[2], null);
  assert.ok(account[2].$setWindowFields);
  assert.equal(account[0].$match.piece, undefined);
  const runs = pipelines.find(p => p.collection === "piece-runs").pipeline;
  assert.match(JSON.stringify(runs[1]), /\$error/);
  assert.doesNotMatch(JSON.stringify(runs.at(-2)), /user|bootId|message|events|stack/);
});

test("source failure is explicit and never leaks raw database errors", async () => {
  const db = { collection: () => ({ aggregate: () => ({ toArray: async () => { throw new Error("mongodb://secret@example"); } }) }) };
  const result = await collectDatabase(db, { hours: 24, minimum: 3, limit: 5, routes: ["fixture"], end: "2026-10-08T00:00:00Z", properties: [], hosts: [] });
  assert.equal(result.unavailable.length, 4);
  assert.doesNotMatch(JSON.stringify(result), /secret|mongodb/);
});

test("admission uses errors and path context, deduplicates and honors daily/active bounds", async t => {
  const f = await fixture(t), r = report();
  await rm(statePath(f.home, f.task.id), { recursive: true });
  assert.equal(opportunities(r)[0].paths[0].count, 8);
  const first = await admit(f.home, { report: r }, { perDay: 1, maxActive: 1 });
  assert.equal(first.admitted.length, 1);
  assert.equal((await admit(f.home, { report: r }, { perDay: 1, maxActive: 1 })).admitted.length, 0);
  assert.equal((await tasks(f.home)).length, 1);
  r.runs[0].errors = 0;
  assert.equal(opportunities(r).length, 0);
  for (const route of ["prompt", "handle", "give", "delete-erase-and-forget-me"]) {
    r.runs[0] = { route, runs: 10, errors: 5 }; assert.equal(opportunities(r).length, 0);
  }
});

test("state stays owner-only and overlapping ticks fail closed", async t => {
  const { home } = await fixture(t);
  await locked(home, async () => assert.rejects(locked(home, async () => {}), /locked/));
  assert.equal((await stat(home)).mode & 0o777, 0o700);
  await save(join(home, "private.json"), {});
  assert.equal((await stat(join(home, "private.json"))).mode & 0o777, 0o600);
});

test("reuse anchors bind source revision and detect edits", async t => {
  const f = await fixture(t);
  const entry = await verifyFinding(f.repo, { id: "addition", file: piece, anchor: "export const add", finding: "Existing arithmetic helper, with an explicit argument contract." });
  const map = { format: "aespatcher.reuse.v1", entries: [entry] };
  assert.equal((await inspectMap(f.repo, map)).entries[0].stale, false);
  await patch(f);
  assert.equal((await inspectMap(f.repo, map)).entries[0].stale, true);
  await assert.rejects(verifyFinding(f.repo, { ...entry, anchor: "missing symbol" }), /anchor missing/);
  await assert.rejects(verifyFinding(f.repo, { ...entry, file: "../secret" }), /Invalid repository/);
});

test("worktree preparation starts at main without copying dirty primary files", async t => {
  const f = await fixture(t);
  await git(f.repo, "remote", "add", "origin", "git@knot.aesthetic.computer:aesthetic.computer/core");
  await git(f.repo, "update-ref", "refs/remotes/origin/main", f.base);
  await save(join(f.home, "reuse.json"), { format: "aespatcher.reuse.v1", entries: [] });
  await writeFile(join(f.repo, "other.mjs"), "unrelated dirty work\n");
  let task;
  try { task = await prepare(f.repo, f.home, { ...f.task, status: "queued" }, { ...defaults(f.repo), fetch: false }); }
  catch (error) {
    if (error.message === "Git worktree failed (75)") { t.skip("Host performance guard deferred worktree creation (exit 75); no bypass"); return; }
    throw error;
  }
  assert.equal(await readFile(join(task.worktree, "other.mjs"), "utf8"), "export const original = true;\n");
  assert.equal(await readFile(join(f.repo, "other.mjs"), "utf8"), "unrelated dirty work\n");
  assert.equal((await snapshot(task.worktree, task.base)).files.length, 0);
});

test("sandbox denies private file reads, source writes and network", { skip: process.platform !== "darwin" }, async t => {
  const f = await fixture(t);
  const secret = join(f.home, "private.txt"); await writeFile(secret, "private");
  const isolated = await mkdtemp(join(tmpdir(), "aespatcher-isolation-"));
  t.after(() => rm(isolated, { recursive: true, force: true }));
  await mkdir(join(isolated, "tests"));
  await writeFile(join(isolated, "tests/isolation.test.mjs"), `import { test } from 'node:test'; import assert from 'node:assert/strict'; import fs from 'node:fs'; import net from 'node:net';
test('read', () => assert.throws(() => fs.readFileSync(${JSON.stringify(secret)})));
test('write', () => assert.throws(() => fs.writeFileSync('source.txt', 'bad')));
test('network', async () => { await new Promise((resolve, reject) => { const s = net.connect(9, '127.0.0.1'); s.on('connect', () => { s.destroy(); reject(Error('network allowed')); }); s.on('error', e => { try { assert.equal(e.code, 'EPERM'); resolve(); } catch (err) { reject(err); } }); }); });
`);
  const result = await testCommand(isolated, "tests/isolation.test.mjs");
  assert.equal(result.code, 0, result.stdout + result.stderr);
});

test("verification proves regression on old source and green on patch", { skip: process.platform !== "darwin" }, async t => {
  const f = await fixture(t), report = await patch(f), before = await snapshot(f.repo, f.base);
  const result = await verifyPatch(f.repo, f.task, report, defaults(f.repo).bounds);
  assert.equal(result.digest, before.digest);
  assert.notEqual(result.reproduction.baselineCode, 0);
  assert.equal((await snapshot(f.repo, f.base)).digest, before.digest);
});

test("environment failures never qualify as a reproduced bug", async t => {
  const f = await fixture(t), report = await patch(f);
  let calls = 0;
  await assert.rejects(verifyPatch(f.repo, f.task, report, defaults(f.repo).bounds, async () => ++calls === 1
    ? { code: 0, stdout: "", stderr: "" } : { code: 1, stdout: "", stderr: "Module missing" }), /named regression/);
  assert.match(await readFile(join(f.repo, piece), "utf8"), /a \+ b/);
});

test("protected paths and oversized changes cannot reach tests", async t => {
  const f = await fixture(t); await patch(f);
  const state = await snapshot(f.repo, f.base);
  assert.throws(() => patchBounds({ ...state, files: [...state.files, { file: "system/backend/auth.mjs" }] }, f.task, defaults(f.repo).bounds), /scope/);
  await assert.rejects(verifyPatch(f.repo, f.task, {}, { ...defaults(f.repo).bounds, maxLines: 1 }), /line budget/);
});

test("review demands qualitative evidence and invalidates changes", async t => {
  const f = await fixture(t); await patch(f);
  const packet = await handCheck(f.repo, f.base);
  const entry = { id: "helper", sourceHash: "abc", stale: false };
  const review = { digest: packet.digest, base: f.base, decision: "approve", kind: "independent-agent", reviewer: "reviewer",
    hand: Object.fromEntries(Object.keys(handQuestions).map(k => [k, "Reviewed the changed function and its existing idiom."])),
    userImpact: "Addition now returns the expected sum.", reproduction: "The named assertion fails on the original implementation.", privacy: "The PR text contains only a public source-level problem.",
    scope: "The patch changes arithmetic only, with no account, permission, personal-data or commerce flow.",
    reuse: [{ id: "helper", sourceHash: "abc", decision: "considered", reason: "The existing helper contract does not fit this assertion." }] };
  assert.equal(validateReview(review, packet, { entries: [entry] }), review);
  assert.throws(() => validateReview({ ...review, hand: {} }, packet, { entries: [entry] }), /Incomplete/);
  await writeFile(join(f.repo, piece), "export const add = () => 99;\n");
  assert.notEqual((await snapshot(f.repo, f.base)).digest, packet.digest);
  assert.throws(() => validateReview(review, packet, { entries: [{ ...entry, stale: true }] }), /stale/);
});

test("publication rejects hidden staged content and compares committed tree independently", async t => {
  const f = await fixture(t); await patch(f);
  const packet = await handCheck(f.repo, f.base);
  await save(join(statePath(f.home, f.task.id), "review-packet.json"), { ...packet, evidence: { digest: packet.digest } });
  await save(join(statePath(f.home, f.task.id), "review.json"), {});
  await writeFile(join(f.repo, "other.mjs"), "hidden staged content\n");
  await git(f.repo, "add", "other.mjs");
  await writeFile(join(f.repo, "other.mjs"), "export const original = true;\n");
  assert.equal((await snapshot(f.repo, f.base)).digest, packet.digest);
  const config = { ...defaults(f.repo), hosting: { kind: "github", remote: "github", repository: "whistlegraph/aesthetic-computer", writableReviewDestination: true } };
  await assert.rejects(publish(f.home, { ...f.task, status: "reviewed" }, config, async () => { throw Error("Must not reach remote"); }), /staged changes/);
  await git(f.repo, "add", "--", piece, "tests/fixture.test.mjs");
  await git(f.repo, "commit", "-qm", "fixture malicious index");
  const committed = await committedSnapshot(f.repo, f.base, await git(f.repo, "rev-parse", "HEAD"));
  assert.notEqual(committed.digest, packet.digest);
});

test("hosting is explicit, uses true draft records, and reconciles dedupe", async () => {
  assert.throws(() => hosting({ hosting: { kind: "unconfigured" } }), /Tangled/);
  assert.throws(() => hosting({ hosting: { kind: "github", repository: "fuserstudio/fuser" } }), /unconfigured/);
  const task = { id: "a".repeat(20), branch: "aespatcher-fixture-aaaaaaaa" }, host = { repository: "whistlegraph/aesthetic-computer" };
  const row = { body: `<!-- aespatcher:${task.id} -->`, isDraft: true };
  assert.deepEqual(await findDraft(host, task, async () => JSON.stringify([row])), row);
  await assert.rejects(findDraft(host, task, async () => JSON.stringify([row, row])), /More than one/);
});
