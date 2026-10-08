import { readFile, writeFile } from "node:fs/promises";
import { join } from "node:path";
import { git, json, save, run, statePath } from "./io.mjs";
import { snapshot, committedSnapshot, validateReview } from "./hand.mjs";
import { inspectMap } from "./reuse.mjs";
import { assertReportPrivacy } from "./signals.mjs";

async function gh(args) {
  const result = await run(["gh", ...args]);
  if (result.code !== 0) throw new Error("GitHub command failed; inspect authentication separately");
  return result.stdout.trim();
}

export function hosting(config) {
  const host = config.hosting;
  if (host?.kind !== "github" || host.writableReviewDestination !== true ||
      host.repository !== "whistlegraph/aesthetic-computer" || !/^[a-z][a-z0-9-]*$/.test(host.remote))
    throw new Error("Draft publishing is unconfigured: AC uses Tangled; explicitly select a writable GitHub review destination before using this adapter");
  return host;
}

export async function findDraft(host, task, call = gh) {
  const rows = JSON.parse(await call(["pr", "list", "--repo", host.repository, "--head", task.branch, "--state", "all", "--limit", "100", "--json", "url,isDraft,headRefOid,body,state,baseRefName"]));
  const found = rows.filter(row => row.body.includes(`<!-- aespatcher:${task.id} -->`));
  if (found.length > 1) throw new Error("More than one remote task match; reconcile manually");
  if (!found.length && rows.length) throw new Error("Remote branch belongs to another PR");
  return found[0] || null;
}

export async function publish(home, task, config, call = gh) {
  const host = hosting(config);
  if (!["reviewed", "publishing"].includes(task.status)) throw new Error("Task needs an independent review");
  const dir = statePath(home, task.id), packet = await json(join(dir, "review-packet.json"));
  const evidence = await json(join(dir, "evidence.json"));
  assertReportPrivacy(`${packet.investigation?.title || ""} ${packet.investigation?.problem || ""} ${packet.investigation?.cause || ""}`, evidence.signals?.privateReports || []);
  const approval = await json(join(dir, "review.json"));
  const state = await snapshot(task.worktree, task.base);
  if (state.digest !== packet.digest || state.head !== (task.commit || task.base) || packet.evidence.digest !== packet.digest) throw new Error("Code or HEAD changed after review");
  for (const row of state.files.filter(row => !row.deleted))
    assertReportPrivacy(await readFile(join(task.worktree, row.file), "utf8"), evidence.signals?.privateReports || []);
  if (task.commit) {
    if (await git(task.worktree, "status", "--porcelain") || (await committedSnapshot(task.worktree, task.base, task.commit)).digest !== packet.digest)
      throw new Error("Committed tree differs from approved patch");
  } else if (await git(task.worktree, "diff", "--cached", "--name-only")) throw new Error("Unexpected staged changes before publication");
  validateReview(approval, packet, await inspectMap(task.worktree, await json(join(home, "reuse.json"))));
  if (await git(task.worktree, "branch", "--show-current") !== task.branch || !/^aespatcher-[a-z0-9-]+-[a-f0-9]{8}$/.test(task.branch)) throw new Error("Unexpected publication branch");
  const remote = await git(task.worktree, "remote", "get-url", host.remote);
  if (!["https://github.com/whistlegraph/aesthetic-computer.git", "git@github.com:whistlegraph/aesthetic-computer.git"].includes(remote)) throw new Error("Review remote is not the configured AC repository");
  const match = await findDraft(host, task, call);
  if (match) {
    if (!match.isDraft || match.state !== "OPEN" || match.baseRefName !== "main" || match.headRefOid !== task.commit) throw new Error("Existing PR differs from the reviewed draft; reconcile manually");
    task.status = "published"; task.url = match.url;
    await save(join(dir, "task.json"), task);
    return task;
  }
  for (const name of ["origin", host.remote]) {
    const ref = (await git(task.worktree, "ls-remote", name, "refs/heads/main")).split(/\s/)[0];
    if (ref !== task.base) throw new Error("Remote main moved or mirror lags; rebase and revalidate before publishing");
  }
  task.status = "publishing";
  await save(join(dir, "task.json"), task);
  if (!task.commit) {
    await git(task.worktree, "add", "--", ...state.files.map(row => row.file));
    await git(task.worktree, "commit", "-m", packet.investigation.title);
    task.commit = await git(task.worktree, "rev-parse", "HEAD");
    await save(join(dir, "task.json"), task);
    if (await git(task.worktree, "status", "--porcelain") || (await committedSnapshot(task.worktree, task.base, task.commit)).digest !== packet.digest || await git(task.worktree, "rev-parse", "HEAD^") !== task.base)
      throw new Error("Commit differs from the reviewed patch");
  }
  const body = `${packet.investigation.problem}\n\n${packet.investigation.cause}\n\nValidation: ${packet.evidence.tests.map(t => `node --test ${t.file}`).join("; ")}. The named regression assertion fails on the original piece and passes on this revision.\n\nHAND review: ${approval.kind} (${approval.reviewer}); source reuse checked against recorded hashes.\n\nBase: ${task.base}\nPatch: ${packet.digest}\n\n<!-- aespatcher:${task.id} -->\n`;
  // The reviewer's privacy attestation covers these human-readable fields; aggregate telemetry stays local.
  const bodyFile = join(dir, "pr-body.txt");
  await writeFile(bodyFile, body, { mode: 0o600 });
  await git(task.worktree, "push", host.remote, `${task.commit}:refs/heads/${task.branch}`);
  await call(["pr", "create", "--repo", host.repository, "--head", task.branch, "--base", "main", "--draft", "--title", packet.investigation.title, "--body-file", bodyFile]);
  const created = await findDraft(host, task, call);
  if (!created || !created.isDraft || created.headRefOid !== task.commit || created.baseRefName !== "main" || created.state !== "OPEN") throw new Error("Draft creation not verified; retry publish to reconcile");
  task.status = "published"; task.url = created.url;
  await save(join(dir, "task.json"), task);
  return task;
}
