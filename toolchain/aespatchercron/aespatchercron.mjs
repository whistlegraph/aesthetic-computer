#!/usr/bin/env node
import { dirname, resolve, join } from "node:path";
import { fileURLToPath } from "node:url";
import { readFile } from "node:fs/promises";
import { defaults, defaultHome, validateConfig } from "./config.mjs";
import { json, save, locked, statePath, git, hash } from "./io.mjs";
import { catalog, collect, validateReport } from "./collect.mjs";
import { seedMap, inspectMap, verifyFinding } from "./reuse.mjs";
import { admit, tasks } from "./queue.mjs";
import { prepare, work, validate, review } from "./worker.mjs";
import { handCheck } from "./hand.mjs";
import { publish } from "./publish.mjs";
import { emptySignals, validateSignals } from "./signals.mjs";

const args = process.argv.slice(2), homeIndex = args.indexOf("--home");
const home = homeIndex < 0 ? defaultHome : resolve(args.splice(homeIndex, 2)[1]);
const [command = "help", ...rest] = args;
const repo = resolve(dirname(fileURLToPath(import.meta.url)), "../..");
const help = `aespatchercron [--home DIR] COMMAND
  init                         create private config + source-anchored reuse map
  collect                      read network analytics, logs and public reports
  scan [REPORT.json]           collect (or import) and admit bounded hypotheses
  tick                         collect + scan + investigate one; stops for review
  status                       list local tasks
  prepare ID                   create an isolated worktree and worker brief
  work ID                      run sandboxed Codex investigation + regression checks
  validate ID                  repeat regression and revision-bound checks
  review ID REVIEW.json        record independent qualitative review
  publish ID                   submit verified draft through configured hosting
  reuse [QUERY]                query current reuse findings and stale flags
  reuse-add FINDING.json        verify an explicit finding at HEAD and update map
  hand BASE [WORKTREE]          inspect metric changes and review questions
No scheduler is installed. Default publication is unconfigured for AC's Tangled host.`;

async function scan(config, file) {
  let evidence;
  if (file) {
    const imported = await json(resolve(file)), routes = await catalog(config.repo);
    evidence = { report: validateReport(imported.report || imported, routes), signals: validateSignals(imported.signals || emptySignals(), routes),
      provenance: { source: "operator-import", revision: await git(config.repo, "rev-parse", "HEAD"),
        importedHash: hash(JSON.stringify(imported)), reportedProvenance: imported.provenance || null } };
  } else evidence = await collect(config.repo, config);
  await save(join(home, "latest.json"), evidence);
  return admit(home, evidence, config.bounds);
}

async function main() {
  if (command === "help" || command === "--help") return help;
  if (command === "hand") return handCheck(resolve(rest[1] || repo), rest[0] || "HEAD");
  return locked(home, async () => {
    if (command === "init") {
      try { await readFile(join(home, "config.json")); throw new Error("Config already exists; init never overwrites it"); }
      catch (error) { if (error.code !== "ENOENT") throw error; }
      await save(join(home, "reuse.json"), await seedMap(repo));
      await save(join(home, "config.json"), defaults(repo));
      return { status: "initialized", home };
    }
    const config = validateConfig(await json(join(home, "config.json")));
    if (command === "status") return (await tasks(home)).map(({ id, route, status, url, failure }) => ({ id, route, status, url, failure }));
    if (command === "reuse") {
      const map = await inspectMap(config.repo, await json(join(home, "reuse.json")));
      if (rest[0]) map.entries = map.entries.filter(e => `${e.id} ${e.file} ${e.finding}`.toLowerCase().includes(rest.join(" ").toLowerCase()));
      return map;
    }
    if (command === "reuse-add") {
      const entry = await verifyFinding(config.repo, await json(resolve(rest[0])));
      const map = await json(join(home, "reuse.json"));
      map.entries = [...map.entries.filter(e => e.id !== entry.id), entry];
      await save(join(home, "reuse.json"), map); return entry;
    }
    if (command === "collect") {
      const evidence = await collect(config.repo, config);
      await save(join(home, "latest.json"), evidence);
      return { report: evidence.report, signals: { coverage: evidence.signals.coverage, leads: evidence.signals.leads, metrics: evidence.signals.metrics,
        privateReportCount: evidence.signals.privateReports.length }, saved: join(home, "latest.json") };
    }
    if (command === "scan") return scan(config, rest[0]);
    if (command === "tick") {
      const result = await scan(config);
      const next = (await tasks(home)).find(t => t.status === "prepared" || t.status === "queued");
      if (!next) return result;
      return work(home, next.status === "queued" ? await prepare(config.repo, home, next, config) : next, config);
    }
    const task = await json(join(statePath(home, rest[0]), "task.json"));
    if (command === "prepare") return prepare(config.repo, home, task, config);
    if (command === "work") return work(home, task, config);
    if (command === "validate") return validate(home, task, config);
    if (command === "review") return review(home, task, resolve(rest[1]));
    if (command === "publish") return publish(home, task, config);
    throw new Error("Unknown command");
  });
}

main().then(result => console.log(typeof result === "string" ? result : JSON.stringify(result, null, 2))).catch(error => {
  console.error(`aespatchercron: ${error.message}`); process.exitCode = 1;
});
