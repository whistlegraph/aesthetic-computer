#!/usr/bin/env node
// refine — propose, probe and compose a better harness, one task at a time
// (docs/harness2.md, step 5).
//
// The method is Harness² (google-research/harness2, Apache-2.0; Xu et al.,
// "Recursive Agent Harnessing for an Open World", arXiv:2610.14224), carried
// over to the harness directories `ac run` takes; its prompts are adapted in
// refine/prompts/. For each task:
//
//   base       run the task under the starting harness
//   propose    two editors read that run and edit the harness — one on how the
//              model works (inv), one on what a piece-maker knows (dom)
//   probe      run the task under each candidate
//   compose    a composer keeps the edits whose runs did better where they aimed
//   select     a selector, shown only the pieces, picks the one to keep
//   reflect    a reflector records what each edit did and at most one lesson,
//              into a library the next task of the same domain reads first
//
// No answer key is used, and keeping the base is a valid outcome. Every task
// starts from the same harness; what carries forward is the library. Runs go
// through `ac run`, so a task of any kind it knows — piece, whistlegraph — works.
//
// Nothing here edits a surface's source. Each task leaves `adopted/` — only the
// sections that changed, itself a harness `ac run --harness` takes — and
// `changes.md`, for a person to read and merge.
//
//   node bin/refine.mjs --tasks tasks/typography                     # list; spends nothing
//   node bin/refine.mjs --live --tasks tasks/typography [--from <harness dir>]
//     [--runs <dir>] [--library refine/library] [--model <claude model>] [--ac-model <id>]
//
// A task costs up to four runs and five Claude sessions.
import { spawn } from "node:child_process";
import { cpSync, existsSync, mkdirSync, readFileSync, readdirSync, rmSync, statSync, writeFileSync } from "node:fs";
import { createHash } from "node:crypto";
import { homedir } from "node:os";
import { basename, join, relative, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { parseArgs } from "node:util";
import { SURFACES, readSurface } from "../src/run-task.mjs";

const aesel = resolve(fileURLToPath(import.meta.url), "../..");
const PROMPTS = join(aesel, "refine/prompts");
const DIMENSIONS = { inv: "dimension-inv.md", dom: "dimension-dom.md" };
const STAGE_ATTEMPTS = 2;
const INFRASTRUCTURE = /braincells|HTTP 40[123]|OpenRouter account|sign in first|session refresh failed|could not reach/i;
const POINTER = "Read MISSION.md in this directory and do what it says. Everything it refers to is under context/. Work only inside this directory.";

// Their own slots are [[name]]: {{name}} is harness syntax the prompts describe.
const prompt = (name, values = {}) =>
  readFileSync(join(PROMPTS, name), "utf8").replace(/\[\[(\w+)\]\]/g, (_, key) => {
    if (!(key in values)) throw Error(`refine/prompts/${name} needs [[${key}]]`);
    return values[key];
  });
const slug = (id) => id.replace(/[^a-z0-9-]+/gi, "__");
const surfaces = () => Object.entries(SURFACES).filter(([, surface]) => existsSync(surface.source));
const slots = (text) => [...text.matchAll(/\{\{(\w+)\}\}/g)].map((m) => m[1]).sort().join(",");

// Every section of a harness directory, keyed "surface/section".
export function sectionsOf(dir) {
  const all = {};
  for (const [name] of surfaces()) {
    if (!existsSync(join(dir, name))) continue;
    for (const [key, text] of Object.entries(Object.assign({}, ...Object.values(readSurface(join(dir, name)))))) all[`${name}/${key}`] = text;
  }
  return all;
}

// The full harness a stage edits: each surface's source files, with a starting
// harness's sections laid over them, so an editor always sees every section.
export function materialise(from, dir) {
  const over = from ? sectionsOf(from) : {};
  for (const [name, surface] of surfaces()) {
    mkdirSync(join(dir, name), { recursive: true });
    for (const file of readdirSync(surface.source).filter((f) => f.endsWith(".md"))) {
      let text = readFileSync(join(surface.source, file), "utf8");
      const own = readSurface(surface.source)[file.replace(/\.md$/, "")];
      for (const [key, body] of Object.entries(own)) {
        const next = over[`${name}/${key}`];
        if (next !== undefined && next !== body) text = text.replace(`## ${key}\n\n${body}\n`, `## ${key}\n\n${next}\n`);
      }
      writeFileSync(join(dir, name, file), text);
    }
  }
  return dir;
}

// What an edited harness may be: the same sections as the source, with the
// same placeholders. Throws the reasons, worded for the editor to act on.
export function validate(dir) {
  const problems = [];
  for (const [name, surface] of surfaces()) {
    const base = Object.assign({}, ...Object.values(readSurface(surface.source)));
    let edited;
    try { edited = existsSync(join(dir, name)) ? Object.assign({}, ...Object.values(readSurface(join(dir, name)))) : base; }
    catch (error) { problems.push(`${name}/: ${error.message}`); continue; }
    for (const key of Object.keys(edited)) if (!(key in base)) problems.push(`${name}/: no code sends a section named ${key} — fold it into an existing section`);
    for (const key of Object.keys(base)) {
      if (!(key in edited)) problems.push(`${name}/: section ${key} was removed — rewrite it instead of deleting it`);
      else if (slots(edited[key]) !== slots(base[key])) problems.push(`${name}/: ${key} must keep exactly the placeholders {${slots(base[key])}}`);
    }
  }
  if (problems.length) throw Error(problems.join("\n"));
}

export function changed(a, b) {
  const before = sectionsOf(a), after = sectionsOf(b);
  return Object.fromEntries(Object.keys(after).filter((key) => before[key] !== after[key]).map((key) => [key, [before[key], after[key]]]));
}

// One file per component, holding only the changed sections.
function writeAdopted(changes, out) {
  rmSync(out, { recursive: true, force: true });
  for (const [name, surface] of surfaces()) {
    for (const [component, own] of Object.entries(readSurface(surface.source))) {
      const keys = Object.keys(own).filter((key) => `${name}/${key}` in changes);
      if (!keys.length) continue;
      mkdirSync(join(out, name), { recursive: true });
      writeFileSync(join(out, name, `${component}.md`), keys.map((key) => `## ${key}\n\n${changes[`${name}/${key}`][1]}\n`).join("\n"));
    }
  }
}

export function parseChoice(reply) {
  return { choice: /```choice\s*\n?\s*([A-Z])\s*\n?\s*```/.exec(reply)?.[1] || "", why: /<why>([\s\S]*?)<\/why>/.exec(reply)?.[1]?.trim() || "" };
}

export function parseExperience(reply) {
  const edits = /<edits>([\s\S]*?)<\/edits>/.exec(reply)?.[1]?.trim() || "";
  const block = /<experience>([\s\S]*?)<\/experience>/.exec(reply)?.[1] || "";
  const field = (name) => new RegExp(`^${name}:\\s*(.+)$`, "m").exec(block)?.[1]?.trim() || "";
  const lesson = { cue: field("cue"), lesson: field("lesson"), evidence: field("evidence"), confidence: field("confidence") };
  return { edits, lesson: lesson.cue && lesson.lesson && lesson.evidence ? lesson : null };
}

// The library for one domain: an entry per task, and an index the next
// round's editors open first.
export function addToLibrary(root, domain, task, { harness, chosen, why, edits, lesson }) {
  const dir = join(root, domain);
  const entry = join(dir, "entries", slug(task.id));
  rmSync(entry, { recursive: true, force: true });
  mkdirSync(entry, { recursive: true });
  if (harness) cpSync(harness, join(entry, "harness"), { recursive: true });
  writeFileSync(join(entry, "EXPERIENCE.md"), [
    `# ${task.id}`, "", ...requestsOf(task).map((r) => `> ${r}`), "", `- chosen: \`${chosen}\``, "",
    ...(why ? ["## Why it was chosen", why, ""] : []),
    ...(edits ? ["## What each edit did", edits, ""] : []),
    ...(lesson ? ["## Lesson", ...["cue", "lesson", "evidence", "confidence"].filter((f) => lesson[f]).map((f) => `- **${f}**: ${lesson[f]}`), ""] : []),
  ].join("\n"));
  const rows = [`# Library — ${domain}`, "",
    "Each entry is an earlier request in this domain: what its round tried, what each edit did, and the lesson it left, if any. Read the cues; open the entries that fit the request in front of you.", ""];
  for (const name of readdirSync(join(dir, "entries")).sort()) {
    const cue = /^- \*\*cue\*\*: (.+)$/m.exec(readFileSync(join(dir, "entries", name, "EXPERIENCE.md"), "utf8"))?.[1];
    rows.push(`## entries/${name}/`, `- record: \`entries/${name}/EXPERIENCE.md\``, ...(cue ? [`- cue: ${cue}`] : []), "");
  }
  writeFileSync(join(dir, "INDEX.md"), rows.join("\n"));
}

const requestsOf = (task) => task.requests ?? (task.prompt ? [task.prompt] : []);

// --- the two things a round spends money on ---------------------------------

function run(command, args, { cwd, timeout }) {
  return new Promise((done) => {
    const child = spawn(command, args, { cwd, stdio: ["ignore", "pipe", "pipe"] });
    let out = "", err = "";
    child.stdout.on("data", (d) => (out += d));
    child.stderr.on("data", (d) => (err += d));
    const timer = setTimeout(() => child.kill("SIGKILL"), timeout * 1000);
    child.on("close", (code) => { clearTimeout(timer); done({ code, out, err }); });
  });
}

// One task under one harness through `ac run`, in a process of its own: the
// harness is fixed at import.
export async function rollout(task, harness, out, { acModel = "", timeout = 900 } = {}) {
  rmSync(out, { recursive: true, force: true });
  mkdirSync(join(out, ".."), { recursive: true });
  const file = `${out}.task.json`;
  writeFileSync(file, JSON.stringify(task));
  const { err } = await run(process.execPath, [join(aesel, "src/run-task.mjs"), "--task", file, "--out", out, "--live",
    ...(harness ? ["--harness", harness] : []), ...(acModel ? ["--model", acModel] : [])], { cwd: aesel, timeout });
  try { return JSON.parse(readFileSync(join(out, "result.json"), "utf8")); }
  catch { return { delivered: false, error: err.trim().split("\n").pop() || "the run left no result", turns: [] }; }
}

// One improver session: Claude Code reading and editing files under `stage`,
// with the person's own settings, hooks and MCP servers kept out.
export async function claude(stage, mission, { model = "", timeout = 1800 } = {}) {
  const { code, out, err } = await run("claude", ["-p", mission, "--output-format", "json",
    "--setting-sources", "", "--strict-mcp-config", "--permission-mode", "acceptEdits",
    "--allowedTools", "Read", "Edit", "Write", "Glob", "Grep", ...(model ? ["--model", model] : [])],
  { cwd: stage, timeout });
  try {
    const reply = JSON.parse(out);
    return { text: String(reply.result || ""), usd: Number(reply.total_cost_usd) || 0, error: reply.is_error ? String(reply.result) : "" };
  } catch { return { text: "", usd: 0, error: `claude exited ${code}: ${err.slice(-400)}` }; }
}

// --- evidence, served as files ----------------------------------------------

// A run's trace with its frames, so the trace's image links resolve.
function serveRun(runDir, to) {
  mkdirSync(to, { recursive: true });
  if (existsSync(join(runDir, "trace.md"))) cpSync(join(runDir, "trace.md"), join(to, "trace.md"));
  else writeFileSync(join(to, "trace.md"), "(this run left no trace)\n");
  if (existsSync(join(runDir, "frames"))) cpSync(join(runDir, "frames"), join(to, "frames"), { recursive: true });
}

// What a selector compares: the last source, the last frame, what was reported.
function serveDeliverable(runDir, result, to) {
  mkdirSync(to, { recursive: true });
  const sources = existsSync(join(runDir, "source")) ? readdirSync(join(runDir, "source")).sort() : [];
  if (sources.length) cpSync(join(runDir, "source", sources.at(-1)), join(to, "piece.mjs"));
  const frames = existsSync(join(runDir, "frames")) ? readdirSync(join(runDir, "frames")).filter((f) => f.endsWith(".png"))
    .sort((a, b) => statSync(join(runDir, "frames", a)).mtimeMs - statSync(join(runDir, "frames", b)).mtimeMs) : [];
  if (frames.length) cpSync(join(runDir, "frames", frames.at(-1)), join(to, "frame.png"));
  writeFileSync(join(to, "check.md"), [result.delivered ? "Every turn delivered." : `Not delivered${result.error ? `: ${result.error}` : "."}`,
    ...(result.turns ?? []).flatMap((t) => [`- turn ${t.turn}: ${t.delivered ? t.acceptance : t.error}`, ...(t.notes ?? []).map((n) => `  - ${n}`)]), ""].join("\n"));
}

// The sections the base run actually received, when its runner could say.
function serveSent(result, to) {
  const lines = Object.entries(result.harness ?? {}).filter(([, report]) => report.sent)
    .flatMap(([name, report]) => report.sent.map((key) => `- ${name}/${key}`));
  writeFileSync(to, lines.length
    ? `# Sections this run received\n\nOnly these reached the model. An edit anywhere else cannot change what this kind of run does.\n\n${lines.join("\n")}\n`
    : "# Sections this run received\n\nThis runner does not report them. Read the trace to judge which sections apply.\n");
}

function stageFor(dir, task, library) {
  rmSync(dir, { recursive: true, force: true });
  mkdirSync(join(dir, "context"), { recursive: true });
  writeFileSync(join(dir, "context/task.md"), `# The request\n\n${requestsOf(task).join("\n")}\n`);
  if (library && existsSync(join(library, "INDEX.md"))) cpSync(library, join(dir, "context/library"), { recursive: true });
  return dir;
}

// Run a stage, check the harness it leaves, and give it one more try with the
// reason when that harness is unusable; after that the stage keeps `fallback`.
async function editStage(stage, mission, fallback, ask, log) {
  writeFileSync(join(stage, "MISSION.md"), mission);
  const edited = join(stage, "context/harness");
  let reason = "", usd = 0, notes = "";
  for (let attempt = 1; attempt <= STAGE_ATTEMPTS; attempt++) {
    const reply = await ask(stage, reason ? `${POINTER}\n\nYOUR PREVIOUS ATTEMPT WAS REJECTED\n\n${reason}\n\nFix it in context/harness/ and reply again.` : POINTER);
    usd += reply.usd;
    if (reply.error) { reason = reply.error; continue; }
    notes = reply.text;
    try { validate(edited); return { dir: edited, notes, usd, fellBack: false }; }
    catch (error) { reason = error.message; log(`  ${basename(stage)} rejected: ${reason.split("\n")[0]}`); }
  }
  rmSync(edited, { recursive: true, force: true });
  cpSync(fallback, edited, { recursive: true });
  return { dir: edited, notes: `(fell back: ${reason})`, usd, fellBack: true };
}

export async function refineTask(task, { runs, library, from = "", model = "", acModel = "", timeout = 900,
  claude: improver = claude, rollout: execute = rollout, log = console.log } = {}) {
  const domain = task.domain || task.kind || "general";
  const round = join(runs, slug(task.id));
  const lib = join(library, domain);
  const ask = (stage, mission) => improver(stage, mission, { model });
  const runOf = (label, harness) => execute(task, harness, join(round, "runs", label), { acModel, timeout });
  let usd = 0;

  const base = materialise(from, join(round, "harness/base"));
  log(`${task.id}: base run`);
  const results = { base: await runOf("base", base) };
  // A base run that never reached the model — out of braincells, an unfunded
  // relay, a lapsed sign-in — is not evidence about the harness. Stop before
  // any improver is paid to read it; the next task would fail the same way.
  const blocked = [results.base.error, ...(results.base.turns ?? []).map((t) => t.error)].find((e) => INFRASTRUCTURE.test(e || ""));
  if (blocked) throw Object.assign(Error(`${task.id}: the base run could not reach the model — ${blocked}`), { infrastructure: true });
  const harnesses = { base };
  const notes = {};
  const runDirOf = (label) => join(round, "runs", Object.keys(results).find((other) => results[other] === results[label]));

  // Propose, then probe each candidate; a candidate equal to the base reuses its run.
  await Promise.all(Object.entries(DIMENSIONS).map(async ([dim, file]) => {
    const stage = stageFor(join(round, `propose-${dim}`), task, lib);
    cpSync(base, join(stage, "context/harness"), { recursive: true });
    serveRun(runDirOf("base"), join(stage, "context/rollouts/base"));
    serveDeliverable(runDirOf("base"), results.base, join(stage, "context/deliverables/base"));
    serveSent(results.base, join(stage, "context/sent.md"));
    const edit = await editStage(stage, `${prompt("surface.md")}\n\n${prompt("propose.md", { dimension: prompt(file), kind: domain })}`, base, ask, log);
    usd += edit.usd;
    harnesses[dim] = join(round, `harness/${dim}`);
    cpSync(edit.dir, harnesses[dim], { recursive: true });
    notes[dim] = edit.notes;
    const edits = Object.keys(changed(base, harnesses[dim]));
    log(`${task.id}: ${dim} ${edit.fellBack ? "fell back" : edits.length ? `changed ${edits.join(", ")}` : "kept the base"}`);
    results[dim] = edits.length ? await runOf(dim, harnesses[dim]) : results.base;
  }));

  // Compose from what the candidate runs showed.
  const compose = stageFor(join(round, "compose"), task, lib);
  cpSync(base, join(compose, "context/harness"), { recursive: true });
  serveRun(runDirOf("base"), join(compose, "context/rollouts/base"));
  serveDeliverable(runDirOf("base"), results.base, join(compose, "context/deliverables/base"));
  serveSent(results.base, join(compose, "context/sent.md"));
  for (const dim of Object.keys(DIMENSIONS)) {
    const pool = join(compose, "context/pool", dim);
    cpSync(harnesses[dim], join(pool, "harness"), { recursive: true });
    serveRun(runDirOf(dim), join(pool, "rollout"));
    writeFileSync(join(pool, "notes.md"), notes[dim] || "(no notes)\n");
    serveDeliverable(runDirOf(dim), results[dim], join(compose, "context/deliverables", dim));
  }
  const composed = await editStage(compose, `${prompt("surface.md")}\n\n${prompt("compose.md")}`, base, ask, log);
  usd += composed.usd;
  harnesses.composed = join(round, "harness/composed");
  cpSync(composed.dir, harnesses.composed, { recursive: true });
  notes.composed = composed.notes;
  const twin = Object.keys(harnesses).find((label) => label !== "composed" && !Object.keys(changed(harnesses[label], harnesses.composed)).length);
  log(`${task.id}: composed ${twin ? `= ${twin}` : `changed ${Object.keys(changed(base, harnesses.composed)).join(", ")}`}`);
  results.composed = twin ? results[twin] : await runOf("composed", harnesses.composed);

  // Select among distinct pieces, anonymised, in an order fixed by the task.
  const distinct = Object.keys(results).filter((label, i, all) => all.findIndex((other) => results[other] === results[label]) === i);
  const order = distinct.map((label) => [createHash("sha256").update(task.id + label).digest("hex"), label]).sort().map(([, label]) => label);
  const letters = Object.fromEntries(order.map((label, i) => [String.fromCharCode(65 + i), label]));
  let chosen = "base", why = "";
  if (order.length > 1) {
    const select = stageFor(join(round, "select"), task, null);
    for (const [letter, label] of Object.entries(letters)) serveDeliverable(runDirOf(label), results[label], join(select, "context/outputs", letter));
    writeFileSync(join(select, "MISSION.md"), prompt("select.md", { n: String(order.length) }));
    const reply = await ask(select, POINTER);
    usd += reply.usd;
    const pick = parseChoice(reply.text);
    if (letters[pick.choice]) { chosen = letters[pick.choice]; why = pick.why; }
    else log(`${task.id}: no usable choice; keeping the base`);
  }
  log(`${task.id}: chose ${chosen}`);

  // Reflect, over every candidate, into the library.
  const reflect = stageFor(join(round, "reflect"), task, lib);
  cpSync(base, join(reflect, "context/harness"), { recursive: true });
  for (const label of Object.keys(results)) {
    serveRun(runDirOf(label), join(reflect, "context/rollouts", label));
    if (label === "base") continue;
    cpSync(harnesses[label], join(reflect, "context/pool", label, "harness"), { recursive: true });
    writeFileSync(join(reflect, "context/pool", label, "notes.md"), notes[label] || "(no notes)\n");
  }
  writeFileSync(join(reflect, "context/chosen.md"), `Chosen: ${chosen}\n\n${why}\n`);
  writeFileSync(join(reflect, "MISSION.md"), prompt("experience.md", { kind: domain }));
  const reflection = await ask(reflect, POINTER);
  usd += reflection.usd;
  const { edits, lesson } = parseExperience(reflection.text);
  addToLibrary(library, domain, task, { harness: harnesses[chosen], chosen, why, edits, lesson });

  // What a person merges: the chosen harness's sections that differ from the source.
  const changes = changed(materialise("", join(round, "harness/source")), harnesses[chosen]);
  writeAdopted(changes, join(round, "adopted"));
  writeFileSync(join(round, "changes.md"), [`# ${task.id} — chose ${chosen}`, "", why, "",
    ...(Object.keys(changes).length ? Object.entries(changes).flatMap(([key, [before, after]]) => [`## ${key}`, "", "Before:", "", before, "", "After:", "", after, ""]) : ["No section changed."]),
    ...(lesson ? ["## Lesson", "", `- cue: ${lesson.cue}`, `- lesson: ${lesson.lesson}`, `- evidence: ${lesson.evidence}`, ""] : [])].join("\n"));
  const summary = { task: task.id, domain, chosen, why, changed: Object.keys(changes),
    delivered: Object.fromEntries(Object.entries(results).map(([label, r]) => [label, !!r.delivered])),
    lesson, claudeUsd: Number(usd.toFixed(4)) };
  writeFileSync(join(round, "summary.json"), JSON.stringify(summary, null, 2) + "\n");
  return summary;
}

function tasksUnder(path) {
  if (statSync(path).isFile()) return [JSON.parse(readFileSync(path, "utf8"))];
  return readdirSync(path, { recursive: true }).map(String).filter((name) => name.endsWith(".json")).sort()
    .map((name) => JSON.parse(readFileSync(join(path, name), "utf8")));
}

if (process.argv[1] && resolve(process.argv[1]) === fileURLToPath(import.meta.url)) {
  const { values } = parseArgs({ options: {
    live: { type: "boolean" }, tasks: { type: "string" }, from: { type: "string", default: "" },
    runs: { type: "string", default: join(homedir(), ".local/share/aesel/refine", new Date().toISOString().replace(/[:.]/g, "-")) },
    library: { type: "string", default: join(aesel, "refine/library") }, model: { type: "string", default: "" },
    "ac-model": { type: "string", default: "" }, "max-tasks": { type: "string", default: "" }, timeout: { type: "string", default: "900" },
  } });
  if (!values.tasks) { console.error("usage: refine [--live] --tasks <task.json | dir> [--from <harness>] [--runs <dir>] [--library <dir>]"); process.exit(2); }
  let tasks = tasksUnder(resolve(values.tasks));
  if (values["max-tasks"]) tasks = tasks.slice(0, Number(values["max-tasks"]));
  if (!values.live) {
    for (const task of tasks) console.log(`${task.domain || task.kind}\t${task.id}\t${requestsOf(task).join(" → ")}`);
    console.log(`${tasks.length} task(s); runs → ${values.runs}; library → ${relative(process.cwd(), values.library) || values.library}\nPass --live to spend AC and Claude allowance on them.`);
    process.exit(0);
  }
  mkdirSync(values.runs, { recursive: true });
  const summaries = [];
  for (const task of tasks) {
    try {
      summaries.push(await refineTask(task, { runs: values.runs, library: resolve(values.library),
        from: values.from ? resolve(values.from) : "", model: values.model, acModel: values["ac-model"], timeout: Number(values.timeout) }));
    } catch (error) {
      if (!error.infrastructure) throw error;
      console.error(`stopped: ${error.message}`);
      break;
    }
  }
  writeFileSync(join(values.runs, "summary.json"), JSON.stringify(summaries, null, 2) + "\n");
  console.log(JSON.stringify(summaries.map(({ task, chosen, changed: sections, delivered }) => ({ task, chosen, sections, delivered })), null, 2));
  console.log(`runs: ${values.runs}`);
}
