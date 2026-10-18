#!/usr/bin/env node
// run-task — one task under one harness, written down (docs/harness2.md, step 3).
//
//   ac run --task <task.json> --out <rundir> --live [--harness <dir>] [--model <id>]
//   ac run --init-harness <dir>
//
// A harness directory holds one subdirectory per surface, each the same
// markdown as its source: aesel/ (aesel/harness/*.md) and, where the private
// vault is linked, whistlegraph/ (apple/whistlegraph/harness/*.md). A surface
// left out runs as the repository has it, and so does any section an edited
// surface leaves out. Edited sections are swapped in before anything that
// places them loads, which is why `ac run` starts this file as a process of
// its own and why everything but the harness modules is imported late.
//
// The run directory: task.json; harness/, what actually ran; events.jsonl,
// every engine notification, step, paint and verdict, with frames written
// beside it under frames/; trace.md, the same as actions and observations;
// source/turn-N.mjs; result.json. Exit status 0 means the task delivered.
import { appendFileSync, existsSync, mkdirSync, readdirSync, readFileSync, writeFileSync } from "node:fs";
import { join, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { parseArgs } from "node:util";
import { parse } from "../bin/sync-harness.mjs";

const root = fileURLToPath(new URL("../../", import.meta.url));

// Each editable surface: its markdown, and the module that compiles it.
export const SURFACES = {
  aesel: { source: join(root, "aesel/harness"), module: join(root, "aesel/src/harness-text.mjs") },
  whistlegraph: { source: join(root, "apple/whistlegraph/harness"), module: join(root, "apple/whistlegraph/Resources/Web/whistlegraph-harness.mjs") },
};

// Runners by task kind. Whistlegraph's is private; it is there when the vault is linked.
const RUNNERS = {
  whistlegraph: {
    module: join(root, "lith/whistlegraph-run.mjs"),
    run: "runWhistlegraphTask",
    missing: "Whistlegraph tasks need the private vault linked: node toolchain/private-products.mjs link",
  },
};

export function readSurface(dir) {
  return Object.fromEntries(readdirSync(dir).filter((f) => f.endsWith(".md")).sort()
    .map((f) => [f.replace(/\.md$/, ""), parse(readFileSync(join(dir, f), "utf8"), f)]));
}
const flat = (components) => Object.assign({}, ...Object.values(components));

// Copy a surface's markdown by content. The public tree's Whistlegraph files
// are links into the vault, and a copied link would let an edited harness
// write straight back into the source it was copied from.
export function copySurface(from, to) {
  mkdirSync(to, { recursive: true });
  for (const f of readdirSync(from).filter((name) => name.endsWith(".md"))) writeFileSync(join(to, f), readFileSync(join(from, f)));
}

// Swap a harness directory's surfaces in, and say what differs from the repository.
export async function applyHarness(dir) {
  const report = {};
  for (const [name, surface] of Object.entries(SURFACES)) {
    const edited = dir && existsSync(join(dir, name)) ? join(dir, name) : null;
    if (!existsSync(surface.source) && !edited) continue;
    if (!edited) { report[name] = { source: "repository", changed: [], added: [], inherited: [] }; continue; }
    const base = existsSync(surface.source) ? flat(readSurface(surface.source)) : {};
    const components = readSurface(edited), sections = flat(components);
    const { useHarness } = await import(surface.module);
    const { unknown } = useHarness(components);
    report[name] = {
      source: edited,
      changed: Object.keys(sections).filter((key) => key in base && base[key] !== sections[key]),
      added: unknown,
      inherited: Object.keys(base).filter((key) => !(key in sections)),
    };
  }
  return report;
}

// events.jsonl, one line per event as it happens. A paint's frames go to
// frames/ as PNGs and the line keeps their file names instead of base64.
export function recorder(out) {
  const file = join(out, "events.jsonl");
  writeFileSync(file, "");
  const events = [];
  return {
    events,
    write(event) {
      let params = event.params ?? {};
      if (event.method === "whistlegraph/paint" && params.frames?.length) {
        params = { ...params, frames: params.frames.map((frame, i) => {
          const name = `frames/turn-${event.turn ?? 0}-render-${params.renderID}-${i + 1}.png`;
          writeFileSync(join(out, name), Buffer.from(frame.png, "base64"));
          return { atMs: frame.atMs, width: frame.width, height: frame.height, file: name };
        }) };
      }
      const line = { at: new Date().toISOString(), turn: event.turn ?? null, method: event.method, params };
      events.push(line);
      appendFileSync(file, JSON.stringify(line) + "\n");
    },
  };
}

// Tokens per turn, summed over its rounds.
export function usageByTurn(events) {
  const usage = {};
  for (const e of events) {
    if (e.method !== "turn/usage" || !e.params?.usage) continue;
    const u = (usage[e.turn] ??= { rounds: 0, input: 0, output: 0 });
    u.rounds += 1;
    u.input += e.params.usage.input_tokens ?? 0;
    u.output += e.params.usage.output_tokens ?? 0;
  }
  return usage;
}

// The transcript as a person (or a harness editor) reads it: per turn, the
// request; per round, what the model said and wrote; then what the preview
// and the reviewer observed.
export function renderTrace(task, events, result = null) {
  const lines = [`# ${task.id || "task"}`, ""];
  lines.push([`kind ${task.kind}`, task.domain && `domain ${task.domain}`, task.model && `model ${task.model}`,
    result && (result.delivered ? "delivered" : "not delivered")].filter(Boolean).join(" · "), "");
  let said = "", code = "", tool = "", round = 0;
  const flush = () => {
    if (said.trim()) lines.push(`**model:** ${said.trim()}`, "");
    if (code) {
      lines.push(`**action:** \`${tool || "tool"}\` (${code.length.toLocaleString("en-US")} characters)`, "", "```json",
        code.length > 4000 ? code.slice(0, 4000) + "\n…" : code, "```", "");
    }
    said = ""; code = "";
  };
  for (const e of events) {
    const p = e.params ?? {};
    if (e.method === "whistlegraph/turn") {
      flush(); round = 0;
      lines.push(`## Turn ${e.turn}`, "", ...String(p.request).split("\n").map((l) => "> " + l), "");
    } else if (e.method === "turn/started") {
      flush(); round += 1;
      lines.push(`### Round ${round}`, "");
    } else if (e.method === "item/agentMessage/delta") {
      said += p.delta ?? "";
    } else if (e.method === "item/modelCode/delta") {
      if (p.tool && p.tool !== tool) { flush(); tool = p.tool; }
      code += p.delta ?? "";
    } else if (e.method === "whistlegraph/step" && /^(repair|review unavailable|checking the picture again)/.test(p.text)) {
      flush();
      lines.push(`_${p.text}_`, "");
    } else if (e.method === "whistlegraph/paint") {
      flush();
      const errors = (p.logs ?? []).filter((l) => l.level === "error");
      lines.push(`**observation:** ${p.rendered ? "painted" : "did not paint"}${p.error ? ` (${p.error})` : ""}` +
        (errors.length ? ` · ${errors.length} runtime error${errors.length === 1 ? "" : "s"}` : ""), "");
      for (const l of errors.slice(0, 3)) lines.push("    " + String(l.text).slice(0, 300));
      if (errors.length) lines.push("");
      if (p.frames?.length) lines.push(p.frames.map((f) => `![${Math.round(f.atMs)} ms](${f.file})`).join(" "), "");
    } else if (e.method === "whistlegraph/review") {
      flush();
      lines.push(`**review:** ${p.passed ? "passed" : "findings"} — ${p.observations ?? ""}`, "");
      for (const f of p.findings ?? []) lines.push(`- ${f}`);
      if (p.findings?.length) lines.push("");
    }
  }
  flush();
  if (result) {
    lines.push("## Result", "");
    for (const t of result.turns) {
      lines.push(`- turn ${t.turn}: ${t.delivered ? `${t.acceptance}${t.repairs ? ", repaired" : ""}` : `not delivered — ${t.error}`}` +
        (t.usage ? ` · ${t.usage.rounds} rounds, ${t.usage.output.toLocaleString("en-US")} tokens out` : ""));
      for (const n of t.notes ?? []) lines.push(`  - ${n}`);
    }
    if (result.error) lines.push(`- error: ${result.error}`);
  }
  return lines.join("\n") + "\n";
}

export async function main(argv = process.argv.slice(2)) {
  const { values } = parseArgs({ args: argv, options: {
    task: { type: "string" }, out: { type: "string" }, harness: { type: "string" }, model: { type: "string" },
    live: { type: "boolean" }, "init-harness": { type: "string" },
  } });
  if (values["init-harness"]) {
    const dir = resolve(values["init-harness"]);
    for (const [name, surface] of Object.entries(SURFACES)) {
      if (!existsSync(surface.source)) continue;
      copySurface(surface.source, join(dir, name));
      console.log(`${name}/  ${readdirSync(join(dir, name)).length} files`);
    }
    return 0;
  }
  if (!values.task || !values.out) throw Error("usage: ac run --task <task.json> --out <rundir> --live [--harness <dir>] [--model <id>]");
  if (!values.live) throw Error("Pass --live: a run spends braincells on real inference.");
  const task = JSON.parse(readFileSync(values.task, "utf8"));
  if (values.model) task.model = values.model;
  const runner = RUNNERS[task.kind];
  if (!runner) throw Error(`Unknown task kind "${task.kind}"; known: ${Object.keys(RUNNERS).join(", ")}`);
  if (!existsSync(runner.module)) throw Error(runner.missing);
  const out = resolve(values.out);
  if (existsSync(out) && readdirSync(out).length) throw Error(`${out} is not empty`);
  mkdirSync(join(out, "frames"), { recursive: true });
  mkdirSync(join(out, "source"));
  writeFileSync(join(out, "task.json"), JSON.stringify(task, null, 2) + "\n");

  const harness = await applyHarness(values.harness ? resolve(values.harness) : null);
  for (const [name, report] of Object.entries(harness)) {
    const surface = SURFACES[name];
    if (existsSync(surface.source)) copySurface(surface.source, join(out, "harness", name));
    if (report.source !== "repository") copySurface(report.source, join(out, "harness", name));
  }

  const { ACSession } = await import("./ac-session.mjs");
  const session = new ACSession();
  if (!session.signedIn) throw Error("sign in first: ac login");
  const record = recorder(out);
  const { [runner.run]: run } = await import(runner.module);
  const startedAt = new Date().toISOString(), started = Date.now();
  let outcome, error = "";
  try {
    outcome = await run({ task, auth: { token: () => session.token() }, onEvent: (event) => record.write(event),
      log: (...words) => record.write({ method: "log", params: { text: words.join(" ") } }) });
  } catch (failure) {
    error = String(failure?.message || failure);
    outcome = { turns: [], delivered: false, reviewed: false };
  }
  for (const t of outcome.turns) if (t.source || t.draft) writeFileSync(join(out, "source", `turn-${t.turn}.mjs`), t.source || t.draft);
  const usage = usageByTurn(record.events);
  const result = {
    schema: 1, task: task.id || "task", kind: task.kind, domain: task.domain || "", model: task.model || "default",
    delivered: outcome.delivered, reviewed: outcome.reviewed, error, startedAt, ms: Date.now() - started, harness,
    turns: outcome.turns.map(({ source, draft, ...t }) => ({ ...t, ...(usage[t.turn] ? { usage: usage[t.turn] } : {}) })),
  };
  writeFileSync(join(out, "result.json"), JSON.stringify(result, null, 2) + "\n");
  writeFileSync(join(out, "trace.md"), renderTrace(task, record.events, result));
  console.log(`${result.delivered ? "✓" : "✕"} ${result.task}: ${result.delivered ? (result.reviewed ? "delivered, picture passed" : "delivered with notes") : "not delivered"} (${(result.ms / 1000).toFixed(0)} s) → ${out}`);
  for (const t of result.turns) console.log(`  turn ${t.turn}: ${t.delivered ? t.acceptance : t.error}`);
  if (error) console.log(`  error: ${error}`);
  return result.delivered ? 0 : 1;
}

if (process.argv[1] && resolve(process.argv[1]) === fileURLToPath(import.meta.url)) {
  main().then((code) => process.exit(code), (error) => { process.stderr.write(`ac run: ${error.message}\n`); process.exit(2); });
}
