#!/usr/bin/env node
// route-trial — does routing by the ask beat one model for everything?
//
//   node bin/route-trial.mjs [--tasks tasks] [--arms qwen,flash,sonnet,auto]
//     [--runs ../tmp/route-trial/<date>] [--jobs 3] [--live]
//
// Every task runs once under each arm through `ac run` (src/run-task.mjs). The
// fixed arms give each task its true tier — the cheapest model that delivers
// it — and `auto` is the router (src/model-router.mjs). The report says how
// often the router's first pick matched the true tier, and what a delivered
// piece cost under each arm, at the prices below.
//
// Without --live it lists the runs and what the router would pick, and spends
// nothing. A finished run is not repeated: delete its directory to redo it.
import { existsSync, mkdirSync, readFileSync, readdirSync, statSync, writeFileSync } from "node:fs";
import { join, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { parseArgs } from "node:util";
import { rollout } from "./refine.mjs";
import { ROUTE_MODELS, ROUTE_TIERS, routeAsk } from "../src/model-router.mjs";
import { HOSTED_MODELS } from "../src/open-models.mjs";

const aesel = resolve(fileURLToPath(import.meta.url), "../..");

// Dollars per million tokens: cache read / input / output. OpenRouter's list
// for the open models (open-models.mjs), Anthropic's for Sonnet. Braincells
// bill twice this. Flash 1017's cache-read price is not published; a tenth of
// input is assumed, as for the rest of the family.
const PRICES = {
  [HOSTED_MODELS.qwen]: [0.064, 0.32, 1.28],
  [HOSTED_MODELS.flash]: [0.03, 0.30, 1.20],
  [HOSTED_MODELS.deepseek]: [0.065, 0.78, 1.57],
  [HOSTED_MODELS.kimi]: [0.30, 3.00, 15.0],
  [HOSTED_MODELS.sonnet]: [0.20, 2.00, 10.0],
  [HOSTED_MODELS.opus]: [0.40, 4.00, 20.0],
};

function tasksUnder(path) {
  if (statSync(path).isFile()) return [JSON.parse(readFileSync(path, "utf8"))];
  return readdirSync(path, { withFileTypes: true }).sort((a, b) => a.name.localeCompare(b.name))
    .flatMap((d) => d.isDirectory() ? tasksUnder(join(path, d.name)) : d.name.endsWith(".json") ? tasksUnder(join(path, d.name)) : []);
}

// What a run spent, priced round by round with the model that ran the round.
export function runCost(events) {
  let usd = 0, unpriced = 0;
  for (const e of events) {
    if (e.method !== "turn/usage" || !e.params?.usage) continue;
    const price = PRICES[e.params.model];
    if (!price) { unpriced += 1; continue; }
    const u = e.params.usage;
    usd += ((u.cache_read_input_tokens ?? 0) * price[0] + ((u.input_tokens ?? 0) + (u.cache_creation_input_tokens ?? 0)) * price[1] + (u.output_tokens ?? 0) * price[2]) / 1e6;
  }
  return { usd, unpriced };
}

const readEvents = (dir) => {
  try { return readFileSync(join(dir, "events.jsonl"), "utf8").split("\n").filter(Boolean).map((l) => JSON.parse(l)); }
  catch { return []; }
};
const routesOf = (events) => events.filter((e) => e.method === "model/routed").map((e) => e.params);
const slug = (id) => id.replace(/[^a-z0-9-]+/gi, "__");

// The router's first pick for a task: its last request decides, since that is
// the turn a revision task exists to test.
const predicted = (task) => routeAsk({ text: task.requests.at(-1), source: task.requests.length > 1 ? "x\n".repeat(40) : "" });

async function pool(items, jobs, work) {
  const queue = [...items];
  await Promise.all(Array.from({ length: jobs }, async () => { while (queue.length) await work(queue.shift()); }));
}

export function report(tasks, arms, runs) {
  const rows = [], totals = Object.fromEntries(arms.map((a) => [a, { delivered: 0, usd: 0, runs: 0 }]));
  let matched = 0, judged = 0, under = 0;
  for (const task of tasks) {
    const row = { task: task.id.replace(/^piece-/, ""), predicted: predicted(task).tier, arms: {} };
    for (const arm of arms) {
      const dir = join(runs, slug(task.id), arm);
      if (!existsSync(join(dir, "result.json"))) continue;
      const result = JSON.parse(readFileSync(join(dir, "result.json"), "utf8"));
      const events = readEvents(dir), cost = runCost(events), routes = routesOf(events);
      row.arms[arm] = { delivered: !!result.delivered, usd: cost.usd, ...(arm === "auto" ? { route: routes.map((r) => r.tier).join("→") } : {}) };
      totals[arm].runs += 1; totals[arm].usd += cost.usd; if (result.delivered) totals[arm].delivered += 1;
    }
    // True tier: the cheapest fixed tier that delivered.
    row.truth = ROUTE_TIERS.find((tier) => row.arms[ROUTE_MODELS[tier]]?.delivered) || (ROUTE_TIERS.every((t) => row.arms[ROUTE_MODELS[t]]) ? "none" : "");
    if (row.truth && row.truth !== "none") {
      judged += 1;
      if (row.truth === row.predicted) matched += 1;
      if (ROUTE_TIERS.indexOf(row.predicted) < ROUTE_TIERS.indexOf(row.truth)) under += 1;
    }
    rows.push(row);
  }
  return { rows, totals, matched, judged, under };
}

function print({ rows, totals, matched, judged, under }, arms) {
  const cell = (a) => a ? `${a.delivered ? "✓" : "✕"} $${a.usd.toFixed(3)}${a.route ? ` ${a.route}` : ""}` : "·";
  const lines = [`| task | router | true | ${arms.join(" | ")} |`, `|---|---|---|${arms.map(() => "---").join("|")}|`];
  for (const r of rows) lines.push(`| ${r.task} | ${r.predicted} | ${r.truth || "·"} | ${arms.map((a) => cell(r.arms[a])).join(" | ")} |`);
  lines.push("", `| arm | delivered | total $ | $ per delivered |`, `|---|---|---|---|`);
  for (const a of arms) {
    const t = totals[a];
    lines.push(`| ${a} | ${t.delivered}/${t.runs} | ${t.usd.toFixed(3)} | ${t.delivered ? (t.usd / t.delivered).toFixed(3) : "—"} |`);
  }
  lines.push("", `router's first pick matched the true tier on ${matched}/${judged}; picked too low on ${under}.`);
  return lines.join("\n");
}

if (process.argv[1] && resolve(process.argv[1]) === fileURLToPath(import.meta.url)) {
  const { values } = parseArgs({ options: {
    tasks: { type: "string", default: join(aesel, "tasks") },
    arms: { type: "string", default: "qwen,flash,sonnet,auto" },
    runs: { type: "string", default: join(aesel, "../tmp/route-trial", new Date().toISOString().slice(0, 10)) },
    jobs: { type: "string", default: "3" },
    timeout: { type: "string", default: "600" },
    live: { type: "boolean" },
  } });
  const tasks = tasksUnder(resolve(values.tasks)), arms = values.arms.split(",").map((s) => s.trim()).filter(Boolean);
  for (const arm of arms) if (arm !== "auto" && !HOSTED_MODELS[arm]) throw Error(`unknown arm ${arm}: use auto or one of ${Object.keys(HOSTED_MODELS).join(", ")}`);
  const runs = resolve(values.runs);
  if (!values.live) {
    for (const task of tasks) { const r = predicted(task); console.log(`${r.tier.padEnd(7)}${task.id.padEnd(36)}${r.reasons.join(", ")}`); }
    console.log(`\n${tasks.length} tasks × ${arms.length} arms = ${tasks.length * arms.length} runs → ${runs}\nPass --live to spend braincells on them.`);
    process.exit(0);
  }
  mkdirSync(runs, { recursive: true });
  const work = tasks.flatMap((task) => arms.map((arm) => ({ task, arm })));
  let done = 0;
  await pool(work, Number(values.jobs), async ({ task, arm }) => {
    const out = join(runs, slug(task.id), arm);
    if (existsSync(join(out, "result.json"))) { done += 1; return; }
    const result = await rollout(task, null, out, { acModel: arm === "auto" ? "auto" : HOSTED_MODELS[arm], timeout: Number(values.timeout) });
    done += 1;
    console.error(`[${done}/${work.length}] ${result.delivered ? "✓" : "✕"} ${task.id} · ${arm}${result.error ? ` — ${result.error}` : ""}`);
  });
  const summary = report(tasks, arms, runs);
  writeFileSync(join(runs, "report.json"), JSON.stringify(summary, null, 2) + "\n");
  const text = print(summary, arms);
  writeFileSync(join(runs, "report.md"), text + "\n");
  console.log(text);
}
