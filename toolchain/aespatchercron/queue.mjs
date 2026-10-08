import { readdir } from "node:fs/promises";
import { join } from "node:path";
import { hash, json, save, privateDir, statePath } from "./io.mjs";
import { eligiblePiece } from "./config.mjs";

export function opportunities(report, signals = { leads: [] }) {
  const found = report.runs.filter(row => eligiblePiece(row.route) && row.errors >= report.minimum).map(row => {
    const paths = report.transitions.filter(edge => edge.to === row.route || edge.from === row.route);
    return { id: hash(`piece-error:${row.route}`).slice(0, 20), route: row.route, kind: "piece-error",
      claim: `${row.route} recorded ${row.errors} errored runs of ${row.runs}; reproduce before proposing a fix.`,
      errors: row.errors, runs: row.runs, paths, score: row.errors * 10 + Math.min(100, paths.reduce((n, p) => n + p.count, 0)) };
  });
  for (const lead of signals.leads) {
    if (!lead.route || !eligiblePiece(lead.route)) continue;
    const id = hash(`${lead.source}:${lead.kind}:${lead.route}:${lead.evidenceRefs.join(",")}`).slice(0, 20);
    if (found.some(row => row.id === id)) continue;
    found.push({ id, route: lead.route, source: lead.source, kind: lead.kind, claim: lead.summary, count: lead.count,
      evidenceRefs: lead.evidenceRefs, paths: report.transitions.filter(edge => edge.to === lead.route || edge.from === lead.route),
      score: (lead.kind === "user-report" ? 50 : 20) + Math.min(100, lead.count) });
  }
  return found.sort((a, b) => b.score - a.score || a.id.localeCompare(b.id));
}

export async function tasks(home) {
  await privateDir(join(home, "tasks"));
  const rows = [];
  for (const id of await readdir(join(home, "tasks"))) {
    if (/^[a-f0-9]{20}$/.test(id)) rows.push(await json(join(statePath(home, id), "task.json")));
  }
  return rows;
}

export async function admit(home, evidence, bounds, now = new Date()) {
  const existing = await tasks(home);
  const active = existing.filter(t => !["idle", "published", "rejected"].includes(t.status)).length;
  const today = now.toISOString().slice(0, 10);
  let room = Math.min(bounds.maxActive - active, bounds.perDay - existing.filter(t => t.createdAt.startsWith(today)).length);
  const admitted = [];
  for (const candidate of opportunities(evidence.report, evidence.signals)) {
    if (room <= 0) break;
    if (existing.some(t => t.id === candidate.id || (t.route === candidate.route && !["idle", "published", "rejected"].includes(t.status)))) continue;
    if (admitted.some(t => t.route === candidate.route)) continue;
    const dir = await privateDir(statePath(home, candidate.id));
    const task = { ...candidate, createdAt: now.toISOString(), status: "queued", evidence: hash(JSON.stringify(evidence)) };
    await save(join(dir, "evidence.json"), evidence);
    await save(join(dir, "task.json"), task);
    admitted.push(task); room--;
  }
  return { admitted, candidates: opportunities(evidence.report, evidence.signals).length,
    manualReview: (evidence.signals?.leads || []).filter(lead => !lead.route || !eligiblePiece(lead.route)).length,
    coverage: evidence.signals?.coverage || [], status: admitted.length ? "queued" : "idle" };
}
