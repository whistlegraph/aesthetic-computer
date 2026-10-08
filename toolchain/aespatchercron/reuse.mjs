import { readFile } from "node:fs/promises";
import { join } from "node:path";
import { git, hash, relativeFile } from "./io.mjs";

export const seeds = [
  ["piece-lifecycle", "system/public/aesthetic.computer/disks/blank.mjs", "function paint", "Start a piece from the existing boot/paint/act/sim/leave lifecycle; inspect the API before adding a wrapper."],
  ["numbers", "system/public/aesthetic.computer/lib/num.mjs", "export const p2", "Reuse point/vector and numeric operations; keep their mutation and range contracts."],
  ["idioms", "system/public/aesthetic.computer/lib/help.mjs", "export function choose", "Reuse choose, anyKey and other small helpers where their semantics match."],
  ["visit-policy", "system/public/aesthetic.computer/lib/visit-model.mjs", "export function visitScopeMatch", "Use the reviewed studio/client boundary, public properties, surfaces and actions."],
  ["account-paths", "system/public/aesthetic.computer/lib/account-activity-model.mjs", "piece_opened", "Account activity supplies bounded runtime sequences; keep account/session identities on the server."],
  ["piece-signals", "system/netlify/functions/piece-log.mjs", 'status: "error"', "piece-runs retains error payloads even when a later completion changes status; aggregate presence, never export raw payloads."],
  ["journey-transport", "toolchain/mcp/analytics-mcp.mjs", "async function journeyReport", "Reuse Lith's read-only SSH transport pattern; accounts mode itself exports identities and must not feed this worker."],
  ["visits-report", "toolchain/analytics/visits-report.mjs", "visitReportPipeline", "Network visit totals describe page loads and coarse landing surfaces; they cannot reconstruct cross-page paths."],
  ["opens-report", "toolchain/analytics/opens-report.mjs", 'collection("app-opens")', "Native opens are counts of installations and launches, not people; optional context rather than patch evidence."],
  ["daily-report", "toolchain/analytics/daily-report.mjs", "metrics-daily", "Daily rollups add aggregate product context; never sum them with raw visit counts."],
  ["posthog-policy", "shared/posthog-policy.mjs", "existing-lith-silo-only", "Keep operational payloads in Lith/Silo; no prompts, chats, contacts, fleet details or raw MCP in PostHog."],
  ["hand", "HAND.md", "a piece of this system should fit in one mind", "Prefer knowable leaves and idiom names; boundary guards and public infrastructure structure are justified, not automatic bloat."],
];

export async function verifyFinding(repo, entry, revision = "HEAD") {
  const file = relativeFile(entry.file);
  if (!/^[a-z0-9-]+$/.test(entry.id) || typeof entry.anchor !== "string" || !entry.anchor || entry.anchor.length > 300 ||
      typeof entry.finding !== "string" || entry.finding.length < 15 || entry.finding.length > 1000) throw new Error("Invalid reuse finding");
  const rev = await git(repo, "rev-parse", "--verify", `${revision}^{commit}`);
  const source = await git(repo, "show", `${rev}:${file}`);
  if (!source.includes(entry.anchor)) throw new Error(`Reuse anchor missing: ${entry.id}`);
  return { id: entry.id, file, anchor: entry.anchor, finding: entry.finding,
    revision: rev, sourceHash: hash(source), anchorLine: source.slice(0, source.indexOf(entry.anchor)).split("\n").length };
}

export async function seedMap(repo) {
  const entries = [];
  for (const [id, file, anchor, finding] of seeds) entries.push(await verifyFinding(repo, { id, file, anchor, finding }));
  return { format: "aespatcher.reuse.v1", entries };
}

export async function inspectMap(repo, map) {
  if (map.format !== "aespatcher.reuse.v1" || !Array.isArray(map.entries)) throw new Error("Invalid reuse map");
  const entries = [];
  for (const entry of map.entries) {
    let reason = null;
    try {
      const verified = await verifyFinding(repo, entry, entry.revision);
      if (verified.sourceHash !== entry.sourceHash) reason = "recorded source hash differs";
      const current = (await readFile(join(repo, relativeFile(entry.file)), "utf8")).trimEnd();
      if (hash(current) !== entry.sourceHash) reason = "source changed since verification";
    } catch { reason = "source or verified revision unavailable"; }
    entries.push({ ...entry, stale: reason !== null, reason });
  }
  return { format: map.format, entries };
}
