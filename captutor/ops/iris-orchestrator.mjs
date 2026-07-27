#!/usr/bin/env node
// iris-orchestrator — makes iris self-driving, safely. Polls her ASSIGNED Asana
// tasks and works them ONE AT A TIME. Code tasks spawn a worker in an isolated
// worktree and end in a PR. Tasks tagged BOTH `mission` and `captutor` run in the
// fixed Captutor workspace and end in a verified Desktop/outbox artifact.
// The orchestrator keeps her mission board as the live work-list
// (in-progress + queued), and posts status to Slack. Extra assignments queue and
// wait. Gated by the ownership guardrail: only ASSIGNED tasks are worked, and if
// the active task is unassigned mid-flight she stands down.
//
// This is the ORCHESTRATION layer — the queue/lock/status/mission. Whether an
// individual worker nails a given task is the worker's job; this guarantees only
// one runs, the human is kept informed, and nothing unassigned is touched.
//
// Dependency-free: node https + child_process (flk, gh). Persistent via launchd.

import { execFileSync, spawn } from "node:child_process";
import https from "node:https";
import { readFileSync, writeFileSync, existsSync, mkdirSync, readdirSync } from "node:fs";
import { homedir } from "node:os";
import { join } from "node:path";

const H = homedir();
const DIR = join(H, ".hermes");
const STATE = join(DIR, "orchestrator-state.json");
const MISSION_FILE = join(H, ".local", "share", "desktop-badge", "mission.json");
const MANUAL_MISSION_FILE = join(H, ".hermes", "manual-mission.json");
const LOG = join(DIR, "logs", "orchestrator.log");

const ASANA = process.env.ASANA_ACCESS_TOKEN || "";
const SLACK = process.env.SLACK_BOT_TOKEN || "";
const USER_GID = process.env.ASANA_USER_GID || "1216250551404992";
const WS_GID = process.env.ASANA_WORKSPACE_GID || "1208084256731239";
const HOME_CHANNEL = process.env.ORCH_CHANNEL || "D0BFUT0D4SF"; // iris<->jeffrey DM
const FUSER = process.env.FUSER_REPO || join(H, "Developer", "fuser");
const CAPTUTOR = process.env.CAPTUTOR_HOME || join(H, "Developer", "captutor");
const OUTBOX = process.env.CAPTUTOR_OUTBOX || join(H, "Desktop", "outbox");
const DESK_CLEANUP = process.env.IRIS_DESK_CLEANUP || join(H, ".local", "bin", "iris-desk-cleanup");
const WORKER_RUN = join(DIR, "bin", "worker-run.sh");
const WORKTREES = join(DIR, "worktrees");
const GH_ENV = { ...process.env, GH_CONFIG_DIR: join(H, ".config", "gh-iris") };
const BASE_REF = process.env.ORCH_BASE_REF || "origin/staging";
const POLL_MS = parseInt(process.env.ORCH_POLL_MS || "60000", 10);
const STALL_MS = parseInt(process.env.ORCH_STALL_MS || String(20 * 60000), 10);
// Routine "starting" / "still working" chatter is intentionally quiet by
// default. Completion, review, stand-down, and failure notices still go to
// Slack. Set ORCH_SLACK_PROGRESS=true when live progress narration is useful.
const SLACK_PROGRESS = process.env.ORCH_SLACK_PROGRESS === "true";
// Review loop: iris watches her own open PRs for teammate feedback (reviews live
// on GitHub, not Asana — the orchestrator never saw them). Ingestion + Slack
// notify is ALWAYS on. Auto-addressing (re-launch a worker on the PR branch to
// fix the feedback) is gated behind this flag so we verify ingestion first.
const REVIEW_AUTOFIX = process.env.ORCH_REVIEW_AUTOFIX === "true";
const REPO = process.env.FUSER_GH_REPO || "fuserstudio/fuser";
const IRIS_LOGIN = process.env.IRIS_GH_LOGIN || "iris-fuser";

function log(m) {
  const line = new Date().toISOString() + " " + m + "\n";
  try { if (!existsSync(join(DIR, "logs"))) mkdirSync(join(DIR, "logs"), { recursive: true }); } catch {}
  try { writeFileSync(LOG, line, { flag: "a" }); } catch {}
  process.stdout.write(line);
}

function httpsJson({ hostname, path, method = "GET", headers = {}, body = null }) {
  return new Promise((resolve, reject) => {
    const data = body ? JSON.stringify(body) : null;
    const req = https.request(
      { hostname, path, method, headers: { ...headers, ...(data ? { "Content-Type": "application/json", "Content-Length": Buffer.byteLength(data) } : {}) }, timeout: 15000 },
      (res) => { let b = ""; res.on("data", (c) => (b += c)); res.on("end", () => { try { resolve(JSON.parse(b)); } catch (e) { reject(e); } }); },
    );
    req.on("error", reject);
    req.on("timeout", () => req.destroy(new Error("timeout")));
    if (data) req.write(data);
    req.end();
  });
}

async function assignedTasks() {
  const fields = "name,notes,completed,permalink_url,tags.name";
  const j = await httpsJson({
    hostname: "app.asana.com",
    path: `/api/1.0/tasks?assignee=${USER_GID}&workspace=${WS_GID}&completed_since=now&opt_fields=${fields}&limit=100`,
    headers: { Authorization: "Bearer " + ASANA },
  });
  if (j.errors) throw new Error(j.errors[0]?.message || "asana error");
  return (j.data || []).filter((t) => !t.completed).map((t) => {
    const tags = (t.tags || []).map((tag) => String(tag.name || "").toLowerCase());
    return {
      gid: t.gid,
      name: t.name || "(untitled)",
      notes: t.notes || "",
      url: t.permalink_url || null,
      tags,
      kind: tags.includes("mission") && tags.includes("captutor") ? "captutor" : "pr",
    };
  });
}

async function asanaComment(gid, text) {
  try {
    const r = await httpsJson({
      hostname: "app.asana.com", path: `/api/1.0/tasks/${gid}/stories`, method: "POST",
      headers: { Authorization: "Bearer " + ASANA }, body: { data: { text } },
    });
    if (r.errors) log("asana comment failed: " + (r.errors[0]?.message || "?"));
  } catch (e) { log("asana comment err: " + e.message); }
}

async function asanaComplete(gid) {
  try {
    const r = await httpsJson({
      hostname: "app.asana.com", path: `/api/1.0/tasks/${gid}`, method: "PUT",
      headers: { Authorization: "Bearer " + ASANA }, body: { data: { completed: true } },
    });
    if (r.errors) log("asana completion failed: " + (r.errors[0]?.message || "?"));
  } catch (e) { log("asana completion err: " + e.message); }
}

async function slack(text) {
  if (!SLACK) { log("no SLACK_BOT_TOKEN, skip: " + text); return; }
  try {
    const r = await httpsJson({ hostname: "slack.com", path: "/api/chat.postMessage", method: "POST",
      headers: { Authorization: "Bearer " + SLACK }, body: { channel: HOME_CHANNEL, text, unfurl_links: false } });
    if (!r.ok) log("slack failed: " + (r.error || "?"));
  } catch (e) { log("slack err: " + e.message); }
}

async function progressSlack(text) {
  if (!SLACK_PROGRESS) { log("slack progress suppressed: " + text); return; }
  await slack(text);
}

function git(args, opts = {}) {
  return execFileSync("git", args, { cwd: FUSER, encoding: "utf8", timeout: 60000, ...opts }).trim();
}
function slug(s) { return String(s).toLowerCase().replace(/[^a-z0-9]+/g, "-").replace(/^-|-$/g, "").slice(0, 24) || "task"; }

// Create a fresh isolated worktree off the latest base tip (idempotent).
function makeWorktree(wt, branch) {
  try { git(["worktree", "remove", "--force", wt]); } catch {}
  try { git(["branch", "-D", branch]); } catch {}
  git(["fetch", "origin", "staging", "--quiet"]);
  git(["worktree", "add", "-b", branch, wt, BASE_REF]);
}
function removeWorktree(wt, branch) {
  try { git(["worktree", "remove", "--force", wt]); } catch {}
  try { git(["branch", "-D", branch]); } catch {}
}

// Launch the headless worker DETACHED (the daemon owns it, not this tick).
function launchWorker(wt, log, prompt, promptFile, extraEnv = {}) {
  writeFileSync(promptFile, prompt);
  const child = spawn("bash", [WORKER_RUN, wt, log, promptFile], {
    cwd: wt, detached: true, stdio: "ignore", env: { ...process.env, ...extraEnv },
  });
  child.unref();
  return child.pid;
}
// The headless run appended its exit marker → the worker has finished.
function workerFinished(log) {
  try { return /=== worker-run exit /.test(readFileSync(log, "utf8")); } catch { return false; }
}

function workerExit(log) {
  try {
    const matches = [...readFileSync(log, "utf8").matchAll(/=== worker-run exit (\d+) /g)];
    return matches.length ? Number(matches.at(-1)[1]) : null;
  } catch { return null; }
}

function captutorDeliveries(taskGid, startedAt = 0) {
  const found = [];
  try {
    for (const name of readdirSync(OUTBOX).filter((f) => f.endsWith(".json")).sort().reverse()) {
      const path = join(OUTBOX, name);
      let doc;
      try { doc = JSON.parse(readFileSync(path, "utf8")); } catch { continue; }
      if (doc.schema !== "captutor-outbox/v1" || doc.status !== "complete" || doc.taskGid !== taskGid) continue;
      if (startedAt && Date.parse(doc.createdAt || 0) < startedAt) continue;
      const video = join(OUTBOX, doc.video || "");
      const captions = join(OUTBOX, doc.captions || "");
      if (existsSync(video) && existsSync(captions)) {
        found.push({ manifest: path, video, captions, metadata: doc });
      }
    }
  } catch {}
  return found;
}

function captutorExpectedFormats(notes) {
  const hit = String(notes || "").match(/^CAPTUTOR_FORMATS:\s*([^\n]+)$/mi);
  return hit ? hit[1].split(",").map((value) => value.trim()).filter(Boolean) : [];
}

function processAlive(pid) {
  if (!pid) return false;
  try { process.kill(pid, 0); return true; } catch { return false; }
}

function releaseActiveWorkspace(active) {
  if (active?.wt && active?.branch) removeWorktree(active.wt, active.branch);
}

function stopActiveWorker(active) {
  if (!active?.pid) return;
  try { process.kill(-active.pid, "SIGTERM"); } catch {}
}

function prForBranch(branch) {
  try {
    const out = execFileSync("gh", ["pr", "list", "--repo", "fuserstudio/fuser", "--head", branch, "--json", "url,state", "--limit", "1"],
      { cwd: FUSER, encoding: "utf8", timeout: 20000, env: GH_ENV }).trim();
    const arr = JSON.parse(out || "[]");
    return arr[0] || null;
  } catch { return null; }
}

// Worktree checked out on an EXISTING PR branch (for addressing review feedback,
// vs makeWorktree which cuts a fresh branch off BASE_REF).
function makeWorktreeOnBranch(wt, branch) {
  try { git(["worktree", "remove", "--force", wt]); } catch {}
  git(["fetch", "origin", branch, "--quiet"]);
  git(["worktree", "add", "--force", "-B", branch, wt, `origin/${branch}`]);
}

// ── GitHub review ingestion (read-only; runs gh under iris's gh-iris config) ──
function gh(args) {
  try { return execFileSync("gh", args, { cwd: FUSER, encoding: "utf8", timeout: 30000, env: GH_ENV }).trim(); }
  catch (e) { log("gh err [" + args.join(" ") + "]: " + (e.stderr || e.message)); return null; }
}
function ghApi(path) {
  const out = gh(["api", "-H", "Accept: application/vnd.github+json", path + (path.includes("?") ? "&" : "?") + "per_page=100"]);
  try { return out ? JSON.parse(out) : []; } catch { return []; }
}
function irisOpenPRs() {
  const out = gh(["pr", "list", "--repo", REPO, "--author", IRIS_LOGIN, "--state", "open",
    "--json", "number,url,title,headRefName,reviewDecision", "--limit", "50"]);
  try { return out ? JSON.parse(out) : []; } catch { return []; }
}
// New teammate feedback on a PR since the recorded high-water marks. Ignores
// iris's own comments. Returns { items:[...], hwm:{comment,review,issue} }.
function prFeedback(number, hwm) {
  const items = [];
  const nhwm = { comment: hwm.comment || 0, review: hwm.review || 0, issue: hwm.issue || 0 };
  for (const c of ghApi(`repos/${REPO}/pulls/${number}/comments`)) {
    if (c.user?.login === IRIS_LOGIN) continue;
    if (c.id > (hwm.comment || 0)) items.push({ kind: "comment", id: c.id, author: c.user?.login || "?", body: c.body || "", loc: `${c.path}:${c.line ?? c.original_line ?? "?"}` });
    nhwm.comment = Math.max(nhwm.comment, c.id);
  }
  for (const r of ghApi(`repos/${REPO}/pulls/${number}/reviews`)) {
    if (r.user?.login === IRIS_LOGIN || !r.state || r.state === "PENDING") continue;
    if (r.id > (hwm.review || 0)) items.push({ kind: "review", id: r.id, author: r.user?.login || "?", state: r.state, body: r.body || `(${r.state})` });
    nhwm.review = Math.max(nhwm.review, r.id);
  }
  for (const c of ghApi(`repos/${REPO}/issues/${number}/comments`)) {
    if (c.user?.login === IRIS_LOGIN) continue;
    if (c.id > (hwm.issue || 0)) items.push({ kind: "issue", id: c.id, author: c.user?.login || "?", body: c.body || "" });
    nhwm.issue = Math.max(nhwm.issue, c.id);
  }
  return { items, hwm: nhwm };
}

function loadState() {
  try {
    const s = JSON.parse(readFileSync(STATE, "utf8"));
    if (!s.reviews) s.reviews = {};
    if (!s.recoveries) s.recoveries = {};
    return s;
  } catch { return { active: null, done: {}, reviews: {}, recoveries: {} }; }
}
function saveState(s) { writeFileSync(STATE, JSON.stringify(s, null, 2) + "\n"); }

function writeMission(active, tasks, done) {
  let manual = null;
  try { manual = JSON.parse(readFileSync(MANUAL_MISSION_FILE, "utf8")); } catch {}
  // Only heartbeat.sh advances lastHeartbeat. Orchestrator ticks still refresh
  // updatedAt/content, but must not impersonate a beat or the five-minute
  // countdown will reset every minute and MacPal will animate false pulses.
  let previousHeartbeat = null;
  try { previousHeartbeat = JSON.parse(readFileSync(MISSION_FILE, "utf8")).lastHeartbeat || null; } catch {}
  const now = new Date().toISOString();
  const lastHeartbeat = previousHeartbeat || now;
  const items = [];
  if (active) items.push({ text: `${active.name}`, status: "in_progress" });
  for (const t of tasks) if (!done[t.gid] && (!active || t.gid !== active.taskGid)) items.push({ text: t.name, status: "pending" });
  // Completed work stays in orchestrator state for audit/deduplication, but an
  // idle mission should actually look clear instead of carrying old trophies.
  const remaining = tasks.filter((t) => !done[t.gid]);
  const recent = active || tasks.length ? Object.values(done).slice(-2) : [];
  for (const d of recent) items.push({
    text: `${d.name} (${d.status === "failed" ? "needs attention" : d.kind === "captutor" ? "rendered" : "shipped"})`,
    status: d.status === "failed" ? "pending" : "done",
  });
  const useManual = !active && tasks.length === 0 && manual?.mission;
  const doc = useManual ? {
    ...manual,
    agent: "iris",
    updatedAt:now,
    lastHeartbeat,
    heartbeatIntervalSeconds:300,
  } : {
    mission: active?.kind === "captutor" ? "rendering a product demo"
      : active ? "working PRs (one at a time)"
      : recent.some((d) => d.status === "failed") ? "mission needs attention"
      : remaining.some((t) => t.kind === "captutor") ? "product demos queued"
      : remaining.length ? "queued PRs" : "idle — no assigned tasks",
    emoji:"🪽", agent:"iris", updatedAt:now,
    items:items.slice(0, 8), lastHeartbeat, heartbeatIntervalSeconds:300,
  };
  try { if (!existsSync(join(H, ".local", "share", "desktop-badge"))) mkdirSync(join(H, ".local", "share", "desktop-badge"), { recursive: true }); } catch {}
  writeFileSync(MISSION_FILE, JSON.stringify(doc, null, 2) + "\n");
}

// Scan iris's open PRs for new teammate feedback. Always notifies (Slack). If
// REVIEW_AUTOFIX and changes were requested AND iris is still the Asana assignee
// of the PR's task, claim the active slot and launch a worker on a worktree of
// the EXISTING branch to address it. Returns true if it claimed the slot.
async function reviewPass(s, assigned) {
  for (const pr of irisOpenPRs()) {
    const rec = s.reviews[pr.number] || (s.reviews[pr.number] = { branch: pr.headRefName, hwm: {}, seeded: false });
    rec.branch = pr.headRefName;
    const { items, hwm } = prFeedback(pr.number, rec.hwm);
    rec.hwm = hwm; // advance always so we never re-alert on the same comments

    // First sight of a PR: seed high-water marks silently — no backlog alerts.
    if (!rec.seeded) { rec.seeded = true; saveState(s); continue; }
    if (!items.length) { saveState(s); continue; }

    const changesRequested = pr.reviewDecision === "CHANGES_REQUESTED" || items.some((i) => i.state === "CHANGES_REQUESTED");
    const line = (i) => `• ${i.author}${i.loc ? ` (${i.loc})` : ""}: ${(i.body || "").split("\n")[0].slice(0, 140)}`;
    await slack(`💬 new review on *${pr.title}* (${pr.url})${changesRequested ? " — changes requested" : ""}:\n${items.slice(0, 4).map(line).join("\n")}`);
    saveState(s);

    if (!(REVIEW_AUTOFIX && changesRequested) || s.active) continue;

    // Ownership hard gate: only auto-address if iris is the CURRENT assignee of
    // this PR's Asana task (mapped via what we recorded when we opened it).
    const taskGid = Object.keys(s.done).find((g) => (s.done[g].pr || "").includes(`/pull/${pr.number}`)) || null;
    if (!taskGid || !assigned.has(taskGid)) {
      await slack(`↳ not auto-addressing #${pr.number} — I'm not the current assignee. flagging for a human.`);
      continue;
    }
    const name = `review-${pr.number}`;
    const wt = join(WORKTREES, name);
    const logf = join(DIR, "logs", `worker-${name}.log`);
    const promptFile = join(DIR, `worker-${name}.prompt`);
    const body = items.map((i) => `- ${i.author}${i.loc ? ` [${i.loc}]` : ""}: ${i.body}`).join("\n");
    const prompt =
      `A teammate left review feedback on your PR #${pr.number} (${pr.url}). Address it end to end.\n\n` +
      `You are in a fresh git worktree already checked out on the EXISTING PR branch ${pr.headRefName} (reset to origin/${pr.headRefName}). Do NOT start a new branch. Rebuild context first: git log, gh pr view ${pr.number} --repo ${REPO}.\n\n` +
      `Review feedback:\n${body}\n\n` +
      `Make the changes, commit only the relevant files, then update the PR:\n` +
      `  git push --force-with-lease origin ${pr.headRefName}\n` +
      `Then reply on the PR summarizing what changed:\n` +
      `  gh pr comment ${pr.number} --repo ${REPO} --body "<summary>"\n` +
      `If a comment is unclear or you disagree, ask on the PR instead of guessing. Print the PR URL when done.`;
    try {
      makeWorktreeOnBranch(wt, pr.headRefName);
      s.active = { taskGid, name: `review: ${pr.title}`, branch: pr.headRefName, wt, log: logf, startedAt: Date.now(), kind: "review", prNumber: pr.number };
      saveState(s);
      try { writeFileSync(logf, ""); } catch {}
      launchWorker(wt, logf, prompt, promptFile);
      await progressSlack(`🛠️ addressing the review on *${pr.title}* — on it now.`);
      log(`launched review worker for PR #${pr.number} (branch ${pr.headRefName})`);
      return true;
    } catch (e) { log("review spawn err: " + e.message); }
  }
  return false;
}

async function tick() {
  if (!ASANA) { log("no ASANA token"); return; }
  let tasks;
  try { tasks = await assignedTasks(); } catch (e) { log("asana poll err: " + e.message); return; }
  const assigned = new Set(tasks.map((t) => t.gid));
  const s = loadState();

  // Guardrail: active task unassigned → stand down. (Review-fix items carry the
  // taskGid they were gated on; a plain item with no taskGid is skipped here.)
  if (s.active && s.active.taskGid && !assigned.has(s.active.taskGid)) {
    log(`active task ${s.active.taskGid} unassigned → standing down`);
    stopActiveWorker(s.active);
    releaseActiveWorkspace(s.active);
    await slack(`🛑 stood down on *${s.active.name}* — it's no longer assigned to me.`);
    s.active = null; saveState(s);
  }

  // Progress the active work. For a review fix the PR already exists, so
  // completion is the worker finishing (not a PR appearing).
  if (s.active) {
    if (s.active.kind === "captutor") {
      const exit = workerExit(s.active.log);
      if (exit !== null) {
        const deliveries = exit === 0
          ? captutorDeliveries(s.active.taskGid, s.active.startedAt)
          : [];
        const formats = new Set(deliveries.map((delivery) => delivery.metadata.format));
        const missing = (s.active.expectedFormats || []).filter((format) => !formats.has(format));
        if (deliveries.length && !missing.length) {
          const artifactLines = deliveries
            .sort((a, b) => String(a.metadata.format).localeCompare(String(b.metadata.format)))
            .flatMap((delivery) => [
              `${delivery.metadata.format}: ${delivery.video}`,
              `manifest: ${delivery.manifest}`,
            ]);
          log(`captutor mission ${s.active.name} → ${deliveries.length} verified artifact(s)`);
          await asanaComment(s.active.taskGid,
            `iris rendered this product-demo mission and placed the verified artifacts in Panda's Desktop outbox:\n${artifactLines.join("\n")}`);
          await asanaComplete(s.active.taskGid);
          await slack(`🎬 rendered *${s.active.name}* → ${deliveries.map((delivery) => delivery.video).join(", ")}`);
          s.done[s.active.taskGid] = {
            name: s.active.name, kind: "captutor", status: "done",
            deliveries, at: Date.now(),
          };
          if (s.recovery?.taskGid === s.active.taskGid) {
            s.recovery = {
              ...s.recovery, status:"complete", updatedAt:Date.now(),
              activity:`Recovered mission completed with ${deliveries.length} verified artifact(s).`,
            };
          }
        } else {
          const detail = missing.length ? `; missing formats: ${missing.join(", ")}` : "";
          const reason = exit === 0 ? "missing-outbox-artifacts" : "worker-exit";
          const failureDetail = exit === 0
            ? `worker exited 0 but produced ${deliveries.length} verified artifact(s)${detail}`
            : `worker exited ${exit} before verified delivery${detail}`;
          log(`captutor mission ${s.active.name} exited ${exit} without every verified outbox artifact${detail}`);
          await asanaComment(s.active.taskGid,
            `iris could not complete this Captutor mission (worker exit ${exit}; ${deliveries.length} verified artifact(s)${detail}). Log: ${s.active.log}`);
          await slack(`⚠️ Captutor mission *${s.active.name}* needs attention — not every requested video reached the outbox.`);
          s.done[s.active.taskGid] = {
            name:s.active.name, kind:"captutor", status:"failed",
            reason, detail:failureDetail, exitCode:exit,
            verifiedArtifacts:deliveries.length, missingFormats:missing,
            log:s.active.log, at:Date.now(),
          };
          if (s.recovery?.taskGid === s.active.taskGid) {
            s.recovery = {
              ...s.recovery, status:"failed", reason, detail:failureDetail,
              updatedAt:Date.now(), activity:`Retry stopped: ${failureDetail}.`,
            };
          }
        }
        s.active = null; saveState(s);
      } else if (Date.now() - s.active.startedAt > 2 * 60000 && !processAlive(s.active.pid)) {
        log(`captutor mission ${s.active.name} lost worker ${s.active.pid || "?"} without an exit marker`);
        await asanaComment(s.active.taskGid,
          `iris's Captutor worker disappeared before writing an exit marker. The task remains incomplete for retry. Log: ${s.active.log}`);
        s.done[s.active.taskGid] = {
          name:s.active.name, kind:"captutor", status:"failed",
          reason:"worker-disappeared",
          detail:`worker ${s.active.pid || "?"} stopped before writing an exit marker`,
          log:s.active.log, at:Date.now(),
        };
        if (s.recovery?.taskGid === s.active.taskGid) {
          s.recovery = {
            ...s.recovery, status:"failed", reason:"worker-disappeared",
            detail:`worker ${s.active.pid || "?"} stopped before writing an exit marker`,
            updatedAt:Date.now(), activity:"Retry worker disappeared before verified delivery.",
          };
        }
        s.active = null; saveState(s);
      } else if (Date.now() - s.active.startedAt > STALL_MS && !s.active.stallNoted) {
        await progressSlack(`⏳ still rendering *${s.active.name}* (${Math.round((Date.now() - s.active.startedAt) / 60000)}m).`);
        s.active.stallNoted = true; saveState(s);
      }
    } else if (s.active.kind === "review") {
      const label = s.active.name.replace(/^review: /, "");
      if (workerFinished(s.active.log)) {
        log(`review worker for #${s.active.prNumber} finished`);
        await slack(`✅ pushed updates for the review on *${label}*${s.active.prNumber ? ` (#${s.active.prNumber})` : ""}.`);
        if (s.active.prNumber && s.reviews[s.active.prNumber]) s.reviews[s.active.prNumber].addressedAt = Date.now();
        removeWorktree(s.active.wt, s.active.branch);
        s.active = null; saveState(s);
      } else if (Date.now() - s.active.startedAt > STALL_MS && !s.active.stallNoted) {
        await progressSlack(`⏳ still addressing the review on *${label}* (${Math.round((Date.now() - s.active.startedAt) / 60000)}m).`);
        s.active.stallNoted = true; saveState(s);
      }
    } else {
      const pr = prForBranch(s.active.branch);
      if (pr) {
        log(`active ${s.active.name} → PR ${pr.url}`);
        await asanaComment(s.active.taskGid, `iris opened a PR for this task: ${pr.url} — ready for review.`);
        await slack(`✅ opened a PR for *${s.active.name}*: ${pr.url}`);
        s.done[s.active.taskGid] = { name: s.active.name, branch: s.active.branch, pr: pr.url, at: Date.now() };
        removeWorktree(s.active.wt, s.active.branch);
        s.active = null; saveState(s);
      } else if (workerFinished(s.active.log)) {
        log(`worker for ${s.active.name} finished without a PR → releasing`);
        await slack(`⚠️ my worker on *${s.active.name}* finished without opening a PR — I'll need a retry (log: ${s.active.log}).`);
        removeWorktree(s.active.wt, s.active.branch);
        s.active = null; saveState(s);
      } else if (Date.now() - s.active.startedAt > STALL_MS) {
        if (!s.active.stallNoted) { await progressSlack(`⏳ still working *${s.active.name}* (${Math.round((Date.now() - s.active.startedAt) / 60000)}m) — flagging in case it's stuck.`); s.active.stallNoted = true; saveState(s); }
      }
    }
  }

  // Watch iris's open PRs for teammate feedback and (gated) address it before
  // starting new work — a requested change on an in-flight PR outranks a fresh
  // task. Notify-only until ORCH_REVIEW_AUTOFIX is on.
  if (!s.active) { try { await reviewPass(s, assigned); } catch (e) { log("reviewPass err: " + e.message); } }

  // Pick up the next assigned task (one at a time).
  if (!s.active) {
    const next = tasks.find((t) => !s.done[t.gid]);
    if (next) {
      const name = `${slug(next.name)}-${next.gid.slice(-6)}`;
      const logf = join(DIR, "logs", `worker-${name}.log`);
      const promptFile = join(DIR, `worker-${name}.prompt`);
      if (next.kind === "captutor") {
        const expectedFormats = captutorExpectedFormats(next.notes);
        const prompt =
          `Execute this assigned Captutor product-demo mission end to end on Panda. DO NOT edit the Fuser repo, create a branch, commit, push, or open a PR.\n\n` +
          `Task: ${next.name}\n${next.notes ? next.notes + "\n" : ""}${next.url ? "Asana: " + next.url + "\n" : ""}\n` +
          `You are already in the current Captutor workspace. Read README.md first. Verify the recorder, GUI Chrome/CDP session, login, and credits as applicable. ` +
          `For UI pathfinding, use the bounded internal frame (\`CDP_PORT=9333 node bin/cdp-frame.mjs --match fuser.studio\`, optionally with \`--screenshot /tmp/preflight.png\`). ` +
          `It returns controls plus React Flow nodes/handles/edges without visible tooling and closes CDP cleanly; do not write ad-hoc attach scripts or repeatedly map unrelated UI. ` +
          `Keep the filmed interaction on Fuser's canvas. Treat the right-side node properties inspector as off-camera setup only: close it before Reel and do not open it during a take unless the task explicitly teaches that inspector. ` +
          `Before Stage, verify System Events Accessibility and the SlabMenubar recording bridge. If macOS presents an in-scope System Settings, Accessibility, Automation, or Screen Recording permission prompt, approve it off camera and verify the grant before continuing; never film a permission dialog. ` +
          `Run Captutor in the FOREGROUND. Invoke the Stage render as a direct foreground child of this worker with a long enough shell timeout. Do not use Monitor, a subagent, a background job, nohup, or a scheduled check-in for the render; remain attached until the command exits and the required outbox files have been verified. ` +
          `Pathfind on the ordinary desktop, but perform every actual take in Captutor's true 2x HiDPI Stage. Render with \`node bin/stage.mjs render <screenplay> --outbox "$CAPTUTOR_OUTBOX"\`; never invoke \`captutor.mjs render\` directly for a mission. ` +
          `The environment already sets CAPTUTOR_TASK_GID and CAPTUTOR_REQUIRE_HIDPI for this task, and Captutor will refuse to start Reel unless Stage and its real 2x display geometry are active. ` +
          `Success means Captutor writes a complete captutor-outbox/v1 manifest plus its MP4 and VTT for every requested format to the outbox. ` +
          `Do not substitute an old render. If blocked, explain the exact blocker and exit nonzero.`;
        try {
          if (!existsSync(CAPTUTOR)) throw new Error(`missing Captutor workspace: ${CAPTUTOR}`);
          if (!existsSync(DESK_CLEANUP)) throw new Error(`missing pre-mission setup: ${DESK_CLEANUP}`);
          log(`running pre-mission setup: ${DESK_CLEANUP}`);
          execFileSync(DESK_CLEANUP, [], { encoding: "utf8", timeout: 120000 });
          mkdirSync(OUTBOX, { recursive: true });
          s.active = {
            taskGid: next.gid, name: next.name, kind: "captutor", cwd: CAPTUTOR,
            log: logf, startedAt: Date.now(), expectedFormats,
            recoveryAttempt:s.recoveries?.[next.gid]?.attempts || 0,
          };
          if (s.recovery?.taskGid === next.gid) {
            s.recovery = {
              ...s.recovery, status:"relaunching", updatedAt:Date.now(),
              activity:`Retry ${s.recovery.attempts}/${s.recovery.maximum} is starting in the foreground.`,
            };
          }
          saveState(s);
          try { writeFileSync(logf, ""); } catch {}
          s.active.pid = launchWorker(CAPTUTOR, logf, prompt, promptFile, {
            CAPTUTOR_TASK_GID: next.gid,
            CAPTUTOR_OUTBOX: OUTBOX,
            CAPTUTOR_REQUIRE_HIDPI: "1",
          });
          saveState(s);
          log(`launched Captutor mission for ${next.name}`);
          await progressSlack(`🎬 starting product-demo mission *${next.name}*${next.url ? " (" + next.url + ")" : ""}.`);
        } catch (e) { log("captutor mission spawn err: " + e.message); }
        writeMission(s.active, tasks, s.done);
        return;
      }

      const branch = `iris/${name}`;
      const wt = join(WORKTREES, name);
      const prompt =
        `Do this assigned task end to end, then OPEN A PULL REQUEST.\n\n` +
        `Task: ${next.name}\n${next.notes ? next.notes + "\n" : ""}${next.url ? "Asana: " + next.url + "\n" : ""}\n` +
        `You are in a fresh git worktree on branch ${branch} off ${BASE_REF}. Make the change, ` +
        `commit only the relevant files, push the branch to origin, then open a PR:\n` +
        `  gh pr create --repo fuserstudio/fuser --base staging --head ${branch} --title "<concise title>" --body "<what + why>"\n` +
        `Print the PR URL when done.`;
      try {
        makeWorktree(wt, branch);
        // Claim + persist the lock BEFORE launching / any async, so a crash or
        // re-tick can never re-pick this task or drop the one-at-a-time lock.
        s.active = { taskGid: next.gid, name: next.name, branch, wt, log: logf, startedAt: Date.now() };
        saveState(s);
        try { writeFileSync(logf, ""); } catch {}
        launchWorker(wt, logf, prompt, promptFile);
        log(`launched worker for ${next.name} (branch ${branch})`);
        const queued = tasks.filter((t) => !s.done[t.gid] && t.gid !== next.gid).length;
        await progressSlack(`🛠️ starting on *${next.name}*${next.url ? " (" + next.url + ")" : ""} — one PR at a time${queued ? `; ${queued} more queued` : ""}.`);
      } catch (e) { log("spawn err: " + e.message); }
    }
  }

  writeMission(s.active, tasks, s.done);
}

log(`iris-orchestrator starting — poll ${POLL_MS}ms, base ${BASE_REF}, review ${REVIEW_AUTOFIX ? "auto-fix" : "notify-only"}, slack progress ${SLACK_PROGRESS ? "on" : "off"}`);
await tick();
setInterval(() => tick().catch((e) => log("tick crash: " + e.message)), POLL_MS);
