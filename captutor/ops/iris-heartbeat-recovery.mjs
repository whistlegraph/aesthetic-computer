#!/usr/bin/env node
// Active half of Iris's heartbeat. The shell heartbeat still publishes a
// liveness pulse; this supervisor repairs one recoverable Captutor stoppage.
// Ownership remains with the orchestrator: requeueing only removes its local
// failure tombstone, and the task will relaunch only if Asana still assigns it
// to Iris.

import { execFileSync } from "node:child_process";
import https from "node:https";
import {
  existsSync, mkdirSync, readFileSync, renameSync, rmSync, writeFileSync,
} from "node:fs";
import { homedir } from "node:os";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";

const home = homedir();
const statePath = process.env.IRIS_ORCHESTRATOR_STATE
  || join(home, ".hermes", "orchestrator-state.json");
const logPath = process.env.IRIS_HEARTBEAT_LOG
  || join(home, ".hermes", "logs", "heartbeat.log");
const lockPath = process.env.IRIS_RECOVERY_LOCK
  || join(home, ".hermes", "iris-heartbeat-recovery.lock");
const captutor = process.env.CAPTUTOR_HOME || join(home, "Developer", "captutor");
const node = process.env.CAPTUTOR_NODE || "/opt/homebrew/bin/node";
const maxRetries = Number(process.env.IRIS_CAPTUTOR_RECOVERY_RETRIES || 1);
const reloadWaitMs = Number(process.env.IRIS_BROWSER_RELOAD_WAIT_MS || 5_000);
const asanaToken = process.env.ASANA_ACCESS_TOKEN || "";
const irisUserGid = process.env.ASANA_USER_GID || "1216250551404992";

const delay = (ms) => new Promise((resolve) => setTimeout(resolve, ms));

function readJson(path) {
  try { return JSON.parse(readFileSync(path, "utf8")); }
  catch { return null; }
}

function atomicWriteJson(path, value) {
  mkdirSync(dirname(path), { recursive:true });
  const temp = `${path}.${process.pid}.tmp`;
  writeFileSync(temp, `${JSON.stringify(value, null, 2)}\n`, { mode:0o600 });
  renameSync(temp, path);
}

function log(message) {
  mkdirSync(dirname(logPath), { recursive:true });
  writeFileSync(logPath, `${new Date().toISOString()} ♥ recovery · ${message}\n`, { flag:"a" });
}

function processAlive(pid) {
  if (!pid) return false;
  try { process.kill(Number(pid), 0); return true; }
  catch { return false; }
}

export function latestRecoverableFailure(state) {
  if (state?.active) return null;
  return Object.entries(state?.done || {})
    .filter(([, record]) => record?.kind === "captutor" && record.status === "failed")
    .map(([taskGid, record]) => ({ taskGid, ...record }))
    .sort((a, b) => Number(b.at || 0) - Number(a.at || 0))[0] || null;
}

export function recoveryAttempts(state, taskGid) {
  return Number(state?.recoveries?.[taskGid]?.attempts || 0);
}

export function markStoppedWorkerFailed(state, now = Date.now()) {
  const active = state?.active;
  if (!active || active.kind !== "captutor" || processAlive(active.pid)) return null;
  if (now - Number(active.startedAt || now) < 2 * 60_000) return null;
  state.done ||= {};
  state.done[active.taskGid] = {
    name:active.name,
    kind:"captutor",
    status:"failed",
    reason:"worker-disappeared",
    detail:`worker ${active.pid || "?"} stopped before verified outbox delivery`,
    log:active.log,
    at:now,
  };
  state.active = null;
  return state.done[active.taskGid];
}

export function beginRecovery(state, failure, now = Date.now(), maximum = maxRetries) {
  state.recoveries ||= {};
  const previous = state.recoveries[failure.taskGid] || {};
  const attempts = Number(previous.attempts || 0);
  if (attempts >= maximum) {
    state.recovery = {
      taskGid:failure.taskGid, mission:failure.name, status:"exhausted",
      attempts, maximum, reason:failure.reason || "missing-outbox-artifacts",
      detail:failure.detail || "No verified Captutor outbox delivery.", updatedAt:now,
    };
    return false;
  }
  state.recovery = {
    taskGid:failure.taskGid, mission:failure.name, status:"checking-browser",
    attempts, maximum, reason:failure.reason || "missing-outbox-artifacts",
    detail:failure.detail || "No verified Captutor outbox delivery.", updatedAt:now,
  };
  return true;
}

export function queueRecovery(state, failure, now = Date.now()) {
  state.recoveries ||= {};
  const attempts = recoveryAttempts(state, failure.taskGid) + 1;
  state.recoveries[failure.taskGid] = {
    attempts, lastAttemptAt:now, reason:failure.reason || "missing-outbox-artifacts",
  };
  state.recovery = {
    ...state.recovery,
    taskGid:failure.taskGid,
    mission:failure.name,
    status:"queued",
    attempts,
    updatedAt:now,
    activity:`Browser healthy; retry ${attempts}/${state.recovery?.maximum || maxRetries} queued.`,
  };
  delete state.done[failure.taskGid];
  return state.recovery;
}

export function validateAssignedCaptutorTask(task, taskGid, userGid = irisUserGid) {
  if (!task || String(task.gid) !== String(taskGid)) {
    throw new Error(`Asana did not return task ${taskGid}`);
  }
  if (task.completed) throw new Error(`task ${taskGid} is already complete`);
  if (String(task.assignee?.gid || "") !== String(userGid)) {
    throw new Error(`task ${taskGid} is not assigned to Iris`);
  }
  const tags = new Set((task.tags || []).map((tag) => String(tag.name || "").toLowerCase()));
  if (!tags.has("mission") || !tags.has("captutor")) {
    throw new Error(`task ${taskGid} is not a Captutor mission`);
  }
  return task;
}

export function rearmRecovery(state, taskGid, now = Date.now()) {
  if (state?.active) {
    throw new Error(`cannot rearm while ${state.active.name || state.active.taskGid || "a worker"} is active`);
  }
  const failure = state?.done?.[taskGid];
  if (!failure || failure.kind !== "captutor" || failure.status !== "failed") {
    throw new Error(`task ${taskGid} has no failed Captutor tombstone to rearm`);
  }
  state.recoveries ||= {};
  const previous = state.recoveries[taskGid] || {};
  const manualRearms = Number(previous.manualRearms || 0) + 1;
  state.recoveries[taskGid] = {
    ...previous,
    manualRearms,
    lastManualRearmAt:now,
    lastManualRearmReason:failure.reason || "missing-outbox-artifacts",
  };
  state.recovery = {
    taskGid,
    mission:failure.name,
    status:"manually-rearmed",
    attempts:Number(previous.attempts || 0),
    maximum:Number(state.recovery?.maximum || maxRetries),
    manualRearms,
    reason:failure.reason || "missing-outbox-artifacts",
    detail:failure.detail || "No verified Captutor outbox delivery.",
    updatedAt:now,
    activity:"Manual rearm verified Asana ownership and browser health; queued for the next orchestrator tick.",
  };
  delete state.done[taskGid];
  return state.recovery;
}

function asanaTask(taskGid) {
  if (!asanaToken) return Promise.reject(new Error("ASANA_ACCESS_TOKEN is required for manual rearm"));
  const fields = encodeURIComponent("name,completed,assignee.gid,tags.name");
  return new Promise((resolve, reject) => {
    const req = https.request({
      hostname:"app.asana.com",
      path:`/api/1.0/tasks/${encodeURIComponent(taskGid)}?opt_fields=${fields}`,
      headers:{ Authorization:`Bearer ${asanaToken}` },
      timeout:15_000,
    }, (res) => {
      let body = "";
      res.on("data", (chunk) => { body += chunk; });
      res.on("end", () => {
        let json;
        try { json = JSON.parse(body); }
        catch { reject(new Error(`Asana returned invalid JSON (${res.statusCode || "?"})`)); return; }
        if ((res.statusCode || 500) >= 400 || json.errors) {
          reject(new Error(json.errors?.[0]?.message || `Asana returned ${res.statusCode}`));
          return;
        }
        resolve(json.data);
      });
    });
    req.on("error", reject);
    req.on("timeout", () => req.destroy(new Error("Asana assignment check timed out")));
    req.end();
  });
}

function browserProbe() {
  try {
    execFileSync(node, [join(captutor, "bin", "cdp-frame.mjs"),
      "--match", "fuser.studio", "--compact", "--timeout", "10000"], {
      cwd:captutor,
      env:{ ...process.env, CDP_PORT:process.env.CDP_PORT || "9333", CAPTUTOR_CDP_EPHEMERAL:"1" },
      encoding:"utf8", timeout:12_000, stdio:["ignore", "pipe", "pipe"],
    });
    return { healthy:true, crash:false, message:"Fuser renderer is responsive." };
  } catch (error) {
    const output = `${error.stdout || ""}\n${error.stderr || ""}`;
    return {
      healthy:false,
      crash:/BROWSER_RENDERER_CRASH|Inspector\.targetCrashed/.test(output),
      message:output.trim().slice(-500) || error.message,
    };
  }
}

async function recoverBrowser(state) {
  let health = browserProbe();
  if (health.healthy) return health;
  if (!health.crash) return health;
  state.recovery.status = "reloading-browser";
  state.recovery.activity = "Renderer crashed; reloading the Fuser tab.";
  state.recovery.updatedAt = Date.now();
  atomicWriteJson(statePath, state);
  execFileSync("/usr/bin/osascript", ["-e",
    'tell application "Google Chrome" to reload active tab of front window'], {
    timeout:10_000, stdio:"ignore",
  });
  await delay(reloadWaitMs);
  health = browserProbe();
  return health;
}

export async function runRecovery() {
  let locked = false;
  try {
    mkdirSync(lockPath);
    locked = true;
  } catch {
    return { action:"busy" };
  }
  try {
    const state = readJson(statePath);
    if (!state) return { action:"no-state" };
    const stopped = markStoppedWorkerFailed(state);
    if (stopped) atomicWriteJson(statePath, state);
    const failure = latestRecoverableFailure(state);
    if (!failure) return { action:"none" };
    if (!beginRecovery(state, failure)) {
      atomicWriteJson(statePath, state);
      log(`${failure.name}: automatic retry exhausted`);
      return { action:"exhausted", failure };
    }
    atomicWriteJson(statePath, state);
    const health = await recoverBrowser(state);
    if (!health.healthy) {
      state.recovery.status = "blocked";
      state.recovery.activity = `Browser recovery failed: ${health.message}`;
      state.recovery.updatedAt = Date.now();
      atomicWriteJson(statePath, state);
      log(`${failure.name}: browser recovery blocked`);
      return { action:"blocked", failure, health };
    }
    const recovery = queueRecovery(state, failure);
    atomicWriteJson(statePath, state);
    log(`${failure.name}: ${recovery.activity}`);
    return { action:"queued", failure, health, recovery };
  } finally {
    if (locked && existsSync(lockPath)) rmSync(lockPath, { recursive:true, force:true });
  }
}

export async function runManualRearm(taskGid) {
  if (!/^\d+$/.test(String(taskGid || ""))) throw new Error("manual rearm requires an exact numeric Asana task gid");
  let locked = false;
  try {
    mkdirSync(lockPath);
    locked = true;
  } catch {
    throw new Error("recovery supervisor is busy");
  }
  try {
    let state = readJson(statePath);
    if (!state) throw new Error(`orchestrator state is unavailable: ${statePath}`);
    // Validate the local target before making a network request. The same
    // validation runs again after browser recovery to avoid overwriting a task
    // the orchestrator may have claimed while this command was checking.
    if (state.active) throw new Error(`cannot rearm while ${state.active.name || state.active.taskGid || "a worker"} is active`);
    const failure = state.done?.[taskGid];
    if (!failure || failure.kind !== "captutor" || failure.status !== "failed") {
      throw new Error(`task ${taskGid} has no failed Captutor tombstone to rearm`);
    }
    const task = validateAssignedCaptutorTask(await asanaTask(taskGid), taskGid);
    state.recovery = {
      taskGid,
      mission:failure.name,
      status:"manual-rearm-checking-browser",
      attempts:recoveryAttempts(state, taskGid),
      maximum:Number(state.recovery?.maximum || maxRetries),
      reason:failure.reason || "missing-outbox-artifacts",
      detail:failure.detail || "No verified Captutor outbox delivery.",
      updatedAt:Date.now(),
      activity:"Manual rearm verified Asana ownership; checking the Fuser renderer.",
    };
    atomicWriteJson(statePath, state);
    const health = await recoverBrowser(state);
    if (!health.healthy) {
      state.recovery.status = "manual-rearm-blocked";
      state.recovery.activity = `Manual rearm kept the failure tombstone: ${health.message}`;
      state.recovery.updatedAt = Date.now();
      atomicWriteJson(statePath, state);
      throw new Error(state.recovery.activity);
    }
    state = readJson(statePath);
    if (!state) throw new Error(`orchestrator state disappeared: ${statePath}`);
    const recovery = rearmRecovery(state, taskGid);
    atomicWriteJson(statePath, state);
    log(`${task.name}: ${recovery.activity}`);
    return { action:"manually-rearmed", task:{ gid:task.gid, name:task.name }, health, recovery };
  } finally {
    if (locked && existsSync(lockPath)) rmSync(lockPath, { recursive:true, force:true });
  }
}

const isMain = process.argv[1] && fileURLToPath(import.meta.url) === process.argv[1];
if (isMain) {
  try {
    const rearmIndex = process.argv.indexOf("--rearm");
    const result = rearmIndex >= 0
      ? await runManualRearm(process.argv[rearmIndex + 1])
      : await runRecovery();
    console.log(JSON.stringify(result));
  } catch (error) {
    console.error(JSON.stringify({ action:"error", error:error.message }));
    process.exitCode = 1;
  }
}
