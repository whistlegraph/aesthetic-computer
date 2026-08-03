#!/usr/bin/env node

import { existsSync, mkdirSync, readFileSync, renameSync, statSync, writeFileSync } from "node:fs";
import { homedir } from "node:os";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";

const home = homedir();
const statePath = process.env.IRIS_ORCHESTRATOR_STATE
  || join(home, ".hermes", "orchestrator-state.json");
const logPath = process.env.IRIS_HEARTBEAT_LOG
  || join(home, ".hermes", "logs", "heartbeat.log");
const maximumBumps = Number(process.env.IRIS_BLOCKING_MISSION_HEARTBEAT_BUMPS || 3);
const stallMs = Number(process.env.IRIS_BLOCKING_MISSION_STALL_MS || 20 * 60_000);
const cooldownMs = Number(process.env.IRIS_BLOCKING_MISSION_BUMP_COOLDOWN_MS || 5 * 60_000);

function atomicWriteJson(path, value) {
  mkdirSync(dirname(path), { recursive:true });
  const temp = `${path}.${process.pid}.tmp`;
  writeFileSync(temp, `${JSON.stringify(value, null, 2)}\n`, { mode:0o600 });
  renameSync(temp, path);
}

function stopWorker(pid) {
  if (!pid) return;
  try { process.kill(-Number(pid), "SIGTERM"); } catch {}
}

export function bumpBlockingMission(state, now = Date.now(), options = {}) {
  const maximum = Number(options.maximumBumps ?? maximumBumps);
  const stall = Number(options.stallMs ?? stallMs);
  const cooldown = Number(options.cooldownMs ?? cooldownMs);
  const mission = state?.priorityMission;
  if (!mission || mission.priority !== "blocking" || mission.status === "complete") {
    return { action:"none" };
  }
  const active = state.active?.taskGid === mission.taskGid ? state.active : null;
  let logProgress = 0;
  try { if (active?.log) logProgress = statSync(active.log).mtimeMs; } catch {}
  const lastProgress = Math.max(Number(active?.lastProgressAt || 0),
    Number(active?.startedAt || 0), logProgress, Number(mission.updatedAt || 0));
  if (active && now - lastProgress < stall) return { action:"active", mission };
  if (!active && mission.status === "queued" && now - lastProgress < stall) {
    return { action:"waiting", mission };
  }

  const bumps = Number(mission.heartbeatBumps || 0);
  if (bumps >= maximum) {
    if (active) stopWorker(active.pid);
    if (active) state.active = null;
    mission.status = "blocked";
    mission.updatedAt = now;
    state.recovery = {
      taskGid:mission.taskGid, mission:mission.name, status:"exhausted",
      attempts:bumps, maximum, reason:"blocking-mission-stalled", updatedAt:now,
      activity:`Heartbeat bump limit reached (${bumps}/${maximum}); human review required.`,
    };
    return { action:"exhausted", mission };
  }
  if (now - Number(mission.lastHeartbeatBumpAt || 0) < cooldown) {
    return { action:"cooldown", mission };
  }

  if (active) stopWorker(active.pid);
  if (active) state.active = null;
  delete state.done?.[mission.taskGid];
  mission.heartbeatBumps = bumps + 1;
  mission.lastHeartbeatBumpAt = now;
  mission.status = "queued";
  mission.updatedAt = now;
  state.recoveries ||= {};
  state.recoveries[mission.taskGid] = {
    ...(state.recoveries[mission.taskGid] || {}),
    attempts:bumps + 1, lastAttemptAt:now, reason:"heartbeat-bump",
  };
  state.recovery = {
    taskGid:mission.taskGid, mission:mission.name, status:"queued",
    attempts:bumps + 1, maximum, reason:"blocking-mission-stalled", updatedAt:now,
    activity:`Heartbeat bumped the blocking mission (${bumps + 1}/${maximum}); lower-priority work remains parked.`,
  };
  return { action:"bumped", mission };
}

export function clearStaleRecovery(state) {
  const recovery = state?.recovery;
  if (!recovery) return false;
  if (state.active?.taskGid === recovery.taskGid) return false;
  if (state.done?.[recovery.taskGid]?.status === "failed") return false;
  if (state.priorityMission?.taskGid === recovery.taskGid
      && state.priorityMission.status !== "complete") return false;
  delete state.recovery;
  return true;
}

export function runPriorityHeartbeat(now = Date.now()) {
  let state;
  try { state = JSON.parse(readFileSync(statePath, "utf8")); }
  catch { return { action:"no-state" }; }
  const cleared = clearStaleRecovery(state);
  const result = bumpBlockingMission(state, now);
  if (cleared || !["none", "active", "waiting", "cooldown"].includes(result.action)) {
    atomicWriteJson(statePath, state);
  }
  if (result.action === "bumped" || result.action === "exhausted") {
    mkdirSync(dirname(logPath), { recursive:true });
    writeFileSync(logPath,
      `${new Date(now).toISOString()} ♥ priority · ${result.mission.name}: ${state.recovery.activity}\n`,
      { flag:"a" });
  }
  return { ...result, clearedStaleRecovery:cleared };
}

const isMain = process.argv[1] && fileURLToPath(import.meta.url) === process.argv[1];
if (isMain) console.log(JSON.stringify(runPriorityHeartbeat()));
