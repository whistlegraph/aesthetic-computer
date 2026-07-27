#!/usr/bin/env node

import { execFileSync } from "node:child_process";
import { mkdirSync, readFileSync, readdirSync, renameSync, writeFileSync } from "node:fs";
import { homedir } from "node:os";
import { basename, join } from "node:path";

const home = homedir();
const statePath = join(home, ".hermes", "orchestrator-state.json");
const outputDir = join(home, ".local", "share", "desktop-badge");
const outputPath = join(outputDir, "agent-progress.json");
const intervalMs = 2_000;

function readJson(path) {
  try { return JSON.parse(readFileSync(path, "utf8")); }
  catch { return null; }
}

function processes() {
  try {
    return execFileSync("/bin/ps", ["-axo", "pid=,ppid=,command="], { encoding: "utf8" })
      .split("\n")
      .map((line) => {
        const match = line.match(/^\s*(\d+)\s+(\d+)\s+(.*)$/);
        return match ? { pid: Number(match[1]), ppid: Number(match[2]), command: match[3] } : null;
      })
      .filter(Boolean);
  } catch {
    return [];
  }
}

function findClaudeSession(workerPid) {
  const row = processes().find((p) => p.ppid === workerPid && /(^|\/)claude\s+-p\b/.test(p.command));
  if (!row) return null;
  const session = readJson(join(home, ".claude", "sessions", `${row.pid}.json`));
  return session?.sessionId ? { pid: row.pid, ...session } : null;
}

function loadTasks(sessionId) {
  const dir = join(home, ".claude", "tasks", sessionId);
  let names = [];
  try { names = readdirSync(dir).filter((name) => /^\d+\.json$/.test(name)); }
  catch { return []; }
  return names
    .map((name) => readJson(join(dir, name)))
    .filter((task) => task && task.subject)
    .sort((a, b) => Number(a.id) - Number(b.id));
}

function latestAssistantActivity(session) {
  if (!session?.sessionId || !session?.cwd) return { activity: "", lastTool: "" };
  const project = session.cwd.replaceAll("/", "-");
  const path = join(home, ".claude", "projects", project, `${session.sessionId}.jsonl`);
  let lines;
  try { lines = readFileSync(path, "utf8").trim().split("\n"); }
  catch { return { activity: "", lastTool: "" }; }
  let activity = "";
  let lastTool = "";
  for (let index = lines.length - 1; index >= 0 && (!activity || !lastTool); index -= 1) {
    let entry;
    try { entry = JSON.parse(lines[index]); } catch { continue; }
    if (entry?.message?.role !== "assistant" || !Array.isArray(entry.message.content)) continue;
    for (const block of [...entry.message.content].reverse()) {
      if (!lastTool && block?.type === "tool_use") lastTool = String(block.name || "");
      if (!activity && block?.type === "text") {
        activity = String(block.text || "")
          .replace(/\s+/g, " ")
          .replace(/^#+\s*/, "")
          .trim()
          .slice(0, 180);
      }
    }
  }
  return { activity, lastTool };
}

function phaseFor(tasks) {
  const activeText = tasks
    .filter((task) => task.status === "in_progress")
    .map((task) => `${task.subject} ${task.activeForm || ""}`)
    .join(" ")
    .toLowerCase();
  const rules = [
    [/inspect|capture hygiene|\bqa\b/, "INSPECTING"],
    [/outbox|deliver|manifest/, "DELIVERING"],
    [/render|generat(e|ing).*video/, "RENDERING"],
    [/narrat|pacing/, "NARRATING"],
    [/author|writing|\bwrite\b/, "AUTHORING"],
    [/preflight|study|research|pattern|probe|selector|mapping/, "PATHFINDING"],
  ];
  return rules.find(([pattern]) => pattern.test(activeText))?.[1]
    || (tasks.length && tasks.every((task) => task.status === "completed") ? "COMPLETE" : "WORKING");
}

function latestFailure(state) {
  return Object.entries(state?.done || {})
    .filter(([, record]) => record?.status === "failed")
    .map(([taskGid, record]) => ({ taskGid, ...record }))
    .sort((a, b) => Number(b.at || 0) - Number(a.at || 0))[0] || null;
}

function publish() {
  const state = readJson(statePath);
  const active = state?.active;
  let payload;
  if (!active?.pid) {
    const recovery = state?.recovery;
    const recovering = recovery && [
      "checking-browser", "reloading-browser", "queued", "relaunching",
    ].includes(recovery.status);
    const failure = latestFailure(state);
    payload = recovering ? {
      schema:"iris-agent-progress/v1",
      updatedAt:new Date().toISOString(),
      state:"working",
      phase:"RECOVERING",
      taskGid:String(recovery.taskGid || ""),
      mission:String(recovery.mission || "Mission"),
      completed:0,
      total:1,
      active:[recovery.activity || "Recovering stopped mission"],
      activity:recovery.detail || recovery.reason || "Recovering stopped mission",
      recoveryAttempt:Number(recovery.attempts || 0),
      recoveryMaximum:Number(recovery.maximum || 0),
    } : failure || recovery?.status === "blocked" || recovery?.status === "exhausted" ? {
      schema:"iris-agent-progress/v1",
      updatedAt:new Date().toISOString(),
      state:"failed",
      phase:"FAILED",
      taskGid:String(failure?.taskGid || recovery?.taskGid || ""),
      mission:String(failure?.name || recovery?.mission || "Mission"),
      completed:0,
      total:1,
      active:[],
      activity:String(recovery?.activity || failure?.detail || failure?.reason || "Mission needs attention"),
      failureReason:String(failure?.reason || recovery?.reason || "unknown"),
      log:String(failure?.log || ""),
    } : {
      schema:"iris-agent-progress/v1",
      updatedAt:new Date().toISOString(),
      state:"idle",
      phase:"IDLE",
      completed:0,
      total:0,
      active:[],
    };
  } else {
    const session = findClaudeSession(Number(active.pid));
    const tasks = session ? loadTasks(session.sessionId) : [];
    const activeTasks = tasks.filter((task) => task.status === "in_progress");
    const completed = tasks.filter((task) => task.status === "completed").length;
    const detail = latestAssistantActivity(session);
    payload = {
      schema: "iris-agent-progress/v1",
      updatedAt: new Date().toISOString(),
      state: "working",
      phase: phaseFor(tasks),
      taskGid: String(active.taskGid || ""),
      mission: String(active.name || "Mission"),
      workerPid: Number(active.pid),
      sessionId: String(session?.sessionId || ""),
      completed,
      total: tasks.length,
      active: activeTasks.map((task) => task.activeForm || task.subject).slice(0, 3),
      activity: detail.activity,
      lastTool: detail.lastTool,
    };
  }
  mkdirSync(outputDir, { recursive: true });
  const temp = `${outputPath}.${process.pid}.tmp`;
  writeFileSync(temp, `${JSON.stringify(payload, null, 2)}\n`, { mode: 0o644 });
  renameSync(temp, outputPath);
}

publish();
setInterval(publish, intervalMs);
