import { spawn } from "node:child_process";
import { readFileSync } from "node:fs";
import { homedir } from "node:os";
import { join } from "node:path";

const PROGRESS_PATH = join(homedir(), ".local", "share", "desktop-badge", "agent-progress.json");
const MISSION_PATH = join(homedir(), ".local", "share", "desktop-badge", "mission.json");

function readJson(path) {
  try { return JSON.parse(readFileSync(path, "utf8")); }
  catch { return null; }
}

export function resolveDirectorGoal(fallback, {
  taskGid = process.env.CAPTUTOR_TASK_GID || "",
  progress = readJson(PROGRESS_PATH),
  mission = readJson(MISSION_PATH),
} = {}) {
  if (progress?.mission && (!taskGid || !progress.taskGid || String(progress.taskGid) === String(taskGid))) {
    return String(progress.mission);
  }
  const active = mission?.items?.find((item) => item.status === "in_progress");
  return String(active?.text || fallback || mission?.mission || "Captutor mission");
}

export class DirectorChannel {
  constructor({
    url = process.env.CAPTUTOR_DIRECTOR_URL || "",
    goal = "Captutor mission",
    taskGid = process.env.CAPTUTOR_TASK_GID || "",
    screenplay = "",
    locale = "en",
    format = "docs",
    token = process.env.CAPTUTOR_DIRECTOR_TOKEN || "",
    fetchImpl = globalThis.fetch,
  } = {}) {
    this.url = url;
    this.fetchImpl = fetchImpl;
    this.token = token;
    this.sequence = 0;
    this.pending = new Set();
    this.voice = null;
    this.state = {
      schema:"captutor-director-state/v1",
      source:"captutor",
      goal, taskGid, screenplay, locale, format,
      phase:"preparing", status:"working",
      beatIndex:null, beatCount:0, currentLine:"", nextLine:"", words:[],
      beatStartedAt:null, updatedAt:new Date().toISOString(),
    };
    this.heartbeat = this.enabled ? setInterval(() => this.publish(), 2_500) : null;
    this.heartbeat?.unref?.();
  }

  get enabled() { return Boolean(this.url && this.fetchImpl); }

  publish(patch = {}) {
    this.state = {
      ...this.state, ...patch,
      sequence:++this.sequence,
      updatedAt:new Date().toISOString(),
    };
    if (!this.enabled) return this.state;
    const controller = new AbortController();
    const timer = setTimeout(() => controller.abort(), 900);
    const request = Promise.resolve(this.fetchImpl(this.url, {
      method:"POST",
      headers:{
        "Content-Type":"application/json",
        ...(this.token ? { Authorization:`Bearer ${this.token}` } : {}),
      },
      body:JSON.stringify(this.state),
      signal:controller.signal,
    })).catch(() => null).finally(() => {
      clearTimeout(timer);
      this.pending.delete(request);
    });
    this.pending.add(request);
    return this.state;
  }

  playVoice(path) {
    this.stopVoice();
    if (!path || process.env.CAPTUTOR_MONITOR_VOICE === "0" || !this.enabled) return;
    try {
      this.voice = spawn("/usr/bin/afplay", [path], { stdio:"ignore" });
      this.voice.once("exit", () => { this.voice = null; });
    } catch { this.voice = null; }
  }

  stopVoice() {
    try { this.voice?.kill("SIGTERM"); } catch {}
    this.voice = null;
  }

  async close(finalPatch = null) {
    if (this.heartbeat) clearInterval(this.heartbeat);
    this.heartbeat = null;
    this.stopVoice();
    if (finalPatch) this.publish(finalPatch);
    await Promise.allSettled([...this.pending]);
  }
}

export function directorBeatState(beats, index, startedAt = Date.now()) {
  const beat = beats[index];
  const next = beats[index + 1];
  return {
    phase:"performing",
    status:"recording",
    beatIndex:index,
    beatCount:beats.length,
    currentLine:beat?.say || "",
    nextLine:next?.say || "",
    words:beat?.words || [],
    beatStartedAt:new Date(startedAt).toISOString(),
    currentAction:beat?.cursorIntent || beat?.logic || "",
  };
}
