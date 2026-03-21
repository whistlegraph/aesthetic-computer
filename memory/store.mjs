// memory/store.mjs
// Local-first encrypted session memory store for Claude/Codex workflows.

import { createHash, randomUUID } from "crypto";
import { existsSync } from "fs";
import { appendFile, mkdir, readFile, readdir, writeFile } from "fs/promises";
import { homedir, hostname } from "os";
import { join } from "path";

import { encryptJSON, resolveMemoryKey } from "./crypto.mjs";
import { enqueueRemoteRecord, flushRemoteQueue, getRemoteConfig } from "./remote.mjs";
import { redactText } from "./redact.mjs";

function nowIso() {
  return new Date().toISOString();
}

function sanitizeSessionId(sessionId) {
  if (!sessionId) return "";
  return String(sessionId).replace(/[^a-zA-Z0-9._:-]/g, "-");
}

function sanitizeDeviceId(deviceId) {
  if (!deviceId) return "";
  return String(deviceId).replace(/[^a-zA-Z0-9._:-]/g, "-");
}

function createId(prefix) {
  return `${prefix}_${Date.now().toString(36)}_${randomUUID().slice(0, 8)}`;
}

function resolveStoreHome() {
  if (process.env.AGENT_MEMORY_HOME) {
    return process.env.AGENT_MEMORY_HOME;
  }
  return join(homedir(), ".ac-agent-memory");
}

function pathsFor(home) {
  return {
    home,
    sessionsDir: join(home, "sessions"),
    eventsDir: join(home, "events"),
    checkpointsDir: join(home, "checkpoints"),
    queueDir: join(home, "queue"),
    queueFile: join(home, "queue", "outbound.ndjson"),
    deviceFile: join(home, "device-id"),
  };
}

async function ensureStore(paths) {
  await mkdir(paths.sessionsDir, { recursive: true });
  await mkdir(paths.eventsDir, { recursive: true });
  await mkdir(paths.checkpointsDir, { recursive: true });
  await mkdir(paths.queueDir, { recursive: true });
}

async function readJsonFile(path) {
  if (!existsSync(path)) return null;
  try {
    const data = await readFile(path, "utf8");
    return JSON.parse(data);
  } catch {
    return null;
  }
}

async function writeJsonFile(path, payload) {
  await writeFile(path, `${JSON.stringify(payload, null, 2)}\n`, "utf8");
}

function sessionPath(paths, sessionId) {
  return join(paths.sessionsDir, `${sanitizeSessionId(sessionId)}.json`);
}

function eventsPath(paths, sessionId) {
  return join(paths.eventsDir, `${sanitizeSessionId(sessionId)}.ndjson`);
}

function checkpointsPath(paths, sessionId) {
  return join(paths.checkpointsDir, `${sanitizeSessionId(sessionId)}.ndjson`);
}

async function readNdjson(path) {
  if (!existsSync(path)) return [];
  const data = await readFile(path, "utf8");
  return data
    .split("\n")
    .map((line) => line.trim())
    .filter(Boolean)
    .map((line) => {
      try {
        return JSON.parse(line);
      } catch {
        return null;
      }
    })
    .filter(Boolean);
}

async function appendNdjson(path, payload) {
  await appendFile(path, `${JSON.stringify(payload)}\n`, "utf8");
}

async function resolveDeviceId(paths) {
  if (process.env.AGENT_DEVICE_ID) {
    return sanitizeDeviceId(process.env.AGENT_DEVICE_ID);
  }

  if (existsSync(paths.deviceFile)) {
    return sanitizeDeviceId((await readFile(paths.deviceFile, "utf8")).trim());
  }

  const generated = `${hostname()}-${randomUUID().slice(0, 8)}`;
  await writeFile(paths.deviceFile, `${generated}\n`, "utf8");
  return generated;
}

async function loadSession(paths, sessionId) {
  return readJsonFile(sessionPath(paths, sessionId));
}

async function saveSession(paths, session) {
  await writeJsonFile(sessionPath(paths, session.session_id), session);
}

export async function createSession(options = {}) {
  const storeHome = resolveStoreHome();
  const paths = pathsFor(storeHome);
  await ensureStore(paths);

  const requestedSessionId = sanitizeSessionId(options.sessionId || options.session_id || "");
  const sessionId = requestedSessionId || createId("session");

  const existing = await loadSession(paths, sessionId);
  if (existing) return existing;

  const timestamp = nowIso();
  const session = {
    version: 1,
    session_id: sessionId,
    title: options.title || "Untitled Session",
    provider: options.provider || "unknown",
    project: options.project || "default",
    created_at: timestamp,
    updated_at: timestamp,
    last_seq: 0,
    last_checkpoint_id: null,
    remembered_from: options.remembered_from || null,
  };

  await saveSession(paths, session);
  return session;
}

function extractPrimaryText(options) {
  if (typeof options.text === "string") return options.text;
  if (typeof options.content === "string") return options.content;
  if (typeof options.message === "string") return options.message;
  if (options.content != null) {
    try {
      return JSON.stringify(options.content);
    } catch {
      return String(options.content);
    }
  }
  return "";
}

export async function commitEvent(options = {}) {
  const storeHome = resolveStoreHome();
  const paths = pathsFor(storeHome);
  await ensureStore(paths);

  const key = await resolveMemoryKey(storeHome);
  const deviceId = await resolveDeviceId(paths);

  const sessionId = sanitizeSessionId(options.sessionId || options.session_id || "");
  const session = await createSession({
    sessionId,
    provider: options.provider,
    title: options.title,
    project: options.project,
  });

  const loadedSession = (await loadSession(paths, session.session_id)) || session;
  const seq = Number(loadedSession.last_seq || 0) + 1;
  const when = options.when || nowIso();
  const primaryText = extractPrimaryText(options);
  const redaction = redactText(primaryText);

  const payload = {
    role: options.role || "user",
    text: primaryText,
    content: options.content ?? null,
    tool_calls: options.tool_calls ?? null,
    source: options.source || "manual",
    context: options.context || null,
    metadata: options.metadata || null,
  };

  const eventRecord = {
    version: 1,
    kind: "event",
    session_id: session.session_id,
    device_id: deviceId,
    seq,
    when,
    provider: options.provider || loadedSession.provider || "unknown",
    model: options.model || null,
    role: payload.role,
    source: payload.source,
    redacted_preview: redaction.preview,
    redaction_hits: redaction.findings,
    ciphertext: encryptJSON(payload, key),
  };

  await appendNdjson(eventsPath(paths, session.session_id), eventRecord);

  const updatedSession = {
    ...loadedSession,
    provider: eventRecord.provider,
    project: options.project || loadedSession.project,
    updated_at: when,
    last_seq: seq,
  };
  await saveSession(paths, updatedSession);

  await enqueueRemoteRecord(paths.queueFile, paths.queueDir, {
    kind: "event",
    session_id: session.session_id,
    payload: eventRecord,
  });

  return {
    session_id: session.session_id,
    seq,
    when,
    redacted_preview: redaction.preview,
    redaction_hits: redaction.findings,
  };
}

function buildSummaryFromEvents(events, maxItems = 8) {
  const previews = events
    .map((event) => event.redacted_preview)
    .filter(Boolean)
    .slice(-1 * Math.max(maxItems * 2, 1));

  const unique = [];
  for (const preview of previews) {
    if (!unique.includes(preview)) unique.push(preview);
  }

  const selected = unique.slice(-1 * maxItems);
  if (selected.length === 0) {
    return "No notable messages captured yet.";
  }

  return selected.map((item, index) => `${index + 1}. ${item}`).join("\n");
}

async function listCheckpointsForSession(paths, sessionId) {
  return readNdjson(checkpointsPath(paths, sessionId));
}

export async function createCheckpoint(options = {}) {
  const sessionId = sanitizeSessionId(options.sessionId || options.session_id || "");
  if (!sessionId) {
    throw new Error("createCheckpoint requires sessionId");
  }

  const storeHome = resolveStoreHome();
  const paths = pathsFor(storeHome);
  await ensureStore(paths);

  const session = await loadSession(paths, sessionId);
  if (!session) {
    throw new Error(`session not found: ${sessionId}`);
  }

  const events = await readNdjson(eventsPath(paths, sessionId));
  const maxEvents = Number(options.maxEvents || 40);
  const scoped = events.slice(-1 * Math.max(maxEvents, 1));

  const providedSummary = options.summary || "";
  const autoSummary = buildSummaryFromEvents(scoped, 8);
  const summaryInput = providedSummary || autoSummary;
  const redactedSummary = redactText(summaryInput);

  const checkpoint = {
    version: 1,
    kind: "checkpoint",
    checkpoint_id: createId("checkpoint"),
    session_id: sessionId,
    when: options.when || nowIso(),
    reason: options.reason || "manual",
    based_on_seq: session.last_seq || 0,
    summary: redactedSummary.redacted,
    redaction_hits: redactedSummary.findings,
  };

  await appendNdjson(checkpointsPath(paths, sessionId), checkpoint);

  const updatedSession = {
    ...session,
    updated_at: checkpoint.when,
    last_checkpoint_id: checkpoint.checkpoint_id,
  };
  await saveSession(paths, updatedSession);

  await enqueueRemoteRecord(paths.queueFile, paths.queueDir, {
    kind: "checkpoint",
    session_id: sessionId,
    payload: checkpoint,
  });

  return checkpoint;
}

export async function listSessions(options = {}) {
  const storeHome = resolveStoreHome();
  const paths = pathsFor(storeHome);
  await ensureStore(paths);

  const files = await readdir(paths.sessionsDir);
  const sessions = [];

  for (const file of files) {
    if (!file.endsWith(".json")) continue;
    const loaded = await readJsonFile(join(paths.sessionsDir, file));
    if (loaded) sessions.push(loaded);
  }

  const filtered = sessions.filter((session) => {
    if (options.project && session.project !== options.project) return false;
    return true;
  });

  filtered.sort((a, b) => {
    const left = new Date(a.updated_at || a.created_at || 0).getTime();
    const right = new Date(b.updated_at || b.created_at || 0).getTime();
    return right - left;
  });

  const limit = Number(options.limit || 50);
  return filtered.slice(0, Math.max(limit, 1));
}

export async function rememberSession(options = {}) {
  const fromSessionId = sanitizeSessionId(options.fromSessionId || options.from_session_id || "");
  if (!fromSessionId) {
    throw new Error("rememberSession requires fromSessionId");
  }

  const storeHome = resolveStoreHome();
  const paths = pathsFor(storeHome);
  await ensureStore(paths);

  const sourceSession = await loadSession(paths, fromSessionId);
  if (!sourceSession) {
    throw new Error(`source session not found: ${fromSessionId}`);
  }

  const checkpoints = await listCheckpointsForSession(paths, fromSessionId);
  let selectedCheckpoint = null;

  if (options.checkpointId) {
    selectedCheckpoint = checkpoints.find(
      (checkpoint) => checkpoint.checkpoint_id === options.checkpointId
    );
  }

  if (!selectedCheckpoint && checkpoints.length > 0) {
    selectedCheckpoint = checkpoints[checkpoints.length - 1];
  }

  const rememberedFrom = {
    session_id: fromSessionId,
    checkpoint_id: selectedCheckpoint?.checkpoint_id || null,
    seq: selectedCheckpoint?.based_on_seq || sourceSession.last_seq || 0,
  };

  const newSession = await createSession({
    sessionId: options.sessionId,
    title: options.title || `Remembered from ${fromSessionId}`,
    provider: options.provider || sourceSession.provider,
    project: options.project || sourceSession.project,
    remembered_from: rememberedFrom,
  });

  if (selectedCheckpoint) {
    await createCheckpoint({
      sessionId: newSession.session_id,
      reason: "remember-seed",
      summary: `Remembered from ${fromSessionId} @ seq ${rememberedFrom.seq}\n${selectedCheckpoint.summary}`,
    });
  }

  return {
    session_id: newSession.session_id,
    remembered_from: rememberedFrom,
  };
}

export async function flushRemote() {
  const storeHome = resolveStoreHome();
  const paths = pathsFor(storeHome);
  await ensureStore(paths);
  return flushRemoteQueue(paths.queueFile);
}

export async function getSession(sessionId) {
  const normalizedSessionId = sanitizeSessionId(sessionId);
  const storeHome = resolveStoreHome();
  const paths = pathsFor(storeHome);
  await ensureStore(paths);
  return loadSession(paths, normalizedSessionId);
}

export async function listSessionCheckpoints(sessionId) {
  const normalizedSessionId = sanitizeSessionId(sessionId);
  const storeHome = resolveStoreHome();
  const paths = pathsFor(storeHome);
  await ensureStore(paths);
  return listCheckpointsForSession(paths, normalizedSessionId);
}

export async function setDeviceId(deviceId) {
  const normalized = sanitizeDeviceId(deviceId);
  if (!normalized) {
    throw new Error("setDeviceId requires a non-empty device id");
  }

  const storeHome = resolveStoreHome();
  const paths = pathsFor(storeHome);
  await ensureStore(paths);
  await writeFile(paths.deviceFile, `${normalized}\n`, "utf8");
  return normalized;
}

export async function inspectStore() {
  const storeHome = resolveStoreHome();
  const paths = pathsFor(storeHome);
  await ensureStore(paths);

  const key = await resolveMemoryKey(storeHome);
  const deviceId = await resolveDeviceId(paths);
  const sessions = await readdir(paths.sessionsDir);
  const events = await readdir(paths.eventsDir);
  const checkpoints = await readdir(paths.checkpointsDir);
  const remote = getRemoteConfig();
  const keyFingerprint = createHash("sha256")
    .update(key)
    .digest("hex")
    .slice(0, 16);

  return {
    home: storeHome,
    device_id: deviceId,
    key_source: process.env.AGENT_MEMORY_KEY ? "env" : "file",
    key_fingerprint: keyFingerprint,
    sessions_count: sessions.filter((name) => name.endsWith(".json")).length,
    event_streams_count: events.filter((name) => name.endsWith(".ndjson")).length,
    checkpoint_streams_count: checkpoints.filter((name) => name.endsWith(".ndjson")).length,
    remote_enabled: remote.enabled,
    remote_endpoint: remote.endpoint || null,
  };
}

export async function buildEnvProfile(options = {}) {
  const storeHome = resolveStoreHome();
  const paths = pathsFor(storeHome);
  await ensureStore(paths);

  const key = await resolveMemoryKey(storeHome);
  const deviceId =
    options.deviceId ? await setDeviceId(options.deviceId) : await resolveDeviceId(paths);

  const lines = [];
  if (options.includeKey) {
    lines.push(`AGENT_MEMORY_KEY=base64:${key.toString("base64")}`);
  }

  lines.push(`AGENT_DEVICE_ID=${deviceId}`);

  const project = options.project || process.env.AGENT_MEMORY_PROJECT || "";
  const provider = options.provider || process.env.AGENT_MEMORY_PROVIDER || "";
  const ticket = options.ticket || process.env.AGENT_MEMORY_TICKET || "";
  const sessionId = options.sessionId || process.env.AGENT_SESSION_ID || "";

  if (project) lines.push(`AGENT_MEMORY_PROJECT=${project}`);
  if (provider) lines.push(`AGENT_MEMORY_PROVIDER=${provider}`);
  if (ticket) lines.push(`AGENT_MEMORY_TICKET=${ticket}`);
  if (sessionId) lines.push(`AGENT_SESSION_ID=${sessionId}`);

  return {
    home: storeHome,
    lines,
  };
}
