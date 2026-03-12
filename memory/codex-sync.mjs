#!/usr/bin/env node
// memory/codex-sync.mjs
// Imports recent Codex user/assistant messages from local Codex session logs.

import { existsSync } from "fs";
import { mkdir, readdir, readFile, stat, writeFile } from "fs/promises";
import { homedir } from "os";
import { basename, join } from "path";

import { commitEvent } from "./index.mjs";

const CODEX_SYNC_CURSOR_VERSION = 1;

function asBool(value, fallback = false) {
  if (value == null || value === "") return fallback;
  return /^(1|true|yes|on)$/i.test(String(value));
}

function nowIso() {
  return new Date().toISOString();
}

function parseArgs(argv) {
  const args = { _: [] };
  for (let i = 0; i < argv.length; i += 1) {
    const token = argv[i];
    if (!token.startsWith("--")) {
      args._.push(token);
      continue;
    }

    const key = token.slice(2);
    const next = argv[i + 1];
    if (next && !next.startsWith("--")) {
      args[key] = next;
      i += 1;
    } else {
      args[key] = true;
    }
  }
  return args;
}

function toInt(value, fallback) {
  const n = Number(value);
  return Number.isFinite(n) ? n : fallback;
}

function resolveMemoryHome() {
  if (process.env.AGENT_MEMORY_HOME) return process.env.AGENT_MEMORY_HOME;
  return join(homedir(), ".ac-agent-memory");
}

function resolveCodexHome() {
  if (process.env.CODEX_HOME) return process.env.CODEX_HOME;
  return join(homedir(), ".codex");
}

async function walkJsonlFiles(rootDir, out = []) {
  if (!existsSync(rootDir)) return out;
  const entries = await readdir(rootDir, { withFileTypes: true });
  for (const entry of entries) {
    const fullPath = join(rootDir, entry.name);
    if (entry.isDirectory()) {
      await walkJsonlFiles(fullPath, out);
      continue;
    }
    if (entry.isFile() && entry.name.endsWith(".jsonl")) {
      out.push(fullPath);
    }
  }
  return out;
}

async function getLatestSessionFiles(codexHome, limit = 3) {
  const sessionsDir = join(codexHome, "sessions");
  const files = await walkJsonlFiles(sessionsDir, []);
  const withTimes = [];
  for (const file of files) {
    try {
      const fileStat = await stat(file);
      withTimes.push({
        file,
        mtimeMs: fileStat.mtimeMs,
      });
    } catch {
      // Ignore unreadable files.
    }
  }

  withTimes.sort((a, b) => b.mtimeMs - a.mtimeMs);
  return withTimes.slice(0, Math.max(1, limit)).map((entry) => entry.file);
}

function sanitizeSessionId(sessionId) {
  return String(sessionId || "").replace(/[^a-zA-Z0-9._:-]/g, "-");
}

function extractCodexSessionId(filePath) {
  const name = basename(filePath, ".jsonl");
  const match = name.match(
    /([0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12})$/i
  );
  if (match?.[1]) return `codex:${match[1]}`;
  return `codex:${name}`;
}

function isBoilerplateMessage(text) {
  if (!text) return true;
  if (text.includes("AGENTS.md instructions for /workspaces/aesthetic-computer")) {
    return true;
  }
  if (text.includes("<permissions instructions>")) {
    return true;
  }
  return false;
}

function truncateText(text, maxChars) {
  if (text.length <= maxChars) return text;
  return `${text.slice(0, maxChars)}\n\n[truncated by codex-sync]`;
}

function cursorFilePath() {
  return join(resolveMemoryHome(), "imports", "codex-sync-cursor.json");
}

async function loadCursor() {
  const path = cursorFilePath();
  if (!existsSync(path)) {
    return { version: CODEX_SYNC_CURSOR_VERSION, files: {} };
  }
  try {
    const parsed = JSON.parse(await readFile(path, "utf8"));
    if (!parsed || typeof parsed !== "object") {
      return { version: CODEX_SYNC_CURSOR_VERSION, files: {} };
    }
    if (!parsed.files || typeof parsed.files !== "object") {
      parsed.files = {};
    }
    return parsed;
  } catch {
    return { version: CODEX_SYNC_CURSOR_VERSION, files: {} };
  }
}

async function saveCursor(cursor) {
  const path = cursorFilePath();
  await mkdir(join(resolveMemoryHome(), "imports"), { recursive: true });
  await writeFile(path, `${JSON.stringify(cursor, null, 2)}\n`, "utf8");
}

function parseJsonLine(line) {
  try {
    return JSON.parse(line);
  } catch {
    return null;
  }
}

function messageFromRecord(record, includeAssistant) {
  if (record?.type !== "event_msg") return null;
  const payload = record.payload || {};
  if (!payload?.type) return null;

  if (payload.type === "user_message") {
    const text = typeof payload.message === "string" ? payload.message : "";
    if (!text.trim()) return null;
    return {
      role: "user",
      text,
      metadata: {
        media_count:
          (Array.isArray(payload.images) ? payload.images.length : 0) +
          (Array.isArray(payload.local_images) ? payload.local_images.length : 0),
      },
    };
  }

  if (includeAssistant && payload.type === "agent_message") {
    const text = typeof payload.message === "string" ? payload.message : "";
    if (!text.trim()) return null;
    return {
      role: "assistant",
      text,
      metadata: {},
    };
  }

  return null;
}

export async function syncCodexSessions(options = {}) {
  const codexHome = options.codexHome || resolveCodexHome();
  const includeAssistant = asBool(
    process.env.AGENT_MEMORY_CODEX_INCLUDE_ASSISTANT,
    false
  );
  const maxSessions = Math.max(1, toInt(options.maxSessions, 3));
  const maxEvents = Math.max(1, toInt(options.maxEvents, 120));
  const maxChars = Math.max(500, toInt(options.maxChars, 20000));

  if (!existsSync(codexHome)) {
    return {
      synced_events: 0,
      scanned_sessions: 0,
      skipped: "codex-home-missing",
      codex_home: codexHome,
    };
  }

  const sessionFiles = await getLatestSessionFiles(codexHome, maxSessions);
  if (sessionFiles.length === 0) {
    return {
      synced_events: 0,
      scanned_sessions: 0,
      skipped: "no-codex-sessions",
      codex_home: codexHome,
    };
  }

  const cursor = await loadCursor();
  let syncedEvents = 0;
  let scannedSessions = 0;
  let reachedEventCap = false;

  for (const file of sessionFiles) {
    scannedSessions += 1;
    const raw = await readFile(file, "utf8");
    const lines = raw
      .split("\n")
      .map((line) => line.trim())
      .filter(Boolean);

    const existing = cursor.files[file] || {};
    const startIndex = Math.min(toInt(existing.lines, 0), lines.length);
    const pending = lines.slice(startIndex);
    const sessionId = sanitizeSessionId(extractCodexSessionId(file));
    let consumedLines = startIndex;

    for (const line of pending) {
      if (syncedEvents >= maxEvents) {
        reachedEventCap = true;
        break;
      }
      consumedLines += 1;

      const record = parseJsonLine(line);
      if (!record) continue;

      const message = messageFromRecord(record, includeAssistant);
      if (!message) continue;
      if (isBoilerplateMessage(message.text)) continue;

      const text = truncateText(message.text, maxChars);

      await commitEvent({
        sessionId,
        provider: "codex",
        role: message.role,
        source: "codex-sync",
        project: process.env.AGENT_MEMORY_PROJECT || "aesthetic-computer",
        model: null,
        text,
        context: {
          codex_file: file,
          codex_timestamp: record.timestamp || null,
        },
        metadata: {
          codex_sync: true,
          imported_at: nowIso(),
          ...(message.metadata || {}),
        },
        title: "Codex Session",
      });

      syncedEvents += 1;
    }

    cursor.files[file] = {
      lines: consumedLines,
      updated_at: nowIso(),
    };

    if (reachedEventCap) {
      break;
    }
  }

  await saveCursor(cursor);

  return {
    synced_events: syncedEvents,
    scanned_sessions: scannedSessions,
    codex_home: codexHome,
    include_assistant: includeAssistant,
  };
}

async function main() {
  const args = parseArgs(process.argv.slice(2));
  const result = await syncCodexSessions({
    maxSessions: args["max-sessions"],
    maxEvents: args["max-events"],
    maxChars: args["max-chars"],
    codexHome: args["codex-home"],
  });
  console.log(JSON.stringify(result, null, 2));
}

if (import.meta.url === `file://${process.argv[1]}`) {
  main().catch((error) => {
    console.error(`codex-sync: ${error.message}`);
    process.exit(1);
  });
}
