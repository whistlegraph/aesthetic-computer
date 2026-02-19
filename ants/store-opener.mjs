#!/usr/bin/env node
// ants/store-opener.mjs
// Stores first ("opener") prompts from Jeffrey's dev sessions to MongoDB.
//
// Usage:
//   node ants/store-opener.mjs                  # from Claude Code UserPromptSubmit hook (stdin = JSON payload)
//   node ants/store-opener.mjs post-commit       # from git post-commit hook
//
// Required env vars: ADMIN_SUB, MONGODB_CONNECTION_STRING, MONGODB_NAME
//
// Schema (openers collection):
//   user:    auth0 sub (from ADMIN_SUB) — same pattern as moods, kidlisp, paintings
//   text:    the opener prompt text (or commit message for post-commit)
//   model:   claude model id
//   when:    Date
//   source:  "claude-code" | "post-commit"
//   context: { repo, branch, commit, cwd, session_id }
//
// Note: handle is NOT stored here — resolve via $lookup on @handles where _id === user

import { existsSync, writeFileSync } from "fs";
import { execSync } from "child_process";
import { MongoClient } from "mongodb";

// Read and parse stdin with a short timeout (handles both hook and CLI contexts)
async function readStdin() {
  return new Promise((resolve) => {
    let data = "";
    let settled = false;
    const done = (result) => {
      if (!settled) {
        settled = true;
        resolve(result);
      }
    };
    process.stdin.setEncoding("utf8");
    process.stdin.on("data", (chunk) => (data += chunk));
    process.stdin.on("end", () => {
      try {
        done(JSON.parse(data));
      } catch {
        done({});
      }
    });
    process.stdin.on("error", () => done({}));
    // Don't wait forever if stdin has no data (post-commit context)
    setTimeout(() => done({}), 500);
  });
}

function getGitInfo() {
  try {
    const branch = execSync("git branch --show-current", {
      encoding: "utf8",
      stdio: ["pipe", "pipe", "ignore"],
    }).trim();
    const commit = execSync("git rev-parse --short HEAD", {
      encoding: "utf8",
      stdio: ["pipe", "pipe", "ignore"],
    }).trim();
    const remoteUrl = execSync("git remote get-url origin 2>/dev/null || echo ''", {
      encoding: "utf8",
      shell: true,
    }).trim();
    const repo = remoteUrl.replace(/.*\//, "").replace(/\.git$/, "") || "aesthetic-computer";
    return { branch, commit, repo };
  } catch {
    return { branch: "unknown", commit: "unknown", repo: "aesthetic-computer" };
  }
}

function getCommitMessage() {
  try {
    return execSync("git log -1 --pretty=%B", {
      encoding: "utf8",
      stdio: ["pipe", "pipe", "ignore"],
    }).trim();
  } catch {
    return "";
  }
}

async function main() {
  const userSub = process.env.ADMIN_SUB;
  const mongoConnStr = process.env.MONGODB_CONNECTION_STRING;
  const mongoName = process.env.MONGODB_NAME;

  if (!userSub || !mongoConnStr || !mongoName) {
    process.stderr.write(
      "store-opener: missing env (ADMIN_SUB, MONGODB_CONNECTION_STRING, MONGODB_NAME) — skipping\n"
    );
    process.exit(0); // Don't block the workflow
  }

  const source = process.argv[2] || "claude-code";
  const payload = source === "claude-code" ? await readStdin() : {};

  const sessionId = payload.session_id || null;
  const promptText = payload.prompt || "";

  // For claude-code: only store the very first prompt of each session
  if (source === "claude-code") {
    if (!promptText) {
      process.exit(0);
    }
    if (sessionId) {
      const markerFile = `/tmp/ac-opener-${sessionId}`;
      if (existsSync(markerFile)) {
        process.exit(0); // Already stored opener for this session
      }
      try {
        writeFileSync(markerFile, new Date().toISOString());
      } catch {
        // Non-fatal; proceed anyway
      }
    }
  }

  const git = getGitInfo();
  // Claude Code sets CLAUDE_MODEL in hook environment; fall back to known default
  const model =
    process.env.CLAUDE_MODEL ||
    process.env.ANTHROPIC_MODEL ||
    "claude-sonnet-4-6";

  const text =
    source === "post-commit"
      ? getCommitMessage() || `[commit: ${git.commit}]`
      : promptText;

  const doc = {
    user: userSub,
    text,
    model,
    when: new Date(),
    source,
    context: {
      repo: git.repo,
      branch: git.branch,
      commit: git.commit,
      cwd: payload.cwd || process.cwd(),
      session_id: sessionId,
    },
  };

  let client;
  try {
    client = new MongoClient(mongoConnStr, {
      serverSelectionTimeoutMS: 8000,
      connectTimeoutMS: 8000,
    });
    await client.connect();
    const db = client.db(mongoName);
    const collection = db.collection("openers");

    // Ensure indexes (idempotent)
    await collection.createIndex({ user: 1, when: -1 }, { background: true });
    await collection.createIndex(
      { "context.session_id": 1 },
      { background: true, sparse: true }
    );

    const result = await collection.insertOne(doc);
    const preview = text.slice(0, 60).replace(/\n/g, " ");
    process.stderr.write(
      `store-opener: ✓ [${source}] — "${preview}${text.length > 60 ? "…" : ""}" (id: ${result.insertedId})\n`
    );
  } catch (err) {
    process.stderr.write(`store-opener: error: ${err.message}\n`);
    // Exit 0 so hook failure doesn't block git/claude operations
  } finally {
    if (client) {
      try {
        await client.close();
      } catch {
        // ignore
      }
    }
  }
}

main();
