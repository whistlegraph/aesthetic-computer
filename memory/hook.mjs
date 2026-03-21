#!/usr/bin/env node
// memory/hook.mjs
// Local-first session memory hook for Claude/Codex workflows.
//
// Usage:
//   node memory/hook.mjs              # Claude hook, stdin JSON payload
//   node memory/hook.mjs post-commit  # Git post-commit hook

import { execSync } from "child_process";

import { commitEvent } from "./index.mjs";

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
  const source = process.argv[2] || "claude-code";
  const payload = source === "claude-code" ? await readStdin() : {};

  const sessionId = payload.session_id || process.env.AGENT_SESSION_ID || null;
  const promptText = payload.prompt || "";

  if (source === "claude-code" && !promptText) {
    process.exit(0);
  }

  const git = getGitInfo();
  const model =
    process.env.CLAUDE_MODEL ||
    process.env.ANTHROPIC_MODEL ||
    "claude-sonnet-4-6";

  const text =
    source === "post-commit"
      ? getCommitMessage() || `[commit: ${git.commit}]`
      : promptText;
  const provider =
    payload.provider ||
    process.env.AGENT_MEMORY_PROVIDER ||
    (source === "claude-code" ? "claude" : "system");
  const project =
    payload.project ||
    process.env.AGENT_MEMORY_PROJECT ||
    git.repo;
  const ticket =
    payload.ticket ||
    process.env.AGENT_MEMORY_TICKET ||
    null;
  const role =
    payload.role ||
    (source === "post-commit" ? "system" : "user");

  try {
    const localSessionId =
      source === "claude-code"
        ? sessionId || undefined
        : `post-commit:${git.repo}`;

    const result = await commitEvent({
      sessionId: localSessionId,
      provider,
      model,
      role,
      source,
      project,
      title: source === "claude-code" ? "Claude Session" : "Post-Commit Session",
      text,
      context: {
        repo: git.repo,
        branch: git.branch,
        commit: git.commit,
        cwd: payload.cwd || process.cwd(),
        session_id: sessionId,
      },
      metadata: {
        user_sub: process.env.ADMIN_SUB || null,
        ticket,
      },
    });

    process.stderr.write(
      `memory-hook: ✓ [${source}] session=${result.session_id} seq=${result.seq}\n`
    );
  } catch (error) {
    process.stderr.write(`memory-hook: error: ${error.message}\n`);
  }
}

main();
