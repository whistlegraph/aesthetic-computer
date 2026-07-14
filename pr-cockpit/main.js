// PR Review Cockpit — main process.
// Responsibilities:
//   - create the BrowserWindow that hosts the renderer (which owns the <webview> grid)
//   - shell out to `gh` to list PRs and hand them to the renderer over IPC
//   - open external URLs in the real browser on request
//
// This app is a personal dev tool. It does no writes to GitHub — it only
// reads the PR list and renders each PR's page inside a real browser context.

"use strict";

const { app, BrowserWindow, ipcMain, shell } = require("electron");
const { execFile } = require("node:child_process");
const path = require("node:path");
const fs = require("node:fs");

// ---------------------------------------------------------------------------
// CONFIG — which repo / author the cockpit points at.
//
// Real values live in a gitignored `cockpit.config.json` next to this file
// (copy `cockpit.config.example.json`). Environment variables override the
// file. The defaults below are neutral placeholders so nothing private is ever
// committed. Precedence: env vars > cockpit.config.json > defaults.
// ---------------------------------------------------------------------------
function loadConfig() {
  const defaults = {
    repo: "owner/repo", // e.g. "cli/cli"
    author: "", // "" = fetch ALL open PRs (no --author filter)
    me: "", // your login — powers the review-priority "mine" score
    state: "open",
    limit: 100,
    onlyToday: false, // true = only PRs created today (local date)
  };

  let fileCfg = {};
  try {
    const p = path.join(__dirname, "cockpit.config.json");
    if (fs.existsSync(p)) fileCfg = JSON.parse(fs.readFileSync(p, "utf8"));
  } catch (err) {
    console.warn("cockpit.config.json is unreadable:", err.message);
  }

  const env = {};
  const e = process.env;
  if (e.COCKPIT_REPO) env.repo = e.COCKPIT_REPO;
  if (e.COCKPIT_AUTHOR !== undefined) env.author = e.COCKPIT_AUTHOR;
  if (e.COCKPIT_ME) env.me = e.COCKPIT_ME;
  if (e.COCKPIT_STATE) env.state = e.COCKPIT_STATE;
  if (e.COCKPIT_LIMIT) env.limit = Number(e.COCKPIT_LIMIT) || defaults.limit;
  if (e.COCKPIT_ONLY_TODAY) env.onlyToday = e.COCKPIT_ONLY_TODAY === "true";

  return { ...defaults, ...fileCfg, ...env };
}

const CONFIG = loadConfig();
// ---------------------------------------------------------------------------

let mainWindow = null;

function createWindow() {
  mainWindow = new BrowserWindow({
    width: 1600,
    height: 1000,
    fullscreen: true,
    backgroundColor: "#0d1117",
    title: "PR Review Cockpit",
    webPreferences: {
      preload: path.join(__dirname, "preload.js"),
      contextIsolation: true,
      nodeIntegration: false,
      // Enable <webview> tags — these are real browser contexts (unlike
      // iframes) so github.com's X-Frame-Options: DENY does not block them.
      webviewTag: true,
    },
  });

  mainWindow.loadFile(path.join(__dirname, "renderer", "index.html"));
  mainWindow.on("closed", () => {
    mainWindow = null;
  });
}

// Run `gh pr list ...` and return parsed JSON (array of PRs), or throw a
// friendly Error the renderer can display.
function listPullRequests() {
  const args = [
    "pr",
    "list",
    "--repo",
    CONFIG.repo,
    "--state",
    CONFIG.state,
    "--json",
    "number,title,url,statusCheckRollup,isDraft,reviewDecision,mergeable,createdAt,updatedAt,author,reviewRequests",
    "--limit",
    String(CONFIG.limit),
  ];

  // Only constrain by author when one is configured; empty means "all PRs".
  if (CONFIG.author) {
    args.push("--author", CONFIG.author);
  }

  return new Promise((resolve, reject) => {
    execFile(
      "gh",
      args,
      { maxBuffer: 20 * 1024 * 1024, timeout: 30000 },
      (err, stdout, stderr) => {
        if (err) {
          if (err.code === "ENOENT") {
            reject(
              new Error(
                "`gh` CLI not found on PATH. Install GitHub CLI and run `gh auth login`."
              )
            );
            return;
          }
          const detail = (stderr || err.message || "").trim();
          reject(
            new Error(
              `gh failed (exit ${err.code ?? "?"}): ${detail || "unknown error"}`
            )
          );
          return;
        }
        try {
          const raw = JSON.parse(stdout || "[]");
          resolve(raw.map(deriveStatus));
        } catch (parseErr) {
          reject(
            new Error(`Could not parse gh JSON output: ${parseErr.message}`)
          );
        }
      }
    );
  });
}

// Collapse statusCheckRollup + reviewDecision into one of:
//   "fail" (red) | "run" (yellow, in-progress) | "ok" (green) | "none"
function deriveStatus(pr) {
  const rollup = Array.isArray(pr.statusCheckRollup)
    ? pr.statusCheckRollup
    : [];

  let anyFailure = false;
  let anyPending = false;
  let anyCheck = false;

  for (const check of rollup) {
    anyCheck = true;
    // Two shapes appear: CheckRun (status/conclusion) and StatusContext (state).
    const conclusion = (check.conclusion || "").toUpperCase();
    const status = (check.status || "").toUpperCase();
    const state = (check.state || "").toUpperCase();

    if (
      conclusion === "FAILURE" ||
      conclusion === "TIMED_OUT" ||
      conclusion === "CANCELLED" ||
      conclusion === "ACTION_REQUIRED" ||
      state === "FAILURE" ||
      state === "ERROR"
    ) {
      anyFailure = true;
    } else if (
      status === "QUEUED" ||
      status === "IN_PROGRESS" ||
      status === "PENDING" ||
      state === "PENDING" ||
      (conclusion === "" && status !== "COMPLETED" && state === "")
    ) {
      anyPending = true;
    }
  }

  const reviewDecision = pr.reviewDecision || null;
  const approved = (reviewDecision || "").toUpperCase() === "APPROVED";

  let ciStatus;
  if (anyFailure) ciStatus = "fail";
  else if (anyPending) ciStatus = "run";
  else if (anyCheck) ciStatus = "ok"; // checks passed (approval is tracked separately)
  else ciStatus = "none";

  const authorLogin = (pr.author && pr.author.login) || "";
  const me = CONFIG.me || "";
  const reviewRequests = Array.isArray(pr.reviewRequests)
    ? pr.reviewRequests
    : [];

  return {
    number: pr.number,
    title: pr.title,
    url: pr.url,
    ciStatus,
    approved,
    isDraft: !!pr.isDraft,
    reviewDecision,
    mergeable: pr.mergeable || "UNKNOWN",
    createdAt: pr.createdAt || "",
    updatedAt: pr.updatedAt || "",
    authorLogin,
    isMine: !!me && authorLogin === me,
    reviewRequestedFromMe:
      !!me && reviewRequests.some((r) => r && r.login === me),
  };
}

// --- IPC ---------------------------------------------------------------------

ipcMain.handle("prs:list", async () => {
  try {
    let prs = await listPullRequests();
    if (CONFIG.onlyToday) {
      const start = new Date();
      start.setHours(0, 0, 0, 0); // local start-of-today
      prs = prs.filter((p) => p.createdAt && new Date(p.createdAt) >= start);
    }
    return { ok: true, prs, config: CONFIG };
  } catch (err) {
    return { ok: false, error: err.message, config: CONFIG };
  }
});

// Perform a real GitHub review action via the `gh` CLI (authed on this host).
// These go through `gh`, NOT the webviews, so they work even when the tiles'
// github.com session isn't signed in.
//
// SAFETY: the review/comment body is passed as a discrete argv element to
// execFile — it is NEVER interpolated into a shell string, so arbitrary text
// (quotes, backticks, $()) cannot break out into command execution.
function runPrAction({ kind, number, body }) {
  const num = String(number).trim();
  if (!/^\d+$/.test(num)) {
    return Promise.resolve({ ok: false, error: `Bad PR number: ${number}` });
  }
  const text = typeof body === "string" ? body.trim() : "";

  let args;
  switch (kind) {
    case "approve":
      // Optional body; only include --body when non-empty.
      args = ["pr", "review", num, "--repo", CONFIG.repo, "--approve"];
      if (text) args.push("--body", text);
      break;
    case "request-changes":
      if (!text) {
        return Promise.resolve({
          ok: false,
          error: "Request-changes needs a non-empty comment.",
        });
      }
      args = [
        "pr",
        "review",
        num,
        "--repo",
        CONFIG.repo,
        "--request-changes",
        "--body",
        text,
      ];
      break;
    case "comment":
      if (!text) {
        return Promise.resolve({
          ok: false,
          error: "Comment needs a non-empty body.",
        });
      }
      args = ["pr", "comment", num, "--repo", CONFIG.repo, "--body", text];
      break;
    case "dryrun":
      // Diagnostic only — echoes the argv that WOULD run, fires nothing.
      return Promise.resolve({
        ok: true,
        dryrun: true,
        output: `gh ${["pr", "…", num, "--repo", CONFIG.repo].join(" ")}`,
      });
    default:
      return Promise.resolve({ ok: false, error: `Unknown action: ${kind}` });
  }

  return new Promise((resolve) => {
    execFile(
      "gh",
      args,
      { maxBuffer: 4 * 1024 * 1024, timeout: 30000 },
      (err, stdout, stderr) => {
        if (err) {
          if (err.code === "ENOENT") {
            resolve({
              ok: false,
              error: "`gh` CLI not found on PATH.",
            });
            return;
          }
          const detail = (stderr || err.message || "").trim();
          resolve({
            ok: false,
            error: `gh failed (exit ${err.code ?? "?"}): ${detail || "unknown error"}`,
          });
          return;
        }
        resolve({
          ok: true,
          output: (stdout || stderr || "done").trim(),
        });
      }
    );
  });
}

ipcMain.handle("pr-action", async (_event, payload) => {
  try {
    return await runPrAction(payload || {});
  } catch (err) {
    return { ok: false, error: err.message };
  }
});

ipcMain.handle("shell:openExternal", async (_event, url) => {
  if (typeof url === "string" && /^https?:\/\//.test(url)) {
    await shell.openExternal(url);
    return { ok: true };
  }
  return { ok: false, error: "Refused to open non-http(s) url." };
});

// --- lifecycle ---------------------------------------------------------------

app.whenReady().then(() => {
  createWindow();
  app.on("activate", () => {
    if (BrowserWindow.getAllWindows().length === 0) createWindow();
  });
});

app.on("window-all-closed", () => {
  if (process.platform !== "darwin") app.quit();
});
