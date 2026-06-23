#!/usr/bin/env node
// deploy-status.mjs — a tiny deploy-status bridge for the slab menubar.
//
// Reports whether a repo's latest commit on each watched branch has finished
// deploying, as JSON the Swift menubar renders into a `Deploy` submenu
// (mirrors `asana`). The signal is Cloudflare's "Workers Builds" GitHub
// check-runs — one per worker per environment (e.g. `Workers Builds:
// api-staging`) — so "deployed or not" is just whether those checks have all
// gone green on the branch tip. No Cloudflare API token needed; the same
// GitHub read access slab already uses is enough.
//
// This file ships in the PUBLIC aesthetic.computer repo, so it carries NO
// repo names or tokens: the watched targets and the GitHub PAT live in an
// UNTRACKED config at ~/.config/slab/deploy-status.json (see
// `deploy-status config`). If no token is set there, we fall back to
// `gh auth token` for machines where the GitHub CLI is signed in.
//
// Subcommands:
//   deploy-status status [--target <name>] [--env <name>]   JSON summary (default)
//   deploy-status config                                    print/create config stub
//   deploy-status open [--target <name>] [--env <name>]     open the deploy in a browser
//
// Per-repo: `targets` is a list, so this watches one repo (fuser) today and
// extends to more by adding entries — `--target <name>` selects one.

import { spawnSync } from "node:child_process";
import { existsSync, mkdirSync, readFileSync, writeFileSync } from "node:fs";
import { homedir } from "node:os";
import { join, dirname } from "node:path";

const HOME = homedir();
const CONFIG_PATH =
  process.env.SLAB_DEPLOY_STATUS_CONFIG ||
  join(HOME, ".config", "slab", "deploy-status.json");
const API = "https://api.github.com";

// ─── config ──────────────────────────────────────────────────────────────

const CONFIG_STUB = {
  _README:
    "slab deploy-status config — UNTRACKED, never committed. List the repos " +
    "to watch under `targets`; each maps an environment name to the branch " +
    "whose tip deploys it. The deploy signal is Cloudflare 'Workers Builds' " +
    "check-runs named `<buildsPrefix> <worker>-<env>`.",
  token: "REPLACE_ME",
  tokenComment:
    "GitHub PAT with read access to the repos' commit statuses/checks. " +
    "Leave as REPLACE_ME (or blank) to fall back to `gh auth token`.",
  targets: [
    {
      name: "example",
      repo: "owner/repo",
      environments: { staging: "staging", production: "main" },
      buildsPrefix: "Workers Builds:",
    },
  ],
  default: "example",
};

function loadConfig() {
  if (!existsSync(CONFIG_PATH)) return null;
  try {
    const cfg = JSON.parse(readFileSync(CONFIG_PATH, "utf8"));
    if (!cfg || !Array.isArray(cfg.targets) || cfg.targets.length === 0)
      return null;
    const real = cfg.targets.filter((t) => t.repo && !/owner\/repo/.test(t.repo));
    if (real.length === 0) return null;
    return { ...cfg, targets: real };
  } catch {
    return null;
  }
}

function writeStub() {
  mkdirSync(dirname(CONFIG_PATH), { recursive: true });
  if (!existsSync(CONFIG_PATH)) {
    writeFileSync(CONFIG_PATH, JSON.stringify(CONFIG_STUB, null, 2) + "\n");
  }
}

// Ask the GitHub CLI for a token. The menubar spawns us under launchd's
// minimal PATH (no Homebrew/fnm shims), so `gh` won't be on PATH — resolve it
// at the usual absolute locations, mirroring the wrapper's node lookup.
function ghToken() {
  const candidates = [
    "gh",
    "/opt/homebrew/bin/gh",
    "/usr/local/bin/gh",
    `${HOME}/.local/bin/gh`,
  ];
  for (const bin of candidates) {
    try {
      const r = spawnSync(bin, ["auth", "token"], { encoding: "utf8" });
      if (r.status === 0 && r.stdout.trim()) return r.stdout.trim();
    } catch {
      // try the next candidate
    }
  }
  return null;
}

// Token precedence: config → env → `gh auth token` (CLI sign-in).
function resolveToken(cfg) {
  if (cfg?.token && !/REPLACE_ME/.test(cfg.token)) return cfg.token;
  if (process.env.GH_TOKEN) return process.env.GH_TOKEN;
  if (process.env.GITHUB_TOKEN) return process.env.GITHUB_TOKEN;
  return ghToken();
}

// ─── GitHub REST ───────────────────────────────────────────────────────────

async function gh(token, path) {
  const res = await fetch(`${API}${path}`, {
    headers: {
      Authorization: `Bearer ${token}`,
      Accept: "application/vnd.github+json",
      "X-GitHub-Api-Version": "2022-11-28",
      "User-Agent": "slab-deploy-status",
    },
  });
  if (!res.ok) {
    const err = new Error(`github ${res.status}`);
    err.status = res.status;
    throw err;
  }
  return res.json();
}

// ─── deploy state ────────────────────────────────────────────────────────

// Roll the per-worker check-runs for one branch tip into a single state.
//   deployed → every Workers Build succeeded
//   building → at least one still queued/in-progress (none failing)
//   failing  → at least one concluded unsuccessfully
//   none     → no Workers Builds checks on this commit
function rollUp(apps) {
  if (apps.length === 0) return "none";
  if (apps.some((a) => a.state === "failing")) return "failing";
  if (apps.some((a) => a.state === "building")) return "building";
  return "deployed";
}

const GLYPH = { deployed: "✓", building: "⏳", failing: "✗", none: "·" };

function appState(run) {
  if (run.status !== "completed") return "building";
  return run.conclusion === "success" || run.conclusion === "neutral"
    ? "deployed"
    : "failing";
}

async function envStatus(token, repo, env, branch, prefix) {
  const head = await gh(token, `/repos/${repo}/commits/${branch}`);
  const sha = head.sha;
  const checks = await gh(
    token,
    `/repos/${repo}/commits/${sha}/check-runs?per_page=100`,
  );
  const wantSuffix = `-${env}`;
  const apps = (checks.check_runs || [])
    .filter((r) => r.name.startsWith(prefix))
    .map((r) => ({
      name: r.name
        .slice(prefix.length)
        .trim()
        .replace(new RegExp(`${wantSuffix}$`), ""),
      raw: r.name.slice(prefix.length).trim(),
      state: appState(r),
      conclusion: r.conclusion,
      url: r.details_url || r.html_url,
      completedAt: r.completed_at,
    }))
    // Only the workers for THIS environment (suffix match), when discernible.
    .filter((a) => a.raw.endsWith(wantSuffix) || a.raw === a.name);
  const state = rollUp(apps);
  const completed = apps.map((a) => a.completedAt).filter(Boolean).sort();
  return {
    env,
    branch,
    sha: sha.slice(0, 7),
    message: (head.commit?.message || "").split("\n")[0],
    author: head.commit?.author?.name || head.author?.login || "",
    committedAt: head.commit?.author?.date || head.commit?.committer?.date || "",
    state,
    deployedAt: state === "deployed" ? completed[completed.length - 1] : "",
    apps: apps.map(({ raw, ...a }) => a),
    url: `https://github.com/${repo}/commit/${sha}`,
  };
}

function pickTarget(cfg, name) {
  const want = name || cfg.default;
  return cfg.targets.find((t) => t.name === want) || cfg.targets[0];
}

async function status(cfg, opts) {
  const token = resolveToken(cfg);
  if (!token)
    return {
      configured: false,
      label: "Deploy: no GitHub token",
    };
  const target = pickTarget(cfg, opts.target);
  const prefix = target.buildsPrefix || "Workers Builds:";
  const envMap = target.environments || { staging: "staging" };
  const wantEnvs =
    opts.env && opts.env !== "all"
      ? { [opts.env]: envMap[opts.env] }
      : envMap;

  const envs = [];
  for (const [env, branch] of Object.entries(wantEnvs)) {
    if (!branch) continue;
    try {
      envs.push(await envStatus(token, target.repo, env, branch, prefix));
    } catch (e) {
      envs.push({ env, branch, state: "error", error: String(e.message || e) });
    }
  }

  // Menubar parent label: "<target> <glyph> <env> …" newest-env-first.
  const summary = envs
    .map((e) => `${GLYPH[e.state] || "?"} ${e.env}`)
    .join("  ");
  return {
    configured: true,
    target: target.name,
    label: `${target.name}  ${summary}`.trim(),
    state: rollUp(
      envs.map((e) => ({ state: e.state === "deployed" ? "deployed" : e.state })),
    ),
    envs,
    url: envs[0]?.url || `https://github.com/${target.repo}`,
  };
}

// ─── CLI ────────────────────────────────────────────────────────────────

function parseFlags(argv) {
  const opts = {};
  for (let i = 0; i < argv.length; i++) {
    if (argv[i] === "--target") opts.target = argv[++i];
    else if (argv[i] === "--env") opts.env = argv[++i];
  }
  return opts;
}

async function main() {
  const [cmd = "status", ...rest] = process.argv.slice(2);
  const opts = parseFlags(rest);

  if (cmd === "config") {
    writeStub();
    console.log(CONFIG_PATH);
    return;
  }

  const cfg = loadConfig();

  if (cmd === "open") {
    if (!cfg) return console.log(CONFIG_PATH);
    const target = pickTarget(cfg, opts.target);
    const url = `https://github.com/${target.repo}/commits/${
      (target.environments || {})[opts.env || "staging"] || "staging"
    }`;
    spawnSync("open", [url]);
    return;
  }

  // status (default)
  if (!cfg) {
    console.log(
      JSON.stringify({
        configured: false,
        label: "Deploy: not set up",
        configPath: CONFIG_PATH,
      }),
    );
    return;
  }
  try {
    console.log(JSON.stringify(await status(cfg, opts)));
  } catch (e) {
    console.log(
      JSON.stringify({
        configured: true,
        label: `Deploy: ${e.status || ""} error`.trim(),
        error: String(e.message || e),
      }),
    );
  }
}

main();
