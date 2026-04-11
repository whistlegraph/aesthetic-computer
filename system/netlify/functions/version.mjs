// Returns deployed commit hash vs latest Tangled origin commit.
// Used by prompt.mjs to show version status on curtain UI

import fs from "fs";
import path from "path";
import { execFile } from "child_process";
import { promisify } from "util";

const execFileAsync = promisify(execFile);
const GIT_REMOTE_OVERRIDE = process.env.VERSION_GIT_REMOTE || "";
const GIT_BRANCH = process.env.VERSION_GIT_BRANCH || "main";
const TANGLED_REPO_URL =
  process.env.VERSION_TANGLED_REPO_URL ||
  "https://knot.aesthetic.computer/aesthetic.computer/core";
const RECENT_COMMIT_COUNT = 10;
const HISTORY_SCAN_LIMIT = 50;

// Get deployed commit from file written during build/deploy.
// In Netlify Dev (local), the .commit-ref on disk is usually a stale leftover
// from a previous deploy, so fall back to `git rev-parse HEAD` instead — the
// running code *is* the current checkout, there's no real "deployment".
async function getDeployedCommit(repoRoot) {
  if (process.env.NETLIFY_DEV === "true" && repoRoot) {
    try {
      return await git(["rev-parse", "HEAD"], repoRoot);
    } catch (e) {
      // Fall through to the file-based lookup
    }
  }
  // Check multiple locations: Netlify writes to cwd/public/, lith deploy writes to system/public/
  const candidates = [
    path.join(process.cwd(), "public", ".commit-ref"),
    path.join(process.cwd(), "..", "system", "public", ".commit-ref"),
  ];
  for (const commitRefPath of candidates) {
    try {
      const commit = fs.readFileSync(commitRefPath, "utf8").trim();
      if (commit && commit.length >= 7) {
        return commit;
      }
    } catch (e) {
      // File doesn't exist or can't be read — try next
    }
  }
  return "unknown";
}

function getRepoRoot() {
  const candidates = [
    path.resolve(process.cwd(), ".."),
    process.cwd(),
  ];

  return candidates.find((candidate) =>
    fs.existsSync(path.join(candidate, ".git"))
  ) || null;
}

async function git(args, repoRoot) {
  const { stdout } = await execFileAsync("git", ["-C", repoRoot, ...args], {
    timeout: 10000,
    maxBuffer: 1024 * 1024,
  });
  return stdout.trim();
}

function parseRecentCommits(rawLog) {
  if (!rawLog) return [];

  return rawLog
    .split("\n")
    .filter(Boolean)
    .map((line) => {
      const [hash = "", message = "no message", author = "unknown", date = null] = line.split("\t");
      return {
        hash: hash.slice(0, 7),
        message: message.slice(0, 60),
        author,
        date,
      };
    });
}

async function getPreferredRemote(repoRoot) {
  if (GIT_REMOTE_OVERRIDE) return GIT_REMOTE_OVERRIDE;

  const remotes = (await git(["remote"], repoRoot))
    .split("\n")
    .map((remote) => remote.trim())
    .filter(Boolean);
  if (remotes.includes("tangled")) return "tangled";
  if (remotes.includes("origin")) return "origin";
  return remotes[0] || "origin";
}

async function getLatestFromTangled(repoRoot, deployedCommit) {
  const gitRemote = await getPreferredRemote(repoRoot);
  await git(["fetch", "--quiet", gitRemote, GIT_BRANCH], repoRoot);

  const remoteRef = `${gitRemote}/${GIT_BRANCH}`;
  const latestCommit = await git(["rev-parse", remoteRef], repoRoot);
  let behindBy = 0;

  if (deployedCommit !== "unknown" && latestCommit) {
    try {
      behindBy = Number(
        await git(["rev-list", "--count", `${deployedCommit}..${remoteRef}`], repoRoot)
      ) || 0;
    } catch {
      behindBy = HISTORY_SCAN_LIMIT;
    }
  }

  const recentRaw = await git([
    "log",
    remoteRef,
    `--max-count=${RECENT_COMMIT_COUNT}`,
    "--pretty=format:%H\t%s\t%an\t%cI",
  ], repoRoot);

  return {
    latestCommit,
    behindBy,
    recentCommits: parseRecentCommits(recentRaw),
  };
}

async function getLatestFromTangledMirror(deployedCommit) {
  const output = await git(
    ["ls-remote", TANGLED_REPO_URL, `refs/heads/${GIT_BRANCH}`],
    process.cwd(),
  );
  const latestCommit = output.split(/\s+/)[0] || "";
  const behindBy =
    deployedCommit !== "unknown" &&
    latestCommit &&
    latestCommit.startsWith(deployedCommit.slice(0, 7))
      ? 0
      : HISTORY_SCAN_LIMIT;

  return {
    latestCommit,
    behindBy,
    recentCommits: latestCommit
      ? [{
          hash: latestCommit.slice(0, 7),
          message: "latest on Tangled",
          author: "tangled.org/aesthetic.computer/core",
          date: null,
        }]
      : [],
  };
}

export default async (request) => {
  const repoRoot = getRepoRoot();
  const deployedCommit = await getDeployedCommit(repoRoot);
  const url = new URL(request.url);
  const clientHash = url.searchParams.get("current");

  // Long-poll mode: if client sends ?current=<hash> matching deployed version,
  // wait ~4 seconds before responding (allows near-instant new-deploy detection)
  if (clientHash && clientHash === deployedCommit.slice(0, 7)) {
    await new Promise((r) => setTimeout(r, 4000));
    // Re-check isn't useful (same function instance), but the NEXT call after
    // a Netlify redeploy will hit the new function with a new .commit-ref
    return new Response(
      JSON.stringify({ changed: false, deployed: deployedCommit.slice(0, 7) }),
      {
        headers: {
          "Content-Type": "application/json",
          "Cache-Control": "no-cache",
        },
      }
    );
  }

  try {
    const {
      latestCommit,
      behindBy,
      recentCommits,
    } = repoRoot
      ? await getLatestFromTangled(repoRoot, deployedCommit)
      : await getLatestFromTangledMirror(deployedCommit);

    const status = behindBy === 0 ? "current" : "behind";

    return new Response(
      JSON.stringify({
        deployed: deployedCommit.slice(0, 7),
        latest: latestCommit?.slice(0, 7),
        status,
        behindBy,
        timestamp: new Date().toISOString(),
        recentCommits, // Include recent commits for ticker
      }),
      {
        headers: {
          "Content-Type": "application/json",
          "Cache-Control": "public, max-age=60", // Cache for 1 minute
        },
      }
    );
  } catch (e) {
    return new Response(
      JSON.stringify({
        deployed: deployedCommit.slice(0, 7),
        status: "unknown",
        error: e.message,
      }),
      {
        status: 200, // Return 200 even on error so UI can handle gracefully
        headers: {
          "Content-Type": "application/json",
          "Cache-Control": "public, max-age=30",
        },
      }
    );
  }
};

export const config = { path: "/api/version" };
