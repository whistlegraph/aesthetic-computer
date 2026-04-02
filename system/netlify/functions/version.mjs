// Returns deployed commit hash vs latest Tangled origin commit.
// Used by prompt.mjs to show version status on curtain UI

import fs from "fs";
import path from "path";
import { execFile } from "child_process";
import { promisify } from "util";

const execFileAsync = promisify(execFile);
const GIT_REMOTE_OVERRIDE = process.env.VERSION_GIT_REMOTE || "";
const GIT_BRANCH = process.env.VERSION_GIT_BRANCH || "main";
const RECENT_COMMIT_COUNT = 10;
const HISTORY_SCAN_LIMIT = 50;

// Get deployed commit from file written during build/deploy
function getDeployedCommit() {
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

async function getLatestFromGitHub(deployedCommit) {
  const res = await fetch(
    "https://api.github.com/repos/whistlegraph/aesthetic-computer/commits?per_page=50",
    {
      headers: {
        "User-Agent": "aesthetic-computer",
        Accept: "application/vnd.github.v3+json",
      },
    }
  );

  if (!res.ok) {
    throw new Error(`GitHub API returned ${res.status}`);
  }

  const commits = await res.json();
  const latestCommit = commits[0]?.sha;

  let behindBy = 0;
  if (deployedCommit !== "unknown" && latestCommit) {
    const idx = commits.findIndex((c) =>
      c.sha.startsWith(deployedCommit.slice(0, 7))
    );
    behindBy = idx === -1 ? HISTORY_SCAN_LIMIT : idx;
  }

  return {
    latestCommit,
    behindBy,
    recentCommits: commits.slice(0, RECENT_COMMIT_COUNT).map((c) => ({
      hash: c.sha.slice(0, 7),
      message: c.commit?.message?.split("\n")[0]?.slice(0, 60) || "no message",
      author: c.commit?.author?.name || c.author?.login || "unknown",
      date: c.commit?.author?.date,
    })),
  };
}

export default async (request) => {
  const deployedCommit = getDeployedCommit();
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
    const repoRoot = getRepoRoot();
    const {
      latestCommit,
      behindBy,
      recentCommits,
    } = repoRoot
      ? await getLatestFromTangled(repoRoot, deployedCommit)
      : await getLatestFromGitHub(deployedCommit);

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
