// Returns deployed commit hash vs latest GitHub commit
// Used by prompt.mjs to show version status on curtain UI

import fs from "fs";
import path from "path";

// Get deployed commit from file written during build (COMMIT_REF is not available at runtime)
function getDeployedCommit() {
  try {
    // Try reading from the .commit-ref file created during build
    const commitRefPath = path.join(process.cwd(), "public", ".commit-ref");
    const commit = fs.readFileSync(commitRefPath, "utf8").trim();
    if (commit && commit.length >= 7) {
      return commit;
    }
  } catch (e) {
    // File doesn't exist or can't be read
  }
  return "unknown";
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
    // Fetch latest commits from GitHub (public, no auth needed)
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

    // Find how many commits behind
    let behindBy = 0;
    if (deployedCommit !== "unknown" && latestCommit) {
      const idx = commits.findIndex((c) =>
        c.sha.startsWith(deployedCommit.slice(0, 7))
      );
      if (idx === -1) {
        behindBy = 50; // More than 50 commits behind or commit not found
      } else {
        behindBy = idx;
      }
    }

    const status = behindBy === 0 ? "current" : "behind";

    // Extract recent commits for ticker display (last 10)
    const recentCommits = commits.slice(0, 10).map((c) => ({
      hash: c.sha.slice(0, 7),
      message: c.commit?.message?.split("\n")[0]?.slice(0, 60) || "no message", // First line, truncated
      author: c.commit?.author?.name || c.author?.login || "unknown",
      date: c.commit?.author?.date,
    }));

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
