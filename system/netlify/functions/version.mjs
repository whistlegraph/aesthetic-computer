// Returns deployed commit hash vs latest GitHub commit
// Used by prompt.mjs to show version status on curtain UI

export default async (request) => {
  const deployedCommit = process.env.COMMIT_REF || "unknown";

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

    return new Response(
      JSON.stringify({
        deployed: deployedCommit.slice(0, 7),
        latest: latestCommit?.slice(0, 7),
        status,
        behindBy,
        timestamp: new Date().toISOString(),
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
