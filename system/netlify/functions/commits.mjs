// Returns paginated commits from the Tangled remote (git log via local mirror).
// Source of truth: tangled.org/aesthetic.computer/core (mirrored on knot.aesthetic.computer).

import fs from "fs";
import path from "path";
import { execFile } from "child_process";
import { promisify } from "util";

const execFileAsync = promisify(execFile);
const GIT_BRANCH = process.env.VERSION_GIT_BRANCH || "main";
const MAX_PER_PAGE = 100;
const FETCH_TTL_MS = 60 * 1000;

let lastFetchAt = 0;

function getRepoRoot() {
  const candidates = [
    path.resolve(process.cwd(), ".."),
    process.cwd(),
  ];
  return candidates.find((c) => fs.existsSync(path.join(c, ".git"))) || null;
}

async function git(args, repoRoot) {
  const { stdout } = await execFileAsync("git", ["-C", repoRoot, ...args], {
    timeout: 15000,
    maxBuffer: 8 * 1024 * 1024,
  });
  return stdout;
}

async function getPreferredRemote(repoRoot) {
  if (process.env.VERSION_GIT_REMOTE) return process.env.VERSION_GIT_REMOTE;
  const remotes = (await git(["remote"], repoRoot))
    .split("\n")
    .map((r) => r.trim())
    .filter(Boolean);
  if (remotes.includes("tangled")) return "tangled";
  if (remotes.includes("origin")) return "origin";
  return remotes[0] || "origin";
}

async function maybeFetch(remote, repoRoot) {
  if (Date.now() - lastFetchAt < FETCH_TTL_MS) return;
  try {
    await git(["fetch", "--quiet", remote, GIT_BRANCH], repoRoot);
    lastFetchAt = Date.now();
  } catch {
    // Ignore; stale data is better than an error.
  }
}

function parseCommits(raw) {
  const blocks = raw.split("<<COMMIT>>").map((b) => b.trim()).filter(Boolean);
  const commits = [];
  for (const block of blocks) {
    const lines = block.split("\n");
    const header = lines[0];
    const parts = header.split("|");
    if (parts.length < 6) continue;
    const [sha, parentField, author, email, date, ...messageParts] = parts;
    const message = messageParts.join("|");

    let additions = 0;
    let deletions = 0;
    let files = 0;
    for (let i = 1; i < lines.length; i++) {
      const line = lines[i].trim();
      if (!line) continue;
      const [a, d, f] = line.split("\t");
      if (!f) continue;
      files += 1;
      if (a === "-" || d === "-") continue;
      additions += Number(a) || 0;
      deletions += Number(d) || 0;
    }

    commits.push({
      sha,
      shortSha: sha.slice(0, 7),
      parents: parentField ? parentField.split(" ").filter(Boolean).length : 0,
      author,
      email,
      date,
      message,
      additions,
      deletions,
      files,
    });
  }
  return commits;
}

export default async (request) => {
  const url = new URL(request.url);
  const page = Math.max(1, parseInt(url.searchParams.get("page") || "1", 10));
  const perPage = Math.min(
    MAX_PER_PAGE,
    Math.max(1, parseInt(url.searchParams.get("per_page") || "30", 10)),
  );
  const skip = (page - 1) * perPage;

  const repoRoot = getRepoRoot();
  if (!repoRoot) {
    return new Response(
      JSON.stringify({ error: "repo not found", commits: [] }),
      { status: 500, headers: { "Content-Type": "application/json" } },
    );
  }

  try {
    const remote = await getPreferredRemote(repoRoot);
    await maybeFetch(remote, repoRoot);

    const raw = await git(
      [
        "log",
        `${remote}/${GIT_BRANCH}`,
        `--skip=${skip}`,
        `--max-count=${perPage}`,
        "--pretty=format:<<COMMIT>>%H|%P|%an|%ae|%cI|%s",
        "--numstat",
      ],
      repoRoot,
    );

    const commits = parseCommits(raw);

    return new Response(
      JSON.stringify({
        page,
        perPage,
        hasMore: commits.length === perPage,
        commits,
      }),
      {
        headers: {
          "Content-Type": "application/json",
          "Cache-Control": "public, max-age=60",
        },
      },
    );
  } catch (e) {
    return new Response(
      JSON.stringify({ error: e.message, commits: [] }),
      { status: 500, headers: { "Content-Type": "application/json" } },
    );
  }
};

export const config = { path: "/api/commits" };
