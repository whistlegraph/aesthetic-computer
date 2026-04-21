// Returns the most recent commit per piece in the disks directory.

import fs from "fs";
import path from "path";
import { execFile } from "child_process";
import { promisify } from "util";

const execFileAsync = promisify(execFile);
const DISKS_PATH = "system/public/aesthetic.computer/disks/";
const GIT_BRANCH = process.env.VERSION_GIT_BRANCH || "main";
const LOOK_BACK = 500; // commits to scan

function getRepoRoot() {
  const candidates = [path.resolve(process.cwd(), ".."), process.cwd()];
  return candidates.find((c) => fs.existsSync(path.join(c, ".git"))) || null;
}

async function git(args, repoRoot) {
  const { stdout } = await execFileAsync("git", ["-C", repoRoot, ...args], {
    timeout: 20000,
    maxBuffer: 16 * 1024 * 1024,
  });
  return stdout;
}

async function getPreferredRemote(repoRoot) {
  if (process.env.VERSION_GIT_REMOTE) return process.env.VERSION_GIT_REMOTE;
  const remotes = (await git(["remote"], repoRoot))
    .split("\n").map((r) => r.trim()).filter(Boolean);
  if (remotes.includes("tangled")) return "tangled";
  if (remotes.includes("origin")) return "origin";
  return remotes[0] || "origin";
}

function parseCommits(raw) {
  const blocks = raw.split("<<COMMIT>>").map((b) => b.trim()).filter(Boolean);
  const result = {};

  for (const block of blocks) {
    const lines = block.split("\n");
    const header = lines[0];
    const parts = header.split("|");
    if (parts.length < 5) continue;
    const [sha, , author, , date, ...msgParts] = parts;
    const message = msgParts.join("|").trim();

    for (let i = 1; i < lines.length; i++) {
      const line = lines[i].trim();
      if (!line) continue;
      const cols = line.split("\t");
      if (cols.length < 3) continue;
      const filePath = cols[2];
      if (!filePath.startsWith(DISKS_PATH)) continue;

      const basename = path.basename(filePath);
      const name = basename.replace(/\.(mjs|lisp|l5|js)$/, "");
      if (!name || name === basename) continue; // no recognized extension

      // Only keep the first (most recent) commit per piece
      if (!result[name]) {
        result[name] = { date, message, author, hash: sha.slice(0, 7) };
      }
    }
  }

  return result;
}

export default async (request) => {
  const repoRoot = getRepoRoot();
  if (!repoRoot) {
    return new Response(JSON.stringify({ commits: {} }), {
      status: 500,
      headers: { "Content-Type": "application/json" },
    });
  }

  try {
    const remote = await getPreferredRemote(repoRoot);

    const raw = await git(
      [
        "log",
        `${remote}/${GIT_BRANCH}`,
        `--max-count=${LOOK_BACK}`,
        "--pretty=format:<<COMMIT>>%H|%P|%an|%ae|%cI|%s",
        "--numstat",
        "--",
        DISKS_PATH,
      ],
      repoRoot,
    );

    const commits = parseCommits(raw);

    return new Response(JSON.stringify({ commits }), {
      headers: {
        "Content-Type": "application/json",
        "Cache-Control": "public, max-age=120",
      },
    });
  } catch (e) {
    return new Response(JSON.stringify({ error: e.message, commits: {} }), {
      status: 500,
      headers: { "Content-Type": "application/json" },
    });
  }
};

export const config = { path: "/api/piece-commits" };
