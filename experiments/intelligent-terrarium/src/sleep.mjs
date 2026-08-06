import { execFileSync } from "node:child_process";
import { mkdir, readFile, stat } from "node:fs/promises";
import { join } from "node:path";
import { atomicWrite, canonical, hash } from "./canonical.mjs";
import { verifyRepository } from "./repository.mjs";

const ALLOWLIST = [
  ".gitignore",
  "seed.json",
  "journal",
  "checkpoints",
  "autobiography",
  "visitors",
  "manifest.json",
];

async function exists(path) {
  try { await stat(path); return true; } catch { return false; }
}

function git(root, args, options = {}) {
  return execFileSync("git", args, { cwd: root, encoding: "utf8", stdio: options.stdio || "pipe" }).trim();
}

function gitMaybe(root, args) {
  try { return git(root, args); } catch { return ""; }
}

function isAllowed(path) {
  return ALLOWLIST.some((allowed) => path === allowed || path.startsWith(`${allowed}/`));
}

async function ensureGit(root) {
  if (!(await exists(join(root, ".git")))) git(root, ["init", "-b", "main"]);
  if (!gitMaybe(root, ["config", "--get", "user.name"])) git(root, ["config", "user.name", "Intelligent Terrarium"]);
  if (!gitMaybe(root, ["config", "--get", "user.email"])) git(root, ["config", "user.email", "terrarium@localhost"]);
  git(root, ["config", "commit.gpgsign", "false"]);
  const ignorePath = join(root, ".gitignore");
  if (!(await exists(ignorePath))) await atomicWrite(ignorePath, ".runtime/\nquarantine/\n");
}

function autobiography(state, stateHash) {
  const handles = Object.keys(state.visitors).sort();
  const lastEpisodes = state.mind.episodes.slice(-8);
  return [
    "# Sleep reflection",
    "",
    `- Event sequence: ${state.lastSeq}`,
    `- Simulation tick: ${state.tick}`,
    `- State hash: \`${stateHash}\``,
    `- Visitors present at sleep: ${handles.length ? handles.join(", ") : "none"}`,
    `- Semantic sounds composed: ${state.sonicCount}`,
    `- Drives: curiosity ${state.mind.drives.curiosity}, rest ${state.mind.drives.rest}, social ${state.mind.drives.social}`,
    "",
    "## Recent remembered facts",
    "",
    ...(lastEpisodes.length ? lastEpisodes.map((episode) => `- tick ${episode.tick}: ${episode.kind} — ${episode.actor}`) : ["- none"]),
    "",
  ].join("\n");
}

export async function sleepCommit(repository, { day = "2026-07-23" } = {}) {
  const root = repository.root;
  await ensureGit(root);
  const manifestPath = join(root, "manifest.json");
  if (await exists(manifestPath)) {
    const previous = JSON.parse(await readFile(manifestPath, "utf8"));
    if (previous.lastSeq === repository.terrarium.state.lastSeq && previous.stateHash === repository.stateHash()) {
      return { status: "no-change", commit: git(root, ["rev-parse", "HEAD"]), ...previous };
    }
  }

  const state = repository.terrarium.snapshot();
  const stateHash = repository.stateHash();
  const checkpoint = { schema: 1, lastSeq: state.lastSeq, stateHash, state };
  const manifest = {
    schema: 1,
    lastSeq: state.lastSeq,
    tick: state.tick,
    stateHash,
    headRecordHash: repository.headRecordHash,
    seedHash: hash(repository.seedDocument),
    profile: repository.seedDocument.profile,
  };
  await mkdir(join(root, "checkpoints"), { recursive: true });
  await mkdir(join(root, "autobiography"), { recursive: true });
  await mkdir(join(root, "visitors"), { recursive: true });
  await atomicWrite(join(root, "checkpoints", "latest.json"), `${canonical(checkpoint)}\n`);
  await atomicWrite(join(root, "manifest.json"), `${canonical(manifest)}\n`);
  await atomicWrite(join(root, "visitors", "handles.json"), `${canonical({ handles: Object.keys(state.visitors).sort() })}\n`);
  await atomicWrite(join(root, "autobiography", `${day}.md`), autobiography(state, stateHash));

  const verification = await verifyRepository(root);
  if (verification.stateHash !== stateHash || verification.lastSeq !== state.lastSeq) {
    throw new Error("sleep replay verification failed");
  }

  const stagedBefore = git(root, ["diff", "--cached", "--name-only"]).split("\n").filter(Boolean);
  if (stagedBefore.some((path) => !isAllowed(path))) throw new Error("refusing to commit pre-staged path outside state allowlist");
  git(root, ["add", "--", ...ALLOWLIST]);
  const staged = git(root, ["diff", "--cached", "--name-only"]).split("\n").filter(Boolean);
  if (staged.some((path) => !isAllowed(path))) throw new Error("refusing to commit path outside state allowlist");
  if (!staged.length) return { status: "no-change", commit: git(root, ["rev-parse", "HEAD"]), ...manifest };

  const message = `sleep: events 1-${String(state.lastSeq).padStart(6, "0")} state ${stateHash.slice(0, 8)}`;
  git(root, ["commit", "--no-gpg-sign", "-m", message]);
  return { status: "committed", commit: git(root, ["rev-parse", "HEAD"]), staged, ...manifest };
}
