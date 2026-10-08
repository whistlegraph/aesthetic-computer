import { readFile, lstat } from "node:fs/promises";
import { join } from "node:path";
import { git, hash, relativeFile, run } from "./io.mjs";

export const handQuestions = {
  mind: "Can one reader hold the changed module's purpose and state in mind? Explain the seam.",
  names: "Do local names use the existing idiom, with longer public names only where earned?",
  comments: "Do comments explain a reason that code cannot? Remove restatements and banner walls.",
  guards: "Which guards face network, user or audio boundaries? Which internal guards are redundant?",
  leaves: "Does the piece retain its lifecycle and stay a small leaf? Is any infrastructure structure justified?",
  reuse: "Which verified helpers were reused or rejected, and why do their contracts fit or not fit?",
};

export function measure(text) {
  const count = re => (text.match(re) || []).length;
  const lines = text ? text.split("\n") : [];
  return { lines: lines.length, comments: lines.filter(s => /^\s*(\/\/|\/\*|\*)/.test(s)).length,
    tryCatch: count(/\btry\s*\{/g), nullGuards: count(/\?\.|\?\?|[!=]==\s*(?:null|undefined)/g),
    banners: count(/\/\/\s*[=*-]{4,}/g), jsdoc: count(/\/\*\*/g),
    emojiLogs: lines.filter(s => /console\.(?:log|warn|error|info)/.test(s) && /\p{Extended_Pictographic}/u.test(s)).length };
}

export async function changedFiles(repo, base) {
  const tracked = (await git(repo, "diff", "--name-only", "-z", base, "--")).split("\0");
  const added = (await git(repo, "ls-files", "--others", "--exclude-standard", "-z")).split("\0");
  return [...new Set([...tracked, ...added].filter(Boolean))].sort().map(relativeFile);
}

export async function snapshot(repo, base) {
  const files = await changedFiles(repo, base), rows = [];
  for (const file of files) {
    const path = join(repo, file);
    let stat;
    try { stat = await lstat(path); } catch (error) { if (error.code !== "ENOENT") throw error; }
    if (!stat) { rows.push({ file, deleted: true }); continue; }
    if (!stat.isFile() || stat.isSymbolicLink() || stat.size > 1024 * 1024) throw new Error("Patch contains nonregular or oversized file");
    rows.push({ file, hash: hash(await readFile(path)), executable: Boolean(stat.mode & 0o111) });
  }
  const head = await git(repo, "rev-parse", "HEAD");
  return { base, head, files: rows, digest: hash(JSON.stringify({ base, files: rows })) };
}

export async function committedSnapshot(repo, base, commit) {
  const names = (await git(repo, "diff", "--name-only", "-z", base, commit, "--")).split("\0").filter(Boolean).sort();
  const files = [];
  for (const name of names) {
    const file = relativeFile(name), entry = await git(repo, "ls-tree", commit, "--", file);
    if (!entry) { files.push({ file, deleted: true }); continue; }
    const mode = entry.split(" ")[0];
    if (!["100644", "100755"].includes(mode)) throw new Error("Committed patch contains a nonregular file");
    const result = await run(["git", "show", `${commit}:${file}`], { cwd: repo });
    if (result.code !== 0) throw new Error("Committed source unavailable");
    files.push({ file, hash: hash(result.stdout), executable: mode === "100755" });
  }
  return { base, head: commit, files, digest: hash(JSON.stringify({ base, files })) };
}

export async function handCheck(repo, base) {
  const state = await snapshot(repo, base), metrics = [];
  for (const row of state.files) {
    let before = "";
    try { before = await git(repo, "show", `${base}:${row.file}`); } catch {}
    const after = row.deleted ? "" : await readFile(join(repo, row.file), "utf8");
    metrics.push({ file: row.file, before: measure(before), after: measure(after) });
  }
  return { ...state, mechanical: { metrics, note: "Regex counts are approximate observations, not a style score or proof of correctness." },
    humanReview: { required: true, questions: handQuestions },
    sources: ["HAND.md", "papers/arxiv-hand-and-loop/handloop.tex",
      "9b3ee0e4349376ef9f3c10fd51fa20d6f1c920c9:system/public/aesthetic.computer/lib/help.mjs",
      "c5644f85b0:system/public/aesthetic.computer/lib/num.mjs"] };
}

export function validateReview(review, packet, reuse) {
  if (review.digest !== packet.digest || review.base !== packet.base || review.decision !== "approve" ||
      !["human", "independent-agent"].includes(review.kind) || typeof review.reviewer !== "string" || review.reviewer.length < 2 ||
      Object.keys(handQuestions).some(k => typeof review.hand?.[k] !== "string" || review.hand[k].trim().length < 15) ||
      typeof review.userImpact !== "string" || review.userImpact.length < 15 || typeof review.reproduction !== "string" || review.reproduction.length < 15 ||
      typeof review.scope !== "string" || review.scope.length < 15 ||
      typeof review.privacy !== "string" || review.privacy.length < 15 || !Array.isArray(review.reuse) || !review.reuse.length) throw new Error("Incomplete review or revision mismatch");
  for (const item of review.reuse) {
    const entry = reuse.entries.find(e => e.id === item.id);
    if (!entry || entry.stale || item.sourceHash !== entry.sourceHash || !["used", "considered"].includes(item.decision) || typeof item.reason !== "string" || item.reason.length < 15) throw new Error("Reuse review is stale or incomplete");
  }
  return review;
}
