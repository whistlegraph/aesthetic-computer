// commits.mjs — extract tokens from commit messages authored by jeffrey.
// Uses `git log --author=<regex>` with the union of jeffrey identities,
// strips Claude co-author trailers, then tokenizes subject + body.

import { execFile } from "node:child_process";
import { promisify } from "node:util";

const exec = promisify(execFile);
const TOKEN_RE = /[a-zA-Z][a-zA-Z0-9'-]*/g;

function* tokenize(text) {
  if (!text) return;
  for (const m of text.matchAll(TOKEN_RE)) {
    const raw = m[0];
    if (raw.length < 2) continue;
    yield { raw, lower: raw.toLowerCase() };
  }
}

function stripGenerated(body) {
  return body
    .split(/\r?\n/)
    .filter((l) => !/^Co-Authored-By:\s*Claude/i.test(l))
    .filter((l) => !/Generated with.*Claude/i.test(l))
    .filter((l) => !/^\s*🤖/.test(l))
    .join("\n");
}

export async function* extract({ manifest, repo }) {
  const ids = manifest.author_identities;
  const pattern = ids.map((s) => s.replace(/[.*+?^${}()|[\]\\]/g, "\\$&")).join("|");

  const args = [
    "-C", repo,
    "log",
    "--no-merges",
    "-E",
    "-z",
    `--author=${pattern}`,
    "--pretty=format:%H%x1f%aI%x1f%an%x1f%s%x1f%b",
  ];

  const { stdout } = await exec("git", args, { maxBuffer: 1024 * 1024 * 1024 });
  const records = stdout.split("\0");

  let n = 0;
  for (const rec of records) {
    if (!rec.trim()) continue;
    const [hash, ts, , subject, body] = rec.split("\x1f");
    if (!hash) continue;
    const text = `${subject || ""}\n${stripGenerated(body || "")}`;
    const ctx = (subject || "").slice(0, 120);
    for (const { raw, lower } of tokenize(text)) {
      yield {
        token: lower,
        casing: raw,
        src: "commits",
        ref: `commits/${hash.slice(0, 7)}`,
        ts,
        context: ctx,
      };
    }
    n++;
  }
  console.error(`       commits: ${n} jeffrey-authored commits processed`);
}
