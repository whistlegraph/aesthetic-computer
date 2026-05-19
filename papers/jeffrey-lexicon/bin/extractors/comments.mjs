// comments.mjs — extract tokens from code comment lines authored by jeffrey.
// Walks a curated set of jeffrey-heavy roots, runs `git blame --line-porcelain`
// per file, and emits tokens only for comment-prefixed lines whose blame author
// matches a jeffrey identity.

import { execFile } from "node:child_process";
import { readdir } from "node:fs/promises";
import { join, relative } from "node:path";
import { promisify } from "node:util";

const exec = promisify(execFile);
const TOKEN_RE = /[a-zA-Z][a-zA-Z0-9'-]*/g;
const COMMENT_RE = /^\s*(?:\/\/+|;+|#+|--+|\/\*+|\*+)\s?(.*?)\s*\*?\/?\s*$/;

const ROOTS = [
  "system/public/aesthetic.computer/disks",
  "system/public/aesthetic.computer/lib",
  "system/netlify/functions",
  "session-server",
  "shared",
  "slab",
  "papers/bin",
  "recap/bin",
  "pop/bin",
];

const EXTS = [".mjs", ".lisp", ".js", ".swift"];

const SKIP_DIRS = new Set([
  "node_modules",
  "vendor",
  "dist",
  "build",
  "out",
  "target",
]);

async function* walk(dir) {
  let ents;
  try {
    ents = await readdir(dir, { withFileTypes: true });
  } catch {
    return;
  }
  for (const e of ents) {
    if (e.name.startsWith(".") || SKIP_DIRS.has(e.name)) continue;
    const p = join(dir, e.name);
    if (e.isDirectory()) yield* walk(p);
    else if (EXTS.some((x) => e.name.endsWith(x))) yield p;
  }
}

function* tokenize(text) {
  for (const m of text.matchAll(TOKEN_RE)) {
    const raw = m[0];
    if (raw.length < 2) continue;
    yield { raw, lower: raw.toLowerCase() };
  }
}

export async function* extract({ manifest, repo }) {
  const jeffrey = new Set(manifest.author_identities);
  let filesSeen = 0;
  let linesKept = 0;

  for (const root of ROOTS) {
    const abs = join(repo, root);
    for await (const file of walk(abs)) {
      filesSeen++;
      let stdout;
      try {
        ({ stdout } = await exec(
          "git",
          ["-C", repo, "blame", "--line-porcelain", "--", relative(repo, file)],
          { maxBuffer: 256 * 1024 * 1024 },
        ));
      } catch {
        continue;
      }

      const lines = stdout.split("\n");
      let curAuthor = null;
      let curTime = null;
      let curLineNo = null;

      for (const l of lines) {
        if (/^[0-9a-f]{40} /.test(l)) {
          const parts = l.split(" ");
          curLineNo = parseInt(parts[2], 10);
          curAuthor = null;
          curTime = null;
        } else if (l.startsWith("author ")) {
          curAuthor = l.slice(7);
        } else if (l.startsWith("author-time ")) {
          curTime = parseInt(l.slice(12), 10);
        } else if (l.startsWith("\t")) {
          const src = l.slice(1);
          if (!jeffrey.has(curAuthor)) continue;
          const m = src.match(COMMENT_RE);
          if (!m || !m[1]) continue;
          // Skip lines that look like commented-out code (have suspicious chars).
          if (/[{}();=]{2,}/.test(m[1])) continue;
          const ts = new Date((curTime || 0) * 1000).toISOString();
          const rel = relative(repo, file);
          const ctx = src.trim().slice(0, 120);
          let kept = false;
          for (const { raw, lower } of tokenize(m[1])) {
            kept = true;
            yield {
              token: lower,
              casing: raw,
              src: "comments",
              ref: `${rel}:${curLineNo}`,
              ts,
              context: ctx,
            };
          }
          if (kept) linesKept++;
        }
      }
    }
  }
  console.error(`       comments: ${filesSeen} files scanned, ${linesKept} jeffrey-authored comment lines kept`);
}
