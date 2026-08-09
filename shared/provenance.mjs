// shared/provenance.mjs — one answer to "what code is this process running?"
//
// Every deploy script already knows the commit it checked out, and every health
// check already knows the port is up. Nothing joined the two, so a box could
// serve a four-hour-old commit and report itself healthy — which is exactly
// what session-server did on 2026-08-08, invisibly, until someone said so in
// chat.
//
// The contract is deliberately tiny, because it has to be cheap enough that all
// eight services adopt it: GET /health → { service, sha, startedAt }. A caller
// that knows knot's tip can then tell, without SSH, whether a box is running
// what was shipped.
//
// Resolution order:
//   1. AC_GIT_SHA — set this when the checkout isn't a git repo (containers,
//      unpacked tarballs, the AC Native kernel which bakes it at compile time).
//   2. `git rev-parse HEAD` in this file's repo.
//   3. null — reported honestly rather than guessed, so a missing sha reads as
//      "unknown" instead of silently passing a comparison.
//
// Resolved once at first call: a process cannot change commit without
// restarting, and a health endpoint should never shell out per request.

import { execFileSync } from "node:child_process";
import { join } from "node:path";

const REPO = join(import.meta.dirname, "..");

let resolved = null;

function resolveSha() {
  const fromEnv = process.env.AC_GIT_SHA?.trim();
  if (fromEnv) return fromEnv;
  try {
    return execFileSync("git", ["rev-parse", "HEAD"], {
      cwd: REPO,
      encoding: "utf8",
      timeout: 3000,
      stdio: ["ignore", "pipe", "ignore"],
    }).trim() || null;
  } catch {
    return null; // Not a repo, no git, or a shallow clone mid-surgery.
  }
}

export function provenance(service) {
  resolved ??= { sha: resolveSha(), startedAt: new Date().toISOString() };
  return { service, sha: resolved.sha, startedAt: resolved.startedAt };
}

// Does `sha` (what a box reports) contain `expected` (what knot has)? Answered
// by the caller, which is the only side that can hold both histories — the box
// itself has no idea it is behind.
export function shaMatches(reported, expected) {
  if (!reported || !expected) return null; // unknown, not "no"
  return reported.startsWith(expected) || expected.startsWith(reported);
}
