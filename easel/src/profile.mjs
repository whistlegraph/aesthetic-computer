// profile.mjs — decide how a session behaves from where it was opened.
//
// Easel began as a piece studio: open it anywhere, get a blank piece, a live
// channel, a QR on the rock and a public URL under your handle. "Pro" is the
// same terminal harness pointed at ordinary work — a client repo, a server —
// where none of that should happen: nothing publishes, no audience, and the
// engine runs with the user's own settings and MCP servers instead of Easel's
// isolation. "Private" is orthogonal: the Slab marker still says the session
// exists and whether it is working, but carries no subject, so the menubar
// writes no memoir of a client's brief.
//
// Both are decided once, at open, from the flags, the environment, and a small
// config of globs at ~/.config/easel/profiles.json — so a client directory can
// be private for good rather than remembered per session. The matcher is a
// few lines on purpose: `~`, `*` and `**` are all a path glob needs here, and a
// dependency for it would be the only one Easel has.
import { readFileSync, realpathSync } from "node:fs";
import { homedir } from "node:os";
import { join, resolve } from "node:path";

const EXAMPLE = {
  private: ["~/fuser/**", "~/ac-worktrees/fuser*/**"],
  pro: ["~/fuser/**"],
};

export function exampleConfig() {
  return JSON.stringify(EXAMPLE, null, 2);
}

// `~/a/**` matches ~/a itself and everything under it; `*` stays inside one
// path segment; `**` crosses them.
export function globToRegExp(pattern, home = homedir()) {
  let glob = String(pattern).trim();
  if (glob === "~") glob = home;
  else if (glob.startsWith("~/")) glob = home + glob.slice(1);
  glob = glob.replace(/\/+$/, "");
  let source = "";
  for (let i = 0; i < glob.length; i += 1) {
    const c = glob[i];
    if (c === "*" && glob[i + 1] === "*") {
      const before = glob[i - 1];
      const after = glob[i + 2];
      if ((before === "/" || before === undefined) && after === undefined) {
        // trailing `/**` — the directory and anything below it.
        source = source.replace(/\/$/, "");
        source += "(/.*)?";
      } else if (before === "/" && after === "/") {
        // `/**/` in the middle — zero or more segments.
        source += "(.*/)?";
        i += 1; // skip the following slash, already consumed
      } else {
        source += ".*";
      }
      i += 1;
      continue;
    }
    if (c === "*") { source += "[^/]*"; continue; }
    if (c === "?") { source += "[^/]"; continue; }
    source += /[.+^${}()|[\]\\]/.test(c) ? `\\${c}` : c;
  }
  return new RegExp(`^${source}$`);
}

export function globMatch(path, pattern, home = homedir()) {
  return globToRegExp(pattern, home).test(String(path).replace(/\/+$/, "") || "/");
}

function readConfig(configPath) {
  const clean = { private: [], pro: [] };
  let parsed;
  try {
    parsed = JSON.parse(readFileSync(configPath, "utf8"));
  } catch {
    return clean;
  }
  if (!parsed || typeof parsed !== "object") return clean;
  for (const key of ["private", "pro"]) {
    if (Array.isArray(parsed[key])) {
      clean[key] = parsed[key].filter((g) => typeof g === "string" && g.trim());
    }
  }
  return clean;
}

// A cwd is usually reached through a symlink or two (worktrees, ~/Desktop
// aliases); match the path as typed and as it really is.
function forms(cwd) {
  const typed = resolve(cwd);
  try {
    const real = realpathSync(typed);
    return real === typed ? [typed] : [typed, real];
  } catch {
    return [typed];
  }
}

function firstMatch(paths, globs, home) {
  for (const glob of globs) {
    for (const path of paths) if (globMatch(path, glob, home)) return glob;
  }
  return null;
}

export function resolveProfile({
  cwd = process.cwd(),
  flags = {},
  configPath,
  env = process.env,
} = {}) {
  const home = env.HOME || homedir();
  const config = readConfig(configPath || join(home, ".config", "easel", "profiles.json"));
  const paths = forms(cwd);
  const reasons = [];

  let name = "piece";
  if (flags.pro === true) {
    name = "pro";
    reasons.push("pro: --pro");
  } else if (flags.pro !== false) {
    const hit = firstMatch(paths, config.pro, home);
    if (hit) {
      name = "pro";
      reasons.push(`pro: cwd matches ${hit}`);
    }
  }

  let isPrivate = false;
  if (flags.private === true) {
    isPrivate = true;
    reasons.push("private: --private");
  } else if (env.EASEL_PRIVATE === "1") {
    isPrivate = true;
    reasons.push("private: EASEL_PRIVATE=1");
  } else {
    const hit = firstMatch(paths, config.private, home);
    if (hit) {
      isPrivate = true;
      reasons.push(`private: cwd matches ${hit}`);
    }
  }

  return {
    name,
    private: isPrivate,
    // Pro publishes nothing; private publishes nothing and advertises state only.
    publish: name === "piece" && !isPrivate,
    advertise: isPrivate ? "status" : "full",
    passthrough: name === "pro",
    reason: reasons.length ? reasons.join("; ") : "piece: default",
  };
}
