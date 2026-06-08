#!/usr/bin/env node
// slab-capture-terminal — re-capture the Slab terminal seed from THIS machine.
//
// Reads the live com.apple.Terminal preferences, pulls out the `Grass` base
// profile + every `Slab-*` settings set, and rewrites:
//   seed/terminal-profiles.plist   (the profiles, verbatim colors + font)
//   seed/terminal-profiles.list    (their names, one per line)
//
// Run this on the reference machine after the menubar / you have tweaked the
// Slab-* palettes so they look right, then commit the seed. `slab-seed-terminal`
// reproduces them on any other machine.
//
// Pure macOS tooling: shells out to `defaults export` + `plutil`. No deps.
//
// Usage: node slab-capture-terminal.mjs [--base Grass] [--prefix Slab-]

import { execFileSync } from "node:child_process";
import { writeFileSync, mkdtempSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import { join, dirname } from "node:path";
import { fileURLToPath } from "node:url";

const argv = process.argv.slice(2);
const opt = (flag, def) => {
  const i = argv.indexOf(flag);
  return i >= 0 && argv[i + 1] ? argv[i + 1] : def;
};
const BASE = opt("--base", "Grass");
const PREFIX = opt("--prefix", "Slab-");

const seedDir = join(dirname(fileURLToPath(import.meta.url)), "..", "seed");
const tmp = mkdtempSync(join(tmpdir(), "slabcap-"));
const full = join(tmp, "term.plist");        // whole prefs, xml1
const wsXml = join(tmp, "ws.xml");            // just the Window Settings dict

try {
  // Export the whole Terminal prefs as XML (preserves binary color/font blobs;
  // JSON can't represent NSData, so we never go through JSON), then extract the
  // Window Settings dict on its own to enumerate the immediate profile names.
  execFileSync("/usr/bin/defaults", ["export", "com.apple.Terminal", full]);
  execFileSync("/usr/bin/plutil", ["-convert", "xml1", full]);
  execFileSync("/usr/bin/plutil", ["-extract", "Window Settings", "xml1", "-o", wsXml, full]);

  // The profile names are the *top-level* <key>s of the Window Settings dict.
  // Each profile value is a <dict>; nested keys live deeper. We read only keys
  // that sit at the dict's first indentation level (plutil pretty-prints with
  // tabs: top dict opens at depth 0, its direct <key>s are one tab in).
  const wsText = execFileSync("/bin/cat", [wsXml], { encoding: "latin1" });
  const lines = wsText.split("\n");
  const allNames = [];
  // Find the opening <dict> then collect <key> lines at exactly one tab indent.
  let started = false;
  for (const line of lines) {
    if (!started) { if (/^\s*<dict>/.test(line)) started = true; continue; }
    const m = line.match(/^\t<key>([^<]*)<\/key>/);
    if (m) allNames.push(m[1]);
  }
  const names = allNames
    .filter((n) => n === BASE || n.startsWith(PREFIX))
    .sort();
  if (!names.includes(BASE)) {
    console.error(`warning: base profile "${BASE}" not found on this machine`);
  }

  // Build the seed plist as XML: { DefaultProfile, Profiles: { name: <dict> } }.
  // We extract each profile's dict from the full XML plist to keep blobs exact.
  let profilesXml = "";
  for (const name of names) {
    const one = join(tmp, "one.xml");
    // plutil keypaths are dot-separated; our profile names contain no dots.
    execFileSync("/usr/bin/plutil", [
      "-extract", `Window Settings.${name}`, "xml1", "-o", one, full,
    ]);
    let body = execFileSync("/bin/cat", [one], { encoding: "utf8" });
    // Strip the XML/plist wrapper, keep the inner <dict>...</dict>.
    body = body.replace(/^[\s\S]*?<plist[^>]*>\s*/, "").replace(/\s*<\/plist>\s*$/, "").trim();
    profilesXml += `\t\t<key>${name}</key>\n${body.replace(/^/gm, "\t\t")}\n`;
  }

  const plist =
`<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
\t<key>DefaultProfile</key>
\t<string>${BASE}</string>
\t<key>Profiles</key>
\t<dict>
${profilesXml}\t</dict>
</dict>
</plist>
`;
  const seedPlist = join(seedDir, "terminal-profiles.plist");
  const seedList = join(seedDir, "terminal-profiles.list");
  writeFileSync(seedPlist, plist);
  // Normalize/validate via plutil.
  execFileSync("/usr/bin/plutil", ["-lint", seedPlist]);
  writeFileSync(seedList, names.join("\n") + "\n");
  console.log(`captured ${names.length} profiles → ${seedPlist}`);
  for (const n of names) console.log(`  ${n}`);
} finally {
  rmSync(tmp, { recursive: true, force: true });
}
