// The version, once.
//
// It was written as a literal in four places — the launcher, both engine
// bridges, and package.json — which is three places to forget. The launcher had
// already drifted: after a self-update it went on announcing the version it was
// written with, while package.json, the file the updater compares, had moved on.
// The bridges announce themselves to the vendor as EASEL_VERSION, so a stale one
// there misreports which Easel is in the field.
//
// package.json is the single source, because it is the file the updater and the
// packer both already read.
import { readFileSync } from "node:fs";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";

export const VERSION = (() => {
  try {
    const root = join(dirname(fileURLToPath(import.meta.url)), "..");
    return JSON.parse(readFileSync(join(root, "package.json"), "utf8")).version || "0.0.0";
  } catch {
    // A version we cannot read must not stop a session from opening.
    return "0.0.0";
  }
})();
