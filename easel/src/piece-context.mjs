import {existsSync,readFileSync} from "node:fs";
import {dirname,join} from "node:path";
import {fileURLToPath} from "node:url";
const ROOT=join(dirname(fileURLToPath(import.meta.url)),"..");

// The guides, in the order a model should meet them: what a piece is, then how
// it draws, then how the code should read. KidLisp last because most sessions
// are JavaScript and it is the longest.
const CONTEXT_FILES = ["pieces.md", "screen.md", "hand.md", "kidlisp.md"];

export function bundledContext() {
  const parts = [];
  for (const name of CONTEXT_FILES) {
    const path = join(ROOT, "context", name);
    if (!existsSync(path)) continue;
    try {
      parts.push(`# ${name}\n\n${readFileSync(path, "utf8")}`);
    } catch {}
  }
  if (!parts.length) return "";
  return [
    "Here are the Aesthetic Computer guides. They are the house rules for a",
    "piece and they win over your own defaults.",
    "",
    parts.join("\n\n---\n\n"),
  ].join("\n");
}

