#!/usr/bin/env node
// Static client knowledge for agents before a live browser exists.

import { execFileSync } from "node:child_process";
import { existsSync, readFileSync } from "node:fs";
import { homedir } from "node:os";
import { join, resolve } from "node:path";
import {
  FUSER_INTELLIGENCE, FUSER_SOURCE_CHECKS, fuserEditor,
} from "../app-intelligence/fuser.mjs";
import { translator } from "../lib/i18n.mjs";

const args = process.argv.slice(2);
const command = args[0];
const client = args[1];
const value = (flag, fallback = null) => {
  const index = args.indexOf(flag);
  return index < 0 ? fallback : args[index + 1];
};
if (!["describe", "verify"].includes(command) || client !== "fuser") {
  console.error("usage: node bin/app-intelligence.mjs describe fuser [concept] [--locale en]\n       node bin/app-intelligence.mjs verify fuser [--source ~/Developer/fuser]");
  process.exit(2);
}

if (command === "describe") {
  const locale = value("--locale", "en");
  const subject = args[2]?.startsWith("--") ? null : args[2];
  const editor = fuserEditor(translator(locale));
  const result = !subject ? {
    schema:FUSER_INTELLIGENCE.schema, id:editor.id, locale,
    source:editor.source, glossary:editor.glossary,
    concepts:editor.concepts, behaviors:editor.behaviors,
  } : editor.concepts[subject] || editor.behaviors[subject] || editor.glossary[subject];
  if (!result) throw new Error(`unknown Fuser intelligence subject: ${subject}`);
  console.log(JSON.stringify(result, null, 2));
} else {
  const defaultSource = join(homedir(), "Developer", "fuser");
  const source = resolve(value("--source", defaultSource));
  const checks = FUSER_SOURCE_CHECKS.map((check) => {
    const path = join(source, check.path);
    const text = existsSync(path) ? readFileSync(path, "utf8") : "";
    const missing = check.contains.filter((needle) => !text.includes(needle));
    return { id:check.id, path:check.path, pass:existsSync(path) && missing.length === 0, missing };
  });
  let revision = null;
  let branch = null;
  try {
    revision = execFileSync("git", ["-C", source, "rev-parse", "HEAD"], { encoding:"utf8" }).trim();
    branch = execFileSync("git", ["-C", source, "branch", "--show-current"], { encoding:"utf8" }).trim();
  } catch {}
  const pass = branch === "staging" && checks.every((check) => check.pass);
  console.log(JSON.stringify({
    schema:"captutor-app-intelligence-verification/v1", client, source,
    expectedBranch:"staging", branch, revision, pass, checks,
  }, null, 2));
  if (!pass) process.exitCode = 3;
}

