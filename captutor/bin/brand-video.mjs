#!/usr/bin/env node
// Apply a Captutor client theme to an already-finished MP4.

import { existsSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { pathToFileURL } from "node:url";
import { applyBrandChrome } from "../lib/brand-chrome.mjs";

const args = process.argv.slice(2);
const value = (flag) => {
  const index = args.indexOf(flag);
  return index < 0 ? null : args[index + 1];
};
const input = resolve(value("--input") || "");
const out = resolve(value("--out") || input.replace(/\.mp4$/i, ".branded.mp4"));
const format = value("--format") || "docs";
const themePath = resolve(value("--theme") || new URL("../themes/fuser.mjs", import.meta.url).pathname);

if (!existsSync(input) || !existsSync(themePath)) {
  console.error("usage: brand-video --input take.mp4 [--out branded.mp4] [--format docs|vertical|youtube|reel] [--theme theme.mjs]");
  process.exit(2);
}

const theme = (await import(pathToFileURL(themePath).href)).default;
const result = applyBrandChrome({ input, out, theme, workDir:dirname(out), format });
console.log(JSON.stringify({ out:result.out, theme:result.layout.id, layout:result.layout }, null, 2));
