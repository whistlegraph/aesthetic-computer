#!/usr/bin/env node
// reel.mjs — one-shot: capture a KidLisp piece + stamp it into a 9:16 reel.
//
//   node marketing/kidlisp-reels/reel.mjs '$tezz'
//   node marketing/kidlisp-reels/reel.mjs '$tezz' --duration 15 --density 3
//   node marketing/kidlisp-reels/reel.mjs '$tezz' --render-only   # reuse frames
//
// Stages:
//   1. capture-kidlisp.mjs — headless, clean (nolabel/nogap/tv), → out/<slug>/frames/
//   2. render-reel.mjs     — fit 1080×1920 + pals side-stamps + $code title columns
//
// Silent on purpose: @jeffrey lays audio under the reel at post time.

import { spawn } from "node:child_process";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const argv = process.argv.slice(2);
const code = argv.find((a) => !a.startsWith("--"));
if (!code) { console.error("usage: reel.mjs '$code' [--duration N] [--density N] [--render-only]"); process.exit(1); }
const renderOnly = argv.includes("--render-only");
// Everything except the $code and the orchestrator-only --render-only flag is
// forwarded verbatim (keeps flag values, e.g. "--duration 61").
const passthru = argv.filter((a) => a !== code && a !== "--render-only");

function run(script, args) {
  return new Promise((res, rej) => {
    const p = spawn("node", [resolve(HERE, "bin", script), code, ...args], { stdio: "inherit" });
    p.on("close", (c) => (c === 0 ? res() : rej(new Error(`${script} exit ${c}`))));
  });
}

if (!renderOnly) await run("capture-kidlisp.mjs", passthru);
await run("render-reel.mjs", passthru);
