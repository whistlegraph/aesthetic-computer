#!/usr/bin/env node
// brush-test.mjs, 2026.11.18
// Run the No Paint classic brush test on a fleet machine and bring the
// results home. Headless Chrome is not run on the working Mac.
//
//   node toolchain/nopaint/brush-test.mjs              every brush
//   node toolchain/nopaint/brush-test.mjs Flip         brushes whose name contains "Flip" ("Flip,Turn" for several)
//   node toolchain/nopaint/brush-test.mjs --video      keep a clip of every Paint
//   node toolchain/nopaint/brush-test.mjs --book       then regenerate the brushbook's entries and strips
//   node toolchain/nopaint/brush-test.mjs --steps      the step test instead: a few answers become a #painting,
//                                                      then the real painting service checks what the page sent
//
// Steps: check the build against its patches (patch-classic.mjs --check),
// copy the build, the test and brushes.json to the host, run the test there,
// copy results.json and the PNGs (and clips) back to tmp/nopaint-brush-test/
// (tmp/nopaint-brush-test-partial/ when only some brushes ran).
// Exit status is the test's: 0 when every brush painted on Paint and held on No.
//
// Env: AC_CAPTURE_HOST (default panda; needs ssh, node via a login zsh,
//      Chrome, and ~/scratch/brushes/node_modules/puppeteer-core).

import { spawnSync } from "node:child_process";
import { mkdirSync } from "node:fs";
import { fileURLToPath } from "node:url";

const ROOT = fileURLToPath(new URL("../../", import.meta.url));
const HOST = process.env.AC_CAPTURE_HOST || "panda";
const DIR = "scratch/brushes";
const args = process.argv.slice(2);
const video = args.includes("--video"), book = args.includes("--book"), steps = args.includes("--steps");
const only = args.find((arg) => !arg.startsWith("--")) || "";
if (only && !/^[\w ,-]+$/.test(only)) throw new Error("brush names are letters, digits and spaces, with commas between them");
const run = (command, list, options = {}) => {
  const result = spawnSync(command, list, { cwd: ROOT, stdio: "inherit", ...options });
  if (result.error) throw result.error;
  return result.status;
};
const must = (...call) => { const status = run(...call); if (status !== 0) process.exit(status ?? 1); };

must("node", ["toolchain/nopaint/patch-classic.mjs", "--check"]);
must("ssh", [HOST, `mkdir -p ${DIR} && cd ${DIR} && { [ -e node_modules/puppeteer-core ] || { echo "install puppeteer-core in ~/${DIR} on ${HOST} first: npm i puppeteer-core"; exit 1; }; } && rm -rf out`]);
must("rsync", ["-az", "--delete", "--exclude", "gallery", "--exclude", "ai", "--exclude", "story", "--exclude", "painting",
  "system/public/nopaint.art/", `${HOST}:${DIR}/site/`]);
must("rsync", ["-az", "tests/browser/book-kit.mjs", "tests/browser/brushbook.e2e.mjs",
  "papers/arxiv-nopaint-brushbook/brushes.json", `${HOST}:${DIR}/`]);
if (steps) {
  must("rsync", ["-az", "tests/browser/classic-steps.e2e.mjs", "system/public/aesthetic.computer/lib/nopaint-pieces.mjs",
    "system/public/aesthetic.computer/lib/painting-state.mjs", "toolchain/nopaint/classic-picker.js", "toolchain/nopaint/classic-tuning.json", `${HOST}:${DIR}/`]);
  const played = run("ssh", [HOST, `cd ${DIR} && rm -rf out-steps && zsh -lic 'OUT=out-steps node classic-steps.e2e.mjs'`]);
  mkdirSync(new URL("tmp/nopaint-steps-test", `file://${ROOT}`), { recursive: true });
  must("rsync", ["-az", "--delete", `${HOST}:${DIR}/out-steps/`, "tmp/nopaint-steps-test/"]);
  const replayed = run("node", ["toolchain/nopaint/steps-replay.mjs"]);
  process.exit(played || replayed || 0);
}
const status = run("ssh", [HOST, `cd ${DIR} && zsh -lic '${video ? "VIDEO=1 " : ""}OUT=out node brushbook.e2e.mjs ${JSON.stringify(only)}'`]);
// A run of a few brushes does not replace the last full run.
const home = only ? "tmp/nopaint-brush-test-partial" : "tmp/nopaint-brush-test";
mkdirSync(new URL(home, `file://${ROOT}`), { recursive: true });
must("rsync", ["-az", "--delete", `${HOST}:${DIR}/out/`, `${home}/`]);
console.log(`results: ${home}/results.json`);
if (book && !only) must("node", ["papers/arxiv-nopaint-brushbook/gen-entries.mjs", "--from", home]);
process.exit(status ?? 1);
