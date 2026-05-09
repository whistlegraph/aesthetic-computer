#!/usr/bin/env node
// qrs.mjs — generate a QR code per segment that resolves to its
// tangled.sh commit URL. Slides.mjs embeds the resulting PNGs bottom-
// right of each chapter.
//
// Reads:
//   recap/audience/<name>.mjs   audience.segments[].commit
//
// Writes:
//   recap/out/qr/<segment>.png  one QR per chapter that defines a commit
//
// Uses the homebrew `qrencode` CLI (same as papers/bin/gen-qrs.mjs).
//
// Usage: node bin/qrs.mjs <audience-name>

import { mkdirSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { execFileSync } from "node:child_process";

const HERE = dirname(fileURLToPath(import.meta.url));
const ROOT = resolve(HERE, "..");
const QR_DIR = `${ROOT}/out/qr`;
mkdirSync(QR_DIR, { recursive: true });

const audienceName = process.argv[2];
if (!audienceName) {
  console.error("usage: qrs.mjs <audience-name>");
  process.exit(2);
}

const TANGLED_BASE = "https://tangled.sh/aesthetic.computer/core/commit";

const { audience } = await import(`${ROOT}/audience/${audienceName}.mjs`);
let count = 0;
for (const seg of audience.segments) {
  if (!seg.commit) continue;
  const url = `${TANGLED_BASE}/${seg.commit}`;
  const out = `${QR_DIR}/${seg.name}.png`;
  // Quiet zone is critical for scannability; size 12 gives ~600px PNG which
  // downsamples cleanly to the ~200px slide footprint.
  execFileSync("qrencode", [
    "-o", out,
    "-s", "12",
    "-m", "2",
    "-l", "M",
    "--foreground", "0c1430",
    "--background", "fcf7c5",
    url,
  ]);
  count++;
  console.log(`  ✓ ${seg.name}.png → ${url}`);
}
console.log(`✓ ${count} QR code(s) → ${QR_DIR}/`);
