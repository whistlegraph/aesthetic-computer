#!/usr/bin/env node
// manifest.mjs — push a star's manifestations list to aesthetic.computer.
//
// The star (macpal, default profile) fetches /api/manifestations?to=<recipient>
// and cycles through the list one per hour in a side bubble. This sets that
// list. Admin-only — uses @jeffrey's login token.
//
//   node macpal/manifest.mjs                          # push fia-manifestations.txt → to=fia, prod
//   node macpal/manifest.mjs --file path/to/list.txt  # push a specific newline list
//   node macpal/manifest.mjs --to fia --local         # → https://localhost:8888
//   node macpal/manifest.mjs --clear --to fia         # empty the list
//
// The list file is one manifestation per line; blank lines are dropped.
// Auth: reads ~/.ac-token (run `node tezos/ac-login.mjs` once to create it).

import fs from "node:fs";
import os from "node:os";
import path from "node:path";
import { fileURLToPath } from "node:url";

const here = path.dirname(fileURLToPath(import.meta.url));
const args = process.argv.slice(2);
const flag = (name) => {
  const i = args.indexOf(name);
  return i !== -1 ? args[i + 1] : undefined;
};
const has = (name) => args.includes(name);

const to = flag("--to") || "fia";
const local = has("--local");
const host =
  flag("--host") ||
  (local ? "https://localhost:8888" : "https://aesthetic.computer");
const file = flag("--file") || path.join(here, `${to}-manifestations.txt`);

let items = [];
if (!has("--clear")) {
  let raw;
  try {
    raw = fs.readFileSync(file, "utf8");
  } catch {
    console.error(`✗ list file not found: ${file}`);
    process.exit(1);
  }
  items = raw
    .split("\n")
    .map((l) => l.trim())
    .filter(Boolean);
  if (!items.length) {
    console.error(`✗ ${file} has no manifestations`);
    process.exit(1);
  }
}

const tokenPath = path.join(os.homedir(), ".ac-token");
let token;
try {
  token = JSON.parse(fs.readFileSync(tokenPath, "utf8")).access_token;
} catch {
  console.error(`✗ No ~/.ac-token found. Log in first:  node tezos/ac-login.mjs`);
  process.exit(1);
}
if (!token) {
  console.error("✗ ~/.ac-token has no access_token — re-run: node tezos/ac-login.mjs");
  process.exit(1);
}

// The dev site serves a self-signed cert on localhost; trust it only there.
if (local) process.env.NODE_TLS_REJECT_UNAUTHORIZED = "0";

const res = await fetch(`${host}/api/manifestations`, {
  method: "POST",
  headers: { Authorization: `Bearer ${token}`, "Content-Type": "application/json" },
  body: JSON.stringify({ to, items }),
});

const out = await res.json().catch(() => ({}));
if (!res.ok) {
  console.error(`✗ ${res.status} ${out.message || ""}`.trim());
  process.exit(1);
}
console.log(
  has("--clear")
    ? `✓ cleared ${to}'s manifestations (seq ${out.seq})`
    : `✓ pushed ${out.items?.length ?? items.length} manifestations to ${to} (seq ${out.seq}) @ ${host}`,
);
