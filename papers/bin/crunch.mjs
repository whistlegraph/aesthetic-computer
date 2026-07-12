#!/usr/bin/env node
// crunch.mjs — build a paper on the oven, from a laptop with no TeX.
//
// Tars the directory, POSTs it to the oven's /paper-crunch, streams the log
// back, and drops the PDF next to the source. The document is never committed
// and never lands on papers.aesthetic.computer — this is for the private ones.
// (The public path is `cli.mjs publish`, which builds from main.)
//
// Usage:
//   node papers/bin/crunch.mjs vault/some-brief
//   node papers/bin/crunch.mjs vault/some-brief --tex brief.tex --out ~/brief.pdf
//   node papers/bin/crunch.mjs vault/some-brief --verbose
//
// Env:
//   OVEN_URL             default https://oven.aesthetic.computer
//   OS_BUILD_ADMIN_KEY   else read from the vault (plain, then GPG)

import { execFileSync } from "node:child_process";
import { existsSync, readFileSync, writeFileSync, statSync } from "node:fs";
import { resolve, dirname, join, basename } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const REPO = resolve(HERE, "..", "..");
const OVEN = (process.env.OVEN_URL || "https://oven.aesthetic.computer").replace(/\/$/, "");

// Build artifacts stay home — the oven makes its own (papers/.gitignore shapes).
const DROSS = ["*.pdf", "*.aux", "*.log", "*.out", "*.toc", "*.bbl", "*.blg", ".git", ".DS_Store"];

const argv = process.argv.slice(2);
const flags = {};
const positional = [];
for (let i = 0; i < argv.length; i++) {
  const a = argv[i];
  if (a.startsWith("--")) {
    const key = a.slice(2);
    if (key === "verbose") flags.verbose = true;
    else flags[key] = argv[++i];
  } else positional.push(a);
}

function die(msg, hint) {
  console.error(`  ✗ ${msg}`);
  if (hint) console.error(`    ${hint}`);
  process.exit(1);
}

// Key: the env var first, then the two places the rest of the toolchain keeps
// it (deploy.sh reads the plaintext; ac-os decrypts the .gpg and caches it).
// Never printed.
function adminKey() {
  const fromEnv = (process.env.OS_BUILD_ADMIN_KEY || "").trim();
  if (fromEnv) return fromEnv;

  const cache = "/tmp/.ac-oven-admin-key";
  if (existsSync(cache)) {
    const key = readFileSync(cache, "utf8").trim();
    if (key) return key;
  }

  const plain = join(REPO, "aesthetic-computer-vault", "oven", "os-build-admin-key.txt");
  if (existsSync(plain)) {
    const key = readFileSync(plain, "utf8").trim();
    if (key) return key;
  }

  const gpg = `${plain}.gpg`;
  if (existsSync(gpg)) {
    try {
      return execFileSync("gpg", ["--pinentry-mode", "loopback", "-d", gpg], {
        stdio: ["inherit", "pipe", "ignore"],
        encoding: "utf8",
      }).trim();
    } catch {}
  }

  die(
    "no oven admin key",
    "set OS_BUILD_ADMIN_KEY, or unlock the vault (aesthetic-computer-vault/oven/os-build-admin-key.txt)",
  );
}

function bundle(dir) {
  const args = ["-czf", "-", "-C", dir, ...DROSS.map((d) => `--exclude=${d}`), "."];
  return execFileSync("tar", args, { maxBuffer: 64 * 1024 * 1024 });
}

// xelatex says a great deal. Keep the stage markers, the errors, and the
// warnings that matter; --verbose for the whole flood.
function worthShowing(line) {
  if (flags.verbose) return true;
  if (/^\s{2}(CRUNCH|UNPACK|FONTS|PASS|BIBTEX|OK|FAILED|WARN)/.test(line)) return true;
  if (/^!/.test(line) || /^l\.\d+/.test(line)) return true;
  if (/^.*\.(tex|sty):\d+:/.test(line)) return true;
  if (/LaTeX Warning: (Citation|Reference|There were undefined)/.test(line)) return true;
  return false;
}

async function ask(pathname, init = {}) {
  const url = `${OVEN}${pathname}`;
  try {
    return await fetch(url, {
      ...init,
      headers: { Authorization: `Bearer ${KEY}`, ...(init.headers || {}) },
    });
  } catch (err) {
    die(`oven unreachable at ${OVEN}`, err.message);
  }
}

// The tail of the log around the first LaTeX complaint — the two lines a
// human actually needs, not the 2000-line dump.
function complaint(lines) {
  const at = lines.findIndex((l) => /^!/.test(l) || /\.(tex|sty):\d+:/.test(l));
  if (at < 0) return lines.slice(-12);
  return lines.slice(at, at + 8);
}

const src = positional[0];
if (!src) {
  console.error("usage: crunch.mjs <paper-dir> [--tex <entrypoint>] [--out <path.pdf>] [--verbose]");
  process.exit(1);
}
const dir = resolve(src);
if (!existsSync(dir) || !statSync(dir).isDirectory()) die(`not a directory: ${src}`);

const KEY = adminKey();
const name = basename(dir);
const out = flags.out ? resolve(flags.out) : join(dir, `${flags.tex ? basename(flags.tex, ".tex") : name}.pdf`);

const tarball = bundle(dir);
console.log(`▸ crunching ${name} on ${OVEN} (${(tarball.length / 1024).toFixed(0)}kb)`);

const started = await ask("/paper-crunch", {
  method: "POST",
  headers: {
    "Content-Type": "application/gzip",
    "x-crunch-name": name,
    ...(flags.tex ? { "x-crunch-tex": basename(flags.tex) } : {}),
  },
  body: tarball,
});

if (started.status === 401) die("oven rejected the admin key", "is OS_BUILD_ADMIN_KEY the current one?");
if (started.status === 409) {
  const { activeJobId } = await started.json().catch(() => ({}));
  die(`oven is already crunching (job ${activeJobId})`, "wait, or cancel it and retry");
}
if (!started.ok) die(`oven refused the bundle (HTTP ${started.status})`, (await started.text()).slice(0, 300));

const { id } = await started.json();

const stream = await ask(`/paper-crunch/${id}/stream`);
if (!stream.ok) die(`cannot stream job ${id} (HTTP ${stream.status})`);

const seen = [];
let outcome = null;
let pending = "";

for await (const chunk of stream.body) {
  pending += Buffer.from(chunk).toString();
  let cut;
  while ((cut = pending.indexOf("\n\n")) >= 0) {
    const frame = pending.slice(0, cut);
    pending = pending.slice(cut + 2);

    const type = frame.match(/^event: (.+)$/m)?.[1];
    const data = JSON.parse(frame.match(/^data: (.+)$/m)?.[1] || "{}");

    if (type === "logs") {
      for (const { line } of data.logs || []) {
        seen.push(line);
        if (worthShowing(line)) console.log(`  ${line.trim()}`);
      }
    } else if (type === "complete") {
      outcome = data;
    }
  }
}

if (!outcome) die(`the oven hung up on job ${id}`, `check ${OVEN}/paper-crunch/${id}`);

if (outcome.status !== "success") {
  console.error("");
  for (const line of complaint(seen)) console.error(`    ${line}`);
  console.error("");
  die(outcome.error || `crunch ${outcome.status}`, flags.verbose ? null : "re-run with --verbose for the full log");
}

const pdf = await ask(`/paper-crunch/${id}/pdf`);
if (!pdf.ok) die(`no PDF came back (HTTP ${pdf.status})`);
writeFileSync(out, Buffer.from(await pdf.arrayBuffer()));

const pages = outcome.pages ? `, ${outcome.pages}pp` : "";
console.log(`  ✓ ${out} (${(statSync(out).size / 1024).toFixed(0)}kb${pages})`);
