#!/usr/bin/env node
// check-columns.mjs — column-overflow gate for the AC papers stack.
//
// @jeffrey's rule: "nothing should ever get out that breaks the columnar
// arrangement." This scans xelatex `.log` files for `Overfull \hbox`
// warnings that exceed a small threshold (default 0.5pt over the measure) —
// exactly the identifiers/paths/tokens that spill past the text column or
// margin. It FAILS LOUDLY (non-zero exit) and lists each offender with the
// overflow amount, the source line number, and the source line text when
// resolvable, so it is visible *which* string broke the column.
//
// Usage:
//   node papers/tools/check-columns.mjs                 # all arxiv-* papers
//   node papers/tools/check-columns.mjs arxiv-fuser     # one or more papers
//   node papers/tools/check-columns.mjs --threshold=1   # custom pt threshold
//   node papers/tools/check-columns.mjs --build         # rebuild via xelatex first
//   node papers/tools/check-columns.mjs --quiet         # only print failures
//
// Dependency-light: Node built-ins only. Reads the `.log` next to each PDF
// (produced by `node papers/cli.mjs build`); pass --build to regenerate.
//
// Exit code: 0 = every scanned paper is column-clean; 1 = at least one
// overflow over threshold (or a paper had no readable log).

import {
  readdirSync,
  readFileSync,
  existsSync,
  statSync,
} from "node:fs";
import { join, dirname, basename } from "node:path";
import { fileURLToPath } from "node:url";
import { execSync } from "node:child_process";

const PAPERS_DIR = join(dirname(fileURLToPath(import.meta.url)), "..");

// Parse one xelatex .log for Overfull \hbox warnings over `threshold` pt.
// Returns [{ amount, lineStart, lineEnd, where, context }].
export function overfullsInLog(logText, threshold = 0.5) {
  const out = [];
  // Match the warning header. The pt amount is always on the same log line;
  // the line-number tail may say "at lines A--B", "at lines A", or
  // "detected at line N" (or nothing, e.g. while \output is active).
  const re =
    /Overfull \\hbox \(([\d.]+)pt too wide\)([^\n]*)/g;
  let m;
  const lines = logText.split("\n");
  // Precompute char-offset → line index is overkill; instead re-find with index.
  let searchFrom = 0;
  while ((m = re.exec(logText)) !== null) {
    const amount = parseFloat(m[1]);
    if (!(amount > threshold)) continue;
    const tail = m[2] || "";
    let lineStart = null;
    let lineEnd = null;
    let where = "";
    const lr = tail.match(/at lines (\d+)(?:--(\d+))?/);
    const dr = tail.match(/detected at line (\d+)/);
    if (lr) {
      lineStart = parseInt(lr[1], 10);
      lineEnd = lr[2] ? parseInt(lr[2], 10) : lineStart;
      where = tail.includes("alignment") ? "table/alignment" : "paragraph";
    } else if (dr) {
      lineStart = lineEnd = parseInt(dr[1], 10);
      where = "detected";
    } else if (/while \\output is active/.test(tail)) {
      where = "output/float";
    } else {
      where = "unknown";
    }
    // Grab the offending box content: TeX prints it on the lines right after
    // the header (the "[]\tenrm ..." dump). Capture up to ~2 non-empty lines.
    const hdrLineIdx = logText.slice(0, m.index).split("\n").length - 1;
    const ctx = [];
    for (let i = hdrLineIdx + 1; i < Math.min(hdrLineIdx + 4, lines.length); i++) {
      const t = lines[i];
      if (!t || !t.trim()) break;
      ctx.push(t.length > 90 ? t.slice(0, 90) + "…" : t);
    }
    out.push({ amount, lineStart, lineEnd, where, context: ctx.join(" ") });
  }
  return out;
}

// Resolve a source line number to its .tex text (trimmed), or null.
function sourceLine(texPath, lineNo) {
  if (!lineNo || !texPath || !existsSync(texPath)) return null;
  try {
    const lines = readFileSync(texPath, "utf8").split("\n");
    const raw = lines[lineNo - 1];
    return raw != null ? raw.trim() : null;
  } catch {
    return null;
  }
}

// Find the .log/.tex pairs to check inside a paper directory.
function logsForDir(dir) {
  const abs = join(PAPERS_DIR, dir);
  let files;
  try {
    files = readdirSync(abs);
  } catch {
    return [];
  }
  return files
    .filter((f) => f.endsWith(".log"))
    .map((f) => ({
      dir,
      name: basename(f, ".log"),
      log: join(abs, f),
      tex: join(abs, basename(f, ".log") + ".tex"),
    }))
    // Only papers that actually have a .tex (skip stray logs).
    .filter((p) => existsSync(p.tex));
}

// Optionally rebuild a paper reproducibly before scanning its log.
function buildDir(dir, name, tex) {
  const abs = join(PAPERS_DIR, dir);
  const epoch = Math.floor(statSync(tex).mtimeMs / 1000).toString();
  try {
    execSync(
      `cd "${abs}" && xelatex -interaction=nonstopmode "${name}.tex" && xelatex -interaction=nonstopmode "${name}.tex"`,
      { stdio: "ignore", timeout: 180000, env: { ...process.env, SOURCE_DATE_EPOCH: epoch } },
    );
  } catch {
    /* xelatex may exit non-zero on warnings; we read the log regardless */
  }
}

export function checkColumns(dirs, opts = {}) {
  const threshold = opts.threshold ?? 0.5;
  const quiet = opts.quiet ?? false;
  const doBuild = opts.build ?? false;
  const results = [];

  for (const dir of dirs) {
    for (const p of logsForDir(dir)) {
      if (doBuild) buildDir(p.dir, p.name, p.tex);
      if (!existsSync(p.log)) {
        results.push({ ...p, error: "no .log (build the paper first)", offenders: [] });
        continue;
      }
      const logText = readFileSync(p.log, "utf8");
      const offenders = overfullsInLog(logText, threshold).map((o) => ({
        ...o,
        src: sourceLine(p.tex, o.lineStart),
      }));
      results.push({ ...p, offenders });
    }
  }
  return results;
}

function listPaperDirs() {
  return readdirSync(PAPERS_DIR).filter((d) => {
    if (!d.startsWith("arxiv-")) return false;
    try {
      return statSync(join(PAPERS_DIR, d)).isDirectory();
    } catch {
      return false;
    }
  });
}

// ---- CLI ----------------------------------------------------------------
function isMain() {
  return process.argv[1] && fileURLToPath(import.meta.url) === process.argv[1];
}

if (isMain()) {
  const args = process.argv.slice(2);
  const flags = args.filter((a) => a.startsWith("--"));
  const positional = args.filter((a) => !a.startsWith("--"));
  const thFlag = flags.find((f) => f.startsWith("--threshold="));
  const threshold = thFlag ? parseFloat(thFlag.split("=")[1]) : 0.5;
  const quiet = flags.includes("--quiet");
  const build = flags.includes("--build");

  const dirs = positional.length ? positional : listPaperDirs();
  const results = checkColumns(dirs, { threshold, quiet, build });

  let failedPapers = 0;
  let totalOffenders = 0;
  let errored = 0;

  console.log(
    `\ncheck-columns — Overfull \\hbox > ${threshold}pt across ${dirs.length} paper dir(s)\n`,
  );

  for (const r of results) {
    if (r.error) {
      errored++;
      console.log(`  ?  ${r.dir}/${r.name}: ${r.error}`);
      continue;
    }
    if (r.offenders.length === 0) {
      if (!quiet) console.log(`  OK ${r.dir}/${r.name}`);
      continue;
    }
    failedPapers++;
    totalOffenders += r.offenders.length;
    console.log(`  XX ${r.dir}/${r.name} — ${r.offenders.length} overflow(s):`);
    for (const o of r.offenders) {
      const loc =
        o.lineStart != null
          ? `line ${o.lineStart}${o.lineEnd !== o.lineStart ? `--${o.lineEnd}` : ""}`
          : `(${o.where})`;
      console.log(`       ${o.amount.toFixed(2)}pt over @ ${loc} [${o.where}]`);
      if (o.src) console.log(`         src: ${o.src.slice(0, 100)}`);
      else if (o.context) console.log(`         box: ${o.context}`);
    }
  }

  const clean = failedPapers === 0 && errored === 0;
  console.log(
    `\n${clean ? "PASS" : "FAIL"} — ${failedPapers} paper(s) with overflow, ` +
      `${totalOffenders} offender(s)` +
      (errored ? `, ${errored} unreadable log(s)` : "") +
      `.\n`,
  );
  process.exit(clean ? 0 : 1);
}
