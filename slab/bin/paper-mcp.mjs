#!/usr/bin/env node
// paper-mcp.mjs — one front door to the Aesthetic Computer papers stack.
//
// The public repo and the adjacent private vault both contain paper sources.
// This MCP makes those sources discoverable by stable, fuzzy names and keeps
// the normal paper loop together: find/read → XeLaTeX build → slab PDF viewer.
// It deliberately indexes known paper roots rather than arbitrary filesystem
// paths, and XeLaTeX runs without shell escape.

import { execFile } from "node:child_process";
import { access, readFile, readdir, realpath, stat } from "node:fs/promises";
import { constants as fsConstants } from "node:fs";
import { homedir } from "node:os";
import { basename, dirname, extname, join, relative, resolve, sep } from "node:path";
import { fileURLToPath } from "node:url";
import { promisify } from "node:util";
import { httpPort, serveHttp, serveStdio } from "../../toolchain/mcp/http-front.mjs";

const pexec = promisify(execFile);
const SCRIPT_DIR = dirname(fileURLToPath(import.meta.url));
const REPO = resolve(SCRIPT_DIR, "../..");
const DEFAULT_VAULT = resolve(REPO, "../aesthetic-computer-vault");
const SKIP_DIRS = new Set([".git", "node_modules", "figures", "logos", "data", "tools", "build", "dist"]);
const SOURCE_EXTS = new Set([".tex", ".md"]);
const PAPER_EXTS = new Set([".tex", ".md", ".pdf"]);
const MAX_READ_CHARS = 60_000;

async function exists(path) {
  try { await access(path, fsConstants.F_OK); return true; }
  catch { return false; }
}

async function configuredRoots() {
  // Fleet layouts differ: neo uses the sibling vault; blueberry keeps the
  // same checkout nested beneath aesthetic-computer. Prefer explicit config,
  // then nested, then sibling. realpath de-duplicates symlinked views.
  const vaultCandidates = [
    process.env.AC_VAULT,
    join(REPO, "aesthetic-computer-vault"),
    DEFAULT_VAULT,
  ].filter(Boolean).map((path) => resolve(path));
  let vault = vaultCandidates[0] || DEFAULT_VAULT;
  for (const candidate of vaultCandidates) {
    if (await exists(candidate)) { vault = candidate; break; }
  }
  const roots = [
    { scope: "public", path: join(REPO, "papers") },
    { scope: "private", path: join(REPO, "papers-private") },
    { scope: "vault", path: join(vault, "papers-private") },
    { scope: "fuser", path: join(vault, "fuser", "papers") },
  ];
  if (process.env.PAPER_ROOT) roots.unshift({ scope: "configured", path: resolve(process.env.PAPER_ROOT) });
  const extra = String(process.env.AC_PAPER_ROOTS || "")
    .split(":").map((p) => p.trim()).filter(Boolean);
  extra.forEach((path, i) => roots.push({ scope: `extra${i + 1}`, path: resolve(path) }));
  const seen = new Set();
  const resolved = [];
  for (const root of roots) {
    if (!(await exists(root.path))) continue;
    const key = await realpath(root.path).catch(() => resolve(root.path));
    if (seen.has(key)) continue;
    seen.add(key);
    resolved.push({ ...root, path: key });
  }
  return resolved;
}

async function walk(dir, out = []) {
  let entries;
  try { entries = await readdir(dir, { withFileTypes: true }); }
  catch { return out; }
  for (const entry of entries) {
    if (entry.name.startsWith(".") || entry.name.endsWith(".aux") || entry.name.endsWith(".log")) continue;
    const path = join(dir, entry.name);
    if (entry.isDirectory()) {
      if (!SKIP_DIRS.has(entry.name)) await walk(path, out);
      continue;
    }
    if (!entry.isFile() || !PAPER_EXTS.has(extname(entry.name).toLowerCase())) continue;
    // Card sheets are derivatives, not the paper users mean by its title.
    if (/-cards\.(tex|pdf)$/i.test(entry.name)) continue;
    out.push(path);
  }
  return out;
}

function displayStem(stem) {
  return stem
    .replace(/^arxiv[-_]/i, "")
    .replace(/[-_]+/g, " ")
    .replace(/\b\w/g, (c) => c.toUpperCase());
}

function plainTex(value) {
  return String(value || "")
    .replace(/\\(?:textsf|texttt|textbf|emph|code|path)\{([^{}]*)\}/g, "$1")
    .replace(/\\[a-zA-Z]+\*?(?:\[[^\]]*\])?/g, "")
    .replace(/[{}~]/g, " ")
    .replace(/\\([_%&#])/g, "$1")
    .replace(/\s+/g, " ")
    .trim();
}

async function titleFromSource(path, fallback) {
  if (!path) return displayStem(fallback);
  let text = "";
  try { text = (await readFile(path, "utf8")).slice(0, 80_000); }
  catch { return displayStem(fallback); }
  if (extname(path).toLowerCase() === ".md") {
    const heading = text.match(/^#\s+(.+)$/m)?.[1];
    return heading?.trim() || displayStem(fallback);
  }
  const candidates = [
    text.match(/pdftitle\s*=\s*\{([^{}]+)\}/s)?.[1],
    text.match(/\\title\s*\{([^{}]+)\}/s)?.[1],
    text.match(/\\fusertitle\s*\n?\s*\{([^{}]+)\}/s)?.[1],
  ].filter(Boolean);
  return plainTex(candidates[0]) || displayStem(fallback);
}

async function catalog() {
  const records = [];
  for (const root of await configuredRoots()) {
    const files = await walk(root.path);
    const grouped = new Map();
    for (const path of files) {
      const ext = extname(path).toLowerCase();
      const stem = basename(path, ext);
      const key = join(dirname(path), stem);
      const rec = grouped.get(key) || {
        scope: root.scope,
        root: root.path,
        stem,
        dir: dirname(path),
        sourcePath: null,
        pdfPath: null,
      };
      if (ext === ".pdf") rec.pdfPath = path;
      else if (!rec.sourcePath || ext === ".tex") rec.sourcePath = path; // TeX wins over Markdown.
      grouped.set(key, rec);
    }
    for (const rec of grouped.values()) {
      const rel = relative(root.path, join(rec.dir, rec.stem)).split(sep).join("/");
      rec.id = `${root.scope}:${rel}`;
      rec.title = await titleFromSource(rec.sourcePath, rec.stem);
      rec.kind = rec.sourcePath ? extname(rec.sourcePath).slice(1) : "pdf";
      const paths = [rec.sourcePath, rec.pdfPath].filter(Boolean);
      const stats = await Promise.all(paths.map((path) => stat(path).catch(() => null)));
      rec.updatedAt = Math.max(0, ...stats.filter(Boolean).map((s) => s.mtimeMs));
      records.push(rec);
    }
  }
  return records.sort((a, b) => b.updatedAt - a.updatedAt || a.id.localeCompare(b.id));
}

function searchable(rec) {
  return `${rec.id} ${rec.title} ${rec.stem} ${rec.sourcePath || ""} ${rec.pdfPath || ""}`.toLowerCase();
}

function score(rec, query) {
  const q = query.toLowerCase();
  const id = rec.id.toLowerCase();
  const stem = rec.stem.toLowerCase();
  const title = rec.title.toLowerCase();
  if (id === q) return 100;
  if (`${rec.scope}:${stem}` === q) return 95; // convenient scope:basename alias
  if (stem === q || title === q) return 90;
  if (id.endsWith(`:${q}`) || id.endsWith(`/${q}`)) return 80;
  if (stem.startsWith(q) || title.startsWith(q)) return 70;
  if (searchable(rec).includes(q)) return 50;
  const words = q.split(/\s+/).filter(Boolean);
  return words.length && words.every((word) => searchable(rec).includes(word)) ? 30 : 0;
}

// A title can exist both as working Markdown and as the finished TeX/PDF.
// When the textual match is otherwise identical, resolve the built paper first;
// explicit stable IDs still win through their higher match score above.
function canonicalRank(rec) {
  if (rec.kind === "tex" && rec.pdfPath) return 3;
  if (rec.kind === "tex") return 2;
  if (rec.pdfPath) return 1;
  return 0;
}

function compareMatches(a, b) {
  return b.score - a.score
    || canonicalRank(b.rec) - canonicalRank(a.rec)
    || b.rec.updatedAt - a.rec.updatedAt
    || a.rec.id.localeCompare(b.rec.id);
}

async function resolvePaper(input, { one = true } = {}) {
  const query = String(input || "").trim();
  if (!query) throw new Error("`paper` is required (use paper_list to discover names).");
  const ranked = (await catalog())
    .map((rec) => ({ rec, score: score(rec, query) }))
    .filter((item) => item.score > 0)
    .sort(compareMatches);
  if (!ranked.length) throw new Error(`no paper resolves «${query}». Use paper_list({query:"${query}"}).`);
  if (!one) return ranked.map((item) => item.rec);
  const best = ranked[0].score;
  const tied = ranked.filter((item) => item.score === best);
  if (tied.length > 1 && best < 90) {
    throw new Error(`«${query}» is ambiguous: ${tied.slice(0, 8).map((item) => item.rec.id).join(", ")}. Use a full paper id.`);
  }
  return ranked[0].rec;
}

function fmtDate(ms) {
  return ms ? new Date(ms).toISOString().slice(0, 10) : "unknown";
}

function fmtPaper(rec) {
  return [
    `• ${rec.title}  [${rec.id}]`,
    `    ${rec.kind} source: ${rec.sourcePath || "—"}`,
    `    pdf: ${rec.pdfPath || "—"}   updated: ${fmtDate(rec.updatedAt)}`,
  ].join("\n");
}

async function toolList({ query, scope, kind, limit = 30 } = {}) {
  let rows = await catalog();
  if (scope) rows = rows.filter((rec) => rec.scope === String(scope).toLowerCase());
  if (kind) rows = rows.filter((rec) => rec.kind === String(kind).toLowerCase());
  if (query) {
    rows = rows.map((rec) => ({ rec, score: score(rec, String(query)) }))
      .filter((item) => item.score > 0)
      .sort(compareMatches)
      .map((item) => item.rec);
  }
  const capped = rows.slice(0, Math.max(1, Math.min(100, Number(limit) || 30)));
  if (!capped.length) return [{ type: "text", text: "(no papers match)" }];
  return [{ type: "text", text: `${capped.length}${rows.length > capped.length ? ` of ${rows.length}` : ""} paper(s):\n\n${capped.map(fmtPaper).join("\n\n")}` }];
}

async function toolFind({ paper } = {}) {
  const rec = await resolvePaper(paper);
  return [{ type: "text", text: fmtPaper(rec) }];
}

function sectionSlice(text, section) {
  if (!section) return text;
  const wanted = String(section).trim().toLowerCase();
  const lines = text.split("\n");
  let start = -1;
  let level = 0;
  for (let i = 0; i < lines.length; i++) {
    const md = lines[i].match(/^(#{1,6})\s+(.+)$/);
    const tex = lines[i].match(/^\\(section|subsection|subsubsection)\*?\{(.+)\}/);
    const label = md?.[2] || tex?.[2];
    if (!label || !plainTex(label).toLowerCase().includes(wanted)) continue;
    start = i;
    level = md ? md[1].length : ({ section: 1, subsection: 2, subsubsection: 3 }[tex[1]] || 1);
    break;
  }
  if (start < 0) throw new Error(`section «${section}» not found.`);
  let end = lines.length;
  for (let i = start + 1; i < lines.length; i++) {
    const md = lines[i].match(/^(#{1,6})\s+/);
    const tex = lines[i].match(/^\\(section|subsection|subsubsection)\*?\{/);
    const nextLevel = md ? md[1].length : tex ? ({ section: 1, subsection: 2, subsubsection: 3 }[tex[1]] || 9) : 9;
    if ((md || tex) && nextLevel <= level) { end = i; break; }
  }
  return lines.slice(start, end).join("\n");
}

async function toolRead({ paper, section, maxChars = 24_000 } = {}) {
  const rec = await resolvePaper(paper);
  let raw;
  if (rec.sourcePath && SOURCE_EXTS.has(extname(rec.sourcePath).toLowerCase())) {
    raw = await readFile(rec.sourcePath, "utf8");
  } else if (rec.pdfPath) {
    let engine = null;
    for (const path of ["/opt/homebrew/bin/pdftotext", "/usr/local/bin/pdftotext", "/usr/bin/pdftotext"]) {
      if (await exists(path)) { engine = path; break; }
    }
    if (!engine) throw new Error(`${rec.id} has only a PDF and pdftotext is unavailable.`);
    const result = await pexec(engine, [rec.pdfPath, "-"], { timeout: 30_000, maxBuffer: 8 * 1024 * 1024 });
    raw = result.stdout || "";
  } else {
    throw new Error(`${rec.id} has no readable source or PDF.`);
  }
  const selected = sectionSlice(raw, section);
  const cap = Math.max(1000, Math.min(MAX_READ_CHARS, Number(maxChars) || 24_000));
  const clipped = selected.length > cap ? `${selected.slice(0, cap)}\n\n[… clipped at ${cap} characters …]` : selected;
  return [{ type: "text", text: `${rec.title} [${rec.id}]\nsource: ${rec.sourcePath || rec.pdfPath}\n\n${clipped}` }];
}

async function texEngine() {
  const xelatex = [process.env.XELATEX, "/Library/TeX/texbin/xelatex", "/usr/local/bin/xelatex", "/opt/homebrew/bin/xelatex"].filter(Boolean);
  for (const path of xelatex) if (await exists(path)) return { kind: "xelatex", path };
  const tectonic = [process.env.TECTONIC, "/opt/homebrew/bin/tectonic", "/usr/local/bin/tectonic"].filter(Boolean);
  for (const path of tectonic) if (await exists(path)) return { kind: "tectonic", path };
  throw new Error("no TeX engine found (install XeLaTeX/Tectonic or set XELATEX/TECTONIC).");
}

async function buildRecord(rec, passes = 2) {
  if (!rec.sourcePath || extname(rec.sourcePath).toLowerCase() !== ".tex") {
    throw new Error(`${rec.id} has no TeX source.`);
  }
  const engine = await texEngine();
  const requestedPasses = Math.max(1, Math.min(4, Number(passes) || 2));
  // Tectonic resolves references to convergence itself; repeating the whole
  // engine adds cost without improving the result.
  const count = engine.kind === "tectonic" ? 1 : requestedPasses;
  let output = "";
  for (let pass = 1; pass <= count; pass++) {
    try {
      const args = engine.kind === "xelatex"
        ? ["-interaction=nonstopmode", "-halt-on-error", "-file-line-error", basename(rec.sourcePath)]
        : ["--keep-logs", "--keep-intermediates", "--synctex", basename(rec.sourcePath)];
      const result = await pexec(engine.path, args, { cwd: rec.dir, timeout: 180_000, maxBuffer: 8 * 1024 * 1024 });
      output += result.stdout || "";
    } catch (error) {
      const log = `${error.stdout || ""}\n${error.stderr || ""}`.trim().split("\n").slice(-45).join("\n");
      throw new Error(`${engine.kind} pass ${pass}/${count} failed for ${rec.id}:\n${log}`);
    }
  }
  const pdfPath = join(rec.dir, `${rec.stem}.pdf`);
  if (!(await exists(pdfPath))) throw new Error(`XeLaTeX finished but did not create ${pdfPath}.`);
  const pdfStat = await stat(pdfPath);
  return { pdfPath, bytes: pdfStat.size, passes: count, engine: engine.kind, tail: output.split("\n").slice(-8).join("\n") };
}

async function notifyArtifactReady(handle, pdfPath) {
  if (!handle) return null;
  try {
    const res = await fetch("http://127.0.0.1:7773/mcp", {
      method: "POST",
      headers: { "content-type": "application/json", accept: "application/json, text/event-stream" },
      body: JSON.stringify({
        jsonrpc: "2.0",
        id: 1,
        method: "tools/call",
        params: {
          name: "prox_artifact_ready",
          arguments: { handle: String(handle), artifacts: [pdfPath], by: "paper-mcp" },
        },
      }),
      signal: AbortSignal.timeout(5000),
    });
    const text = await res.text();
    if (!res.ok || /"isError"\s*:\s*true/.test(text)) throw new Error(`HTTP ${res.status}: ${text.slice(0, 180)}`);
    return "prox notified";
  } catch (error) {
    return `prox notification failed: ${error.message}`;
  }
}

async function toolBuild({ paper, passes = 2, notifyHandle } = {}) {
  const rec = await resolvePaper(paper);
  const built = await buildRecord(rec, passes);
  const notified = await notifyArtifactReady(notifyHandle, built.pdfPath);
  return [{ type: "text", text: `built ${rec.title} [${rec.id}]\n${built.pdfPath}\n${built.bytes} bytes · ${built.passes} ${built.engine} pass(es)${notified ? `\n${notified}` : ""}` }];
}

async function toolOpen({ paper, build = false } = {}) {
  const rec = await resolvePaper(paper);
  let pdfPath = rec.pdfPath;
  if ((!pdfPath || !(await exists(pdfPath))) && build) pdfPath = (await buildRecord(rec, 2)).pdfPath;
  if (!pdfPath || !(await exists(pdfPath))) throw new Error(`${rec.id} has no PDF. Call paper_build first or pass build:true.`);
  const slabPdf = join(REPO, "slab", "bin", "slab-pdf");
  // Preview is the reliable fleet default while slab-pdf's PDFKit panel can
  // consume a request without presenting a window. Keep slab-pdf available as
  // the non-macOS/project fallback rather than silently claiming it opened.
  if (process.platform === "darwin") await pexec("/usr/bin/open", ["-a", "Preview", pdfPath], { timeout: 10_000 });
  else if (await exists(slabPdf)) await pexec(slabPdf, [pdfPath], { timeout: 10_000 });
  else await pexec("/usr/bin/open", [pdfPath], { timeout: 10_000 });
  return [{ type: "text", text: `opened ${rec.title} [${rec.id}]\n${pdfPath}` }];
}

const TOOLS = [
  {
    name: "paper_list",
    description: "List and search the Aesthetic Computer papers stack: public papers plus private/vault papers, including Fuser client papers. Returns stable paper IDs, source/PDF paths, kind, and modification date.",
    inputSchema: {
      type: "object",
      properties: {
        query: { type: "string", description: "Optional fuzzy title, id, filename, or path search." },
        scope: { type: "string", enum: ["public", "private", "vault", "fuser", "configured"], description: "Optional paper-root filter." },
        kind: { type: "string", enum: ["tex", "md", "pdf"], description: "Optional preferred-source kind." },
        limit: { type: "integer", minimum: 1, maximum: 100, default: 30 },
      },
    },
  },
  {
    name: "paper_find",
    description: "Resolve a paper title, basename, path fragment, or stable ID to its exact source and PDF paths. Refuses ambiguous fuzzy matches.",
    inputSchema: { type: "object", properties: { paper: { type: "string" } }, required: ["paper"] },
  },
  {
    name: "paper_read",
    description: "Read TeX or Markdown source for a resolved paper, optionally narrowed to one heading/section. Intended for orientation and source-backed edits; output is capped.",
    inputSchema: {
      type: "object",
      properties: {
        paper: { type: "string" },
        section: { type: "string", description: "Optional heading/section name fragment." },
        maxChars: { type: "integer", minimum: 1000, maximum: 60000, default: 24000 },
      },
      required: ["paper"],
    },
  },
  {
    name: "paper_build",
    description: "Build a resolved .tex paper in place with XeLaTeX (no shell escape), normally two passes. SIDE EFFECT: writes the PDF and normal TeX auxiliary files beside the source.",
    inputSchema: {
      type: "object",
      properties: {
        paper: { type: "string" },
        passes: { type: "integer", minimum: 1, maximum: 4, default: 2 },
        notifyHandle: { type: "string", description: "Optional stable local prox handle to wake with the completed PDF via prox_artifact_ready." },
      },
      required: ["paper"],
    },
  },
  {
    name: "paper_open",
    description: "Open a resolved paper PDF in Preview on macOS (or the available native viewer elsewhere). Pass build:true to build missing TeX output first. SIDE EFFECT: opens a local viewer window.",
    inputSchema: {
      type: "object",
      properties: {
        paper: { type: "string" },
        build: { type: "boolean", default: false },
      },
      required: ["paper"],
    },
  },
];

async function callTool(name, args) {
  switch (name) {
    case "paper_list": return toolList(args || {});
    case "paper_find": return toolFind(args || {});
    case "paper_read": return toolRead(args || {});
    case "paper_build": return toolBuild(args || {});
    case "paper_open": return toolOpen(args || {});
    default: throw new Error(`Unknown tool: ${name}`);
  }
}

async function handleMessage(message) {
  const { id, method, params } = message;
  try {
    switch (method) {
      case "initialize":
        return {
          jsonrpc: "2.0", id,
          result: {
            protocolVersion: "2024-11-05",
            capabilities: { tools: {} },
            serverInfo: { name: "paper-mcp", version: "1.0.0" },
          },
        };
      case "initialized":
      case "notifications/initialized": return null;
      case "ping": return { jsonrpc: "2.0", id, result: {} };
      case "tools/list": return { jsonrpc: "2.0", id, result: { tools: TOOLS } };
      case "tools/call": {
        const content = await callTool(params?.name, params?.arguments);
        return { jsonrpc: "2.0", id, result: { content } };
      }
      default: return { jsonrpc: "2.0", id, error: { code: -32601, message: `Method not found: ${method}` } };
    }
  } catch (error) {
    if (method === "tools/call") {
      return { jsonrpc: "2.0", id, result: { isError: true, content: [{ type: "text", text: String(error.message || error) }] } };
    }
    return { jsonrpc: "2.0", id, error: { code: -32000, message: String(error.message || error) } };
  }
}

const port = httpPort(process.argv, 7777);
if (port) serveHttp({ handleMessage, port, banner: "📄 paper-mcp shared daemon" });
else serveStdio({ handleMessage, banner: "📄 paper-mcp started (paper_list, paper_find, paper_read, paper_build, paper_open)" });
