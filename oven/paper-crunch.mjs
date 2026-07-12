// paper-crunch.mjs — On-demand xelatex for a private, uncommitted document.
//
// Upload a self-contained .tar.gz of a paper directory, get a PDF back. This
// is the *opposite* of papers-builder.mjs: nothing here is committed, nothing
// is deployed. It never writes to system/public/papers.aesthetic.computer/,
// never runs `papers/cli.mjs publish|deploy`, and never touches the git clone
// — it only *reads* the house .sty files and webfonts out of it, so a bundle
// that uses the AC styles resolves its fonts. That constraint is the whole
// reason this module exists: the oven is the only box with the toolchain, and
// some documents must never reach papers.aesthetic.computer.
//
// The bundle is untrusted. Entries are checked before extraction, the gzip
// stream is capped mid-flight, the build runs shell-escape off under a
// wall-clock deadline, and the sources are shredded the moment the PDF is out.

import { promises as fs, existsSync, createWriteStream } from "fs";
import path from "path";
import zlib from "zlib";
import { Readable, Transform } from "stream";
import { pipeline } from "stream/promises";
import { randomUUID } from "crypto";
import { spawn, execFile } from "child_process";
import { fileURLToPath } from "url";

const MAX_RECENT_JOBS = 20;
const MAX_LOG_LINES = 2000;

const MAX_BUNDLE_BYTES = 32 * 1024 * 1024; // compressed upload
const MAX_EXTRACT_BYTES = 192 * 1024 * 1024; // decompressed tar
const BUILD_TIMEOUT_MS = 180_000; // whole 4-pass chain
const JOB_TTL_MS = 30 * 60 * 1000; // then the PDF is gone too

const WORK_ROOT = process.env.CRUNCH_WORK_DIR || "/tmp/oven-crunch";

// The house .sty + webfonts live in the oven's git clone. Off the droplet
// (local dev) fall back to this repo, so a laptop with a TeX install can
// exercise the same sandbox.
const HERE = path.dirname(fileURLToPath(import.meta.url));
const ASSET_ROOT = [process.env.NATIVE_GIT_DIR, "/opt/oven/native-git", path.resolve(HERE, "..")]
  .filter(Boolean)
  .find((dir) => existsSync(path.join(dir, "system", "public", "type", "webfonts")));

const jobs = new Map();
const jobOrder = [];
let activeJobId = null;

function nowISO() {
  return new Date().toISOString();
}

function stripAnsi(s) {
  return String(s || "").replace(/\u001b\[[0-9;]*m/g, "");
}

function addLogLine(job, stream, line) {
  const clean = stripAnsi(line).replace(/\r/g, "").trimEnd();
  if (!clean) return;
  job.logs.push({ ts: nowISO(), stream, line: clean });
  if (job.logs.length > MAX_LOG_LINES)
    job.logs.splice(0, job.logs.length - MAX_LOG_LINES);
  job.updatedAt = nowISO();
}

function makeSnapshot(job, opts = {}) {
  const { includeLogs = false, tail = 200 } = opts;
  const snap = {
    id: job.id,
    name: job.name,
    tex: job.tex,
    status: job.status,
    stage: job.stage,
    percent: job.percent,
    createdAt: job.createdAt,
    startedAt: job.startedAt,
    updatedAt: job.updatedAt,
    finishedAt: job.finishedAt,
    error: job.error,
    pages: job.pages,
    pdfBytes: job.pdfBytes,
    pdfReady: !!job.pdfPath,
    expiresAt: job.expiresAt,
    logCount: job.logs.length,
    elapsedMs: job.startedAt
      ? (job.finishedAt ? Date.parse(job.finishedAt) : Date.now()) -
        Date.parse(job.startedAt)
      : 0,
  };
  if (includeLogs) {
    const start = Math.max(0, job.logs.length - Math.max(0, tail));
    snap.logs = job.logs.slice(start);
  }
  return snap;
}

function wireStream(job, proc, streamName) {
  let pending = "";
  const s = streamName === "stdout" ? proc.stdout : proc.stderr;
  s.on("data", (chunk) => {
    pending += chunk.toString();
    let idx;
    while ((idx = pending.indexOf("\n")) >= 0) {
      addLogLine(job, streamName, pending.slice(0, idx));
      pending = pending.slice(idx + 1);
    }
  });
  s.on("end", () => {
    if (pending) addLogLine(job, streamName, pending);
  });
}

function tar(args, cwd) {
  return new Promise((resolve, reject) => {
    execFile("tar", args, { cwd, timeout: 60_000, maxBuffer: 16 * 1024 * 1024 }, (err, stdout) => {
      if (err) return reject(err);
      resolve(stdout);
    });
  });
}

// A tar entry we will not extract: anything that could land outside the job
// dir, plus links of any kind (a symlink to /etc would be readable by xelatex).
function rejectEntry(name) {
  if (!name || name.startsWith("/") || /^[A-Za-z]:/.test(name)) return "absolute path";
  if (name.split("/").includes("..")) return "parent traversal";
  return null;
}

// Decompress through a hard ceiling, so a gzip bomb can't fill the droplet.
async function gunzipCapped(bundle, tarPath) {
  let seen = 0;
  const ceiling = new Transform({
    transform(chunk, _enc, cb) {
      seen += chunk.length;
      if (seen > MAX_EXTRACT_BYTES)
        return cb(new Error(`bundle expands past ${MAX_EXTRACT_BYTES} bytes`));
      cb(null, chunk);
    },
  });
  await pipeline(Readable.from(bundle), zlib.createGunzip(), ceiling, createWriteStream(tarPath));
}

// Lay out a fake repo root, because ac-paper-*.sty loads fonts by the relative
// path a real paper sees: `../../system/public/type/webfonts/` from
// papers/<dir>/. So the sources go two levels down and the webfonts hang off
// the sandbox root. The house styles are linked in beside them, so a bundle
// may \usepackage{ac-paper-essay} without shipping a copy.
async function pitchSandbox(job) {
  const root = path.join(job.dir, "root");
  const papers = path.join(root, "papers");
  const src = path.join(papers, "src");
  await fs.mkdir(src, { recursive: true });

  if (!ASSET_ROOT) {
    addLogLine(job, "stderr", "  WARN: no AC asset root — house fonts and styles unavailable");
    return { root, papers, src };
  }

  const type = path.join(root, "system", "public", "type");
  await fs.mkdir(type, { recursive: true });
  await fs.symlink(
    path.join(ASSET_ROOT, "system", "public", "type", "webfonts"),
    path.join(type, "webfonts"),
  );

  for (const sty of await fs.readdir(path.join(ASSET_ROOT, "papers"))) {
    if (!sty.endsWith(".sty")) continue;
    await fs.symlink(path.join(ASSET_ROOT, "papers", sty), path.join(papers, sty));
  }
  addLogLine(job, "stdout", `  FONTS ${path.join(ASSET_ROOT, "system/public/type/webfonts")}`);

  return { root, papers, src };
}

async function unpack(job, src) {
  const tarPath = path.join(job.dir, "bundle.tar");
  await gunzipCapped(job.bundle, tarPath);
  job.bundle = null; // the upload buffer has served its purpose

  const names = (await tar(["-tf", tarPath])).split("\n").filter(Boolean);
  if (!names.length) throw new Error("bundle is empty");
  for (const name of names) {
    const why = rejectEntry(name);
    if (why) throw new Error(`refusing bundle entry (${why}): ${name}`);
  }

  // -tv leads each line with the mode string; `l`/`h` are sym/hard links.
  for (const line of (await tar(["-tvf", tarPath])).split("\n")) {
    if (line[0] === "l" || line[0] === "h")
      throw new Error(`refusing bundle entry (link): ${line.trim()}`);
  }

  await tar(["-xf", tarPath, "-C", src, "--no-same-owner"]);
  await fs.rm(tarPath, { force: true });
  addLogLine(job, "stdout", `  UNPACK ${names.length} entries`);
}

// The entrypoint is the one .tex with a \documentclass — unless the caller
// named it, in which case take them at their word.
async function findTex(src, wanted) {
  const texs = (await fs.readdir(src)).filter((f) => f.endsWith(".tex"));
  if (wanted) {
    const named = wanted.endsWith(".tex") ? wanted : `${wanted}.tex`;
    if (!texs.includes(named))
      throw new Error(`no ${named} in bundle (found: ${texs.join(", ") || "no .tex at all"})`);
    return named;
  }
  if (!texs.length) throw new Error("no .tex at the top of the bundle");

  const docs = [];
  for (const t of texs) {
    const body = await fs.readFile(path.join(src, t), "utf8").catch(() => "");
    if (body.includes("\\documentclass")) docs.push(t);
  }
  if (docs.length === 1) return docs[0];
  if (docs.length > 1)
    throw new Error(`ambiguous entrypoint — pass --tex (candidates: ${docs.join(", ")})`);
  throw new Error(`no .tex declares a \\documentclass (found: ${texs.join(", ")})`);
}

function killGroup(proc) {
  try {
    process.kill(-proc.pid, "SIGKILL");
  } catch {
    try {
      proc.kill("SIGKILL");
    } catch {}
  }
}

function run(job, cmd, args, cwd, env, msLeft) {
  return new Promise((resolve) => {
    const proc = spawn(cmd, args, {
      cwd,
      env,
      detached: true, // own process group, so a hung pass dies whole
      stdio: ["ignore", "pipe", "pipe"],
    });
    job.process = proc;
    wireStream(job, proc, "stdout");
    wireStream(job, proc, "stderr");

    const alarm = setTimeout(() => {
      job.timedOut = true;
      killGroup(proc);
    }, Math.max(1, msLeft));

    proc.on("error", (err) => {
      clearTimeout(alarm);
      job.process = null;
      addLogLine(job, "stderr", `  ${cmd}: ${err.message}`);
      resolve(127);
    });
    proc.on("close", (code) => {
      clearTimeout(alarm);
      job.process = null;
      resolve(code);
    });
  });
}

// The silent failure: fontspec can't find a font, falls back to nullfont, and
// xelatex still exits 0 with a stub PDF. Same sniff as papers/cli.mjs.
async function logShowsBrokenBuild(logPath) {
  const log = await fs.readFile(logPath, "utf8").catch(() => "");
  return log.includes("! Package fontspec Error") || log.includes("nullfont");
}

async function pdfPages(pdfPath) {
  return new Promise((resolve) => {
    execFile("pdfinfo", [pdfPath], { timeout: 10_000 }, (err, stdout) => {
      if (err) return resolve(null);
      const m = stdout.match(/^Pages:\s+(\d+)/m);
      resolve(m ? parseInt(m[1], 10) : null);
    });
  });
}

async function crunch(job) {
  job.status = "running";
  job.startedAt = nowISO();
  job.stage = "unpack";
  job.percent = 5;

  const { papers, src } = await pitchSandbox(job);
  await unpack(job, src);

  job.tex = await findTex(src, job.wantedTex);
  const base = job.tex.replace(/\.tex$/, "");
  addLogLine(job, "stdout", `  CRUNCH ${job.tex}`);

  const env = {
    ...process.env,
    TERM: "dumb",
    TEXINPUTS: `.:${papers}:`,
    openout_any: "p", // xelatex writes inside the sandbox or not at all
    shell_escape: "f",
  };
  const deadline = Date.now() + BUILD_TIMEOUT_MS;
  const xelatex = ["-interaction=nonstopmode", "-file-line-error", "-no-shell-escape", job.tex];

  const passes = [
    ["xelatex", xelatex, "pass 1"],
    ["bibtex", [base], "bibtex"],
    ["xelatex", xelatex, "pass 2"],
    ["xelatex", xelatex, "pass 3"],
  ];

  for (const [i, [cmd, args, label]] of passes.entries()) {
    job.stage = label;
    job.percent = 10 + Math.round((i / passes.length) * 80);
    addLogLine(job, "stdout", `  ${label.toUpperCase()}`);
    await run(job, cmd, args, src, env, deadline - Date.now());
    if (job.timedOut) throw new Error(`build exceeded ${BUILD_TIMEOUT_MS / 1000}s — killed`);
    if (job.status === "cancelled") throw new Error("cancelled");
    // Exit codes are advisory here: xelatex returns non-zero on warnings and
    // bibtex on a missing .bib. The PDF is the only success criterion.
  }

  job.stage = "verify";
  job.percent = 95;

  const built = path.join(src, `${base}.pdf`);
  if (!existsSync(built)) throw new Error(`no PDF produced — see the log for the LaTeX error`);
  if (await logShowsBrokenBuild(path.join(src, `${base}.log`)))
    throw new Error("fonts failed to load (fontspec/nullfont) — the PDF would be a stub");

  const out = path.join(job.dir, "out.pdf");
  await fs.rename(built, out);
  job.pdfPath = out;
  job.pdfBytes = (await fs.stat(out)).size;
  job.pages = await pdfPages(out);
  addLogLine(
    job,
    "stdout",
    `  OK ${base}.pdf — ${job.pdfBytes} bytes${job.pages ? `, ${job.pages}pp` : ""}`,
  );
}

async function runCrunchJob(job) {
  try {
    await crunch(job);
    job.status = "success";
    job.stage = "done";
    job.percent = 100;
  } catch (err) {
    job.status = job.status === "cancelled" ? "cancelled" : "failed";
    job.stage = job.status;
    job.error = err.message || String(err);
    addLogLine(job, "stderr", `  FAILED: ${job.error}`);
  } finally {
    job.finishedAt = nowISO();
    job.bundle = null;
    job.expiresAt = new Date(Date.now() + JOB_TTL_MS).toISOString();
    // Shred the sources either way — a private document does not linger on the
    // droplet. The PDF (already moved out) survives until the job is reaped.
    await fs.rm(path.join(job.dir, "root"), { recursive: true, force: true }).catch(() => {});
    await fs.rm(path.join(job.dir, "bundle.tar"), { force: true }).catch(() => {});
    if (activeJobId === job.id) activeJobId = null;
  }
}

async function reap() {
  const now = Date.now();
  for (const id of [...jobOrder]) {
    const job = jobs.get(id);
    if (!job || id === activeJobId) continue;
    if (!job.expiresAt || Date.parse(job.expiresAt) > now) continue;
    await fs.rm(job.dir, { recursive: true, force: true }).catch(() => {});
    jobs.delete(id);
    jobOrder.splice(jobOrder.indexOf(id), 1);
  }
  // Anything on disk we've forgotten (a restart mid-job) goes too.
  for (const name of await fs.readdir(WORK_ROOT).catch(() => [])) {
    if (jobs.has(name)) continue;
    await fs.rm(path.join(WORK_ROOT, name), { recursive: true, force: true }).catch(() => {});
  }
}

setInterval(() => reap().catch(() => {}), 5 * 60 * 1000).unref();

export async function startPaperCrunch(bundle, options = {}) {
  if (!Buffer.isBuffer(bundle) || !bundle.length) {
    const err = new Error("empty bundle — POST a gzipped tar of the paper directory");
    err.code = "CRUNCH_BAD_BUNDLE";
    throw err;
  }
  if (bundle.length > MAX_BUNDLE_BYTES) {
    const err = new Error(`bundle over ${MAX_BUNDLE_BYTES} bytes`);
    err.code = "CRUNCH_BAD_BUNDLE";
    throw err;
  }
  if (activeJobId) {
    const err = new Error(`Paper crunch already running: ${activeJobId}`);
    err.code = "CRUNCH_BUSY";
    err.activeJobId = activeJobId;
    throw err;
  }

  const id = randomUUID().slice(0, 10);
  const job = {
    id,
    name: String(options.name || "paper").replace(/[^\w.-]/g, "-").slice(0, 60),
    wantedTex: options.tex ? path.basename(String(options.tex)) : null,
    tex: null,
    dir: path.join(WORK_ROOT, id),
    bundle,
    status: "queued",
    stage: "queued",
    percent: 0,
    createdAt: nowISO(),
    startedAt: null,
    updatedAt: nowISO(),
    finishedAt: null,
    expiresAt: null,
    process: null,
    timedOut: false,
    pdfPath: null,
    pdfBytes: null,
    pages: null,
    error: null,
    logs: [],
  };

  await fs.mkdir(job.dir, { recursive: true });

  jobs.set(id, job);
  jobOrder.unshift(id);
  while (jobOrder.length > MAX_RECENT_JOBS) {
    const old = jobOrder.pop();
    const stale = jobs.get(old);
    if (old === activeJobId || !stale) continue;
    jobs.delete(old);
    fs.rm(stale.dir, { recursive: true, force: true }).catch(() => {});
  }
  activeJobId = id;
  runCrunchJob(job).catch(() => {});
  return makeSnapshot(job);
}

export function getPaperCrunch(jobId, opts = {}) {
  const job = jobs.get(jobId);
  return job ? makeSnapshot(job, opts) : null;
}

export function getPaperCrunchesSummary() {
  return {
    activeJobId,
    active: activeJobId ? makeSnapshot(jobs.get(activeJobId)) : null,
    recent: jobOrder
      .map((id) => jobs.get(id))
      .filter(Boolean)
      .map((j) => makeSnapshot(j)),
  };
}

export function getPaperCrunchPdf(jobId) {
  const job = jobs.get(jobId);
  if (!job) return null;
  if (!job.pdfPath || !existsSync(job.pdfPath)) return null;
  return { path: job.pdfPath, name: `${job.name}.pdf`, bytes: job.pdfBytes };
}

export function cancelPaperCrunch(jobId) {
  const job = jobs.get(jobId);
  if (!job) return { ok: false, error: "not found" };
  if (job.status !== "running") return { ok: false, error: "not running" };
  job.status = "cancelled";
  if (job.process) killGroup(job.process);
  return { ok: true };
}

export const CRUNCH_LIMITS = { MAX_BUNDLE_BYTES, MAX_EXTRACT_BYTES, BUILD_TIMEOUT_MS, JOB_TTL_MS };
