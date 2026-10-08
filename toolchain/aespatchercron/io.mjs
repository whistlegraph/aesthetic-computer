import { spawn } from "node:child_process";
import { createHash, randomUUID } from "node:crypto";
import { mkdir, readFile, writeFile, rename, lstat, rm, chmod } from "node:fs/promises";
import { join, resolve } from "node:path";

export const hash = value => createHash("sha256").update(value).digest("hex");
export const json = async path => JSON.parse(await readFile(path, "utf8"));

export async function privateDir(path) {
  await mkdir(path, { recursive: true, mode: 0o700 });
  const st = await lstat(path);
  if (!st.isDirectory() || st.isSymbolicLink() || st.uid !== process.getuid()) throw new Error("Unsafe state directory");
  await chmod(path, 0o700);
  return path;
}

export async function save(path, value) {
  const tmp = `${path}.${randomUUID()}.tmp`;
  await writeFile(tmp, JSON.stringify(value, null, 2) + "\n", { mode: 0o600, flag: "wx" });
  await rename(tmp, path);
}

export async function locked(home, work) {
  await privateDir(home);
  const lock = join(home, "lock");
  try { await mkdir(lock, { mode: 0o700 }); }
  catch (error) { if (error.code === "EEXIST") throw new Error("Runner locked; inspect lock/owner.json before removing a stale lock"); throw error; }
  try {
    await save(join(lock, "owner.json"), { pid: process.pid, at: new Date().toISOString() });
    return await work();
  } finally { await rm(lock, { recursive: true }); }
}

// No shell; timeout and output limits also stop a worker's descendants.
export function run(argv, { cwd, input, timeout = 60000, limit = 2 * 1024 * 1024, env = {}, cleanEnv = false } = {}) {
  if (!Array.isArray(argv) || !argv.length || argv.some(s => typeof s !== "string" || s.includes("\0"))) throw new Error("Expected command argv");
  return new Promise((resolveRun, reject) => {
    const child = spawn(argv[0], argv.slice(1), { cwd, detached: true, env: { ...(cleanEnv ? {} : process.env), AC_NO_AUTO_DEPLOY: "1", ...env }, stdio: ["pipe", "pipe", "pipe"] });
    const chunks = [[], []]; let size = 0, failure;
    const stop = message => {
      failure = new Error(message);
      try { process.kill(-child.pid, "SIGKILL"); } catch {}
    };
    const timer = setTimeout(() => stop("Command timed out"), timeout);
    [child.stdout, child.stderr].forEach((stream, i) => stream.on("data", data => {
      size += data.length;
      if (size > limit) stop("Command output limit exceeded"); else chunks[i].push(data);
    }));
    child.on("error", error => { clearTimeout(timer); reject(new Error(`Command unavailable (${error.code || "spawn"})`)); });
    child.on("close", (code, signal) => {
      clearTimeout(timer);
      if (failure) reject(failure);
      else resolveRun({ code, signal, stdout: Buffer.concat(chunks[0]).toString(), stderr: Buffer.concat(chunks[1]).toString() });
    });
    child.stdin.on("error", () => {});
    child.stdin.end(input);
  });
}

export async function git(repo, ...args) {
  const result = await run(["git", ...args], { cwd: repo });
  if (result.code !== 0) throw new Error(`Git ${args[0]} failed (${result.code ?? result.signal})`);
  return result.stdout.trimEnd();
}

export function relativeFile(file) {
  if (typeof file !== "string" || !/^[a-zA-Z0-9_./-]+$/.test(file) || file.startsWith("/") || file.split("/").some(s => s === ".." || s === ".git" || !s)) throw new Error("Invalid repository file");
  return file;
}

export function statePath(home, id) {
  if (!/^[a-f0-9]{20}$/.test(id)) throw new Error("Invalid task id");
  return resolve(home, "tasks", id);
}
