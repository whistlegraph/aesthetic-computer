// Headless logic timings with counted drawing stubs; never actual render FPS.
import { spawn } from "node:child_process";
import { readFile } from "node:fs/promises";
import { extname } from "node:path";

function bounded(value, fallback, min, max, name) {
  const number = value === undefined ? fallback : value;
  if (!Number.isSafeInteger(number) || number < min || number > max) throw new Error(`${name} must be ${min}–${max}.`);
  return number;
}

export async function benchmarkPiece({ file, frames, warmup, width, height, seed, timeoutMs, signal } = {}) {
  if (!file || extname(file) !== ".mjs") throw new Error("Headless logic benchmarks currently support .mjs pieces only.");
  // The child needs stable Node permissions. Never silently fall back to an
  // unrestricted process on an older installed runtime.
  if (Number(process.versions.node.split(".")[0]) < 22 || !process.allowedNodeEnvironmentFlags.has("--permission")) throw new Error("Headless benchmarks require Node with --permission support; use Node 24 or newer.");
  const options = {
    frames: bounded(frames, 600, 1, 1200, "frames"),
    warmup: bounded(warmup, 60, 0, 120, "warmup"),
    width: bounded(width, 800, 1, 4096, "width"),
    height: bounded(height, 600, 1, 4096, "height"),
    seed: bounded(seed, 1, 0, 4294967295, "seed"),
    timeoutMs: bounded(timeoutMs, 3000, 100, 10000, "timeoutMs"),
  };
  signal?.throwIfAborted();
  const [source, worker] = await Promise.all([
    readFile(file, "utf8"), readFile(new URL("./perf-worker.mjs", import.meta.url), "utf8"),
  ]);
  if (Buffer.byteLength(source) > 1_048_576) throw new Error("The piece exceeds the benchmark's 1 MB source limit.");
  signal?.throwIfAborted();
  return new Promise((resolve, reject) => {
    const child = spawn(process.execPath, ["--permission", "--no-addons", "--max-old-space-size=64", "--experimental-vm-modules", "--input-type=module", "-e", worker], {
      env: { NODE_NO_WARNINGS: "1" },
      stdio: ["pipe", "pipe", "pipe"],
    });
    let output = "", errors = "", failure;
    const stop = (error) => { failure ||= error; child.kill("SIGKILL"); };
    const timer = setTimeout(() => stop(new Error(`Headless benchmark exceeded ${options.timeoutMs} ms.`)), options.timeoutMs);
    const abort = () => stop(signal.reason || new Error("Benchmark cancelled."));
    signal?.addEventListener("abort", abort, { once: true });
    child.stdin.on("error", () => {});
    child.stdout.on("data", (chunk) => {
      output += chunk;
      if (output.length > 65536) stop(new Error("Benchmark output exceeded its limit."));
    });
    child.stderr.on("data", (chunk) => { if (errors.length < 8192) errors += chunk; });
    child.on("error", (error) => { clearTimeout(timer); signal?.removeEventListener("abort", abort); reject(error); });
    child.on("close", (code) => {
      clearTimeout(timer);
      signal?.removeEventListener("abort", abort);
      if (failure) return reject(failure);
      if (code !== 0) return reject(new Error(`Headless benchmark: ${errors.trim() || `process exited ${code}`}`));
      try { resolve(JSON.parse(output)); } catch { reject(new Error("Invalid benchmark result.")); }
    });
    child.stdin.end(JSON.stringify({ ...options, source }));
  });
}
