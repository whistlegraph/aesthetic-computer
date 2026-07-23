import { spawn } from "node:child_process";
import { readFile } from "node:fs/promises";

function metricsFrom(output, latencyMs, smapsPeakRssKb) {
  const evalMatch = /eval time\s*=\s*[\d.]+ ms\s*\/\s*(\d+) tokens\s*\(\s*([\d.]+) tokens per second\s*\)/i.exec(output);
  const promptMatch = /prompt eval time\s*=\s*[\d.]+ ms\s*\/\s*(\d+) tokens\s*\(\s*([\d.]+) tokens per second\s*\)/i.exec(output);
  const summaryMatch = /\[\s*Prompt:\s*([\d.]+) t\/s\s*\|\s*Generation:\s*([\d.]+) t\/s\s*\]/i.exec(output);
  return {
    latencyMs: Math.round(latencyMs * 100) / 100,
    outputTokens: evalMatch ? Number(evalMatch[1]) : null,
    outputTokensPerSecond: evalMatch ? Number(evalMatch[2]) : (summaryMatch ? Number(summaryMatch[2]) : null),
    promptTokens: promptMatch ? Number(promptMatch[1]) : null,
    promptTokensPerSecond: promptMatch ? Number(promptMatch[2]) : (summaryMatch ? Number(summaryMatch[1]) : null),
    smapsPeakRssKb,
  };
}

async function rssKb(pid) {
  try {
    const text = await readFile(`/proc/${pid}/smaps_rollup`, "utf8");
    return Number(/^Rss:\s+(\d+) kB$/m.exec(text)?.[1] || 0);
  } catch {
    return 0;
  }
}

export function createLlamaCliInfer({ binary, model, threads = 4, nice = "/usr/bin/nice" } = {}) {
  if (!binary || !model) throw new TypeError("llama-cli binary and model are required");
  return ({ prompt, contextTokens, maxOutputTokens, timeoutMs, signal }) => new Promise((resolve, reject) => {
    const subcommand = /(?:^|\/)llama$/.test(binary) ? ["cli"] : [];
    const args = [
      "-n", "15", binary,
      ...subcommand,
      "--model", model,
      "--prompt", prompt,
      "--ctx-size", String(contextTokens),
      "--n-predict", String(maxOutputTokens),
      "--threads", String(threads),
      "--batch-size", "256",
      "--ubatch-size", "256",
      "--n-gpu-layers", "0",
      "--device", "none",
      "--fit", "off",
      "--cache-type-k", "q8_0",
      "--cache-type-v", "q8_0",
      "--seed", "23",
      "--temp", "0.2",
      "--top-k", "20",
      "--top-p", "0.8",
      "--perf",
      "--no-warmup",
      "--no-context-shift",
      "--conversation",
      "--single-turn",
      "--reasoning", "off",
      "--reasoning-budget", "0",
      "--no-display-prompt",
      "--simple-io",
    ];
    const started = performance.now();
    const child = spawn(nice, args, {
      stdio: ["ignore", "pipe", "pipe"],
      env: { ...process.env, CUDA_VISIBLE_DEVICES: "" },
    });
    let stdout = "";
    let stderr = "";
    let smapsPeakRssKb = 0;
    let timedOut = false;
    const sample = setInterval(() => {
      void rssKb(child.pid).then((value) => { smapsPeakRssKb = Math.max(smapsPeakRssKb, value); });
    }, 25);
    sample.unref();
    const timer = setTimeout(() => {
      timedOut = true;
      child.kill("SIGKILL");
    }, timeoutMs);
    timer.unref();
    const abort = () => {
      timedOut = true;
      child.kill("SIGKILL");
    };
    signal?.addEventListener("abort", abort, { once: true });
    child.stdout.on("data", (chunk) => {
      stdout += chunk;
      if (stdout.length > 65_536) child.kill("SIGKILL");
    });
    child.stderr.on("data", (chunk) => {
      stderr += chunk;
      if (stderr.length > 262_144) child.kill("SIGKILL");
    });
    child.once("error", reject);
    child.once("close", async (code, killedBy) => {
      clearTimeout(timer);
      clearInterval(sample);
      signal?.removeEventListener("abort", abort);
      smapsPeakRssKb = Math.max(smapsPeakRssKb, await rssKb(child.pid));
      if (timedOut) {
        const error = new Error("llama-cli timed out");
        error.code = "ETIMEDOUT";
        reject(error);
      } else if (code !== 0) {
        const error = new Error(`llama-cli exited ${code ?? killedBy ?? "unknown"}`);
        error.code = "ELLAMA";
        reject(error);
      } else {
        const combined = `${stderr}\n${stdout}`;
        const metrics = metricsFrom(combined, performance.now() - started, smapsPeakRssKb);
        if (/failed to initialize samplers|\berror:/i.test(combined)
          || metrics.outputTokensPerSecond === 0) {
          const error = new Error("llama-cli produced no usable generation");
          error.code = "ELLAMA";
          reject(error);
          return;
        }
        const promptAt = stdout.lastIndexOf(prompt);
        const generated = promptAt >= 0 ? stdout.slice(promptAt + prompt.length) : stdout;
        resolve({
          text: generated.trim(),
          metrics,
        });
      }
    });
  });
}
