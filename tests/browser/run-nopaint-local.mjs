// One-command local No Paint e2e: reuse a healthy site when one exists,
// otherwise start Netlify dev, wait for HTTPS readiness, run the journey, and
// always stop only the process group this runner created.

import { spawn } from "node:child_process";
import http from "node:http";
import https from "node:https";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";

const ROOT = dirname(dirname(dirname(fileURLToPath(import.meta.url))));
const BASE = process.env.AC_TEST_URL || "http://localhost:8888";

function healthy() {
  const url = new URL("/nopaint?seed=nopaint-e2e-v1&test=1", BASE);
  const client = url.protocol === "https:" ? https : http;
  return new Promise((resolve) => {
    const request = client.get(url, {
      rejectUnauthorized: false,
      timeout: 1500,
    }, (response) => {
      response.resume();
      resolve((response.statusCode || 0) < 500);
    });
    request.on("timeout", () => request.destroy());
    request.on("error", () => resolve(false));
  });
}

async function waitForSite(site, timeoutMs = 30000) {
  const deadline = Date.now() + timeoutMs;
  while (Date.now() < deadline) {
    if (await healthy()) return;
    if (site.exitCode !== null) {
      throw new Error(`local AC site exited before becoming healthy (${site.exitCode})`);
    }
    await new Promise((resolve) => setTimeout(resolve, 500));
  }
  throw new Error(`local AC site did not become healthy at ${BASE}`);
}

function run(command, args, options = {}) {
  return spawn(command, args, { stdio: "inherit", ...options });
}

let site = null;
let exitCode = 1;
try {
  if (!(await healthy())) {
    console.log(`Starting the local AC site for No Paint e2e at ${BASE}…`);
    site = run(process.execPath, [
      join(ROOT, "tests", "browser", "ac-static-server.mjs"),
      join(ROOT, "system", "public", "aesthetic.computer"),
      "8888",
    ], {
      cwd: ROOT,
      detached: true,
    });
    site.on("exit", (code) => {
      if (code && code !== 0) console.error(`local AC site exited early (${code})`);
    });
    await waitForSite(site);
  } else {
    console.log(`Reusing the local AC site at ${BASE}.`);
  }

  const test = run(process.execPath, [
    join(ROOT, "tests", "browser", "nopaint-journey.test.mjs"),
  ], {
    cwd: ROOT,
    env: { ...process.env, AC_TEST_URL: BASE },
  });
  exitCode = await new Promise((resolve) => test.on("exit", (code) => resolve(code ?? 1)));
} finally {
  if (site?.pid) {
    console.log("Stopping the local AC site started by this run…");
    try {
      process.kill(-site.pid, "SIGTERM");
    } catch {}
  }
}

process.exit(exitCode);
