#!/usr/bin/env node
// Run Chrome DevTools MCP against one leased browser on a fleet Mac.
//
// The wrapper owns all lifecycle edges: acquire the remote fleet-browser
// lease, open a process-owned SSH tunnel, run the upstream MCP server, touch
// the lease while alive, then release and close the tunnel on every normal or
// signalled exit. A remote reaper handles SIGKILL or controller loss.

import { spawn, spawnSync } from "node:child_process";
import { hostname } from "node:os";
import net from "node:net";

function parse(argv) {
  const out = {
    leaseCommand: "~/.local/bin/fleet-browser",
    package: "chrome-devtools-mcp@latest",
    touchMs: 5 * 60 * 1000,
  };
  for (const arg of argv) {
    const match = arg.match(/^--([^=]+)=(.*)$/);
    if (!match) continue;
    const [, key, value] = match;
    if (key === "host") out.host = value;
    else if (key === "lease-command") out.leaseCommand = value;
    else if (key === "expected-user") out.expectedUser = value;
    else if (key === "package") out.package = value;
    else if (key === "touch-ms") out.touchMs = Number(value);
  }
  if (!out.host || !/^[A-Za-z0-9._-]+$/.test(out.host)) {
    throw new Error("--host=<fleet-ssh-name> is required");
  }
  if (!/^[A-Za-z0-9_@./~-]+$/.test(out.leaseCommand)) {
    throw new Error("--lease-command contains unsafe characters");
  }
  return out;
}

function freePort() {
  return new Promise((resolve, reject) => {
    const server = net.createServer();
    server.unref();
    server.on("error", reject);
    server.listen(0, "127.0.0.1", () => {
      const { port } = server.address();
      server.close(error => (error ? reject(error) : resolve(port)));
    });
  });
}

function remoteLease(config, verb, owner, { quiet = false } = {}) {
  const args = [
    config.host,
    config.leaseCommand,
    verb,
    "--owner",
    owner,
    "--json",
  ];
  if (verb === "acquire" && config.expectedUser) args.push("--expected-user", config.expectedUser);
  const result = spawnSync("ssh", args, { encoding: "utf8", timeout: 25000 });
  if (result.status !== 0 && !quiet) {
    throw new Error(result.stderr.trim() || result.stdout.trim() || `${verb} failed`);
  }
  if (result.status !== 0) return null;
  try {
    return JSON.parse(result.stdout || "{}");
  } catch {
    if (!quiet) throw new Error(`${verb} returned invalid JSON`);
    return null;
  }
}

async function waitForTunnel(port, tunnel, timeoutMs = 12000) {
  const deadline = Date.now() + timeoutMs;
  while (Date.now() < deadline) {
    if (tunnel.exitCode !== null) {
      throw new Error(`SSH tunnel exited ${tunnel.exitCode}`);
    }
    const ready = await new Promise(resolve => {
      const socket = net.createConnection({ host: "127.0.0.1", port });
      socket.setTimeout(500);
      socket.once("connect", () => { socket.destroy(); resolve(true); });
      socket.once("timeout", () => { socket.destroy(); resolve(false); });
      socket.once("error", () => resolve(false));
    });
    if (ready) return;
    await new Promise(resolve => setTimeout(resolve, 200));
  }
  throw new Error(`timed out waiting for fleet Chrome on local port ${port}`);
}

function terminate(child, signal = "SIGTERM") {
  if (!child || child.exitCode !== null) return;
  try { child.kill(signal); } catch {}
}

async function main() {
  const config = parse(process.argv.slice(2));
  const owner = `${hostname()}:chrome-devtools:${process.pid}`.replace(/[^A-Za-z0-9._:@/-]/g, "-");
  let acquired = false;
  let tunnel;
  let mcp;
  let touchTimer;
  let cleaning = false;

  const cleanup = () => {
    if (cleaning) return;
    cleaning = true;
    clearInterval(touchTimer);
    terminate(mcp);
    terminate(tunnel);
    if (acquired) remoteLease(config, "release", owner, { quiet: true });
  };

  for (const signal of ["SIGINT", "SIGTERM", "SIGHUP"]) {
    process.once(signal, () => {
      cleanup();
      process.exit(128 + ({ SIGHUP: 1, SIGINT: 2, SIGTERM: 15 })[signal]);
    });
  }
  process.once("exit", cleanup);

  try {
    const lease = remoteLease(config, "acquire", owner);
    if (!lease?.port || !lease?.webSocketPath) {
      throw new Error("fleet browser lease returned no DevTools endpoint");
    }
    acquired = true;
    const localPort = await freePort();
    tunnel = spawn(
      "ssh",
      [
        "-N",
        "-o", "ExitOnForwardFailure=yes",
        "-o", "ServerAliveInterval=15",
        "-o", "ServerAliveCountMax=2",
        "-L", `${localPort}:127.0.0.1:${lease.port}`,
        config.host,
      ],
      { stdio: ["ignore", "ignore", "inherit"] },
    );
    await waitForTunnel(localPort, tunnel);

    touchTimer = setInterval(
      () => remoteLease(config, "touch", owner, { quiet: true }),
      config.touchMs,
    );
    touchTimer.unref();

    mcp = spawn(
      "npx",
      [
        "-y",
        config.package,
        `--ws-endpoint=ws://127.0.0.1:${localPort}${lease.webSocketPath}`,
        "--no-usage-statistics",
      ],
      { stdio: "inherit" },
    );
    const code = await new Promise((resolve, reject) => {
      mcp.once("error", reject);
      mcp.once("exit", value => resolve(value ?? 1));
    });
    cleanup();
    process.exitCode = code;
  } catch (error) {
    console.error(`fleet-chrome-devtools: ${error.message || error}`);
    cleanup();
    process.exitCode = 1;
  }
}

main();
