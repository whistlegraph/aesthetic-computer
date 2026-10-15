#!/usr/bin/env node
// Pass only a registered service's keys to its fixed local executable, never
// serialize them through stdout, shell evaluation, command arguments or MCP.
import { spawn } from "node:child_process";
import { requireCredentials } from "./credentials.mjs";

try {
  const [service, executable, ...args] = process.argv.slice(2);
  if (service !== "slack" || !executable?.startsWith("/")) throw new Error("Expected slack and an absolute executable path");
  const { values } = requireCredentials(service);
  const env = { ...process.env, ...values };
  if (process.execve) process.execve(executable, [executable, ...args], env);
  else {
    const child = spawn(executable, args, { env, stdio: "inherit" });
    for (const signal of ["SIGINT", "SIGTERM"]) process.on(signal, () => child.kill(signal));
    child.on("error", () => { console.error("slack-mcp: cannot start executable"); process.exitCode = 1; });
    child.on("exit", code => { process.exitCode = code ?? 1; });
  }
} catch (error) { console.error(error.message); process.exitCode = 1; }
