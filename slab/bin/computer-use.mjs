#!/usr/bin/env node
// Read-only suite diagnostic; suitable for any CLI or host adapter.
import { inspectMachineLeases } from "../lib/computer-use-lease.mjs";
import { createComputerUseClient, COMPUTER_USE_SERVERS } from "../lib/computer-use-client.mjs";
const command = process.argv[2] || "doctor";
if (command !== "doctor") {
  console.error("usage: node slab/bin/computer-use.mjs doctor");
  process.exitCode = 1;
} else {
  const results = await Promise.all(Object.entries(COMPUTER_USE_SERVERS).map(async ([name, url]) => {
    const start = performance.now();
    try {
      const { servers } = await createComputerUseClient({ servers: { [name]: url }, timeoutMs: 3000 }).discover();
      return { name, ok: true, ms: Math.round(performance.now() - start), protocol: servers[0].protocolVersion,
        server: servers[0].serverInfo, workflowGuidance: Boolean(servers[0].instructions) };
    } catch (error) {
      return { name, ok: false, ms: Math.round(performance.now() - start), error: error.message };
    }
  }));
  console.log(JSON.stringify({ services: results, inputLeases: inspectMachineLeases() }, null, 2));
  if (results.some(result => !result.ok)) process.exitCode = 1;
}
