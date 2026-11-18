#!/usr/bin/env node
// steps-replay.mjs, 2026.11.18
// Run the requests the classic step test captured through the real painting
// service (system/backend/painting-wips.mjs), with its storage kept in
// memory. The browser test plays the service itself; this is the other half:
// it proves the service's own checks accept what the page sends.
//
//   node toolchain/nopaint/steps-replay.mjs [tmp/nopaint-steps-test/wip-calls.json]
//
// The file is OUT/wip-calls.json from tests/browser/classic-steps.e2e.mjs.

import { readFileSync } from "node:fs";
import { createPaintingWipService } from "../../system/backend/painting-wips.mjs";

const file = process.argv[2] || new URL("../../tmp/nopaint-steps-test/wip-calls.json", import.meta.url);
const calls = JSON.parse(readFileSync(file, "utf8"));
const rows = [], states = new Map();
let stateId = 0;
const matches = (row, query) => Object.entries(query).every(([key, value]) => key.split(".").reduce((at, part) => at?.[part], row) === value);
const repo = {
  // A copy, as a database gives: the service compares what it read with what it wrote.
  find: async (query) => { const row = rows.find((item) => matches(item, query)); return row ? structuredClone(row) : null; },
  code: async () => "r3pl",
  insert: async (record) => { rows.push({ _id: rows.length + 1, ...record }); },
  update: async (query, fields) => { const row = rows.find((item) => matches(item, query)); if (row) Object.assign(row, fields); return Boolean(row); },
  writeState: async (buffer) => { states.set(++stateId, buffer); return stateId; },
  readState: async (id) => states.get(id),
  deleteState: async (id) => { states.delete(id); },
};
const service = createPaintingWipService(repo);
let failed = 0;
for (const [n, body] of calls.entries()) {
  // The test's own service named the painting "t3st"; this one names it "r3pl".
  const input = body.code ? { ...body, code: "r3pl" } : body;
  try {
    const result = body.action === "create" ? await service.create(input, null)
      : body.action === "save" ? await service.save(input, null)
      : await service.read(input.code, Boolean(input.state), null, input.key);
    console.log(`ok   ${String(n + 1).padStart(2)} ${body.action.padEnd(6)} → #${result.code} revision ${result.revision}, ${result.steps} steps${result.canEdit === undefined ? "" : `, canEdit ${result.canEdit}`}`);
  } catch (error) {
    failed++;
    console.log(`FAIL ${String(n + 1).padStart(2)} ${body.action.padEnd(6)} → ${error.status || ""} ${error.message}`);
  }
}
const painting = rows[0];
console.log(`${calls.length - failed} of ${calls.length} accepted${painting ? `; the painting is #${painting.code} v${painting.wip.steps} (${painting.status})` : ""}`);
process.exit(failed ? 1 : 0);
