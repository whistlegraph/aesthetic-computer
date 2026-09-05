import assert from "node:assert/strict";
import { chmod, mkdir, mkdtemp, readFile, rm, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";
import test from "node:test";

import { buildLedger, createWorkerServer, workerConfig } from "../bin/prox-worker.mjs";

const listen = (server) => new Promise((resolve, reject) => {
  server.once("error", reject);
  server.listen(0, "127.0.0.1", () => resolve(server.address().port));
});
const close = (server) => new Promise((resolve, reject) => server.close((error) => error ? reject(error) : resolve()));

test("headless Prox sanitizes its ledger and exposes only allowlisted systemd jobs", async (t) => {
  const root = await mkdtemp(join(tmpdir(), "prox-worker-"));
  t.after(() => rm(root, { recursive: true, force: true }));
  const ledgerDir = join(root, "ledger");
  const advertiseDir = join(ledgerDir, "advertise");
  await mkdir(advertiseDir, { recursive: true });
  await writeFile(join(advertiseDir, "mediascholar.json"), JSON.stringify({
    id: "candidate-1",
    host: "spoofed-host",
    name: "Media Scholar!!!",
    kind: "session",
    status: "awaiting",
    subject: "Candidate ready",
    updated: 42,
  }));

  const calls = join(root, "systemctl.calls");
  const fakeSystemctl = join(root, "systemctl");
  await writeFile(fakeSystemctl, `#!/bin/sh\nprintf '%s\\n' "$*" >> '${calls}'\ncase "$*" in\n  *show*) printf 'ActiveState=inactive\\nSubState=dead\\nResult=success\\nExecMainStatus=0\\nMemoryCurrent=0\\nCPUUsageNSec=0\\n' ;;\nesac\n`);
  await chmod(fakeSystemctl, 0o700);

  const config = workerConfig({
    PROX_WORKER_HOST: "jasellite",
    PROX_WORKER_BIND: "127.0.0.1",
    PROX_WORKER_LEDGER_DIR: ledgerDir,
    PROX_WORKER_ADVERTISE_DIR: advertiseDir,
    PROX_WORKER_LOCAL_FILE: join(ledgerDir, "local.json"),
    PROX_WORKER_SYSTEMCTL: fakeSystemctl,
  });
  const ledger = await buildLedger(config, "100.64.0.10");
  assert.equal(ledger.entries[0].host, "jasellite");
  assert.equal(ledger.entries[0].name, "media-scholar");
  assert.equal(ledger.entries[0].kind, "agent");

  const server = createWorkerServer(config, "127.0.0.1");
  const port = await listen(server);
  t.after(() => close(server));
  const denied = await fetch(`http://127.0.0.1:${port}/job`, {
    method: "POST",
    headers: { "content-type": "application/json" },
    body: JSON.stringify({ job: "shell", action: "start", command: "touch /tmp/no" }),
  });
  assert.equal(denied.status, 400);

  const started = await fetch(`http://127.0.0.1:${port}/job`, {
    method: "POST",
    headers: { "content-type": "application/json" },
    body: JSON.stringify({ job: "mediascholar", action: "start" }),
  });
  assert.equal(started.status, 202);
  assert.match(await readFile(calls, "utf8"), /--user start --no-block mediascholar\.service/);

  const status = await fetch(`http://127.0.0.1:${port}/job`, {
    method: "POST",
    headers: { "content-type": "application/json" },
    body: JSON.stringify({ job: "mediascholar", action: "status" }),
  }).then((response) => response.json());
  assert.equal(status.properties.ActiveState, "inactive");
});
