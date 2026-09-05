import assert from "node:assert/strict";
import { chmod, mkdir, mkdtemp, readFile, rm, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";
import test from "node:test";

import { buildLedger, buildMediascholarStatus, createWorkerServer, workerConfig } from "../bin/prox-worker.mjs";

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
  await writeFile(fakeSystemctl, `#!/bin/sh\nprintf '%s\\n' "$*" >> '${calls}'\ncase "$*" in\n  *show*) printf 'ActiveState=inactive\\nSubState=dead\\nResult=success\\nExecMainStatus=0\\nMemoryCurrent=0\\nCPUUsageNSec=0\\n' ;;\n  *list-timers*) printf '[]\\n' ;;\nesac\n`);
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

test("headless Prox exposes a path-free public Mediascholar status", async (t) => {
  const root = await mkdtemp(join(tmpdir(), "prox-scholar-"));
  t.after(() => rm(root, { recursive: true, force: true }));
  const home = join(root, "mediascholar");
  const runDir = join(home, "runs", "20260905-170000-000Z");
  await mkdir(runDir, { recursive: true });
  await writeFile(join(runDir, "run.json"), JSON.stringify({
    id: "20260905-170000-000Z",
    status: "authoring-paper",
    startedAt: "2026-09-05T17:00:00.000Z",
    worktree: "/private/worktree",
    provider: "secret-provider",
  }));
  await writeFile(join(runDir, "topic.json"), JSON.stringify({
    title: "Synthetic media question",
    question: "What changes when a medium can inspect its own circulation?",
    claim: "Self-inspection becomes a material property of the medium.",
    whyNow: "Machine-readable cultural infrastructure is becoming ordinary.",
    terms: ["circulation", "reflexivity"],
    signals: [
      { title: "Public source", url: "https://example.org/source", kind: "paper", relevance: "Evidence" },
      { title: "Private file", url: "file:///private/note", kind: "note", relevance: "Do not expose" },
    ],
  }));
  const fakeSystemctl = join(root, "systemctl");
  await writeFile(fakeSystemctl, `#!/bin/sh\ncase "$*" in\n  *show*mediascholar.service*) printf 'ActiveState=activating\\nSubState=start\\nResult=success\\nExecMainStatus=0\\n' ;;\n  *show*) printf 'ActiveState=inactive\\nSubState=dead\\nResult=success\\nExecMainStatus=0\\n' ;;\n  *list-timers*) printf '[]\\n' ;;\nesac\n`);
  await chmod(fakeSystemctl, 0o700);

  const config = workerConfig({
    PROX_WORKER_BIND: "127.0.0.1",
    PROX_WORKER_LEDGER_DIR: join(root, "ledger"),
    PROX_WORKER_SYSTEMCTL: fakeSystemctl,
    PROX_WORKER_MEDIASCHOLAR_RUNNER: join(root, "missing-runner.mjs"),
    MEDIASCHOLAR_HOME: home,
    MEDIASCHOLAR_ENABLED: "1",
  });
  const status = await buildMediascholarStatus(config);
  assert.equal(status.state, "working");
  assert.equal(status.phase, "authoring-paper");
  assert.equal(status.current.topic.signals.length, 1);
  assert.equal(status.current.topic.signals[0].url, "https://example.org/source");
  assert.equal(JSON.stringify(status).includes("/private/"), false);
  assert.equal(JSON.stringify(status).includes("secret-provider"), false);

  await writeFile(fakeSystemctl, `#!/bin/sh\ncase "$*" in\n  *show*mediascholar-bootstrap.service*) printf 'ActiveState=activating\\nSubState=start\\nResult=success\\nExecMainStatus=0\\n' ;;\n  *show*) printf 'ActiveState=inactive\\nSubState=dead\\nResult=success\\nExecMainStatus=0\\n' ;;\n  *list-timers*) printf '[]\\n' ;;\nesac\n`);
  const bootstrapping = await buildMediascholarStatus(config);
  assert.equal(bootstrapping.state, "working");
  assert.equal(bootstrapping.phase, "bootstrap");
  assert.equal(bootstrapping.headline, "Installing the paper mill");

  const server = createWorkerServer(config, "127.0.0.1");
  const port = await listen(server);
  t.after(() => close(server));
  const response = await fetch(`http://127.0.0.1:${port}/status/mediascholar`);
  assert.equal(response.status, 200);
  assert.equal((await response.json()).system, "mediascholar");
});
