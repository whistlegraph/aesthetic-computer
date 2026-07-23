#!/usr/bin/env node
import { mkdir, stat } from "node:fs/promises";
import { resolve } from "node:path";
import { memoryCgroup, requireMemoryMax } from "./cgroup.mjs";
import { outputHash, Terrarium } from "./core.mjs";
import { StateRepository, verifyRepository } from "./repository.mjs";
import { sleepCommit } from "./sleep.mjs";

function option(name, fallback) {
  const index = process.argv.indexOf(name);
  return index === -1 ? fallback : process.argv[index + 1];
}

async function demo() {
  const root = resolve(option("--root", "./terrarium-state"));
  const ticks = Number(option("--ticks", "600"));
  const requiredMax = Number(option("--require-memory-max", "0"));
  if (requiredMax) await requireMemoryMax(requiredMax);
  await mkdir(root, { recursive: true });
  let repository;
  try {
    await stat(resolve(root, "seed.json"));
    repository = await StateRepository.open(root, { segmentId: "demo" });
  } catch {
    repository = await StateRepository.create(root, { seed: "intelligent-terrarium-demo-v1", profile: "1gb" });
    repository.segmentPath = resolve(root, "journal", "segments", "demo.ndjson");
  }

  if (!repository.terrarium.state.visitors["@alex"]) {
    await repository.transact("visitor-enter", { handle: "@alex", position: { x: 1, y: 1.7, z: -2 } });
    await repository.transact("visitor-signal", { handle: "@alex", signal: "hello terrarium" });
  }
  const outputs = [];
  let remaining = ticks;
  while (remaining > 0) {
    const amount = Math.min(10, remaining);
    const result = await repository.transact("advance", { ticks: amount });
    outputs.push(...result.outputs);
    remaining -= amount;
  }
  const sleep = await sleepCommit(repository);
  const verification = await verifyRepository(root);
  const candidateA = Terrarium.fromSnapshot(repository.terrarium.snapshot());
  const candidateB = Terrarium.fromSnapshot(repository.terrarium.snapshot());
  const nextA = candidateA.apply({ seq: candidateA.state.lastSeq + 1, kind: "advance", payload: { ticks: 53 } });
  const nextB = candidateB.apply({ seq: candidateB.state.lastSeq + 1, kind: "advance", payload: { ticks: 53 } });
  if (outputHash(nextA) !== outputHash(nextB) || candidateA.stateHash() !== candidateB.stateHash()) {
    throw new Error("next seeded behavior is not deterministic");
  }
  const memory = await memoryCgroup();
  console.log(JSON.stringify({
    ok: verification.stateHash === repository.stateHash(),
    root,
    stateHash: verification.stateHash,
    lastSeq: verification.lastSeq,
    headRecordHash: verification.headRecordHash,
    commit: sleep.commit,
    sleepStatus: sleep.status,
    sonicEvents: outputs.length,
    sonicOutputHash: outputHash(outputs),
    nextBehaviorHash: outputHash(nextA),
    rss: process.memoryUsage().rss,
    memory,
    listener: "none",
  }, null, 2));
}

const command = process.argv[2];
if (command === "demo") {
  await demo();
} else if (command === "verify") {
  console.log(JSON.stringify(await verifyRepository(resolve(option("--root", "./terrarium-state"))), null, 2));
} else {
  console.error("usage: cli.mjs demo|verify [--root PATH] [--ticks N] [--require-memory-max BYTES]");
  process.exitCode = 2;
}
