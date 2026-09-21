// Same native request/done protocol as the SSH agent, without local processes.
// The caller holds the machine lease until both sidecars have been read.
import { socketFrame } from "./frame-socket.mjs";
import { access, mkdir, readFile, rename, unlink, writeFile } from "node:fs/promises";
import { join } from "node:path";
import { randomUUID } from "node:crypto";
import { setTimeout as delay } from "node:timers/promises";

export async function localFrame(stateDir, mode, { timeoutMs = 15000 } = {}) {
  if (process.env.SLAB_FRAME_TRANSPORT !== 'files') {
    const result = await socketFrame(join(stateDir, 'frame-native.sock'), mode, { timeoutMs });
    if (result) return result;
  }
  await mkdir(stateDir, { recursive: true });
  const done = join(stateDir, "frame.done");
  await unlink(done).catch(error => { if (error.code !== "ENOENT") throw error; });
  const temp = join(stateDir, `frame-${randomUUID()}.req`);
  try {
    await writeFile(temp, mode);
    // The native watcher must never consume a partially written request.
    await rename(temp, join(stateDir, "frame.req"));
  } finally {
    await unlink(temp).catch(error => { if (error.code !== "ENOENT") throw error; });
  }
  const deadline = performance.now() + timeoutMs;
  for (;;) {
    try { await access(done); break; }
    catch (error) { if (error.code !== "ENOENT") throw error; }
    // Do not read old sidecars or resend input when the outcome is unknown.
    if (performance.now() >= deadline) throw new Error("local frame timeout; outcome unknown, request not retried");
    await delay(10);
  }
  const [json, jpg] = await Promise.all([
    readFile(join(stateDir, "frame.out.json"), "utf8"),
    readFile(join(stateDir, "frame.out.jpg")).catch(error => {
      if (error.code === "ENOENT") return Buffer.alloc(0);
      throw error;
    }),
  ]);
  return { json, jpg };
}
