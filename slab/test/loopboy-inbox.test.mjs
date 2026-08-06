import test from "node:test";
import assert from "node:assert/strict";
import { mkdtemp, readdir, rm } from "node:fs/promises";
import { join } from "node:path";
import { tmpdir } from "node:os";

test("Loopboy bus isolates contacts, survives route replacement, and claims once", async () => {
  const root = await mkdtemp(join(tmpdir(), "loopboy-inbox-"));
  process.env.SLAB_LOOPBOY_INBOX = root;
  process.env.SLAB_LOOPBOY_BUS = root;
  const { enqueueLoopboyEvent, waitLoopboyEvent } = await import(
    `../lib/loopboy-inbox.mjs?test=${Date.now()}`
  );
  const gimipi = "41852042-FE5B-41E9-8760-D4680D73928C";
  const meloza = "3DEB321E-A29B-4C32-8DB1-9DC8F382E0F3";
  try {
    await enqueueLoopboyEvent({ sessionId: gimipi, contact: "alex", kind: "heartbeat" });
    await enqueueLoopboyEvent({ sessionId: meloza, contact: "loretta", kind: "message" });

    const melozaEvent = await waitLoopboyEvent(meloza, { contact: "loretta", timeoutMs: 0 });
    assert.equal(melozaEvent.contact, "loretta");
    assert.equal(melozaEvent.sessionId, meloza);

    // A newly launched guarded session for the same contact inherits the
    // durable bus instead of abandoning messages under the old route id.
    const replacement = "AAAAAAAA-BBBB-CCCC-DDDD-EEEEEEEEEEEE";
    const gimipiEvent = await waitLoopboyEvent(replacement, { contact: "alex", timeoutMs: 0 });
    assert.equal(gimipiEvent.contact, "alex");
    assert.equal(gimipiEvent.sessionId, gimipi);

    assert.equal(await waitLoopboyEvent(gimipi, { contact: "alex", timeoutMs: 0 }), null);
    assert.equal(await waitLoopboyEvent(meloza, { contact: "loretta", timeoutMs: 0 }), null);
  } finally {
    await rm(root, { recursive: true, force: true });
  }
});

test("Loopboy inbox coalesces unattended heartbeats", async () => {
  const root = await mkdtemp(join(tmpdir(), "loopboy-heartbeat-"));
  process.env.SLAB_LOOPBOY_INBOX = root;
  process.env.SLAB_LOOPBOY_BUS = root;
  const { enqueueLoopboyEvent, waitLoopboyEvent } = await import(
    `../lib/loopboy-inbox.mjs?heartbeat=${Date.now()}`
  );
  const sessionId = "41852042-FE5B-41E9-8760-D4680D73928C";
  try {
    await enqueueLoopboyEvent({ sessionId, contact: "alex", kind: "heartbeat", excerpt: "first" });
    await enqueueLoopboyEvent({ sessionId, contact: "alex", kind: "heartbeat", excerpt: "latest" });
    assert.equal((await readdir(join(root, "alex"))).length, 1);
    assert.equal((await waitLoopboyEvent(sessionId, { contact: "alex", timeoutMs: 0 })).excerpt, "latest");
  } finally {
    await rm(root, { recursive: true, force: true });
  }
});
