import assert from "node:assert/strict";
import test from "node:test";

import { createDirectorMonitor, directorMonitorHtml } from "../lib/director-monitor.mjs";

test("director monitor is a legible left-hand guide toward Panda", () => {
  const html = directorMonitorHtml();
  assert.match(html, /PANDA LIVE/);
  assert.match(html, /font-size:clamp\(42px,6\.8vw,108px\)/);
  assert.match(html, /new EventSource\('\/events'\)/);
});

test("director monitor authenticates updates and serves the current state", async (t) => {
  const { server } = createDirectorMonitor({ token:"shared-secret" });
  await new Promise((resolve) => server.listen(0, "127.0.0.1", resolve));
  t.after(() => new Promise((resolve) => server.close(resolve)));
  const { port } = server.address();
  const url = `http://127.0.0.1:${port}`;
  const state = {
    schema:"captutor-director-state/v1",
    goal:"Record the Settings tour",
    phase:"performing",
    status:"recording",
    beatIndex:2,
    beatCount:9,
    currentLine:"Choose Editor.",
    nextLine:"Turn on Curated Nodes.",
    words:[],
    beatStartedAt:null,
    updatedAt:new Date().toISOString(),
  };

  const rejected = await fetch(`${url}/state`, {
    method:"POST", headers:{ "Content-Type":"application/json" }, body:JSON.stringify(state),
  });
  assert.equal(rejected.status, 401);
  const accepted = await fetch(`${url}/state`, {
    method:"POST",
    headers:{ "Content-Type":"application/json", Authorization:"Bearer shared-secret" },
    body:JSON.stringify(state),
  });
  assert.equal(accepted.status, 204);
  assert.deepEqual(await fetch(`${url}/state`).then((response) => response.json()), state);
});
