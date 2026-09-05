import assert from "node:assert/strict";
import { readFile } from "node:fs/promises";
import test from "node:test";

import { handler, sanitizeMediascholarStatus } from "../../system/netlify/functions/mediascholar-status.mjs";

test("public Mediascholar status strips private worker fields", () => {
  const status = sanitizeMediascholarStatus({
    state: "working",
    phase: "authoring-paper",
    headline: "Writing a Botted Paper",
    detail: "Drafting safely",
    updatedAt: "2026-09-05T17:00:00Z",
    nextCheckAt: "2026-09-05T18:00:00Z",
    gates: { processor: "ready", memory: "ready", disk: "ready" },
    current: {
      id: "run-1",
      status: "authoring-paper",
      startedAt: "2026-09-05T17:00:00Z",
      worktree: "/home/jas/private",
      provider: "private-provider",
      topic: {
        title: "A live inquiry",
        question: "How do computational media expose their own conditions?",
        claim: "They make infrastructure available as an aesthetic material.",
        whyNow: "The substrate is newly inspectable.",
        terms: ["infrastructure"],
        signals: [
          { title: "Source", url: "https://example.com/paper", kind: "paper", relevance: "Direct evidence" },
          { title: "Local", url: "file:///home/jas/note", kind: "note", relevance: "Private" },
        ],
      },
    },
    activity: { runs: 1, providerRuns: 1, candidates: [] },
  });
  const text = JSON.stringify(status);
  assert.equal(status.current.topic.signals.length, 1);
  assert.equal(text.includes("/home/jas"), false);
  assert.equal(text.includes("private-provider"), false);
  assert.equal(status.safeguards.autoPublish, false);
});

test("status endpoint degrades safely when no worker route is configured", async () => {
  const prior = process.env.MEDIASCHOLAR_WORKER_URL;
  delete process.env.MEDIASCHOLAR_WORKER_URL;
  try {
    const response = await handler({ httpMethod: "GET" });
    const body = JSON.parse(response.body);
    assert.equal(response.statusCode, 200);
    assert.equal(body.state, "unavailable");
    assert.equal(body.safeguards.autoPublish, false);
  } finally {
    if (prior === undefined) delete process.env.MEDIASCHOLAR_WORKER_URL;
    else process.env.MEDIASCHOLAR_WORKER_URL = prior;
  }
});

test("scholar page polls the public status endpoint and names the release gate", async () => {
  const html = await readFile(new URL("../../system/public/papers.aesthetic.computer/scholar/index.html", import.meta.url), "utf8");
  assert.match(html, /fetch\("\/api\/mediascholar-status"/);
  assert.match(html, /Candidates cannot publish themselves\./);
  assert.doesNotMatch(html, /100\.\d+\.\d+\.\d+/);
});
