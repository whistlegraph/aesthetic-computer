import { createInterface } from "node:readline";

function send(message) {
  process.stdout.write(`${JSON.stringify(message)}\n`);
}

createInterface({ input: process.stdin }).on("line", (line) => {
  const message = JSON.parse(line);
  if (message.method === "initialize") {
    send({ id: message.id, result: { userAgent: "fake" } });
  } else if (message.method === "test/error") {
    send({id:message.id,error:{code:-32000,message:"stream unavailable",data:{status:503,codexErrorInfo:{responseStreamDisconnected:{}}}}});
  } else if (message.method === "test/exit") {
    process.exit(1);
  } else if (message.method === "account/rateLimits/read") {
    if (process.env.AESEL_TEST_RATE_LIMITS === "unavailable") {
      send({ id: message.id, error: { code: -32000, message: "Rate limits unavailable for API key" } });
      return;
    }
    if (process.env.AESEL_TEST_RATE_LIMITS === "silent") return;
    const limits = {
      limitId: "codex",
      primary: { usedPercent: 23, windowDurationMins: 300, resetsAt: 4102444800 },
      secondary: { usedPercent: 61, windowDurationMins: 10080, resetsAt: 4102444800 },
    };
    send({ id: message.id, result: { rateLimits: { limitId: "other", primary: { usedPercent: 99 } }, rateLimitsByLimitId: { codex: limits } } });
  } else if (message.method === "test/rateLimits") {
    send({ method: "account/rateLimits/updated", params: { rateLimits: message.params } });
    send({ id: message.id, result: {} });
  } else if (message.method === "thread/start" || message.method === "thread/resume") {
    send({
      id: message.id,
      result: {
        thread: { id: message.params.threadId || "thread-1", turns: [] },
        model: "test-model",
        modelProvider: "test",
        cwd: message.params.cwd,
        approvalPolicy: "on-request",
        approvalsReviewer: "user",
        sandbox: { type: "workspaceWrite" },
      },
    });
  } else if (message.method === "turn/start") {
    send({ id: message.id, result: { turn: { id: "turn-1", status: "inProgress", items: [] } } });
    send({
      method: "turn/started",
      params: { threadId: "thread-1", turn: { id: "turn-1", status: "inProgress", items: [] } },
    });
    send({
      method: "item/commandExecution/requestApproval",
      id: 900,
      params: {
        threadId: "thread-1",
        turnId: "turn-1",
        itemId: "item-1",
        command: "npm test",
        startedAtMs: Date.now(),
      },
    });
  } else if (message.id === 900 && message.result?.decision === "accept") {
    send({
      method: "item/agentMessage/delta",
      params: { threadId: "thread-1", turnId: "turn-1", itemId: "answer-1", delta: "Tests pass." },
    });
    send({
      method: "turn/completed",
      params: { threadId: "thread-1", turn: { id: "turn-1", status: "completed", items: [] } },
    });
  }
});
