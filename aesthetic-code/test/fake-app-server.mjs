import { createInterface } from "node:readline";

function send(message) {
  process.stdout.write(`${JSON.stringify(message)}\n`);
}

createInterface({ input: process.stdin }).on("line", (line) => {
  const message = JSON.parse(line);
  if (message.method === "initialize") {
    send({ id: message.id, result: { userAgent: "fake" } });
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
