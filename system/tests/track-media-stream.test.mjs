import assert from "node:assert/strict";
import { spawnSync } from "node:child_process";
import test from "node:test";

// Each scenario has its own process: an unhandled producer rejection must fail
// the regression even after the HTTP handler has already returned its body.
async function exercise() {
  const { readFileSync } = await import("node:fs");
  const { setImmediate: tick } = await import("node:timers/promises");
  const [filename, scenario] = process.argv.slice(1);
  const state = { events: [], connects: 0, saves: 0, disconnects: 0 };
  let releaseSave;
  const saving = new Promise((resolve) => { releaseSave = resolve; });
  const fixtures = {
    authorize: async () => null,
    connect: async () => {
      state.connects++;
      return {
        db: {},
        disconnect: () => {
          state.disconnects++;
          if (scenario === "cleanup-throw") throw new Error("cleanup fixture");
          if (scenario === "cleanup-reject") return Promise.reject(new Error("cleanup fixture"));
          return tick();
        },
      };
    },
    sealPaintingWip: async () => {
      state.saves++;
      if (scenario === "cancel-during-save") await saving;
      if (scenario === "save-error" || scenario === "cancel-error-event") {
        throw new Error("save fixture");
      }
      return { code: "fixture", slug: "fixture" };
    },
    stream: (handler) => handler,
  };

  // Inject external services without installing dependencies or touching a DB.
  // Execute the real handler, including its actual Web Stream and cleanup.
  const source = readFileSync(filename, "utf8")
    .replace(/^import \{([^}]+)\} from "[^"]+";$/gm,
      (_, names) => `const {${names}} = fixtures;`)
    .replace("export const handler", "const handler");
  const handler = new Function("fixtures", `${source}\nreturn handler;`)(fixtures);
  const response = await handler({
    httpMethod: "POST",
    headers: {},
    body: scenario === "invalid-json" ? "{" : JSON.stringify({ext: "png", wip: "fixture"}),
  });
  const reader = response.body.getReader();
  for (;;) {
    const { value, done } = await reader.read();
    if (done) break;
    const text = new TextDecoder().decode(value);
    state.events.push(text);
    if (scenario === "cancel-early" ||
        (scenario === "cancel-complete" && text.includes("event: complete")) ||
        (scenario === "cancel-error-event" && text.includes("event: error"))) {
      await reader.cancel("client disconnected");
      break;
    }
    if (scenario === "cancel-during-save" && text.includes('"stage":"database"')) {
      await tick();
      await reader.cancel("client disconnected");
      releaseSave();
      break;
    }
  }
  await tick();
  await tick();
  await tick();
  console.log(JSON.stringify(state));
}

const handler = new URL("../netlify/functions/track-media-stream.mjs", import.meta.url);
for (const scenario of ["complete", "cancel-complete", "cancel-early", "cancel-during-save",
  "invalid-json", "save-error", "cancel-error-event", "cleanup-throw", "cleanup-reject"]) {
  test(`track-media-stream survives ${scenario}`, () => {
    const child = spawnSync(process.execPath, [
      "--unhandled-rejections=strict", "--input-type=module", "-e",
      `await (${exercise.toString()})();`, handler.pathname, scenario,
    ], { encoding: "utf8", timeout: 5000 });
    assert.equal(child.status, 0, child.stderr || child.error?.message);
    const result = JSON.parse(child.stdout.trim().split("\n").at(-1));
    const early = scenario === "cancel-early" || scenario === "invalid-json";
    assert.equal(result.connects, early ? 0 : 1);
    assert.equal(result.disconnects, early ? 0 : 1);
    assert.equal(result.saves, early ? 0 : 1);
    if (["complete", "cancel-complete", "cleanup-throw", "cleanup-reject"].includes(scenario)) {
      assert.ok(result.events.some((event) => event.includes("event: complete")));
    }
    if (["invalid-json", "save-error", "cancel-error-event"].includes(scenario)) {
      assert.ok(result.events.some((event) => event.includes("event: error")));
    }
    if (scenario.startsWith("cleanup-")) assert.match(child.stderr, /cleanup fixture/);
  });
}
