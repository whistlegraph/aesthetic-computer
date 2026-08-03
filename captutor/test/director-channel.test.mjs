import assert from "node:assert/strict";
import test from "node:test";

import {
  DirectorChannel, directorBeatState, resolveDirectorGoal,
} from "../lib/director-channel.mjs";

test("director goal follows the matching Iris task", () => {
  assert.equal(resolveDirectorGoal("Fallback", {
    taskGid:"42",
    progress:{ taskGid:"42", mission:"Record the Settings tour" },
    mission:{ items:[{ status:"in_progress", text:"Older mission" }] },
  }), "Record the Settings tour");
  assert.equal(resolveDirectorGoal("Fallback", {
    taskGid:"42",
    progress:{ taskGid:"99", mission:"Wrong task" },
    mission:{ items:[{ status:"in_progress", text:"Tour every node" }] },
  }), "Tour every node");
});

test("director beat state carries exact narration timing and the next line", () => {
  const words = [{ text:"Open", fromMs:0, toMs:300 }];
  assert.deepEqual(directorBeatState([
    { say:"Open Settings.", words, cursorIntent:"Open the settings panel" },
    { say:"Choose Editor." },
  ], 0, Date.parse("2026-07-29T16:00:00.000Z")), {
    phase:"performing",
    status:"recording",
    beatIndex:0,
    beatCount:2,
    currentLine:"Open Settings.",
    nextLine:"Choose Editor.",
    words,
    beatStartedAt:"2026-07-29T16:00:00.000Z",
    currentAction:"Open the settings panel",
  });
});

test("director publisher sends authenticated state and remains optional", async () => {
  const calls = [];
  const director = new DirectorChannel({
    url:"http://chicken.test/state",
    token:"shared-secret",
    goal:"Settings tour",
    fetchImpl:async (url, options) => calls.push({ url, options }),
  });
  director.publish({ currentLine:"Open Settings" });
  await director.close({ phase:"complete", status:"complete" });
  assert.equal(calls.length, 2);
  assert.equal(calls[0].options.headers.Authorization, "Bearer shared-secret");
  assert.equal(JSON.parse(calls[0].options.body).currentLine, "Open Settings");
  assert.equal(JSON.parse(calls[1].options.body).phase, "complete");

  const disabled = new DirectorChannel();
  assert.equal(disabled.publish({ currentLine:"No monitor" }).currentLine, "No monitor");
});
