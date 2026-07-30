import assert from "node:assert/strict";
import test from "node:test";

import {
  classifyAssignedTask, nextRunnableTask, prioritySort, shouldPreempt,
  unresolvedBlockingTask, validateMissionApproval, validateMissionReceipt,
} from "../ops/iris-mission-policy.mjs";
import {
  bumpBlockingMission, clearStaleRecovery,
} from "../ops/iris-priority-heartbeat.mjs";

function task(overrides = {}) {
  return classifyAssignedTask({
    gid:"joey", name:"Recover UGC Deal Tracker", tags:["mission"],
    notes:"MISSION_ID: fuser-joeydon-recovery\nMISSION_KIND: fuser-recovery\nMISSION_PRIORITY: blocking\nEXECUTION_HOST: chicken\nORIGINAL_ASSET_ID: original",
    ...overrides,
  });
}

test("blocking recovery directives create an operational mission", () => {
  const mission = task();
  assert.equal(mission.kind, "mission");
  assert.equal(mission.priority, "blocking");
  assert.equal(mission.executionHost, "chicken");
});

test("blocking mission parks lower-priority work until receipt completion", () => {
  const mission = task();
  const lower = { gid:"taco", name:"Talking Taco", kind:"captutor", priority:"normal" };
  const tasks = prioritySort([lower, mission]);
  assert.equal(unresolvedBlockingTask(tasks, {}), mission);
  assert.equal(nextRunnableTask(tasks, {}), mission);
  assert.equal(shouldPreempt({ taskGid:"taco", priority:"normal" }, mission), true);
  const failed = { joey:{ status:"failed" } };
  assert.equal(nextRunnableTask(tasks, failed), null);
  const complete = { joey:{ status:"done" } };
  assert.equal(nextRunnableTask(tasks, complete), lower);
});

test("failed Captutor assignment halts the queue until recovery or unassignment", () => {
  const failed = { gid:"english", name:"Image node — English", kind:"captutor" };
  const queued = { gid:"french", name:"Image node — French", kind:"captutor" };
  assert.equal(nextRunnableTask([failed, queued], {
    english:{ kind:"captutor", status:"failed" },
  }), null);
  assert.equal(nextRunnableTask([failed, queued], {}), failed);
  assert.equal(nextRunnableTask([queued], {
    english:{ kind:"captutor", status:"failed" },
  }), queued);
});

test("mission receipt requires Chicken, a fresh candidate, chunk verification, and reviewed handoff", () => {
  const mission = task();
  const receipt = {
    schema:"iris-mission-receipt/v1", taskGid:"joey",
    missionId:"fuser-joeydon-recovery", status:"complete",
    executionHost:"chicken", originalAssetId:"original",
    originalPreserved:true, chunkedResourceVerified:true,
    candidateAssetId:"candidate", ownerHandoff:{ approved:true, projectId:"project" },
  };
  assert.equal(validateMissionReceipt(receipt, mission), receipt);
  assert.throws(() => validateMissionReceipt({ ...receipt, executionHost:"panda" }, mission), /expected chicken/);
  assert.throws(() => validateMissionReceipt({ ...receipt, originalPreserved:false }, mission), /preservation/);
  assert.throws(() => validateMissionReceipt({ ...receipt, candidateAssetId:"original" }, mission), /fresh repaired candidate/);
});

test("mission approval must independently match task, host, and original asset", () => {
  const mission = task();
  const approval = {
    schema:"iris-mission-approval/v1", taskGid:"joey",
    missionId:"fuser-joeydon-recovery", executionHost:"chicken",
    originalAssetId:"original", approvedBy:"@jeffrey",
    approvedAt:"2026-07-27T22:15:00-07:00",
  };
  assert.equal(validateMissionApproval(approval, mission), approval);
  assert.throws(() => validateMissionApproval({ ...approval, executionHost:"neo" }, mission), /host mismatch/);
  assert.throws(() => validateMissionApproval({ ...approval, originalAssetId:"other" }, mission), /asset mismatch/);
});

test("heartbeat bumps a stalled blocker and clears its tombstone", () => {
  const state = {
    active:{ taskGid:"joey", name:"Recover", priority:"blocking", pid:999999, startedAt:1 },
    done:{ joey:{ status:"failed" } },
    priorityMission:{ taskGid:"joey", name:"Recover", priority:"blocking", status:"active", updatedAt:1 },
  };
  const result = bumpBlockingMission(state, 10_000, { stallMs:100, cooldownMs:0, maximumBumps:3 });
  assert.equal(result.action, "bumped");
  assert.equal(state.active, null);
  assert.equal(state.done.joey, undefined);
  assert.equal(state.priorityMission.heartbeatBumps, 1);
  assert.match(state.recovery.activity, /lower-priority work remains parked/);
});

test("heartbeat gives a newly queued blocker time to launch", () => {
  const state = {
    active:null,
    done:{},
    priorityMission:{
      taskGid:"joey", name:"Recover", priority:"blocking", status:"queued", updatedAt:9_950,
    },
  };
  const result = bumpBlockingMission(state, 10_000, { stallMs:100, cooldownMs:0, maximumBumps:3 });
  assert.equal(result.action, "waiting");
  assert.equal(state.priorityMission.heartbeatBumps, undefined);
});

test("heartbeat is bounded and stale unrelated recovery state is removed", () => {
  const state = {
    active:null,
    done:{},
    recovery:{ taskGid:"old", status:"queued" },
    priorityMission:{ taskGid:"joey", name:"Recover", priority:"blocking", status:"blocked", heartbeatBumps:3 },
  };
  assert.equal(clearStaleRecovery(state), true);
  const result = bumpBlockingMission(state, 10_000, { cooldownMs:0, maximumBumps:3 });
  assert.equal(result.action, "exhausted");
  assert.equal(state.priorityMission.status, "blocked");
});
