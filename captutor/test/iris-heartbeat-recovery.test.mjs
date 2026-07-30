import assert from "node:assert/strict";
import test from "node:test";

import {
  beginRecovery, latestRecoverableFailure, queueRecovery, rearmRecovery,
  validateAssignedCaptutorTask,
} from "../ops/iris-heartbeat-recovery.mjs";

test("heartbeat selects the newest failed Captutor mission only while idle", () => {
  const state = {
    active:null,
    done:{
      old:{ name:"Old", kind:"captutor", status:"failed", at:10 },
      code:{ name:"Code", kind:"pr", status:"failed", at:30 },
      taco:{ name:"Talking Taco", kind:"captutor", status:"failed", at:20 },
    },
  };
  assert.equal(latestRecoverableFailure(state).taskGid, "taco");
  state.active = { taskGid:"live" };
  assert.equal(latestRecoverableFailure(state), null);
});

test("heartbeat queues exactly one retry and preserves an audit trail", () => {
  const state = {
    active:null,
    done:{ taco:{
      name:"Talking Taco", kind:"captutor", status:"failed",
      reason:"missing-outbox-artifacts", detail:"worker exited 0", at:20,
    } },
  };
  const failure = latestRecoverableFailure(state);
  assert.equal(beginRecovery(state, failure, 100, 1), true);
  const recovery = queueRecovery(state, failure, 101);
  assert.equal(recovery.status, "queued");
  assert.equal(recovery.attempts, 1);
  assert.equal(state.done.taco, undefined);
  assert.equal(state.recoveries.taco.reason, "missing-outbox-artifacts");

  state.done.taco = { ...failure, status:"failed", at:200 };
  assert.equal(beginRecovery(state, latestRecoverableFailure(state), 201, 1), false);
  assert.equal(state.recovery.status, "exhausted");
});

test("manual rearm clears only the requested failed Captutor tombstone", () => {
  const state = {
    active:null,
    done:{
      taco:{
        name:"Talking Taco", kind:"captutor", status:"failed",
        reason:"missing-outbox-artifacts", detail:"worker exited 0", at:20,
      },
      other:{ name:"Other", kind:"captutor", status:"failed", at:10 },
    },
    recoveries:{ taco:{ attempts:1, lastAttemptAt:15 } },
    recovery:{ taskGid:"taco", status:"exhausted", maximum:1 },
  };
  const recovery = rearmRecovery(state, "taco", 100);
  assert.equal(recovery.status, "manually-rearmed");
  assert.equal(recovery.attempts, 1);
  assert.equal(recovery.manualRearms, 1);
  assert.equal(state.done.taco, undefined);
  assert.equal(state.done.other.status, "failed");
  assert.equal(state.recoveries.taco.lastManualRearmAt, 100);
  assert.equal(state.recoveries.taco.lastAttemptAt, 15);
});

test("manual rearm refuses active work and non-failed targets", () => {
  assert.throws(() => rearmRecovery({ active:{ name:"Live" }, done:{} }, "taco"), /Live is active/);
  assert.throws(() => rearmRecovery({ active:null, done:{} }, "taco"), /no failed Captutor tombstone/);
  assert.throws(() => rearmRecovery({ active:null, done:{ taco:{ kind:"pr", status:"failed" } } }, "taco"), /no failed Captutor tombstone/);
});

test("manual rearm validates live Asana ownership and mission tags", () => {
  const task = {
    gid:"123", completed:false, assignee:{ gid:"iris" },
    tags:[{ name:"mission" }, { name:"Captutor" }],
  };
  assert.equal(validateAssignedCaptutorTask(task, "123", "iris"), task);
  assert.throws(() => validateAssignedCaptutorTask({ ...task, completed:true }, "123", "iris"), /already complete/);
  assert.throws(() => validateAssignedCaptutorTask({ ...task, assignee:{ gid:"other" } }, "123", "iris"), /not assigned to Iris/);
  assert.throws(() => validateAssignedCaptutorTask({ ...task, tags:[] }, "123", "iris"), /not a Captutor mission/);
});
