import assert from "node:assert/strict";
import test from "node:test";

import {
  beginRecovery, latestRecoverableFailure, queueRecovery,
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
