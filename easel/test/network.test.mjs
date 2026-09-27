import test from 'node:test';
import assert from 'node:assert/strict';
import {retryNetwork, httpError} from '../src/network.mjs';

test('transient failures retry twice; authorization and source errors do not retry',async()=>{
  for(const [error,count] of [[new TypeError('Load failed'),3],[httpError('unavailable',503),3],[httpError('sign in',401),1],[new Error('invalid source'),1]]) {
    let calls=0;const waits=[];
    await assert.rejects(retryNetwork(()=>{calls++;throw error;},{sleep:async ms=>waits.push(ms)}));
    assert.equal(calls,count);assert.equal(waits.length,count-1);
  }
});

test('stalled transfers abort and exhaust a bounded number of attempts',async()=>{
  const signals=[];
  await assert.rejects(retryNetwork(signal=>{signals.push(signal);return new Promise(()=>{});},{timeoutMs:5,sleep:async()=>{}}),{name:'TimeoutError'});
  assert.equal(signals.length,3);assert(signals.every(signal=>signal.aborted));
});
