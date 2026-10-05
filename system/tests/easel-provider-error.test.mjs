import test from 'node:test';
import assert from 'node:assert/strict';
import {inferenceProviderFailure} from '../backend/easel-provider-error.mjs';
const body=limit_source=>JSON.stringify({error:{message:'private provider details',metadata:{limit_source}}});
test('provider 402 explains service credit limits separately from the user wallet',()=>{
 assert.match(inferenceProviderFailure(402,body('openrouter_in_flight_budget')),/Wait for them to settle/);
 assert.match(inferenceProviderFailure(402,body('openrouter_key_limit')),/API key has reached its spending limit/);
 assert.match(inferenceProviderFailure(402,body('openrouter_credits')),/needs more provider credit/);
 for(const detail of [body('unknown'),'', '<html>not JSON</html>', body('openrouter_credits')]){
  const message=inferenceProviderFailure(402,detail);
  assert.match(message,/not your AC braincell allowance/);
  assert.doesNotMatch(message,/private provider details|not JSON|unknown/);
 }
});
test('other upstream failures remain generic and do not expose response bodies',()=>{
 assert.equal(inferenceProviderFailure(503,body('openrouter_credits')),'Inference provider returned 503.');
});
