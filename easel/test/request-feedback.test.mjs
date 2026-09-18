import test from 'node:test';
import assert from 'node:assert/strict';
import {requestFeedback} from '../src/request-feedback.mjs';
import {frameLayout,renderFrame} from '../src/render.mjs';
test('waiting identifies Luna and measures elapsed time from submission',()=>{
 assert.match(requestFeedback({status:'waiting',model:'openai/gpt-5.6-luna',requestStartedAt:1000,lastRequestEventAt:2000},4000),/^Waiting for Luna · 3s/);
});
test('a quiet stream is reported as quiet, not failed or completed',()=>{
 assert.match(requestFeedback({status:'generating',requestStartedAt:1000,lastRequestEventAt:2000},18000),/Receiving reply · 17s · no update for 16s/);
 assert.doesNotMatch(requestFeedback({status:'generating',requestStartedAt:1000,lastRequestEventAt:17000},18000),/no update/);
});
test('approval waits are not described as stalled',()=>{
 assert.doesNotMatch(requestFeedback({status:'approval',requestStartedAt:1000,lastRequestEventAt:1000},90000),/no update/);
});
test('controls remain on the final two rows as the terminal resizes',()=>{
 for(const rows of [10,18,24,40])for(const columns of [32,80,140]){
  const state={statusChrome:false,entries:[],input:'hello',account:'@jeffrey',status:'ready'};
  const lines=renderFrame(state,columns,rows,false).split('\n');
  assert.equal(lines.length,rows);
  assert.equal(frameLayout(state,rows).trayStartRow,rows-4);
  assert.match(lines.at(-2),/hello/);
  assert.match(lines.at(-1),/help/);
 }
});
