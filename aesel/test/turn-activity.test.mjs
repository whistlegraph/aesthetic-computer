import assert from 'node:assert/strict';
import test from 'node:test';
import {beginTurnActivity,recordToolActivity,recordTurnUsage,recordCodexUsage,finishTurnActivity,measuredUsage,FEED_LIMIT} from '../src/turn-activity.mjs';

test('the feed stays bounded, counts parallel tools once, and never stores command payloads', () => {
  const state={};beginTurnActivity(state,'turn','thread',0);
  for(let i=0;i<30;i++) {
    const item={id:`t${i}`,type:'commandExecution',command:'RAW_SCRIPT',aggregatedOutput:'RAW_OUTPUT'};
    recordToolActivity(state,item);recordToolActivity(state,item);
    recordToolActivity(state,{...item,exitCode:i===29?1:0},true);
  }
  assert.equal(state.turnActivity.tools.size,30);
  assert.equal(state.turnActivity.feed.length,FEED_LIMIT);
  assert.equal(state.turnActivity.feed.at(-1).status,'failed');
  assert.doesNotMatch(JSON.stringify(state.turnActivity.feed),/RAW_/);
  const receipt=finishTurnActivity(state,'completed',61000);
  assert.equal(receipt.text,'30 tools · 1m 1s');
  assert.equal(state.turnActivity.feed.length,0);
});

test('provider usage counts cache buckets and reasoning subsets correctly', () => {
  assert.equal(measuredUsage({input_tokens:100,output_tokens:30,cache_read_input_tokens:50,cache_creation_input_tokens:10}).tokens,190);
  assert.equal(measuredUsage({inputTokens:100,outputTokens:30,cacheReadInputTokens:50,cacheCreationInputTokens:10}).tokens,190);
  assert.equal(measuredUsage({inputTokens:100,cachedInputTokens:50,outputTokens:30,reasoningOutputTokens:20}).tokens,130);
  assert.equal(measuredUsage({total_tokens:130,input_tokens:100,output_tokens:30}).tokens,130);
  assert.equal(measuredUsage({}).metered,false);
  assert.equal(measuredUsage({input_tokens:0,output_tokens:0}).metered,true);
});

test('all inference rounds contribute, with reported costs only', () => {
  const state={};beginTurnActivity(state,'turn','thread',0);
  recordTurnUsage(state,{input_tokens:100,output_tokens:20,cost:.002});
  recordTurnUsage(state,{inputTokens:150,outputTokens:30,costUSD:.003});
  const receipt=finishTurnActivity(state,'completed',12000);
  assert.equal(receipt.text,'0 tools · 12s · 300 tokens · $0.0050');
  assert.equal(state.spend.tokens,300);
  assert.equal(state.spend.usd,.005);
});

test('Codex totals are differenced across rounds and turns, including after reify', () => {
  let state={};beginTurnActivity(state,'a','thread',0);
  const update=(turn,tokens,reasoning,lastTokens,lastReasoning)=>recordCodexUsage(state,{threadId:'thread',turnId:turn,tokenUsage:{total:{totalTokens:tokens,reasoningOutputTokens:reasoning},last:{totalTokens:lastTokens,reasoningOutputTokens:lastReasoning}}});
  update('a',10000,1000,100,20); // Earlier history on resume is not charged to this turn.
  update('a',10000,1000,100,20); // Duplicate cumulative event.
  update('a',10200,1040,200,40);
  const receipt=finishTurnActivity(state,'completed',5000);
  assert.equal(receipt.text,'0 tools · 5s · 300 tokens · 60 reasoning tokens');
  update('a',10250,1050,50,10); // A final reading may arrive just after completion.
  assert.equal(receipt.text,'0 tools · 5s · 350 tokens · 70 reasoning tokens');
  state={spend:JSON.parse(JSON.stringify(state.spend))};
  beginTurnActivity(state,'b','thread',6000);
  update('b',10400,1060,150,10);
  assert.equal(finishTurnActivity(state,'interrupted',9000).text,'stopped · 0 tools · 3s · 150 tokens · 10 reasoning tokens');
  assert.equal(state.spend.tokens,10400);
  beginTurnActivity(state,'c','other-thread',10000);
  assert.equal(state.spend.codexUsage,undefined);
  update('b',10500,1080,100,20);
  assert.equal(state.turnActivity.tokens,0,'old provider events cannot count toward a new thread');
});

test('missing usage stays absent and a new turn clears old tools and totals', () => {
  const state={};beginTurnActivity(state,'a','thread',0);
  recordToolActivity(state,{id:'tool',type:'mcpToolCall',tool:'mcp__ac__ac_symbol',arguments:{code:'RAW_CODE'}});
  assert.equal(state.turnActivity.feed[0].text,'reading source');
  assert.equal(finishTurnActivity(state,'failed',1000).text,'failed · 1 tool · 1s');
  beginTurnActivity(state,'b','thread',2000);
  assert.equal(state.turnActivity.tools.size,0);
  assert.equal(state.turnActivity.tokens,0);
  assert.equal(state.turnActivity.feed.length,0);
});
