import assert from 'node:assert/strict';
import test from 'node:test';
import {tasks} from '../bin/speed-tasks.mjs';
const result=code=>JSON.stringify({code});
test('coding benchmark checks reversal, adjacency, ordering, and input preservation',()=>{
 assert.equal(tasks.coding.check(result(`function mergeRanges(ranges){
 const sorted=ranges.map(([a,b])=>[Math.min(a,b),Math.max(a,b)]).sort((a,b)=>a[0]-b[0]);
 const out=[]; for(const pair of sorted){const last=out.at(-1);if(last&&pair[0]<=last[1]+1)last[1]=Math.max(last[1],pair[1]);else out.push(pair);}return out;
 }`)),true);
 assert.equal(tasks.coding.check(result('function mergeRanges(ranges){return ranges.sort((a,b)=>a[0]-b[0]);}')),false);
 assert.equal(tasks.coding.check('```js\nfunction mergeRanges(){}\n```'),false);
 assert.equal(tasks.coding.check(result('function mergeRanges(){while(true){}}')),false);
 assert.equal(tasks.coding.check(result('function mergeRanges(){return process.env;}')),false);
});
test('fixed replies must match fully; a partial or verbose reply is not a fast success',()=>{
 assert.equal(tasks.ping.check(' SABLE\n'),true);
 assert.equal(tasks.ping.check('SABL'),false);
 assert.equal(tasks.sequence.check(Array.from({length:40},(_,i)=>i+1).join(', ')),true);
 assert.equal(tasks.sequence.check('1, 2, 3'),false);
});
