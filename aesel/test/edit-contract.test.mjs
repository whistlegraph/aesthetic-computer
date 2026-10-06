import test from 'node:test';
import assert from 'node:assert/strict';
import {compileEditContract,sourceChecks,validateCandidate,runEditExperiment} from '../src/edit-contract.mjs';
import {selectedBranch} from '../../apple/whistlegraph/Resources/Web/branch-context.mjs';

test('compiler preserves exact selected-branch requests, superseding edits and latest intent',()=>{
  const ledger={head:3,versions:[{id:0,parent:null,source:''},{id:1,parent:0,request:'draw cats',source:''},{id:2,parent:1,request:'add unwanted branch',source:''},{id:3,parent:1,request:'keep background solid',source:'export const caption="Cats";'}]};
  const request='Make the background flash now';
  const text=compileEditContract({request,...selectedBranch(ledger),source:ledger.versions[3].source});
  assert.match(text,/Make the background flash now/);assert.match(text,/keep background solid/);
  assert.doesNotMatch(text,/unwanted branch/);assert.match(text,/unless the latest request supersedes/);
});
test('API checks inspect calls, not comments, prose strings or unrelated HSL text',()=>{
  assert.deepEqual(sourceChecks('// ink(`hsl(0)`)\nconst prose="ink.box()"; export function paint({ink}) {ink(255,0,0).box(0,0,5,5);}'),[]);
  assert.equal(sourceChecks('export function paint({ink}) {ink(`hsl(${30},100%,50%)`);ink.box(0,0,2,2);}').length,2);
  assert.equal(sourceChecks('export function paint(')[0].code,'syntax');
});
test('a frame for another source cannot validate the candidate; painted does not erase errors',()=>{
  const source='export function paint({wipe}) {wipe(0);}';
  assert.equal(validateCandidate(source,{sourceHash:'old',rendered:true},'new').passed,false);
  const proof={sourceHash:'new',rendered:true,logs:[{level:'error',text:'failed'}]};
  assert.equal(validateCandidate(source,proof,'new').findings[0].code,'runtime-error');
  assert.equal(validateCandidate(source,{...proof,logs:[]},'new').acceptance,'unreviewed');
});
test('repair is bounded to one, and only actionable findings spend another generation',async()=>{
  let calls=0;
  const invalid={passed:false,sourceHash:'hash',findings:[{code:'unsupported-hsl',message:'Use RGB'}]};
  const result=await runEditExperiment({prompt:'cats',generate:async(prompt,repair)=>{calls++;if(repair)assert.match(prompt,/REPAIR THIS CANDIDATE ONCE/);return true;},inspect:async()=>invalid,cancelled:()=>false});
  assert.equal(calls,2);assert.equal(result.repairs,1);assert.equal(result.validation.passed,false);
  calls=0;
  await runEditExperiment({prompt:'cats',generate:async()=>{calls++;return true;},inspect:async()=>({...invalid,findings:[{code:'unverified-render'}]}),cancelled:()=>false});
  assert.equal(calls,1);
  calls=0;
  await runEditExperiment({prompt:'cats',generate:async()=>{calls++;return false;},inspect:async()=>invalid,cancelled:()=>false});
  assert.equal(calls,1);
});
test('stop prevents validation and repair after generation returns',async()=>{
  let stopped=false,calls=0;
  const result=await runEditExperiment({prompt:'cats',generate:async()=>{calls++;stopped=true;return true;},inspect:async()=>{throw Error('should not inspect');},cancelled:()=>stopped});
  assert.equal(calls,1);assert.equal(result.cancelled,true);
});

test('an already stopped attempt never purchases generation',async()=>{
 const result=await runEditExperiment({prompt:'cats',generate:async()=>{throw Error('should not generate');},inspect:async()=>{throw Error('should not inspect');},cancelled:()=>true});
 assert.equal(result.cancelled,true);assert.equal(result.repairs,0);
});
