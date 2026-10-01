import test from 'node:test';
import assert from 'node:assert/strict';
import {readAttempt,saveAttempt,claimAttempt} from '../Resources/Web/attempt-recovery.mjs';
import {PieceVersions} from '../Resources/Web/piece-versions.mjs';
function fixture(){const data=new Map();const storage={getItem:k=>data.get(k)||null,setItem:(k,v)=>data.set(k,v),removeItem:k=>data.delete(k)};const versions=new PieceVersions(storage,'piece-versions','base');saveAttempt(storage,'piece',{id:'ask-1',parent:0,baseSource:'base',text:'full mixed audio input',displayText:'make a flower',localText:'make a flower',status:'working',retries:0});return {storage,versions};}
test('crash before commit recovers full request once, then needs explicit retry',()=>{
 const {storage,versions}=fixture();assert.equal(claimAttempt(storage,'piece',versions.value).text,'full mixed audio input');
 assert.equal(claimAttempt(storage,'piece',versions.value),null);
 assert.equal(claimAttempt(storage,'piece',versions.value,true).retries,2);
});
test('crash after commit cannot duplicate the version',()=>{
 const {storage,versions}=fixture();const args={source:'flower',request:'full mixed audio input',layers:1,parent:0,requestID:'ask-1'};
 versions.commit(args);assert.equal(claimAttempt(storage,'piece',versions.value),null);assert.equal(readAttempt(storage,'piece'),null);
 assert.equal(versions.commit(args).id,1);assert.equal(versions.value.versions.length,2);
});
test('recovery refuses a different branch or source; failed requests need manual retry',()=>{
 const {storage,versions}=fixture();saveAttempt(storage,'piece',{...readAttempt(storage,'piece'),status:'failed'});
 assert.equal(claimAttempt(storage,'piece',versions.value),null);assert.ok(claimAttempt(storage,'piece',versions.value,true));
 versions.commit({source:'other',request:'different ask',layers:1});assert.equal(claimAttempt(storage,'piece',versions.value,true),null);
});
