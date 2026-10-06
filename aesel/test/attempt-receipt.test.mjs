import test from 'node:test';import assert from 'node:assert/strict';
import {ReceiptJournal,AttemptReceipt,hashSource} from '../src/attempt-receipt.mjs';
import {validateReceipt} from '../../system/backend/whistlegraph-receipt.mjs';
const requestID='11111111-1111-4111-8111-111111111111';
const storage=()=>{const values=new Map();return {getItem:k=>values.get(k),setItem:(k,v)=>values.set(k,v)};};
export async function receiptFixture(){
 const journal=new ReceiptJournal(storage(),'piece');
 return new AttemptReceipt({journal,requestID,parent:0,parentHash:await hashSource('old'),path:'compiled',model:'tested/model'});
}
test('per-round usage is replaced rather than double-counted; missing cost stays null',async()=>{
 const r=await receiptFixture();r.request();
 const usage={input_tokens:12,output_tokens:8};r.notify('turn/usage',{usage});r.notify('turn/usage',{usage});
 assert.equal(r.value.rounds.length,1);assert.equal(r.value.rounds[0].usage.outputTokens,8);assert.equal(r.value.rounds[0].usage.costUSD,null);
 r.request();assert.equal(r.value.rounds[1].usage,null);
 r.notify('model/reported',{reported:'actual/model',providerRequestID:'message-123'});
 assert.equal(r.value.rounds[1].providerRequestID,'message-123');
});
test('receipt boundary strips content and ignores client claims of acceptance',async()=>{
 const r=await receiptFixture();r.request();r.finish('failed',null);
 const clean=validateReceipt({...r.value,prompt:'secret request',audio:'samples',source:'code',acceptance:'approved',rounds:[{...r.value.rounds[0],payload:'secret'}]});
 assert.equal(clean.acceptance,'unreviewed');assert.equal(clean.provenance,'client-observed');assert.equal(clean.rounds[0].usage,null);
 assert.doesNotMatch(JSON.stringify(clean),/secret|samples|approved/);
 assert.throws(()=>validateReceipt({...r.value,status:'running'}));assert.throws(()=>validateReceipt({...r.value,rounds:Array(33).fill({})}));
});
test('interrupted attempts survive a restart and acknowledgements suppress resending',async()=>{
 const s=storage(),journal=new ReceiptJournal(s,'piece');
 const r=new AttemptReceipt({journal,requestID,parent:0,parentHash:await hashSource('old'),path:'current',model:'tested/model'});r.request();
 const resumed=new ReceiptJournal(s,'piece');assert.equal(resumed.pending().status,'interrupted');assert.equal(resumed.pending().elapsedMs,null);
 resumed.acknowledge(r.value.id);assert.equal(new ReceiptJournal(s,'piece').pending(),null);
});
test('retention is bounded and source-linked observations contain no console content',async()=>{
 const r=await receiptFixture();
 r.observe({kind:'console',sourceHash:await hashSource('new'),requestID:3,event:{level:'error',message:'private log'}});
 assert.doesNotMatch(JSON.stringify(r.value),/private log/);
 for(let i=0;i<110;i++)r.journal.save({...r.value,id:crypto.randomUUID(),status:'failed'});
 assert.equal(r.journal.rows.length,100);
});
