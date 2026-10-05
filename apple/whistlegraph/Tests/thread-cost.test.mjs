import test from 'node:test';
import assert from 'node:assert/strict';
import {ReceiptJournal, AttemptReceipt, RECEIPT_LIMIT} from '../../../aesel/src/attempt-receipt.mjs';
const storage = () => { const m=new Map(); return {getItem:k=>m.get(k)??null,setItem:(k,v)=>m.set(k,v)}; };
const receipt = (id, costs, status='success') => ({format:1,id,status,rounds:costs.map(costUSD=>({httpStatus:200,usage:{costUSD}}))});
test('thread total includes failed attempts, repairs and reviews; duplicate delivery replaces usage',()=>{
 const s=storage(),j=new ReceiptJournal(s,'a');
 j.save(receipt('one',[0.25,0.125,0.125]));j.save(receipt('two',[0.5],'failed'));
 j.save(receipt('two',[0.5],'failed'));
 assert.deepEqual(j.cost.snapshot(),{usd:1,partial:false});
 assert.deepEqual(new ReceiptJournal(s,'a').cost.snapshot(),{usd:1,partial:false});
 assert.deepEqual(new ReceiptJournal(s,'b').cost.snapshot(),{usd:0,partial:false});
});
test('lifetime cost survives journal eviction, reload, and updates to retained attempts',()=>{
 const s=storage(),j=new ReceiptJournal(s,'a');
 for(let i=0;i<RECEIPT_LIMIT+5;i++)j.save(receipt(String(i),[1]));
 assert.equal(j.rows.length,RECEIPT_LIMIT);
 const restored=new ReceiptJournal(s,'a');
 assert.deepEqual(restored.cost.snapshot(),{usd:105,partial:false});
 restored.save(receipt('104',[2]));
 assert.deepEqual(restored.cost.snapshot(),{usd:106,partial:false});
});
test('missing provider costs are partial, known zero and rejected HTTP requests are not',()=>{
 const j=new ReceiptJournal(storage(),'a');
 j.save(receipt('one',[null])); assert.deepEqual(j.cost.snapshot(),{usd:0,partial:true});
 j.save(receipt('one',[0])); assert.deepEqual(j.cost.snapshot(),{usd:0,partial:false});
 j.save({format:1,id:'rejected',status:'failed',rounds:[{httpStatus:429,usage:null}]});
 assert.equal(j.cost.snapshot().partial,false);
});
test('legacy receipts migrate and a full old journal marks possibly missing history',()=>{
 const s=storage();s.setItem('a-receipts',JSON.stringify([{receipt:receipt('old',[0.5])}]));
 assert.deepEqual(new ReceiptJournal(s,'a').cost.snapshot(),{usd:0.5,partial:false});
 s.setItem('b-receipts',JSON.stringify(Array.from({length:100},(_,i)=>({receipt:receipt(String(i),[1])}))));
 assert.deepEqual(new ReceiptJournal(s,'b').cost.snapshot(),{usd:100,partial:true});
});
test('live receipt usage reaches cost without waiting for successful completion',()=>{
 const j=new ReceiptJournal(storage(),'a');
 const r=new AttemptReceipt({requestID:'test',parent:0,parentHash:null,path:'compiled',model:'test',journal:j});
 const round=r.request();r.headers(round,{status:200});
 r.notify('turn/usage',{usage:{cost:0.25}});
 r.notify('turn/usage',{usage:{cost:0.25}});
 assert.deepEqual(j.cost.snapshot(),{usd:0.25,partial:false});
});
