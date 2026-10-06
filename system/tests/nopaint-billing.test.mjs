import test from 'node:test';
import assert from 'node:assert/strict';
import {randomUUID} from 'node:crypto';
import {noPaintBilling,ensureNoPaintBillingIndexes} from '../backend/nopaint-billing.mjs';

test('a zero-price hosted model gets a receipt without debiting daily or purchased Braincells',async()=>{
  const rows=new Map(),usage={tokens:1000000},writes=[];
  const db={client:{startSession:()=>({withTransaction:async work=>work(),endSession:async()=>{}})},
    collection(name) {
      if(name==='ac-credit-wallets')throw Error('A free move must not reserve a wallet');
      if(name==='ai-usage')return {
        findOne:async()=>usage,
        updateOne:async(filter,update)=>{writes.push(update);assert.equal(update.$inc?.tokens,undefined);},
      };
      return {find:()=>[],findOne:async({_id})=>rows.get(_id),
        insertOne:async row=>rows.set(row._id,row),
        updateOne:async({_id},update)=>Object.assign(rows.get(_id),update.$set)};
    }};
  const billing=noPaintBilling(db,{limit:0});
  const receipt=await billing.begin({user:'user',handle:'user',requestId:randomUUID(),hash:'hash',braincells:0,model:'free-image'});
  assert.deepEqual([receipt.braincells,receipt.free,receipt.paid],[0,0,0]);
  assert.equal(await billing.finish(receipt.id,true),true);
  assert.equal(rows.get(receipt.id).charged,0);
  assert.ok(writes.some(update=>update.$inc?.asks===1));
});

// Financial integration tests opt into Mongo and use only randomly named test
// collections. No real account, wallet, artwork or token is read or modified.
test('No Paint reserves free credits then paid credits; settlement, replay and recovery are atomic',
  {skip:process.env.NOPAINT_BILLING_INTEGRATION!=='1'}, async()=>{
    const {connect,closePool}=await import('../backend/database.mjs');
    const {db}=await connect(),prefix='test-nopaint-'+randomUUID()+'-',names=new Set();
    const isolated={client:db.client,collection(name){names.add(prefix+name);return db.collection(prefix+name);}};
    let at=new Date('2026-10-05T23:59:00Z');
    const billing=noPaintBilling(isolated,{now:()=>at,limit:60});
    const request=(user='test-user')=>({user,handle:user,requestId:randomUUID(),hash:randomUUID(),braincells:40,model:'test-model'});
    try {
      await ensureNoPaintBillingIndexes(isolated);
      for(const name of ['ai-usage','ac-credit-wallets','nopaint-move-requests'])await isolated.collection(name).insertOne({_id:'fixture'});
      const wallets=isolated.collection('ac-credit-wallets'),usage=isolated.collection('ai-usage'),receipts=isolated.collection('nopaint-move-requests');
      await wallets.insertOne({_id:'test-user',balance:100});
      const input=request(),first=await billing.begin(input);
      assert.equal(first.free,40);assert.equal(first.paid,0);
      await assert.rejects(billing.begin(input),{status:409});
      assert.equal(await billing.finish(first.id,true),true);
      assert.equal(await billing.finish(first.id,false),false);
      const second=await billing.begin(request());assert.equal(second.free,20);assert.equal(second.paid,20);
      assert.equal((await wallets.findOne({_id:'test-user'})).balance,80);
      await billing.finish(second.id,false);
      assert.equal((await wallets.findOne({_id:'test-user'})).balance,100);
      assert.equal((await usage.findOne({_id:'test-user:2026-10-05'})).tokens,40);
      const third=await billing.begin(request());
      at=new Date('2026-10-06T00:00:00Z');
      await billing.finish(third.id,true);
      assert.equal((await wallets.findOne({_id:'test-user'})).daily['2026-10-05'],20);
      const abandoned=await billing.begin(request());at=new Date(+at+301000);
      assert.equal(await billing.reconcile(),1);assert.equal(await billing.finish(abandoned.id,true),false);
      const race=await Promise.allSettled([billing.begin(request('race')),billing.begin(request('race'))]);
      assert.equal(race.filter(result=>result.status==='fulfilled').length,1);
      assert.equal(race.find(result=>result.status==='rejected').reason.status,402);
      const receipt=await receipts.findOne({_id:first.id});assert.equal(receipt.charged,40);
      for(const privateField of ['image','token','prompt'])assert.equal(privateField in receipt,false);
    } finally {
      for(const name of names)await db.collection(name).drop();
      await closePool();
    }
  });
