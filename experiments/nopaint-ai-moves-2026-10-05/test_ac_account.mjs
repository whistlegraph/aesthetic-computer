import test from 'node:test';
import assert from 'node:assert/strict';
import {createHash} from 'node:crypto';
import {fileURLToPath} from 'node:url';
import {createAccountBridge} from './ac_account.mjs';

function setup(catalog = {models:[{id:'ac-klein',available:true,braincells:4000,quote:'test-quote'}]}) {
  let moves=0;
  const bridge=createAccountBridge({session:{signedIn:true,token:async()=> 'private-test-token'},
    fetch:async(url,options)=>{
      if(url.endsWith('/userinfo'))return Response.json({sub:'owner'});
      if(url.includes('/handle?'))return Response.json({handle:'alice'});
      if(url.endsWith('/api/easel-credits'))return Response.json({handle:'@alice',remaining:12000,purchased:4000});
      if(options.method==='POST'){moves++;return Response.json({image:'test-image',billing:{braincells:4000}});}
      if (catalog instanceof Error) throw catalog;
      return Response.json(catalog);
    }});
  return {bridge,moves:()=>moves};
}
test('AC account status returns a handle and balance without tokens or email',async()=>{
  const {bridge}=setup();const status=await bridge({action:'login'});
  assert.equal(status.handle,'@alice');assert.equal(status.remaining,12000);assert.equal(status.purchased,4000);
  assert.ok(!JSON.stringify(status).includes('private-test-token'));
  assert.equal(status.account_id,createHash('sha256').update('owner').digest('hex'));
});
test('account switch is rejected before a cloud request or image read',async()=>{
  const {bridge,moves}=setup();
  await assert.rejects(bridge({action:'move',account_id:'another-account',before:'/nonexistent'}),/account changed/);
  assert.equal(moves(),0);
});
test('empty or disabled cloud catalogs preserve the signed-in Braincell account',async()=>{
  for (const [catalog, expected] of [
    [{models:[]}, /not configured/],
    [{models:[{id:'ac-klein',available:false}]}, /disabled/],
    [new Error('private upstream details'), /Could not load/],
  ]) {
    const {bridge}=setup(catalog); const status=await bridge({action:'status'});
    assert.equal(status.connected,true); assert.equal(status.handle,'@alice');
    assert.equal(status.remaining+status.purchased,16000);
    assert.match(status.remote_status,expected);
    assert.ok(!JSON.stringify(status).includes('private upstream details'));
  }
});
test('provider funding details reach the app separately from the signed-in balance',async()=>{
  const service={available:false,code:'provider_funding',message:'AC cloud unavailable',
    detail:'AC needs to fund OpenRouter. Your Braincells remain available.',balance_usd:.27,minimum_balance_usd:1};
  const {bridge}=setup({models:[],service});
  const status=await bridge({action:'status'});
  assert.deepEqual(status.remote_service,service);assert.equal(status.remote_status,service.detail);
  assert.equal(status.connected,true);assert.equal(status.remaining+status.purchased,16000);
});
test('an interrupted account read retries once without replaying a paid move',async()=>{
  let reads=0, writes=0;
  const reset=()=>Object.assign(new TypeError('fetch failed'),{cause:{code:'ECONNRESET'}});
  const bridge=createAccountBridge({session:{token:async()=> 'private-test-token'},fetch:async(url,options)=>{
    if(url.endsWith('/userinfo')) {
      if(++reads===1)throw reset();
      return Response.json({sub:'owner'});
    }
    if(url.includes('/handle?'))return Response.json({handle:'alice'});
    if(options.method==='POST'){writes++;throw reset();}
    throw Error('Unexpected request');
  }});
  await assert.rejects(bridge({action:'move',account_id:createHash('sha256').update('owner').digest('hex'),
    before:fileURLToPath(new URL('./starts/blank.png',import.meta.url))}),{code:'offline'});
  assert.equal(reads,2); assert.equal(writes,1);
});
test('persistent read resets stop after one retry',async()=>{
  let calls=0;
  const bridge=createAccountBridge({session:{token:async()=> 'private-test-token'},fetch:async()=>{
    calls++;throw Object.assign(new TypeError('fetch failed'),{cause:{code:'ECONNRESET'}});
  }});
  await assert.rejects(bridge({action:'status'}),{code:'offline'});
  assert.equal(calls,2);
});
test('a persistent client reuses verification but rechecks when the token changes',async()=>{
  let token='first', checks=0;
  const bridge=createAccountBridge({session:{token:async()=>token},fetch:async(url)=>{
    if(url.endsWith('/userinfo')){checks++;return Response.json({sub:token});}
    if(url.includes('/handle?'))return Response.json({handle:'alice'});
    if(url.endsWith('/api/easel-credits'))return Response.json({handle:'@alice',remaining:12000,purchased:4000});
    return Response.json({models:[]});
  }});
  const first=await bridge({action:'status'});
  await bridge({action:'status'});
  assert.equal(checks,1);
  token='second';
  const second=await bridge({action:'status'});
  assert.equal(checks,2);
  assert.notEqual(first.account_id,second.account_id);
});
