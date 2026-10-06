import test from 'node:test';
import assert from 'node:assert/strict';
import {createCloudStatus} from '../backend/nopaint-status.mjs';

const offer={id:'ac-openrouter:test/image',provider:'openrouter'};
test('provider funding is distinct from user Braincells and checks are read-only, cached, and recover',async()=>{
  let clock=0, balance=.265365539, calls=0;
  const status=createCloudStatus({key:'private-key',now:()=>clock,ttl:1000,fetch:async(url,options)=>{
    calls++;assert.equal(url,'https://openrouter.ai/api/v1/credits');
    assert.equal(options.method,undefined);assert.equal(options.body,undefined);
    return Response.json({data:{total_credits:25,total_usage:25-balance}});
  }});
  const config={enabled:true,offers:[offer]};
  const [a,b]=await Promise.all([status(config),status(config)]);
  assert.equal(calls,1);assert.deepEqual(a,b);assert.equal(a.code,'provider_funding');
  assert.equal(a.available,false);assert.match(a.detail,/Your Braincells remain available/);
  assert.ok(Math.abs(a.balance_usd-balance)<1e-8);assert.equal(a.minimum_balance_usd,1);
  assert.ok(!JSON.stringify(a).includes('private-key'));
  balance=5;clock=999;assert.equal((await status(config)).available,false);
  clock=1001;assert.equal((await status(config)).code,'ready');assert.equal(calls,2);
  assert.equal((await status({enabled:false,offers:[offer]})).code,'disabled');
});
test('missing model configuration and unknown provider health do not blame user balance',async()=>{
  const status=createCloudStatus({key:'key',fetch:async()=>{throw Error('private upstream payload');}});
  assert.equal((await status()).code,'not_configured');
  const service=await status({enabled:true,offers:[offer]});
  assert.equal(service.code,'status_unavailable');assert.equal(service.available,false);
  assert.ok(!JSON.stringify(service).includes('private upstream payload'));
});
test('a failed OpenRouter balance check does not disable a working different provider',async()=>{
  const status=createCloudStatus({key:'key',fetch:async()=>Response.json({data:{total_credits:1,total_usage:.8}})});
  const fal={id:'ac-klein'};
  assert.equal((await status({enabled:true,offers:[fal]})).available,true);
  const mixed=await status({enabled:true,offers:[offer,fal]});
  assert.equal(mixed.available,true);assert.deepEqual(mixed.unavailable_models,[offer.id]);
  assert.equal(mixed.provider_status.code,'provider_funding');
});
