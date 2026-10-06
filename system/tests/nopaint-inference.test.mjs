import test from 'node:test';
import assert from 'node:assert/strict';
import {randomUUID} from 'node:crypto';
import sharp from 'sharp';
import {createHandler} from '../netlify/functions/nopaint-inference.mjs';
import {moveOffer,moveInput,createNoPaintProvider} from '../backend/nopaint-provider.mjs';

const image=(await sharp({create:{width:256,height:256,channels:3,background:'#74746f'}}).png().toBuffer()).toString('base64');
const offer = moveOffer(.01);
const event = (values={}) => ({httpMethod:'POST',headers:{authorization:'Bearer test'},body:JSON.stringify({
  requestId:randomUUID(), image, seed:12, strength:.25, quote:offer.quote, ...values,
})});
function fixture(overrides={}) {
  const calls = {begin:[],finish:[],generate:0};
  const handler = createHandler({authorize:async()=>({sub:'owner',email_verified:true}),
    getHandleOrEmail:async()=> '@alice', enabled:true, offer,
    billing:{begin:async value=>{calls.begin.push(value);return {id:'receipt',braincells:value.braincells,free:100,paid:value.braincells-100};},
      finish:async (id,ok)=>{calls.finish.push([id,ok]);return true;}},
    generate:async()=>{calls.generate++;return {image,model:offer.model};},...overrides});
  return {handler,calls};
}
test('cloud tariff shares the existing Braincells conversion and requires explicit configuration',()=>{
  assert.equal(offer.braincells,4000);
  assert.equal(moveOffer(undefined),null);assert.equal(moveOffer(NaN),null);assert.equal(moveOffer(-1),null);
  assert.notEqual(moveOffer(.02).quote,offer.quote);
});
test('only bounded complete 256 RGB images and valid settings reach generation',()=>{
  const input=JSON.parse(event().body);assert.ok(moveInput(input).hash);
  for(const change of [{image:'garbage'},{seed:-1},{seed:2**31},{strength:1},{requestId:'bad'}])
    assert.throws(()=>moveInput({...input,...change}),{status:400});
  const other=Buffer.from(image,'base64');other.writeUInt32BE(1024,16);
  assert.throws(()=>moveInput({...input,image:other.toString('base64')}),{status:400});
});
test('a hint is bounded, changes the move identity, and steers the prompt',async()=>{
  const {movePrompt}=await import('../backend/nopaint-move-prompt.mjs');
  const input=JSON.parse(event().body), plain=moveInput(input), hinted=moveInput({...input,hint:'  more  moss '});
  assert.equal(hinted.hint,'more moss');assert.notEqual(hinted.hash,plain.hash);
  for(const hint of [7,'x'.repeat(201)]) assert.throws(()=>moveInput({...input,hint}),{status:400});
  assert.match(movePrompt(hinted),/painter's hint: "more moss"/);
  assert.doesNotMatch(movePrompt(plain),/hint/);
});
test('identity and price come from the server; replay never charges or generates twice',async()=>{
  const {handler,calls}=fixture();
  const input=event({handle:'@victim',user:'victim',braincells:1});
  const first=await handler(input),second=await handler(input);
  assert.equal(first.statusCode,200);assert.deepEqual(second,first);
  assert.equal(calls.begin.length,1);assert.equal(calls.generate,1);
  assert.equal(calls.begin[0].user,'owner');assert.equal(calls.begin[0].handle,'alice');assert.equal(calls.begin[0].braincells,4000);
  const receipt=JSON.parse(first.body);assert.equal(receipt.handle,'@alice');assert.equal(receipt.billing.braincells,4000);
  const changed={...input,body:JSON.stringify({...JSON.parse(input.body),seed:13})};
  assert.equal((await handler(changed)).statusCode,409);
});
test('anonymous, unverified, handleless, disabled and stale-price moves never reserve',async()=>{
  for(const overrides of [{authorize:async()=>null},{authorize:async()=>({sub:'owner',email_verified:false})},
    {getHandleOrEmail:async()=> 'email@example.test'},{enabled:false}]) {
    const {handler,calls}=fixture(overrides);assert.ok((await handler(event())).statusCode>=400);assert.equal(calls.begin.length,0);
  }
  const {handler,calls}=fixture();
  assert.equal((await handler({...event(),headers:{}})).statusCode,401);
  assert.equal((await handler(event({quote:'stale'}))).statusCode,409);assert.equal(calls.generate,0);
});
test('insufficient balance never calls the provider; failure refunds the reservation',async()=>{
  const poor=fixture({billing:{begin:async()=>{throw Object.assign(Error('No balance'),{status:402});}}});
  assert.equal((await poor.handler(event())).statusCode,402);assert.equal(poor.calls.generate,0);
  const broken=fixture({generate:async()=>{throw Object.assign(Error('Provider unavailable'),{status:502});}});
  assert.equal((await broken.handler(event())).statusCode,502);assert.deepEqual(broken.calls.finish,[['receipt',false]]);
});
test('catalog exposes provider funding and blocks generation before reserving Braincells',async()=>{
  const service={available:false,code:'provider_funding',message:'AC cloud unavailable',detail:'AC needs to fund OpenRouter.'};
  const {handler,calls}=fixture({status:async()=>service});
  const catalog=JSON.parse((await handler({httpMethod:'GET'})).body);
  assert.deepEqual(catalog.service,service);assert.equal(catalog.models[0].available,false);
  const response=await handler(event());assert.equal(response.statusCode,503);
  assert.equal(JSON.parse(response.body).code,'provider_funding');
  assert.equal(calls.generate,0);assert.equal(calls.begin.length,0);
});
test('concurrent replay joins the same pending move',async()=>{
  let release;const gate=new Promise(r=>release=r);let generated=0;
  const {handler,calls}=fixture({generate:async()=>{generated++;await gate;return {image};}});
  const input=event(),first=handler(input),second=handler(input);
  await new Promise(r=>setImmediate(r));release();
  assert.equal((await first).statusCode,200);assert.equal((await second).statusCode,200);
  assert.equal(generated,1);assert.equal(calls.begin.length,1);
});
test('provider adapter submits one complete canvas and uses no retries or arbitrary download URLs',async()=>{
  const calls=[];
  const generate=createNoPaintProvider({key:'test-key',sleep:async()=>{},fetch:async(url,options)=>{
    calls.push([url,options]);
    if(options.method==='POST')return Response.json({status_url:'https://queue.fal.run/status',response_url:'https://queue.fal.run/result'});
    if(url.endsWith('/status'))return Response.json({status:'COMPLETED'});
    return Response.json({images:[{url:'data:image/png;base64,'+image}]});
  }});
  const result=await generate(JSON.parse(event().body));assert.equal(result.width,256);assert.equal(result.height,256);
  const payload=JSON.parse(calls[0][1].body);assert.equal(payload.image_urls[0],'data:image/png;base64,'+image);
  assert.equal(payload.num_images,1);assert.equal(payload.enable_safety_checker,true);assert.match(payload.prompt,/quarter of the image/);
  assert.equal(calls.filter(([,options])=>options.method==='POST').length,1);
});
test('provider callback URLs cannot receive credentials on another host',async()=>{
  let calls=0;
  const generate=createNoPaintProvider({key:'test-key',fetch:async()=>{calls++;return Response.json({status_url:'https://example.test/status'});}});
  await assert.rejects(generate(JSON.parse(event().body)),{status:502});assert.equal(calls,1);
});
