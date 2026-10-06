import test from 'node:test';
import assert from 'node:assert/strict';
import {randomUUID} from 'node:crypto';
import sharp from 'sharp';
import {openRouterOffers, createOpenRouterProvider, profiles} from '../backend/nopaint-openrouter.mjs';
import {movePrompt} from '../backend/nopaint-move-prompt.mjs';
import {createHandler} from '../netlify/functions/nopaint-inference.mjs';
const image=(await sharp({create:{width:256,height:256,channels:3,background:'#74746f'}}).png().toBuffer()).toString('base64');
const offers=openRouterOffers(JSON.stringify([
  {model:'black-forest-labs/flux.2-klein-4b',usd:.02},
  {model:'google/gemini-3.1-flash-image',usd:.06},
]));

test('only configured reviewed image models receive an explicit Braincells quote',()=>{
  assert.deepEqual(openRouterOffers(),[]);
  for(const value of ['bad','null','{}','[null]', '[{"model":"unknown","usd":0.01}]']) assert.deepEqual(openRouterOffers(value),[]);
  assert.equal(offers.length,2);assert.notEqual(offers[0].quote,offers[1].quote);
  assert.equal(offers[0].braincells,8000);assert.equal(offers[0].previews,false);
});
test('all enables the complete reviewed image-editing catalog with explicit prices',()=>{
  const all=openRouterOffers('all');
  assert.equal(all.length,50);assert.equal(new Set(all.map(item=>item.quote)).size,50);
  assert.ok(all.every(item=>Number.isSafeInteger(item.braincells)&&item.braincells>=0));
  assert.equal(all.find(item=>item.model==='openai/gpt-5-image').braincells,8000);
  assert.equal(all.find(item=>item.model==='openai/gpt-5-image-mini').braincells,6000);
  assert.equal(all.find(item=>item.model==='inclusionai/ming-image-0.1-design-layer').braincells,0);
  for(const model of Object.keys(profiles))assert.ok(all.some(offer=>offer.model===model));
});
test('every enabled profile sends its own supported settings and one reference without retrying',async()=>{
  for(const offer of openRouterOffers('all')) {
    let request, calls=0;
    const generate=createOpenRouterProvider({key:'test-only',fetch:async(url,options)=>{
      calls++;request=JSON.parse(options.body);return Response.json({data:[{b64_json:image}]});
    }});
    await generate({image,strength:.5,seed:7},offer);
    assert.equal(calls,1);assert.equal(request.model,offer.model);
    assert.equal(request.input_references.length,1);
    assert.equal(request.prompt,movePrompt({strength:.5,seed:7,model:offer.model}));
    for(const [key,value] of Object.entries(profiles[offer.model].options))
      assert.equal(request[key],key==='seed'?7:value);
    if(offer.model.startsWith('krea/'))assert.equal(request.n,undefined);
    else assert.equal(request.n,1);
    assert.notEqual(request.output_format,'svg');
  }
});
test('move prompts vary the operation and affected area instead of imposing a painting style',()=>{
  const small=movePrompt({seed:42,strength:.25});
  assert.equal(small,movePrompt({seed:42,strength:.25}));
  assert.match(small,/quarter of the image/);
  assert.match(movePrompt({seed:42,strength:.5}),/half of the image/);
  assert.match(movePrompt({seed:42,strength:.75}),/entire image/);
  assert.doesNotMatch(small,/abstract painting|Preserve most/);
  assert.ok(new Set(Array.from({length:64},(_,seed)=>movePrompt({seed,strength:.75}))).size>=14);
  assert.match(movePrompt({seed:22,strength:.5,model:'google/gemini-nano-banana-2.1'}),/crisp geometric/);
});
test('OpenRouter receives exactly one full input, the selected model and supported size; output returns to 256',async()=>{
  const calls=[];
  const generate=createOpenRouterProvider({key:'test-only',fetch:async(url,options)=>{
    calls.push([url,options]);return Response.json({data:[{b64_json:image,media_type:'image/png'}],usage:{cost:.04}});
  }});
  const output=await generate({image,strength:.25,seed:3},offers[1]);
  assert.equal(calls.length,1);assert.equal(calls[0][0],'https://openrouter.ai/api/v1/images');
  assert.equal(calls[0][1].redirect,'error');
  const request=JSON.parse(calls[0][1].body);
  assert.equal(request.model,offers[1].model);assert.equal(request.resolution,'512');assert.equal(request.n,1);
  assert.equal(request.input_references[0].image_url.url,'data:image/png;base64,'+image);
  assert.equal(request.seed,undefined);assert.equal(output.width,256);assert.equal(output.height,256);
  assert.equal(output.provider_cost_usd,.04);assert.equal(JSON.stringify(output).includes('test-only'),false);
});
test('provider errors are not retried; external image URLs are not followed',async()=>{
  for(const response of [()=>new Response('',{status:402}),()=>Response.json({data:[{url:'https://untrusted.test/image.png'}]})]) {
    let calls=0;
    const generate=createOpenRouterProvider({key:'test-only',fetch:async()=>{calls++;return response();}});
    await assert.rejects(generate({image,strength:.25,seed:3},offers[0]));assert.equal(calls,1);
  }
});
test('GPT Image 2 and Nano Banana 2.1 use their supported edit settings',async()=>{
  for(const [model,option,value] of [
    ['openai/gpt-image-2','quality','low'],
    ['google/gemini-nano-banana-2.1','resolution','1K'],
  ]) {
    const [offer]=openRouterOffers(JSON.stringify([{model,usd:.05}]));
    let request;
    const generate=createOpenRouterProvider({key:'test-only',fetch:async(url,options)=>{
      request=JSON.parse(options.body);return Response.json({data:[{b64_json:image}]});
    }});
    await generate({image,strength:.5,seed:3},offer);
    assert.equal(request.model,model);assert.equal(request[option],value);
    assert.equal(request.aspect_ratio,'1:1');assert.equal(request.n,1);
    assert.equal(request.input_references.length,1);assert.equal(request.seed,undefined);
  }
});
test('gateway selects and bills by server quote; mismatched model never calls provider',async()=>{
  const billed=[],generated=[];
  const handler=createHandler({authorize:async()=>({sub:'owner',email_verified:true}),getHandleOrEmail:async()=>'@owner',
    offers,enabled:true,billing:{begin:async r=>{billed.push(r);return {...r,id:'receipt',free:r.braincells,paid:0};},finish:async()=>true},
    generate:async(input,offer)=>{generated.push(offer.model);return {image,model:offer.model};}});
  const event={httpMethod:'POST',headers:{authorization:'Bearer test'},body:JSON.stringify({
    requestId:randomUUID(),image,seed:1,strength:.25,model:offers[1].id,quote:offers[0].quote})};
  assert.equal((await handler(event)).statusCode,409);assert.equal(generated.length,0);
  event.body=JSON.stringify({...JSON.parse(event.body),quote:offers[1].quote});
  assert.equal((await handler(event)).statusCode,200);assert.deepEqual(generated,[offers[1].model]);
  assert.equal(billed[0].braincells,offers[1].braincells);
});
