import test from 'node:test';
import assert from 'node:assert/strict';
import sharp from 'sharp';
import {imageRequest,generateEaselImage} from '../backend/easel-images.mjs';
test('hosted image tools use generation or an actual reference edit, retaining provenance',async()=>{
 const png=await sharp({create:{width:2,height:2,channels:4,background:'#ffff00'}}).png().toBuffer();
 for(const reference of [undefined,png.toString('base64')]) {
  const input=imageRequest({jobId:'test-job-00000001',prompt:'A small yellow square',reference});
  let calls=0;
  const result=await generateEaselImage(input,{key:'test-secret',fetchImpl:async(url,options)=>{
   calls++;assert.match(url,reference?/\/edits$/:/\/generations$/);assert.equal(options.headers.Authorization,'Bearer test-secret');
   if(reference) {assert(options.body instanceof FormData);assert.equal(options.body.get('image[]').type,'image/png');}
   else assert.equal(JSON.parse(options.body).output_format,'png');
   return new Response(JSON.stringify({data:[{b64_json:png.toString('base64')}]}),{headers:{'x-request-id':'test-request'}});
  }});
  assert.equal(calls,1);assert.equal(result.provenance.mode,reference?'edit':'generate');assert.equal(result.provenance.requestId,'test-request');
  assert.equal((await sharp(Buffer.from(result.png,'base64')).metadata()).width,2);
 }
});
test('invalid requests and upstream failures never make duplicate paid requests',async()=>{
 assert.throws(()=>imageRequest({jobId:'x',prompt:'x'}),/job/);
 assert.throws(()=>imageRequest({jobId:'test-job-00000001',prompt:'x',model:'another'}),/model/);
 let calls=0;
 await assert.rejects(generateEaselImage(imageRequest({jobId:'test-job-00000001',prompt:'x'}),{key:'test',fetchImpl:async()=>{calls++;return new Response('',{status:503});}}),/not retried/);
 assert.equal(calls,1);
});
test('hosted fal Flare uses official queue schema and server credentials, returning PNG provenance',async()=>{
 const model='openai/gpt-image-2.5/flare/text-to-image';
 const png=await sharp({create:{width:2,height:2,channels:4,background:'#ff77aa'}}).png().toBuffer();
 const calls=[];
 const input=imageRequest({jobId:'test-fal-00000001',provider:'fal',model,prompt:'A pink circle',size:'1536x1024',quality:'low'});
 const result=await generateEaselImage(input,{key:'server-fal-secret',fetchImpl:async(url,options)=>{
  url=String(url);calls.push({url,...options});
  if(options.method==='POST'){
   assert.equal(url,`https://queue.fal.run/${model}`); assert.equal(options.headers.Authorization,'Key server-fal-secret');
   assert.deepEqual(JSON.parse(options.body),{prompt:'A pink circle',image_size:{width:1536,height:1024},num_images:1,output_format:'png',quality:'low'});
   return Response.json({request_id:'fal-request',status_url:'https://queue.fal.run/openai/requests/fal-request/status',response_url:'https://queue.fal.run/openai/requests/fal-request'});
  }
  if(url.endsWith('/status'))return Response.json({status:'COMPLETED'});
  if(url.startsWith('https://queue.fal.run/'))return Response.json({images:[{url:'https://v3.fal.media/generated.png'}]});
  assert.equal(options.headers,undefined); assert.equal(options.redirect,'error'); return new Response(png);
 }});
 assert.equal(calls.filter(c=>c.method==='POST').length,1); assert.equal(result.provenance.provider,'fal'); assert.equal(result.provenance.model,model); assert.equal(result.provenance.requestId,'fal-request');
 assert.equal((await sharp(Buffer.from(result.png,'base64')).metadata()).width,2);
 assert.equal(JSON.stringify(result).includes('server-fal-secret'),false);
});
test('hosted allowlist rejects wrong providers, implicit fal model, and Flare edits before spending',async()=>{
 const base={jobId:'test-fal-00000002',prompt:'A circle',provider:'fal',model:'openai/gpt-image-2.5/flare/text-to-image'};
 for(const input of [{...base,provider:'other'},{...base,model:undefined},{...base,model:'fal-ai/flux/dev'},{...base,reference:'YWJj'}]) assert.throws(()=>imageRequest(input),/provider\/model|generation only/);
 let calls=0;
 await assert.rejects(generateEaselImage(imageRequest(base),{key:'test',fetchImpl:async()=>{calls++;return new Response('',{status:503});}}),/not retried/);
 assert.equal(calls,1);
 await assert.rejects(generateEaselImage(imageRequest(base),{fetchImpl:()=>assert.fail('must not spend')}),/not configured/);
});
