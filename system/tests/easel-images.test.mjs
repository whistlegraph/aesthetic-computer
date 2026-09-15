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
