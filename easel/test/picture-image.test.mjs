import test from 'node:test';
import assert from 'node:assert/strict';
import {hostedPictureImage} from '../src/picture-image.mjs';
import {plan,FAL_FLARE} from '../media/picture/illy.mjs';
test('AC hosted client selects fal Flare explicitly without sending provider credentials',async()=>{
 const p=plan({provider:'fal',model:FAL_FLARE,prompt:'pink dot'});let calls=0;
 const result=await hostedPictureImage(p,{jobId:'test-job-00000001',session:{token:async()=>'ac-token'},fetchImpl:async(url,options)=>{
  calls++; assert.equal(url,'https://aesthetic.computer/api/easel-image');assert.equal(options.headers.Authorization,'Bearer ac-token');
  const body=JSON.parse(options.body);assert.equal(body.model,FAL_FLARE);assert.equal(body.provider,'fal');assert.equal(body.jobId,'test-job-00000001');assert.equal(body.key,undefined);
  return Response.json({png:Buffer.from('test-png').toString('base64'),provenance:{provider:'fal',model:FAL_FLARE}});
 }});
 assert.equal(calls,1);assert.equal(result.provenance.model,FAL_FLARE);
});
test('Flare cannot silently become an edit or an unsupported hosted model',async()=>{
 assert.throws(()=>plan({provider:'fal',model:FAL_FLARE,prompt:'edit',reference:'composite.png'}),/matching/);
 await assert.rejects(hostedPictureImage({provider:'fal',model:'fal-ai/flux/dev'}, {session:{token:()=>assert.fail('no auth')}}),/own provider key/);
});
