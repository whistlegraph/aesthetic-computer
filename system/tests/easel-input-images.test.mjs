import test from 'node:test';
import assert from 'node:assert/strict';
import {imageInputBound} from '../backend/easel-input-images.mjs';
import {inferenceRequest} from '../backend/easel-policy.mjs';
const data='iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAQAAAC1HAwCAAAAC0lEQVR42mP8/x8AAwMCAO+aKxkAAAAASUVORK5CYII=';
const image={type:'image',source:{type:'base64',media_type:'image/png',data}};
const body=content=>({messages:[{role:'user',content}]});
test('static PNGs add a pixel reservation and pass the shared free/paid request policy',()=>{
 assert.equal(imageInputBound(body('hello')),0);
 assert.equal(imageInputBound(body([image])),1026,'twice the provider rate (one token per 750 pixels) plus overhead');
 assert.ok(inferenceRequest(body([image])).model);
});
test('invalid, oversized, remote, animated and unbounded media fail before inference',()=>{
 const large=Buffer.from(data,'base64');large.writeUInt32BE(769,16);
 const animation=Buffer.from(data,'base64');animation.write('acTL',37);
 for(const value of [{...image,source:{type:'url',url:'https://example.com/image.png'}},
  {...image,source:{...image.source,data:'garbage'}},
  {...image,source:{...image.source,data:'a'.repeat(700004)}},
  {...image,source:{...image.source,data:large.toString('base64')}},
  {...image,source:{...image.source,data:animation.toString('base64')}},
  {type:'document'},{type:'video'},{type:'input_audio'},{type:'image_url'}]){
  assert.throws(()=>inferenceRequest(body([value])));
 }
 assert.throws(()=>imageInputBound(body(Array(17).fill(image))));
});
