import test from 'node:test';
import assert from 'node:assert/strict';
import {handler} from '../netlify/functions/oven-complete.mjs';
test('oven callback requires configured authentication before any database or publication work',async()=>{
 const original=process.env.OVEN_CALLBACK_SECRET;
 try{
  delete process.env.OVEN_CALLBACK_SECRET;
  assert.equal((await handler({httpMethod:'POST',body:'{}'})).statusCode,503);
  process.env.OVEN_CALLBACK_SECRET='test-only';
  assert.equal((await handler({httpMethod:'POST',body:'{"secret":"wrong"}'})).statusCode,401);
 }finally{if(original===undefined)delete process.env.OVEN_CALLBACK_SECRET;else process.env.OVEN_CALLBACK_SECRET=original;}
});
test('a new ZIP upload remains retryable before any record is created when conversion is unconfigured', async () => {
 const {handler: trackMedia} = await import('../netlify/functions/track-media.mjs');
 const original=process.env.OVEN_CALLBACK_SECRET;
 try {
  delete process.env.OVEN_CALLBACK_SECRET;
  const result=await trackMedia({httpMethod:'POST',body:'{"ext":"zip"}',headers:{}});
  assert.equal(result.statusCode,503);
  assert.match(JSON.parse(result.body).error,/retry/);
 } finally {if(original===undefined)delete process.env.OVEN_CALLBACK_SECRET;else process.env.OVEN_CALLBACK_SECRET=original;}
});
