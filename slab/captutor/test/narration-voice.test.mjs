import test from 'node:test';import assert from 'node:assert/strict';import{narrationRequest}from'../lib/narration-voice.mjs';
test('stock narrator requests stock provider with measured alignment',()=>{assert.deepEqual(narrationRequest('eleven:neutral:0','Hi Koto.'),{from:'Hi Koto.',provider:'eleven',voice:'neutral:0',withTimestamps:true});});
test('unknown voice cannot fall back to Jeffrey',()=>{assert.throws(()=>narrationRequest('iris','Hi'));assert.equal(narrationRequest('jeffrey','Hi').provider,'jeffrey');});
