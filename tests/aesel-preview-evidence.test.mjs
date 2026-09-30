import test from 'node:test';
import assert from 'node:assert/strict';
import {createHash} from 'node:crypto';
import {createPreviewEvidence} from '../system/public/aesthetic.computer/lib/preview-evidence.mjs';

test('worker evidence hashes actual source and only accompanies frames after activation and paint',async()=>{
 const messages=[],e=createPreviewEvidence(m=>messages.push(m.content));
 const source='export function paint() {} // 🌸';
 const id=await e.begin(source,{sessionID:'thread',revision:2,requestID:1,sourceHash:'forged'});
 assert.equal(id.sourceHash,createHash('sha256').update(source).digest('hex'));
 e.paint();assert.equal(e.frame(),null);
 e.activate(id);assert.equal(e.frame(),null);
 e.paint();assert.deepEqual(e.frame(),id);
 e.record('error','x'.repeat(3000));assert.equal(messages.at(-1).event.message.length,2000);
 const newer=await e.begin('next',{sessionID:'thread',revision:3,requestID:2});
 e.activate(id);e.paint();assert.equal(e.frame(),null);
 e.activate(newer);e.paint();assert.equal(e.frame().revision,3);
 await e.begin('ordinary piece');e.record('log','private');assert.equal(e.frame(),null);
 assert.notEqual(messages.at(-1).event?.message,'private');
});
