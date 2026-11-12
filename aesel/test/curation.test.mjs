import test from 'node:test';
import assert from 'node:assert/strict';
import {setCuration,curatedChoice} from '../src/curation.mjs';
import {pickerModels,normalizeSettings} from '../src/provider-picker.mjs';
const SONNET='anthropic/claude-sonnet-5.5',PRO='deepseek/deepseek-v4-pro';
test('a curated account sees only its menu on AC, default first-class, no Automatic',()=>{
 setCuration({models:[SONNET,PRO],default:SONNET,by:'@jeffrey'});
 try{
  const ids=pickerModels({backend:'ac',model:SONNET}).map(c=>c.id);
  assert.deepEqual(ids.sort(),[PRO,SONNET].sort());
  assert.match(pickerModels({backend:'ac',model:SONNET}).find(c=>c.id===SONNET).detail,/curated by @jeffrey/);
  assert.ok(pickerModels({backend:'open',model:''}).length>2,'other providers stay open');
  assert.equal(curatedChoice('anthropic/claude-opus-5.5'),SONNET);
  assert.equal(curatedChoice(PRO),PRO);
  assert.equal(normalizeSettings({model:''},{provider:'ac',model:PRO,effort:'',autopublish:false}).model,SONNET);
  assert.throws(()=>normalizeSettings({model:'anthropic/claude-opus-5.5'},{provider:'ac',model:PRO,effort:'',autopublish:false}),/curated by @jeffrey/);
 }finally{setCuration(null);}
 assert.equal(pickerModels({backend:'ac',model:''})[0].label,'Automatic');
 assert.equal(curatedChoice('anthropic/claude-opus-5.5'),'anthropic/claude-opus-5.5');
});
