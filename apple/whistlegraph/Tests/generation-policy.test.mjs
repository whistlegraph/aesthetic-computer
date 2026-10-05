import assert from 'node:assert/strict';
import {generationProfile,DEFAULT_MODEL,REPAIR_MODEL} from '../Resources/Web/generation-policy.mjs';
for(const handle of ['', 'fixture', 'Jeffrey', 'jeffrey-other']) {
  assert.equal(generationProfile(handle).model,DEFAULT_MODEL);
  assert.equal(generationProfile(handle).rounds,4);
  assert.equal(generationProfile(handle).maxTokens,4096);
  assert.equal(generationProfile(handle,{repair:true}).model,REPAIR_MODEL);
}
assert.equal(generationProfile('jeffrey').model,'anthropic/claude-opus-5');
assert.equal(generationProfile('jeffrey').rounds,12);
assert.equal(generationProfile('jeffrey').maxTokens,16384);
assert.equal(generationProfile('jeffrey',{repair:true}).rounds,4);
console.log('PASS personal demo profile; other handles retain existing limits.');

assert.equal(generationProfile('fixture',{model:'anthropic/claude-opus-5'}).model,DEFAULT_MODEL);
assert.equal(generationProfile('jeffrey',{model:DEFAULT_MODEL}).model,DEFAULT_MODEL);
assert.equal(generationProfile('jeffrey',{model:DEFAULT_MODEL,repair:true}).model,REPAIR_MODEL);
assert.equal(generationProfile('jeffrey').personalRelay,true);
assert.equal(!!generationProfile('fixture').personalRelay,false);
assert.equal(generationProfile('jeffrey',{model:DEFAULT_MODEL}).personalRelay,false);

assert.equal(generationProfile('jeffrey',{model:'openai/gpt-6-astra'}).personalRelay,true);
assert.equal(generationProfile('jeffrey',{model:'openai/gpt-6-astra'}).model,'openai/gpt-6-astra');
for (const handle of ['', 'fixture', 'Jeffrey', 'jeffrey-other']) assert.equal(generationProfile(handle,{model:'openai/gpt-6-astra'}).model,DEFAULT_MODEL);
