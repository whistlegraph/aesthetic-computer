import assert from 'node:assert/strict';
import {generationProfile,DEFAULT_MODEL,FLASH_MODEL,REPAIR_MODEL} from '../Resources/Web/generation-policy.mjs';
assert.equal(DEFAULT_MODEL,'anthropic/claude-sonnet-5.5');
for(const handle of ['', 'fixture', 'Jeffrey', 'jeffrey-other']) {
  // Everyone gets hosted Sonnet with the full Claude budget.
  assert.equal(generationProfile(handle).model,DEFAULT_MODEL);
  assert.equal(generationProfile(handle).rounds,12);
  assert.equal(generationProfile(handle).maxTokens,16384);
  assert.equal(generationProfile(handle).personalRelay,false);
  assert.equal(generationProfile(handle,{repair:true}).model,DEFAULT_MODEL);
  assert.equal(generationProfile(handle,{model:'anthropic/claude-opus-5.5'}).model,'anthropic/claude-opus-5.5');
  assert.equal(generationProfile(handle,{model:'anthropic/claude-opus-5.5'}).personalRelay,false);
  // DeepSeek keeps its lean budget and its text-only repair model.
  assert.equal(generationProfile(handle,{model:FLASH_MODEL}).maxTokens,4096);
  assert.equal(generationProfile(handle,{model:FLASH_MODEL,repair:true}).model,REPAIR_MODEL);
  // The text-only repair model cannot take the chalk PNG.
  assert.equal(generationProfile(handle,{model:FLASH_MODEL,repair:true,image:true}).model,FLASH_MODEL);
}
assert.equal(generationProfile('jeffrey',{personalAccess:true}).model,'anthropic/claude-opus-5');
assert.equal(generationProfile('jeffrey',{personalAccess:true}).rounds,12);
assert.equal(generationProfile('jeffrey',{personalAccess:true}).maxTokens,16384);
assert.equal(generationProfile('jeffrey',{personalAccess:true,repair:true}).rounds,4);
console.log('PASS hosted Sonnet default for everyone; relay models only with access.');

assert.equal(generationProfile('fixture',{model:'anthropic/claude-opus-5'}).model,DEFAULT_MODEL);
assert.equal(generationProfile('jeffrey',{personalAccess:true,model:DEFAULT_MODEL}).model,DEFAULT_MODEL);
assert.equal(generationProfile('jeffrey',{personalAccess:true,model:FLASH_MODEL,repair:true}).model,REPAIR_MODEL);
assert.equal(generationProfile('jeffrey',{personalAccess:true}).personalRelay,true);
assert.equal(!!generationProfile('fixture').personalRelay,false);
assert.equal(generationProfile('jeffrey',{personalAccess:true,model:DEFAULT_MODEL}).personalRelay,false);

assert.equal(generationProfile('jeffrey',{personalAccess:true,model:'openai/gpt-6-astra'}).personalRelay,true);
assert.equal(generationProfile('jeffrey',{personalAccess:true,model:'openai/gpt-6-astra'}).model,'openai/gpt-6-astra');
for (const handle of ['', 'fixture', 'Jeffrey', 'jeffrey-other']) assert.equal(generationProfile(handle,{model:'openai/gpt-6-astra'}).model,DEFAULT_MODEL);
const trial={personalAccess:true};
assert.equal(generationProfile('fifi',trial).model,'anthropic/claude-opus-5');
assert.equal(generationProfile('fifi',{...trial,model:'openai/gpt-6-astra'}).personalRelay,true);
assert.equal(generationProfile('fifi',{...trial,model:DEFAULT_MODEL}).personalRelay,false);
assert.equal(generationProfile('fifi',{model:'anthropic/claude-opus-5'}).model,DEFAULT_MODEL);
const {hasPersonalAccess,fetchPersonalAccess}=await import('../Resources/Web/personal-access.mjs');
const access={personal:true,providers:['claude','codex'],expiresAt:new Date(2000).toISOString()};
assert.equal(hasPersonalAccess(access,1999),true);assert.equal(hasPersonalAccess(access,2000),false);
assert.equal(hasPersonalAccess({personal:true}),false);
assert.equal(await fetchPersonalAccess('x',{fetch:async()=>({ok:false})}),null);
assert.equal(await fetchPersonalAccess('x',{fetch:async()=>{throw Error('offline')}}),null);
console.log('PASS verified trial capability, expiry and fail-closed discovery.');

// The handle alone grants nothing; access comes from the relay.
assert.equal(generationProfile('jeffrey').model,DEFAULT_MODEL);
