import assert from 'node:assert/strict';
import {generationProfile,modelChoices,DEFAULT_MODEL,FLASH_MODEL,REPAIR_MODEL} from '../Resources/Web/generation-policy.mjs';
assert.equal(DEFAULT_MODEL,'anthropic/claude-sonnet-5.5');
for(const handle of ['', 'fixture', 'jeffrey', 'Jeffrey']) for(const personalAccess of [false,true]) {
  // Everyone, the owner and relay-granted accounts included, uses hosted Sonnet.
  const p=generationProfile(handle,{personalAccess});
  assert.equal(p.model,DEFAULT_MODEL);assert.equal(p.personalRelay,false);
  assert.equal(p.rounds,12);assert.equal(p.maxTokens,16384);
  assert.equal(generationProfile(handle,{repair:true}).model,DEFAULT_MODEL);
  assert.equal(generationProfile(handle,{model:'anthropic/claude-opus-5.5'}).model,'anthropic/claude-opus-5.5');
  // Relay models are no longer offered; a saved one falls back to the default.
  for(const relay of ['anthropic/claude-opus-5','openai/gpt-6-astra'])assert.equal(generationProfile(handle,{model:relay,personalAccess}).model,DEFAULT_MODEL);
  // DeepSeek keeps its lean budget and its text-only repair model.
  assert.equal(generationProfile(handle,{model:FLASH_MODEL}).maxTokens,4096);
  assert.equal(generationProfile(handle,{model:FLASH_MODEL,repair:true}).model,REPAIR_MODEL);
  // The text-only repair model cannot take the chalk PNG.
  assert.equal(generationProfile(handle,{model:FLASH_MODEL,repair:true,image:true}).model,FLASH_MODEL);
}
assert.deepEqual(modelChoices().map(m=>m.id),['anthropic/claude-sonnet-5.5','anthropic/claude-opus-5.5',FLASH_MODEL,REPAIR_MODEL]);
console.log('PASS hosted Sonnet default for everyone; no relay models.');
const {hasPersonalAccess,fetchPersonalAccess}=await import('../Resources/Web/personal-access.mjs');
const access={personal:true,providers:['claude','codex'],expiresAt:new Date(2000).toISOString()};
assert.equal(hasPersonalAccess(access,1999),true);assert.equal(hasPersonalAccess(access,2000),false);
assert.equal(hasPersonalAccess({personal:true}),false);
assert.equal(await fetchPersonalAccess('x',{fetch:async()=>({ok:false})}),null);
assert.equal(await fetchPersonalAccess('x',{fetch:async()=>{throw Error('offline')}}),null);
console.log('PASS verified trial capability, expiry and fail-closed discovery.');
