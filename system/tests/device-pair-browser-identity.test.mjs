import assert from 'node:assert/strict';
import test, { mock } from 'node:test';
let kind='browser', identityError=null, credentials=0, updates=[];
const pairs={createIndex:async()=>{},findOne:async()=>({_id:'ABC234',kind,claimed:false}),updateOne:async(q,value)=>{updates.push(value);return {matchedCount:1};}};
mock.module('../backend/database.mjs',{exports:{connect:async()=>({db:{collection:name=>name==='device-pairs'?pairs:{findOne:async()=>null}},disconnect:async()=>{}})}});
mock.module('../backend/authorization.mjs',{exports:{authorize:async()=>({sub:'email|otp'})}});
mock.module('../backend/oskiewar-identity.mjs',{exports:{fighterIdentity:async()=>{if(identityError)throw identityError;return {sub:'auth0|primary',handle:'tester'};}}});
mock.module('../backend/device-creds.mjs',{exports:{getDeviceCreds:async()=>{credentials++;return {claudeToken:'never-transfer',githubPat:'never-transfer'};}}});
const {handler}=await import('../netlify/functions/device-pair.mjs');
const claim=()=>handler({httpMethod:'POST',headers:{Authorization:'Bearer own-otp-token'},body:JSON.stringify({action:'claim',code:'ABC234'})});
test('browser pairing displays verified alias handle but transfers only the requesting identity token',async()=>{
  const response=await claim();assert.equal(response.statusCode,200);
  const stored=updates.at(-1).$set;
  assert.equal(stored.handle,'tester');assert.equal(stored.sub,'email|otp');assert.equal(stored.token,'own-otp-token');
  assert.equal(credentials,0);assert.equal('claudeToken' in stored,false);assert.equal('githubPat' in stored,false);
});
test('failed alias ownership check cannot claim a pairing',async()=>{
  identityError=Object.assign(Error('Unverified account'),{status:403});
  const count=updates.length;const response=await claim();assert.equal(response.statusCode,403);assert.equal(updates.length,count);
  identityError=null;
});
test('native device credentials still require a direct handle owner',async()=>{
  kind='native';const count=updates.length;const response=await claim();
  assert.equal(response.statusCode,404);assert.equal(updates.length,count);assert.equal(credentials,0);
});
