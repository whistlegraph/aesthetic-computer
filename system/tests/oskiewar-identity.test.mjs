import assert from 'node:assert/strict';
import test, { mock } from 'node:test';

const handles = [{_id:'auth0|primary',handle:'tester'}];
let alias = 'tester', locked = false, requests = [];
const profiles = new Map([
  ['auth0|primary',{email:'player@example.test',email_verified:true}],
  ['email|otp',{email:'player@example.test',email_verified:true}],
]);
mock.module('../backend/database.mjs', {exports:{connect:async()=>({db:{collection:()=>({
  findOne:async query=>handles.find(row=>Object.entries(query).every(([key,value])=>row[key]===value)),
})}})}});
mock.module('../backend/authorization.mjs', {exports:{
  handleFor:async()=>alias,
  userEmailFromID:async sub=>{requests.push(sub);return profiles.get(sub);},
}});
mock.module('../backend/account-lock.mjs', {exports:{accountLocked:async()=>locked}});
const {fighterIdentity}=await import('../backend/oskiewar-identity.mjs');

test('email-code sign-in recovers the handle owner only with matching verified addresses',async()=>{
  assert.deepEqual(await fighterIdentity('auth0|primary'),{sub:'auth0|primary',handle:'tester'});
  assert.deepEqual(requests,[],'the direct handle owner needs no alias lookup');
  assert.deepEqual(await fighterIdentity('email|otp'),{sub:'auth0|primary',handle:'tester'});
  for(const bad of [{email:'other@example.test',email_verified:true},{email:'player@example.test',email_verified:false},{email:'',email_verified:true}]){
    profiles.set('email|otp',bad);
    await assert.rejects(fighterIdentity('email|otp'),{status:403});
  }
  profiles.set('email|otp',{email:'player@example.test',email_verified:true});
  profiles.set('auth0|primary',{email:'player@example.test',email_verified:false});
  await assert.rejects(fighterIdentity('email|otp'),{status:403});
  profiles.set('auth0|primary',{email:'player@example.test',email_verified:true});
  locked=true;
  await assert.rejects(fighterIdentity('email|otp'),{status:401});
  locked=false; profiles.delete('email|otp');
  await assert.rejects(fighterIdentity('email|otp'),{status:503});
  alias=undefined;
  assert.deepEqual(await fighterIdentity('email|new'),{sub:'email|new',handle:null});
});
