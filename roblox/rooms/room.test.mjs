import test from 'node:test';
import assert from 'node:assert/strict';
import {starterRoom,validateRoom,drawPath,localRoomEdit} from '../../shared/roblox-room.mjs';
import {createRoomHandler,validRobloxLaunch} from '../../system/backend/whistlegraph-roblox.mjs';
test('room grammar rejects executable fields, duplicate IDs, nonfinite and excessive geometry',()=>{
  for(const mutate of [r=>r.script='return 1',r=>r.objects[1].id=r.objects[0].id,r=>r.objects[0].size[0]=NaN,r=>r.objects[0].bounce=101,r=>r.objects[0].source='print(1)',r=>r.objects=Array(65).fill(r.objects[0])]){const r=starterRoom();mutate(r);assert.throws(()=>validateRoom(r));}
  const r=starterRoom(),copy=validateRoom(r);copy.spawn[0]=20;assert.equal(r.spawn[0],0);
});
test('drawing creates finite, bounded platform segments; edits preserve unrelated objects',()=>{
  const room=starterRoom(),next=drawPath(room,[[0,0],[8,8],[16,8]]);
  assert.equal(next.objects.length,6);assert.equal(room.objects.length,4);assert.equal(next.objects[4].yaw,45);
  assert.throws(()=>drawPath(room,[[0,0],[99,0]]));
  const bounced=localRoomEdit(room,'make the bridge bounce');assert.equal(bounced.objects[1].bounce,55);assert.deepEqual(bounced.objects[0],room.objects[0]);
  assert.equal(localRoomEdit(room,'make a racing simulator'),null);
});
function fixture(){
  let row=null;
  const db={read:async()=>row,save:async(owner,base,room,id)=>{if(row?.requestId===id)return row;if((row?.revision||0)!==base)return null;return row={room,revision:base+1,requestId:id};},applied:async(owner,revision)=>{if(row?.revision===revision)row.appliedRevision=revision;}};
  const config={owner:'maker',playerId:'123',bridgeKey:'x'.repeat(32),launchURL:'https://www.roblox.com/share?code=fixture&type=ExperienceDetails'};
  const handler=createRoomHandler({authenticate:async h=>h.authorization==='Bearer maker'?'maker':h.authorization==='Bearer other'?'other':null,store:async()=>db,config:()=>config});
  const call=async(method,body,headers={Authorization:'Bearer maker'},queryStringParameters={})=>{const r=await handler({httpMethod:method,body:body&&JSON.stringify(body),headers,queryStringParameters});return {status:r.statusCode,...JSON.parse(r.body)};};
  return {call,config};
}
test('private pilot authenticates both ends, rejects impersonation and conflicts, and acknowledges only the current revision',async()=>{
  const {call}=fixture();const save={action:'save',expectedRevision:0,requestId:'11111111-1111-4111-8111-111111111111',room:starterRoom()};
  assert.equal((await call('POST',save,{})).status,401);assert.equal((await call('POST',save,{Authorization:'Bearer other'})).status,403);
  assert.equal((await call('POST',{...save,playerId:'999'})).status,400);
  assert.equal((await call('POST',save)).revision,1);assert.equal((await call('POST',save)).revision,1);
  assert.equal((await call('POST',{...save,requestId:'22222222-2222-4222-8222-222222222222'})).status,409);
  assert.equal((await call('GET',null,{'x-whistlegraph-bridge-key':'wrong'},{playerId:'123'})).status,401);
  const headers={'x-whistlegraph-bridge-key':'x'.repeat(32)};
  assert.equal((await call('GET',null,headers,{playerId:'999'})).status,404);
  assert.equal((await call('GET',null,headers,{playerId:'123'})).revision,1);
  await call('POST',{action:'applied',playerId:'123',revision:1},headers);
  assert.equal((await call('GET')).appliedRevision,1);
});
test('unconfigured pilots do not fabricate launch URLs',async()=>{
  const {call,config}=fixture();config.launchURL='';assert.equal((await call('GET')).available,false);
  for(const url of ['javascript:alert(1)','https://www.roblox.com.evil/share?code=x','https://evil@www.roblox.com/share?code=x','https://www.roblox.com/games/123'])assert.equal(validRobloxLaunch(url),false);
});
