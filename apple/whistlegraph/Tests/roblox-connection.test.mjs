import test from 'node:test';
import assert from 'node:assert/strict';
import {saveForLaunch,launchURL} from '../Resources/Web/roblox-connection.mjs';
import {starterRoom} from '../../../shared/roblox-room.mjs';

function storage(){const data=new Map();return {getItem:k=>data.get(k)??null,setItem:(k,v)=>data.set(k,v),removeItem:k=>data.delete(k)};}
const link='https://www.roblox.com/share?code=fixture&type=ExperienceDetails';
test('a lost save reply is retried with the original id before saving a newer edit',async()=>{
  const s=storage(),calls=[],room=starterRoom();let revision=0,lost=true;
  const accepted=new Map();
  const request=async(method,body)=>{
    calls.push(structuredClone(body));
    if(!accepted.has(body.requestId)){
      assert.equal(body.expectedRevision,revision);
      accepted.set(body.requestId,{revision:++revision,launchURL:link});
    }
    if(lost){lost=false;throw Error('connection lost');}
    return accepted.get(body.requestId);
  };
  await assert.rejects(saveForLaunch({storage:s,owner:'maker',room,request,uuid:()=> 'first'}),/connection lost/);
  const newer=structuredClone(room);newer.objects[0].bounce=50;
  const result=await saveForLaunch({storage:s,owner:'maker',room:newer,request,uuid:()=> 'second'});
  assert.deepEqual(calls.map(c=>[c.requestId,c.expectedRevision]),[['first',0],['first',0],['second',1]]);
  assert.equal(result.revision,2);
  assert.equal(s.getItem('whistlegraph-roblox-cloud-maker-pending'),null);
  assert.deepEqual(JSON.parse(s.getItem('whistlegraph-roblox-cloud-maker')),{revision:2,source:JSON.stringify(newer)});
});
test('room journals are isolated by signed-in owner and do not accept missing acknowledgments',async()=>{
  const s=storage(),room=starterRoom();
  const request=async()=>({revision:1,launchURL:link});
  await saveForLaunch({storage:s,owner:'one',room,request});
  let expected;
  await assert.rejects(saveForLaunch({storage:s,owner:'two',room,request:async(_,body)=>{expected=body.expectedRevision;return {revision:8,launchURL:link};}}),/not acknowledged/);
  assert.equal(expected,0);assert.equal(s.getItem('whistlegraph-roblox-cloud-two'),null);
  assert.ok(s.getItem('whistlegraph-roblox-cloud-two-pending'));
  await assert.rejects(saveForLaunch({storage:s,owner:'',room,request}),/account/);
});
test('a successful save cannot launch an arbitrary application or domain',async()=>{
  for(const url of ['roblox://placeId=123','https://www.roblox.com.evil.test/share?code=x','https://user@www.roblox.com/share?code=x','https://www.roblox.com/games/123','https://www.roblox.com/share'])assert.throws(()=>launchURL(url));
  const s=storage();
  await assert.rejects(saveForLaunch({storage:s,owner:'one',room:starterRoom(),request:async()=>({revision:1,launchURL:'https://example.com/share?code=x'})}),/invalid/);
  assert.equal(s.getItem('whistlegraph-roblox-cloud-one'),null);
  assert.ok(s.getItem('whistlegraph-roblox-cloud-one-pending'));
});
