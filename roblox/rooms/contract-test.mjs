// Run with LUAU_BIN=/path/to/luau node roblox/rooms/contract-test.mjs.
// Executes the actual Luau validator against cases also checked by JavaScript.
import assert from 'node:assert/strict';
import {mkdtemp,copyFile,writeFile,rm} from 'node:fs/promises';
import {tmpdir} from 'node:os';
import {join} from 'node:path';
import {execFileSync} from 'node:child_process';
import {starterRoom,validateRoom,drawPath} from '../../shared/roblox-room.mjs';
const cases=[['starter',starterRoom(),true],['drawn path',drawPath(starterRoom(),[[0,10],[12,0],[0,-10]]),true]];
for(const [name,change] of [
  ['script',r=>{r.script='print(1)';}],
  ['empty',r=>{r.objects=[];}],
  ['duplicate',r=>{r.objects.push({...r.objects[0]});}],
  ['over budget',r=>{r.objects=Array.from({length:65},(_,i)=>({...r.objects[0],id:'item-'+i}));}],
  ['negative size',r=>{r.objects[0].size[0]=-1;}],
  ['fractional color',r=>{r.objects[0].color[0]=.5;}],
  ['unknown behavior',r=>{r.objects[0].kind='script';}],
  ['high bounce',r=>{r.objects[0].bounce=101;}],
  ['yaw',r=>{r.objects[0].yaw=181;}],
  ['infinite position',r=>{r.objects[0].position[0]=Infinity;}],
  ['NaN position',r=>{r.objects[0].position[0]=NaN;}],
  ['sparse vector',r=>{delete r.spawn[1];}],
  ['sparse objects',r=>{delete r.objects[1];}],
  ['invalid id',r=>{r.objects[0].id=']==]';}],
]){const room=starterRoom();change(room);cases.push([name,room,false]);}
function lua(value){
  if(typeof value==='number')return Number.isNaN(value)?'(0/0)':!Number.isFinite(value)?'math.huge':String(value);
  if(value===undefined||value===null)return 'nil';
  if(Array.isArray(value))return '{'+Array.from(value,lua).join(',')+'}';
  if(typeof value==='object')return '{'+Object.entries(value).map(([k,v])=>'['+JSON.stringify(k)+']='+lua(v)).join(',')+'}';
  return JSON.stringify(value);
}
const dir=await mkdtemp(join(tmpdir(),'whistlegraph-luau-'));
try{
  await copyFile(new URL('src/server/Room.luau',import.meta.url),join(dir,'Room.luau'));
  let script='local Room = require("./Room")\n';
  for(const [name,room,expected] of cases){
    let valid=true;try{validateRoom(room);}catch{valid=false;}
    assert.equal(valid,expected,name+' JavaScript');
    script+=`assert(pcall(Room.validate, ${lua(room)}) == ${expected}, ${JSON.stringify(name+' Luau')})\n`;
  }
  script+=`print("PASS ${cases.length} shared JavaScript/Luau grammar cases")\n`;
  await writeFile(join(dir,'contract.luau'),script);
  process.stdout.write(execFileSync(process.env.LUAU_BIN||'luau',[join(dir,'contract.luau')],{encoding:'utf8'}));
}finally{await rm(dir,{recursive:true,force:true});}
