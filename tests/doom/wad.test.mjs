import test from 'node:test';
import assert from 'node:assert/strict';
import {compileWad,validateLevel,starterLevel,inspectWad} from '../../system/public/doom-player/wad.mjs';
test('exports a classic PWAD with a valid single-subsector map and broadphase blockmap',()=>{
  const level=starterLevel(),data=compileWad(level),{kind,lumps}=inspectWad(data);
  assert.equal(kind,'PWAD');assert.deepEqual(lumps.map(l=>l.name),['E1M1','THINGS','LINEDEFS','SIDEDEFS','VERTEXES','SEGS','SSECTORS','NODES','SECTORS','REJECT','BLOCKMAP']);
  const lump=name=>{const l=lumps.find(l=>l.name===name);return new DataView(data.buffer,l.offset,l.size);};
  const things=lump('THINGS');assert.equal(things.byteLength,(level.things.length+1)*10);assert.equal(things.getInt16(6,true),1);assert.equal(things.getInt16(4,true),90);assert.equal(things.getInt16(2,true),-level.depth/2+96);
  const lines=lump('LINEDEFS');for(let i=0;i<4;i++){assert.equal(lines.getUint16(i*14+10,true),i);assert.equal(lines.getUint16(i*14+12,true),65535);}assert.equal(lines.getUint16(3*14+6,true),11);
  assert.equal(lump('SSECTORS').getUint16(0,true),4);assert.equal(lump('NODES').byteLength,0);
  const blocks=lump('BLOCKMAP'),cols=blocks.getUint16(4,true),rows=blocks.getUint16(6,true),list=blocks.getUint16(8,true)*2;
  for(let i=0;i<cols*rows;i++)assert.equal(blocks.getUint16(8+i*2,true)*2,list);
  assert.deepEqual(Array.from({length:6},(_,i)=>blocks.getUint16(list+i*2,true)),[0,0,1,2,3,65535]);
});
test('rejects out of bounds things, blocked spawn, unsupported textures and geometry',()=>{
  const l=starterLevel();for(const change of [{width:385},{height:63},{wall:'NOPE'},{rooms:[]},{things:[{x:0,y:-288,type:3001,angle:0}]},{things:[{x:9000,y:0,type:3001,angle:0}]},{things:[{x:0,y:0,type:9999,angle:0}]}])assert.throws(()=>validateLevel({...l,...change}));
  assert.equal(compileWad({...l,width:2048,depth:2048,things:[]}).length>0,true);
});
test('rejects corrupt WAD directories and lumps before engine loading',()=>{
  assert.throws(()=>inspectWad(new Uint8Array(12)));const bytes=compileWad(starterLevel()),v=new DataView(bytes.buffer);v.setInt32(8,bytes.length,true);assert.throws(()=>inspectWad(bytes));
  const other=compileWad(starterLevel()),view=new DataView(other.buffer);view.setInt32(view.getInt32(8,true)+4,other.length,true);assert.throws(()=>inspectWad(other));
});

test('MIME upload recognizes .wad, normalizes its alias, and rejects invalid bytes',async()=>{
  const {createHandler}=await import('../../system/netlify/functions/mime.mjs');
  const saved=[],collection={createIndex:async()=>{},findOne:async()=>null,insertOne:async row=>saved.push(row)};
  const handler=createHandler(async()=>({db:{collection:()=>collection}}));
  const request=(type,data=compileWad(starterLevel()))=>handler({httpMethod:'POST',body:JSON.stringify({file:{name:'arena.wad',...(type?{type}:{}),data:Buffer.from(data).toString('base64')}})});
  assert.equal((await request()).statusCode,200);assert.equal(saved.at(-1).file.type,'application/x-doom-wad');
  assert.equal((await request('application/octet-stream')).statusCode,200);assert.equal(saved.at(-1).file.type,'application/x-doom-wad');
  assert.equal((await request('application/x-doom')).statusCode,200);assert.equal(saved.at(-1).file.type,'application/x-doom-wad');
  assert.match(JSON.parse((await request()).body).code,/^[A-Za-z0-9]+$/);
  const count=saved.length;assert.equal((await request('application/x-doom-wad',new Uint8Array(12))).statusCode,400);assert.equal(saved.length,count);
});
