import test from 'node:test';
import assert from 'node:assert/strict';
import { inspectROM, isNES, NES_TYPE, MAX_ROM } from '../public/nes-player/rom.mjs';
import { inputROM } from '../tests/fixtures/nes-input.mjs';
import { createHandler } from '../netlify/functions/mime.mjs';

test('NES validation bounds iNES, trainers and NES 2.0 allocations before emulation', () => {
  const bytes = inputROM();
  assert.equal(inspectROM(bytes).mapper, 0);
  assert.equal(inspectROM(bytes).supported, true);
  assert.ok(isNES({name:'Game.NES', type:'application/octet-stream'}));
  assert.ok(isNES({type:'application/x-nes'}));
  assert.ok(!isNES({name:'game.zip', type:'application/zip'}));
  for (const bad of [new Uint8Array(), new Uint8Array(16), bytes.slice(0, -1), new Uint8Array(MAX_ROM + 1)]) assert.throws(() => inspectROM(bad));
  const trainer = new Uint8Array(bytes.length + 512); trainer.set(bytes.slice(0,16)); trainer[6] |= 4; trainer.set(bytes.slice(16), 528);
  assert.equal(inspectROM(trainer).trainer, 512);
  trainer[4] = 255; assert.throws(() => inspectROM(trainer), /Incomplete/);
  bytes[7] = 8; assert.equal(inspectROM(bytes).format, 'NES 2.0');
  bytes[9] = 15; bytes[4] = 255; assert.throws(() => inspectROM(bytes), /size/);
  bytes[9] = 0; bytes[4] = 1; bytes[8] = 15;
  assert.equal(inspectROM(bytes).supported, false);
});

test('MIME stores canonical NES files, preserves bytes and rejects truncated ROMs', async () => {
  const rows = [], posts = {createIndex:async()=>{}, findOne:async({code})=>rows.find(row=>row.code===code), insertOne:async row=>rows.push(row)};
  const db = {collection:name=>{assert.equal(name,'mimechan');return posts;}};
  const handler=createHandler(async()=>({db}));
  const bytes=inputROM(), data=Buffer.from(bytes).toString('base64');
  for (const type of ['', 'application/octet-stream', 'application/x-nes', NES_TYPE]) {
    const response=await handler({httpMethod:'POST',body:JSON.stringify({file:{name:'Game.NES',type,data}})});
    assert.equal(response.statusCode,200,response.body);
    assert.equal(JSON.parse(response.body).board,NES_TYPE);
  }
  const reply=await handler({httpMethod:'GET',queryStringParameters:{file:rows[0].code}});
  assert.equal(reply.statusCode,200);assert.equal(reply.headers['Content-Type'],NES_TYPE);
  assert.deepEqual(Buffer.from(reply.body,'base64'),Buffer.from(bytes));
  const bad=await handler({httpMethod:'POST',body:JSON.stringify({file:{name:'bad.nes',data:Buffer.from(bytes.slice(0,16)).toString('base64')}})});
  assert.equal(bad.statusCode,400);assert.match(bad.body,/Incomplete/);assert.equal(rows.length,4);
  bytes[6] = 0xf0; bytes[7] = 0xf0;
  const unsupported = await handler({httpMethod:'POST',body:JSON.stringify({file:{name:'unsupported.nes',data:Buffer.from(bytes).toString('base64')}})});
  assert.equal(unsupported.statusCode,400);assert.match(unsupported.body,/not supported/);assert.equal(rows.length,4);
});
