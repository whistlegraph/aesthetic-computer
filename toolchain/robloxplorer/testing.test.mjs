import test from 'node:test';
import assert from 'node:assert/strict';
import {launchURL, control, api} from './testing.mjs';
import {handleMessage} from './testing-mcp.mjs';
test('launch URL pins numeric place and optional UUID without arbitrary URL parameters', () => {
  assert.equal(launchURL('123'), 'roblox://placeId=123');
  assert.match(launchURL('123','12345678-1234-1234-1234-123456789abc'), /gameInstanceId=/);
  for (const value of ['123&accessCode=x','../1','0']) assert.throws(()=>launchURL(value));
  assert.throws(()=>launchURL('123','x&userId=1'));
});
test('testing MCP rejects arbitrary shell/key input before any host action', async () => {
  await assert.rejects(control({action:'do shell script'}));
  await assert.rejects(control({action:'forward',milliseconds:5000}));
  await assert.rejects(api({command:'publish'}));
  const r=await handleMessage({id:1,method:'tools/call',params:{name:'roblox_capture',arguments:{path:'/tmp/arbitrary'}}});
  assert.equal(r.result.isError,true);
});
test('MCP initialization, discovery, notifications, and errors use protocol envelopes', async () => {
  assert.equal((await handleMessage({id:1,method:'initialize'})).result.serverInfo.name,'roblox-testing');
  assert.equal((await handleMessage({id:2,method:'tools/list'})).result.tools.length,6);
  assert.equal(await handleMessage({method:'notifications/initialized'}),null);
  assert.equal((await handleMessage({id:3,method:'tools/call',params:{name:'unknown'}})).result.isError,true);
});
