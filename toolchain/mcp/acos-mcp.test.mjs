import test from 'node:test';
import assert from 'node:assert/strict';
import { commandFor,sshArgs,handleMessage } from './acos-mcp.mjs';

test('SSH requires a configured alias, never an option or shell expression',()=>{
  for(const ssh of ['-oProxyCommand=anything','root@host','x; reboot','x $(id)','x\nHost *',''])
    assert.throws(()=>sshArgs({ssh},'fixed command'));
  const args=sshArgs({ssh:'julia-acos'},'fixed command');
  assert.equal(args.at(-2),'julia-acos');
  assert(args.includes('StrictHostKeyChecking=yes'));
  assert(args.includes('BatchMode=yes'));
});
test('logs are bounded and restricted to known paths',()=>{
  for(const lines of [0,201,-2,1.5,'80; reboot',null]) {
    if(lines!==null)assert.throws(()=>commandFor('acos_logs',{lines}));
  }
  for(const source of ['/etc/shadow','../config.json','runtime; reboot','toString'])
    assert.throws(()=>commandFor('acos_logs',{source}));
  assert.equal(commandFor('acos_logs',{source:'controller',lines:20}),'/bin/busybox tail -n 20 /tmp/ac-usb-controller.log');
});
test('notifications produce no response and tools cannot issue mutations',async()=>{
  assert.equal(await handleMessage({method:'notifications/initialized'}),null);
  const list=await handleMessage({id:1,method:'tools/list'});
  assert.deepEqual(list.result.tools.map(t=>t.name),['acos_list','acos_status','acos_logs']);
  assert(list.result.tools.every(t=>t.annotations.readOnlyHint));
  assert.throws(()=>commandFor('reboot'));
});
