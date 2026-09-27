import test from 'node:test';
import assert from 'node:assert/strict';
import {stat} from 'node:fs/promises';
import {createSettingsController,serveSettings,callSettings,isHarnessRequest} from '../src/harness-settings.mjs';
import {mcpConfig,codexMcpArgs,handle} from '../src/tools.mjs';
function fixture(){
 let state={provider:'claude',model:'opus',effort:'',autopublish:false},busy=true,opened=0,failed=false;
 const changes=[];
 const controller=createSettingsController({read:()=>state,isBusy:()=>busy,open:()=>{opened++;},normalize:(patch,pending)=>({...state,...pending,...patch}),apply:async next=>{if(failed)throw Error('Could not connect');changes.push(next);state=next;}});
 return {controller,changes,get opened(){return opened;},setBusy:v=>busy=v,setFailed:v=>failed=v};
}
test('settings read/open are immediate; provider changes wait until turn completion',async()=>{
 const f=fixture();assert.equal((await f.controller.call({action:'read'})).provider,'claude');
 assert.equal((await f.controller.call({action:'open'})).status,'opened');assert.equal(f.opened,1);
 const queued=await f.controller.call({action:'update',provider:'codex'});assert.equal(queued.status,'queued');assert.equal(queued.provider,'claude');assert.equal(queued.pending.provider,'codex');assert.equal(f.changes.length,0);
 await f.controller.call({action:'update',model:'test-model',effort:'high'});
 f.setBusy(false);const result=await f.controller.flush();assert.equal(result.provider,'codex');assert.equal(result.model,'test-model');assert.equal(result.effort,'high');assert.equal(f.changes.length,1);
 assert.equal((await f.controller.flush()).status,'unchanged');
});
test('interrupted requests cancel pending changes; failed switches keep prior settings',async()=>{
 const f=fixture();await f.controller.call({action:'update',provider:'codex'});await f.controller.flush({cancel:true});assert.equal(f.changes.length,0);assert.equal(f.controller.pending,null);
 f.setBusy(false);f.setFailed(true);await assert.rejects(f.controller.call({action:'update',provider:'codex'}),/connect/);assert.equal((await f.controller.call({action:'read'})).provider,'claude');
});
test('settings rejects malformed and unsupported controls without applying them',async()=>{
 const f=fixture();for(const args of [{action:'read',provider:'codex'},{action:'update'},{action:'update',provider:'unknown'},{action:'update',model:'x\ncommand'},{action:'update',autopublish:'true'},{action:'update',apiKey:'secret'},{action:'exec',command:'anything'}])await assert.rejects(f.controller.call(args));assert.equal(f.changes.length,0);
});
test('session socket executes the same controls, is private, and disappears on close',async t=>{
 const f=fixture(),bridge=await serveSettings(args=>f.controller.call(args));t.after(()=>bridge.close());
 assert.equal((await stat(bridge.socket)).mode&0o777,0o600);
 assert.equal((await callSettings({action:'read'},{socket:bridge.socket})).provider,'claude');
 assert.equal((await callSettings({action:'update',provider:'codex'},{socket:bridge.socket})).status,'queued');
 await assert.rejects(callSettings({action:'read'},{socket:''}),/outside a running session/);
});
test('Claude and Codex MCP configs explicitly forward only the session control socket',()=>{
 const environment={EASEL_HARNESS_SOCKET:'/tmp/session/settings.sock',PRIVATE_TOKEN:'not-forwarded'};
 const config=mcpConfig('/tmp',environment);assert.equal(config.mcpServers.ac.env.EASEL_HARNESS_SOCKET,environment.EASEL_HARNESS_SOCKET);assert.equal(config.mcpServers.ac.env.PRIVATE_TOKEN,undefined);
 assert(codexMcpArgs('/tmp',environment).some(arg=>arg.includes('env.EASEL_HARNESS_SOCKET=')));
});
test('MCP settings call reaches the live session without editing the workspace',async t=>{
 const f=fixture(),bridge=await serveSettings(args=>f.controller.call(args));t.after(()=>bridge.close());
 const previous=process.env.EASEL_HARNESS_SOCKET;process.env.EASEL_HARNESS_SOCKET=bridge.socket;t.after(()=>{if(previous===undefined)delete process.env.EASEL_HARNESS_SOCKET;else process.env.EASEL_HARNESS_SOCKET=previous;});
 const reply=await handle({id:1,method:'tools/call',params:{name:'aesel_settings',arguments:{action:'open'}}},{cwd:'/tmp'});
 assert.equal(JSON.parse(reply.result.content[0].text).status,'opened');assert.equal(f.opened,1);
});

test('obvious harness requests skip canvas capture but ordinary piece requests keep it',()=>{
 for(const text of ['open settings','show me your settings','switch to Codex','switch provider','which model are you using in your settings?','what is your provider?','which model are you using?']) {
  assert.equal(isHarnessRequest(text),true,text);
 }
 for(const text of ['3+3 = ?','draw a model of a donkey','show me a settings panel in the piece','what is 3+3?'])assert.equal(isHarnessRequest(text),false,text);
});
test('parallel setting updates merge instead of losing the earlier change',async()=>{
 const f=fixture();await Promise.all([f.controller.call({action:'update',provider:'codex'}),f.controller.call({action:'update',effort:'high'})]);
 await f.controller.flush();assert.equal(f.changes[0].provider,'codex');assert.equal(f.changes[0].effort,'high');
});
