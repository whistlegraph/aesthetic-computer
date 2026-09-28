import test from 'node:test';
import assert from 'node:assert/strict';
import {approvalFor,ApprovalQueue,approvalShape} from '../src/approvals.mjs';
const input=(id=1)=>({id,method:'item/tool/requestUserInput',params:{questions:[{id:'approve',question:'Allow ac_api?',options:[{label:'Allow'},{label:'Allow for this session'},{label:'Cancel'}]}]}});
test('Codex tool approvals answer with selected labels, not command decisions',()=>{
 const a=approvalFor(input());
 assert.deepEqual(a.responses.y,{answers:{approve:{answers:['Allow']}}});
 assert.deepEqual(a.responses.a,{answers:{approve:{answers:['Allow for this session']}}});
 assert.deepEqual(a.responses.n,{answers:{approve:{answers:['Cancel']}}});
 assert.equal(a.kind,'tool');
});
test('MCP empty confirmations use action/content; arbitrary forms/auth are never accepted',()=>{
 const base={id:2,method:'mcpServer/elicitation/request',params:{serverName:'ac',mode:'form',message:'Allow?',requestedSchema:{type:'object',properties:{}}}};
 assert.deepEqual(approvalFor(base).responses.y,{action:'accept',content:{}});
 assert.equal(approvalFor(base).responses.a,undefined);
 for(const params of [
  {...base.params,mode:'url',url:'https://example.com/auth'},
  {...base.params,mode:'openai/userVerification',challenge:'proof'},
  {...base.params,requestedSchema:{type:'object',properties:{token:{type:'string',default:'secret'}},required:['token']}},
  {...base.params,requestedSchema:{type:'object',properties:{persist:{type:'string',default:'always'}}}},
 ]){
  const a=approvalFor({...base,params});assert.equal(a.kind,'unsupported');assert.equal(a.responses.y,undefined);assert.equal(a.responses.a,undefined);assert.deepEqual(a.responses.n,{action:'cancel',content:null});
 }
});
test('three parallel requests queue; only the current choice resolves, and stale owners cannot answer',()=>{
 const q=new ApprovalQueue(), owner={},nextOwner={};
 q.enqueue(input(1),owner);q.enqueue(input(2),owner);q.enqueue(input(3),owner);
 assert.equal(q.current.id,1);assert.equal(q.answer('x',owner),null);
 assert.equal(q.answer('y',owner).approval.id,1);assert.equal(q.current.id,2);
 q.resolve(2,owner);assert.equal(q.current.id,3);
 assert.equal(q.answer('y',nextOwner),null);assert.equal(q.current,null);
});
test('normal command approvals retain session decision and tool questions cannot invent approval choices',()=>{
 const a=approvalFor({id:4,method:'item/commandExecution/requestApproval',params:{command:'node --check piece.mjs'}});
 assert.equal(a.kind,'command');assert.deepEqual(a.responses.a,{decision:'acceptForSession'});
 const r=input();r.params.questions[0].options=[{label:'Delete everything'}];assert.equal(approvalFor(r).responses.y,undefined);
 r.params.questions[0].isSecret=true;assert.equal(approvalFor(r).kind,'unsupported');
});


test('approval diagnostic records bounded shape without payload values or enum labels',()=>{
 const shape=approvalShape({method:'mcpServer/elicitation/request',params:{serverName:'ac',mode:'form',message:'SECRET_MESSAGE',url:'SECRET_URL',challenge:'SECRET_CHALLENGE',_meta:{kind:'SECRET_METADATA'},requestedSchema:{properties:{persist:{type:'string',enum:['SECRET_OPTION'],default:'SECRET_DEFAULT'},token:{type:'string',value:'SECRET_VALUE'}}}}});
 assert.deepEqual(shape,{method:'mcpServer/elicitation/request',serverName:'ac',mode:'form',metadataKeys:['kind'],properties:[{name:'persist',type:'string'},{name:'token',type:'string'}]});
 assert.doesNotMatch(JSON.stringify(shape),/SECRET/);
 const many=Object.fromEntries(Array.from({length:100},(_,i)=>['x'.repeat(200)+i,{type:'malicious-value'}]));
 const bounded=approvalShape({method:'test',params:{_meta:many,requestedSchema:{properties:many}}});
 assert.equal(bounded.properties.length,12);assert.equal(bounded.metadataKeys.length,12);assert.ok(bounded.properties.every(p=>p.name.length<=80&&p.type==='unspecified'));
});

 test('default YOLO accepts every server tool confirmation but does not fabricate input', async()=>{
  const {defaultApprovalResponse}=await import('../src/approvals.mjs');
  const request={id:1,method:'mcpServer/elicitation/request',params:{serverName:'ac',mode:'form',requestedSchema:{type:'object',properties:{}}}};
  assert.deepEqual(defaultApprovalResponse(request),{action:'accept',content:{}});
  for(const serverName of ['external','ac','easel-media','custom-server'])assert.deepEqual(defaultApprovalResponse({...request,params:{...request.params,serverName}}),{action:'accept',content:{}});
  assert.deepEqual(defaultApprovalResponse(input()),{answers:{approve:{answers:['Allow']}}});
  for(const method of ['item/commandExecution/requestApproval','item/fileChange/requestApproval'])assert.deepEqual(defaultApprovalResponse({id:2,method,params:{}}),{decision:'accept'});
  assert.equal(defaultApprovalResponse({...request,params:{...request.params,mode:'url'}}),null);
  assert.equal(defaultApprovalResponse({...request,params:{...request.params,requestedSchema:{type:'object',properties:{password:{type:'string'}}}}}),null);
 });
