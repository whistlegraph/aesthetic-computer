import test from 'node:test';
import assert from 'node:assert/strict';
import {pickerModels,pickerKey,pickerEfforts,pickerLines,drawerKey,drawerSelect} from '../src/provider-picker.mjs';
import {AppServer} from '../src/app-server.mjs';
import {renderFrame,headerAction,modelControls} from '../src/render.mjs';
const catalog=[{model:'example',displayName:'Example',isDefault:true,supportedReasoningEfforts:[{reasoningEffort:'low'},{reasoningEffort:'high'}]},{model:'other',supportedReasoningEfforts:[{reasoningEffort:'medium'}]}];
test('picker follows model-specific effort capabilities and clears stale effort',()=>{
 const p={backend:'codex',model:'example',effort:'high',row:1,catalog};
 assert.deepEqual(pickerEfforts(p),['','low','high']);
 const next=pickerKey(p,'\x1b[C');assert.equal(next.model,'other');assert.equal(next.effort,'');assert.deepEqual(pickerEfforts(next),['','medium']);
 assert.equal(pickerKey({...p,row:0},'\x1b[C').effort,'');
 assert.deepEqual(pickerEfforts({...p,backend:'ac'}),['']);
});
test('picker navigation, explicit apply, cancel and render',()=>{
 const p={backend:'codex',model:'example',effort:'high',row:0,catalog};
 assert.equal(pickerKey(p,'\t').row,1);assert.equal(pickerKey(p,'\x1b[Z').row,3);
 assert.equal(pickerKey(p,'\r').row,1);assert.equal(pickerKey({...p,row:3},'\r').action,'apply');
 assert.equal(pickerKey(p,'\x1b').action,'cancel');assert.equal(p.model,'example');
 assert.match(pickerLines(p).join('\n'),/Example/);
 const frame=renderFrame({entries:[],settings:p,input:'',workspace:'test'},100,30,false);
 assert.match(frame,/Bring your own provider/);assert.match(frame,/Effort\s+high/);
});
test('Codex sends selected effort on every turn and leaves defaults unset',async()=>{
 const engine=new AppServer({cwd:process.cwd(),effort:'high'});engine.threadId='test';
 let request;engine.request=async(method,params)=>{request={method,params};return {turn:{id:'turn'}};};
 await engine.startTurn('hello');assert.equal(request.params.effort,'high');
 engine.effort='';await engine.startTurn('next');assert.equal(Object.hasOwn(request.params,'effort'),false);
});

test('bottom controls and dropdown share click geometry and preserve transcript',()=>{
 const settings={backend:'codex',model:'example',effort:'high',row:0,index:2,catalog};
 const state={entries:[{kind:'user',text:'Keep this visible'}],settings,input:'',workspace:'test'};
 for(const width of [32,80,120]){
   const controls=modelControls(state,width);
   for(const c of controls)assert.equal(headerAction(state,width,24,c.x,22),c.action);
   assert.equal(headerAction(state,width,24,3,19),'choice:2');
   const frame=renderFrame(state,width,24,false);assert.match(frame,/Keep this visible/);
 }
 const next=drawerSelect(settings,1);assert.equal(next.backend,'claude');assert.equal(next.row,1);
 assert.equal(drawerKey(next,'\x1b').action,'cancel');
 assert.equal(drawerKey({...next,row:3,index:0},'\r').action,'apply');
 const colored=renderFrame(state,80,24,true);assert.match(colored,/\x1b\[48;/);
});

test('braincell picker cannot expose saved or custom model choices',()=>{
 for(const model of ['opus','anthropic/claude-opus-5','custom']){
  const p={backend:'ac',model,row:1};
  assert.deepEqual(pickerModels(p),[{id:'openai/gpt-5.6-luna',label:'Automatic'}]);
 }
});
