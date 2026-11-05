import test from 'node:test';
import assert from 'node:assert/strict';
import {createNotepatControls} from '../public/aesthetic.computer/lib/notepat-controls.mjs';
import {createMidiController} from '../public/aesthetic.computer/lib/midi-controller.mjs';
import {createNotepatPanel,notepatPanelLayout,notepatPanelItems} from '../public/aesthetic.computer/lib/notepat-panel.mjs';
const tick=()=>new Promise(r=>setImmediate(r));
test('headless controls publish MIDI choices, require explicit access, and preserve program/channel routing',async()=>{
 const sent=[],bytes=[],gm=[],focus=[],events=new EventTarget(),visibility=new EventTarget();let requests=0;
 const port={id:'output',name:'Piano',state:'connected',send:data=>bytes.push(data)};
 const access={inputs:new Map(),outputs:new Map([[port.id,port]])};
 const controls=createNotepatControls({send:e=>sent.push(e),context:()=>null,focus:v=>focus.push(v),events,visibility,
  createMidi:options=>createMidiController({...options,navigator:{requestMIDIAccess:async()=>{requests++;return access}}}),
  createGm:()=>({program:p=>gm.push(p),panic(){},note(){},stop(){},update(){}})});
 controls.message({op:'open'});assert.equal(requests,0);assert.equal(sent.at(-1).content.controlsOpen,true);assert.deepEqual(focus,[],'opening never locks canvas keys');
 controls.message({op:'connect'});await tick();assert.equal(requests,1);assert.equal(sent.at(-1).content.midi.outputs[0].name,'Piano');
 controls.message({op:'select-output',value:'output'});assert.deepEqual(bytes.at(-1),[0xc0,0]);
 controls.message({op:'select-channel',value:'3'});assert.deepEqual(bytes.at(-1),[0xc3,0]);
 controls.message({op:'select-sound',value:'gm:12'});assert.deepEqual(sent.at(-1),{type:'notepat:control',content:{kind:'program',program:12}});
 controls.message({op:'select-sound',value:'harp'});assert.equal(sent.at(-1).content.wave,'harp');
 controls.message({op:'local',enabled:false});assert.equal(sent.at(-1).content.enabled,false);
 controls.message({op:'record'});assert.equal(sent.at(-1).content.kind,'record');
 controls.message({op:'stop-notes'});assert.equal(sent.at(-1).content.kind,'panic');
 const letter=new Event('keydown',{cancelable:true});Object.defineProperty(letter,'key',{value:'c'});events.dispatchEvent(letter);assert.equal(letter.defaultPrevented,false);
 const escape=new Event('keydown',{cancelable:true});Object.defineProperty(escape,'key',{value:'Escape'});events.dispatchEvent(escape);assert.equal(escape.defaultPrevented,true);assert.equal(sent.at(-1).content.controlsOpen,false);
 controls.message({op:'close'});assert.equal(sent.at(-1).content.controlsOpen,false);
 controls.dispose();const count=sent.length;events.dispatchEvent(new Event('blur'));assert.equal(sent.length,count);
});
test('every instrument and MIDI port remains reachable on narrow and short screens',()=>{
 const state={midi:{supported:true,connected:true,inputId:'all',inputs:Array.from({length:25},(_,i)=>({id:String(i),name:`Keyboard ${i}`})),outputs:[]},instrument:{wave:'gm',program:0,local:true}};
 for(const screen of [{width:96,height:96},{width:187,height:338},{width:472,height:190}])for(const view of ['main','sounds','inputs','outputs','channels']){
  const first=notepatPanelLayout(screen,{view,state});let seen=[];
  for(let page=0;page<first.pages;page++){
   const layout=notepatPanelLayout(screen,{view,state,page});
   assert.ok(layout.x>=0&&layout.x+layout.w<=screen.width&&layout.y+layout.h<=screen.height);
   for(const c of layout.controls){assert.ok(c.x>=layout.x&&c.x+c.w<=layout.x+layout.w&&c.y+c.h<=layout.y+layout.h);if(c.action==='select')seen.push(c.id)}
  }
  assert.deepEqual(seen,notepatPanelItems(view,state).map(c=>c.id));
 }
});
test('AC panel consumes pointer hits, allows musical keys, and changes a sound through the host',()=>{
 const calls=[];let closed=0;
 class Button{constructor(x,y,w,h){Object.assign(this,{x,y,w,h})}act(e,cb){if(e.is('lift')&&e.x>=this.x&&e.x<this.x+this.w&&e.y>=this.y&&e.y<this.y+this.h)cb.push()}paint(f){f({})}}
 const api={screen:{width:472,height:190},ui:{Button},pens:()=>[]};
 const event=(name,x,y)=>({name,x,y,key:'c',is:type=>name===type||name.startsWith(type+':')});
 const panel=createNotepatPanel({host:v=>calls.push(v),close:()=>closed++});panel.open();panel.state({instrument:{wave:'gm',program:0,local:true},midi:{supported:true}});
 assert.equal(panel.act(event('keyboard:down:c'),api),false);
 const click=(view,id)=>{const row=notepatPanelLayout(api.screen,{view}).controls.find(c=>c.id===id);assert.equal(panel.act(event('lift',row.x+2,row.y+2),api),true)};
 click('main','sounds');click('sounds','gm:1');assert.deepEqual(calls.at(-1),{op:'select-sound',value:'gm:1'});
 click('sounds','close');assert.equal(closed,1);
});
