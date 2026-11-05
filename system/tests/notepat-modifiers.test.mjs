import test from 'node:test';
import assert from 'node:assert/strict';
import {createNotepatModifiers,LINGER_SECONDS} from '../public/aesthetic.computer/lib/notepat-modifiers.mjs';
import {createGmBridge} from '../public/aesthetic.computer/lib/notepat-gm-bridge.mjs';
import {playLoadingTone} from '../public/aesthetic.computer/lib/gm.mjs';

test('Shift follows its octave, both shifts and Caps Lock span both; key-up and focus reset cannot leave Shift stuck',()=>{
 const m=createNotepatModifiers();
 const key=(key,code,down,more={})=>m.observe({key,code,name:`keyboard:${down?'down':'up'}:${key.toLowerCase()}`,...more});
 key('Shift','ShiftLeft',true);assert.equal(m.linger('c'),true);assert.equal(m.linger('+c'),false);
 key('Shift','ShiftRight',true);assert.equal(m.linger('+c'),true);
 key('Shift','ShiftLeft',false);assert.equal(m.linger('c'),false);assert.equal(m.linger('+c'),true);
 m.resetMomentary();assert.equal(m.linger('+c'),false);
 key('CapsLock','CapsLock',true,{caps:true});assert.equal(m.linger('c'),true);assert.equal(m.linger('+c'),true);
 key('CapsLock','CapsLock',true,{caps:false});assert.equal(m.linger('c'),false);
 key('C','KeyC',true,{caps:true,shift:false});assert.equal(m.linger('+c'),true,'reads Caps Lock already enabled before page focus');
 assert.equal(LINGER_SECONDS,4);
});
test('brackets latch independent drum sides once per press and reserve shifted braces too',()=>{
 const m=createNotepatModifiers();
 const event={name:'keyboard:down:[',key:'[',code:'BracketLeft'};
 assert.equal(m.observe(event).changed,true);assert.equal(m.drums('c'),true);assert.equal(m.drums('+c'),false);
 m.observe({...event,repeat:true});m.observe({...event,name:'keyboard:up:['});assert.equal(m.drums('c'),true);
 m.observe({name:'keyboard:down:}',key:'}',code:'BracketRight',shift:true});assert.equal(m.drums('+g#'),true);
 m.observe(event);assert.equal(m.drums('c'),false);
 assert.equal(m.observe({...event,meta:true}),null);assert.equal(m.drums('c'),false);
});
test('a long GM release can be shortened by retrigger or panic',async()=>{
 const times=[];
 const param=()=>({value:0,setValueAtTime(){},linearRampToValueAtTime(){},cancelAndHoldAtTime(){}});
 const ctx={currentTime:1,sampleRate:48000,destination:{},
   createBuffer:(_,n)=>({getChannelData:()=>new Float32Array(n)}),
   createGain:()=>({gain:param(),connect(){},disconnect(){}}),
   createBufferSource:()=>({playbackRate:param(),connect(){},disconnect(){},start(){},stop(t){times.push(t)}})};
 const bridge=createGmBridge({context:()=>ctx,destination:()=>ctx.destination,
   load:async()=>({prefetch:async()=>[],play:(note,opts)=>playLoadingTone(ctx,note,opts)})});
 await bridge.program(0);await bridge.note({id:1,program:0,note:60});
 bridge.stop(1,4);assert.equal(times[0],5.02);
 ctx.currentTime=1.1;bridge.panic();assert.ok(times[1]<1.3,'panic interrupts the fading voice');
});
