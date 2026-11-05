import test from 'node:test';
import assert from 'node:assert/strict';
import {chordIntervals,createChordVoice,createNotepatKeyRoute} from '../public/aesthetic.computer/lib/notepat-chords.mjs';
import {createNotepatModifiers} from '../public/aesthetic.computer/lib/notepat-modifiers.mjs';
import {notepatPianoLayout,pianoReachOffsets,pianoNoteAt} from '../public/aesthetic.computer/lib/notepat-piano-layout.mjs';

test('Menu Band chord grammar and sided Command/Option preserve the other hand',()=>{
 const m=createNotepatModifiers();
 const event=(key,code,down,flags)=>({key,code,name:`keyboard:${down?'down':'up'}:${key.toLowerCase()}`,...flags});
 m.observe(event('Meta','MetaLeft',true,{meta:true}));
 assert.deepEqual(chordIntervals(m.chord('c')),[4,7]);assert.deepEqual(chordIntervals(m.chord('+c')),[]);
 m.observe(event('Alt','AltLeft',true,{meta:true,alt:true}));assert.deepEqual(chordIntervals(m.chord('c')),[2,7]);
 m.observe(event('Control','ControlLeft',true,{meta:true,alt:true,ctrl:true}));
 assert.deepEqual(chordIntervals(m.chord('c')),[5,7]);assert.deepEqual(chordIntervals(m.chord('+c')),[4,8]);
 m.observe(event('Meta','MetaLeft',false,{meta:false,alt:true,ctrl:true}));assert.deepEqual(chordIntervals(m.chord('c')),[3,6]);
 m.observe(event('Control','ControlLeft',false,{meta:false,alt:true,ctrl:false}));assert.deepEqual(chordIntervals(m.chord('c')),[3,7]);
 m.resetMomentary();assert.deepEqual(chordIntervals(m.chord('c')),[]);
 m.observe(event('C','KeyC',true,{meta:true}));assert.deepEqual(chordIntervals(m.chord('+c')),[4,7]);
 m.observe(event('Meta','MetaLeft',false,{meta:false}));assert.deepEqual(chordIntervals(m.chord('+c')),[],'sideless fallback clears on physical release');
});
test('live morph keeps root and common fifth, overlapping roots own their voices, release and panic clear every member',()=>{
 const starts=[],kills=[];
 const start=note=>{const id=starts.length;starts.push({id,note});return {kill:fade=>kills.push({id,fade})}};
 const first=createChordVoice({root:60,start,intervals:[4,7]});
 first.morph([3,7]);assert.deepEqual(starts.map(v=>v.note),[60,64,67,63]);assert.deepEqual(kills,[{id:1,fade:.02}]);
 first.morph([3,7]);assert.equal(starts.length,4);
 const second=createChordVoice({root:67,start,intervals:[4,7]});first.kill(4);
 assert.equal(kills.some(k=>k.id===4),false,'other chord root is independent');
 first.morph([4,8]);assert.equal(starts.length,7,'released chords cannot resurrect');
 first.kill(.02);second.kill(.02);assert.ok([0,2,3,4,5,6].every(id=>kills.some(k=>k.id===id&&k.fade===.02)));
});
test('physical musical routing handles Option symbols and releases after modifier/focus changes without claiming editors',()=>{
 const route=createNotepatKeyRoute(),events=[];let prevented=0;
 const e={code:'KeyC',key:'ç',altKey:true,preventDefault(){prevented++}};
 assert.equal(route(e,true,false,v=>events.push(v)),false);
 assert.equal(route(e,true,true,v=>events.push(v)),true);assert.equal(events[0].key,'c');assert.equal(events[0].alt,true);
 assert.equal(route({...e,key:'c',altKey:false},false,false,v=>events.push(v)),true);assert.equal(events[1].name,'keyboard:up:c');assert.equal(prevented,2);
 assert.equal(route({...e,code:'ArrowLeft'},true,true,v=>events.push(v)),false);
});
test('held edge notes reveal their real keys within a fixed viewport and return home on release',()=>{
 for(const screen of [{width:472,height:190},{width:187,height:338}]){
  const stacked=screen.width<=screen.height;
  for(const note of ['-a#','-b','++c','++c#','++g']){
   const offsets=pianoReachOffsets([note],stacked),layout=notepatPianoLayout(screen,{offsets});
   const key=layout.keys.find(k=>k.note===note);assert.ok(key,`${note} revealed`);
   assert.equal(pianoNoteAt(layout,key.x+key.w/2,key.y+key.h-1),note);
   assert.ok(layout.keys.every(k=>k.x>=k.clip.x&&k.x+k.w<=k.clip.x+k.clip.w));
   if(stacked)assert.equal(offsets[note.startsWith('-')?1:0],0,'other octave stays still');
  }
  const home=notepatPianoLayout(screen,{offsets:pianoReachOffsets([],stacked)});
  assert.equal(home.keys.length,24);assert.ok(!home.keys.some(k=>k.note==='++c'));
 }
});
