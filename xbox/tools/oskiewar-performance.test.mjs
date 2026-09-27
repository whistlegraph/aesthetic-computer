import test from 'node:test';
import assert from 'node:assert/strict';
import {menuBandCue,currentMenuBand,spatialPerformance,selectNativeStatus} from './oskiewar-performance.mjs';
test('Menu Band follows the shared cue epoch, longest part, notes and rests',()=>{
  const cue=menuBandCue({action:'play',at:100,info:{title:'Sophia',bpm:'120',startEpoch:'105',notes:'60:1,r:1,64:2',notes2:'48:6'}},100);
  assert.equal(cue.duration,3);
  assert.equal(currentMenuBand(cue,104).playing,false);
  assert.deepEqual(currentMenuBand(cue,105.25).notes,[60,48]);
  assert.deepEqual(currentMenuBand(cue,105.75).notes,[48]);
  assert.equal(currentMenuBand(cue,109),null);
  assert.equal(menuBandCue({action:'stop'}),null);
});
test('native transport uses score time, and ready state retains title without pretending to play',()=>{
  const score={name:'Trio',events:[{t:1,dur:1,hz:440}]};
  let p=spatialPerformance({schema:'trio-native-status-v1',phase:'ready',duration:50,scoreTime:null},score);
  assert.equal(p.title,'Trio');assert.equal(p.playing,false);assert.equal(p.elapsed,0);
  assert.equal(p.source,'Macneopolitan Trio');
  p=spatialPerformance({phase:'playing',scoreName:'Spatial',scoreDuration:60,scoreTime:1.5,glow:.4},score);
  assert.equal(p.title,'Spatial');assert.deepEqual(p.notes,[69]);assert.equal(p.intensity,.4);
  assert.equal(p.elapsed,1.5);assert.equal(p.source,'Notepat Spatial');
});

test('stale playing file cannot hide an advancing alternate transport',()=>{
  const clocks=[];
  const old={phase:'playing',audioTime:1,scoreTime:1};
  const live={phase:'playing',audioTime:2,scoreTime:2};
  selectNativeStatus([old,live],clocks,0);
  assert.equal(selectNativeStatus([old,{...live,audioTime:5}],clocks,3000).audioTime,5);
});
test('mismatched prepared score never supplies the running title or notes',()=>{
  const p=spatialPerformance({phase:'playing',arrangementHash:'old',duration:10,scoreTime:1},
    {hash:'next',name:'Next composition',events:[{t:0,dur:10,hz:440}]});
  assert.equal(p.title,'Spatial composition');assert.deepEqual(p.notes,[]);
});

test('a stopped feed retains the last composition instead of an old trio file',()=>{
  const clocks=[];
  const statuses=[{phase:'ready',audioTime:10,scoreTime:null},{phase:'playing',audioTime:20,scoreTime:278,scoreName:'Current work'}];
  selectNativeStatus(statuses,clocks,0);
  const value=selectNativeStatus(statuses,clocks,3000);
  assert.equal(value.scoreName,'Current work');assert.equal(value.phase,'stale');
});

test('Trio lyrics follow the audio clock, skip rests, and refuse a different arrangement',async()=>{
  const {lyricPlan}=await import('./oskiewar-performance.mjs');
  const plan=lyricPlan({arrangementHash:'same',title:'Our trio',payloads:[{member:'neo',info:{bpm:60,lyrics:'hel-lo world / sing on',notes:'r:2,60:1,62:1,64:1,r:2,65:1,67:1'}}]});
  const status={schema:'trio-native-status-v1',arrangementHash:'same',phase:'playing',duration:10,audioTime:103.2,origin:100};
  const p=spatialPerformance(status,null,plan);
  assert.equal(p.playing,true);assert.ok(Math.abs(p.elapsed-3.2)<1e-8);
  assert.equal(p.lyrics[0].words[0].text,'hello');
  assert.deepEqual(p.lyrics[0].words[0],{text:'hello',start:2,end:4});
  assert.equal(spatialPerformance({...status,phase:'ready',origin:null},null,plan).lyrics.length,0);
  assert.equal(spatialPerformance({...status,phase:'stale'},null,plan).lyrics.length,0);
  assert.equal(spatialPerformance({...status,arrangementHash:'other'},null,plan).lyrics.length,0);
  assert.equal(spatialPerformance({...status,audioTime:111},null,plan).lyrics.length,0);
});
test('late Menu Band notification keeps the shared start and sung-word timing',()=>{
  const cue=menuBandCue({action:'play',info:{bpm:60,startEpoch:100,lyrics:'one two',notes:'60:1,r:1,64:1'}},102);
  assert.equal(cue.start,100);
  assert.equal(currentMenuBand(cue,102.3).lyrics[0].words[1].start,2);
});
test('mismatched lyric syllable count is withheld',async()=>{
  const {lyricPart}=await import('./oskiewar-performance.mjs');
  assert.equal(lyricPart({bpm:60,notes:'60:1',lyrics:'too ma-ny'}).lines.length,0);
});
