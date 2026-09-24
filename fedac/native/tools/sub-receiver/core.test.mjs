import test from 'node:test';import assert from 'node:assert/strict';import {readFile} from 'node:fs/promises';import {makeSubScore,isLive,windowEvents} from './core.mjs';
const original=JSON.parse(await readFile(new URL('../../scores/notespatial-native.nsscore',import.meta.url)));const sub=makeSubScore(original);
test('sub keeps bass/kick timing, duration and score length while lowering pitch',()=>{assert.equal(sub.dur,original.dur);assert.ok(sub.events.length>0);for(const e of sub.events){const lane=original.lanes.find(l=>l.name===e.lane);const index=Number(e.id.split('-')[1]);assert.equal(e.t,lane.events[index].t);assert.equal(e.dur,lane.events[index].dur);assert.equal(e.hz,lane.events[index].hz/2);assert.ok(e.g<=.65);}});
test('new bass partials and kick clicks do not reject the sub score or become low pitched clicks',()=>{
 const e={t:0,dur:.2,g:.4,wave:'sine'};
 const s={dur:1,gain:.36,lanes:[{name:'bass',events:[{...e,hz:110},{...e,hz:880}]},{name:'kick',events:[{...e,hz:2500,wave:'noise'},{...e,hz:78}]}]};
 assert.deepEqual(makeSubScore(s).events.map(e=>[e.id,e.hz]),[['bass-0',55],['kick-1',39]]);
 s.lanes[0].events.push({...e,hz:NaN});assert.throws(()=>makeSubScore(s),/Invalid sub score/);
});
test('stale, stopped, error, and finished sources cannot sound',()=>{const s={phase:'playing',scoreTime:120,scoreDuration:774};assert.equal(isLive(s,100),true);for(const x of [{...s,phase:'ready'},{...s,error:'bad'},{...s,scoreTime:775}])assert.equal(isLive(x,100),false);assert.equal(isLive(s,2000),false);});
test('join window never replays past notes or includes the next window boundary',()=>{const events=[{t:1},{t:2},{t:3}];assert.deepEqual(windowEvents(events,2,3),[{t:2}]);});
