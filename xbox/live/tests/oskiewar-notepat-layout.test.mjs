import test from 'node:test';
import assert from 'node:assert/strict';
import {readFileSync} from 'node:fs';
import {createContext,runInContext} from 'node:vm';
import {pixelGlyph} from '../../../fedac/native/lib/oskiewar-host.mjs';
const source=readFileSync(new URL('../oskiewar.js',import.meta.url),'utf8');

test('Notepat score labels fit their bands and viewport using actual AC bitmap metrics',()=>{
 for(const width of [1024,1366,1920]) {
  const text=[],rects=[],noop=()=>{};
  const write=(label,x,y,size,...color)=>text.push({label,x,y,size,color});
  write.measureGlyph=(char,size)=>pixelGlyph(char,size,.5).advance/.5;
  const context=createContext({Math,Date,performance,structuredClone,
   runtime:()=>({monotonicUs:1000000,unixMs:1790000000000}),
   gamepad:()=>({connected:false,down:[]}),capabilities:()=>({platform:'ac-native'}),
   gameView:()=>({width,height:768}),telemetry:noop,gameSignal:noop,drum:noop,synth:noop,
   wipe:noop,box:noop,line:noop,triangle:noop,triangle3d:noop,write,systemWrite:write});
  runInContext(source+'\nboot();syncGameView();',context);
  context.captureRect=(x,y,w,h,color)=>rects.push({x,y,w,h,color});
  runInContext('screenRect=captureRect;',context);
  for(const elapsed of [0,90,390,770]) {
   text.length=0;rects.length=0;
   runInContext(`globalThis.__oskiewarStageState={performance:{active:true,playing:true,visual:'notepat-score-v1',look:{color:[100,150,220]},movement:'test',title:'Notepat Spatial',source:'the walk',elapsed:${elapsed},duration:NOTEPAT_TV_SCORE.duration},receivedAt:Date.now()};drawPerformanceStage();`,context);
   const bands=rects.filter(r=>['23,32,47','86,116,151'].includes(r.color.join(',')));
   assert.ok(bands.length>5);
   for(const band of bands) {
    assert.ok(band.y>=44,'section names clear the native battery badge');
    const labels=text.filter(t=>t.y>=band.y&&t.y<band.y+band.h&&t.x>=band.x&&t.x<band.x+band.w);
    for(const label of labels) {
     const measured=[...label.label].reduce((sum,c)=>sum+write.measureGlyph(c,label.size),0);
     assert.ok(label.x+measured<=band.x+band.w,`${label.label} crosses its section at width ${width}`);
    }
   }
   for(const tick of text.filter(t=>/^\+\d+s$/.test(t.label))) {
    const measured=[...tick.label].reduce((sum,c)=>sum+write.measureGlyph(c,tick.size),0);
    assert.ok(tick.x>=0&&tick.x+measured<=width*.97,`${tick.label} leaves the viewport`);
   }
   const footer=text.find(t=>t.label==='notepat spatial');
   const sub=text.filter(t=>t.label==='sub').at(-1);
   assert.ok(footer&&sub&&sub.y+pixelGlyph('s',sub.size,.5).height/.5< footer.y-12);
  }
 }
});
