#!/usr/bin/env node
// Prepare a temporary, bounded console benchmark. Never deploys by itself.
import {readFileSync, writeFileSync} from 'node:fs';
import {createHash} from 'node:crypto';
import {resolve} from 'node:path';
const args=process.argv.slice(2),opt=(key,fallback)=>args.includes(key)?args[args.indexOf(key)+1]:fallback;
const input=resolve(opt('--source','xbox/live/oskiewar.js'));
const output=resolve(opt('--out','/tmp/oskiewar-native-benchmark.js'));
const label=opt('--label','candidate'),seed=7341,requireSavedModel=args.includes('--require-saved-model');
const source=readFileSync(input,'utf8'),hash=createHash('sha256').update(source).digest('hex');
if(!source.includes('let freeskateLook = (Date.now()>>>0)||1;'))throw Error('Update benchmark appearance seed anchor');
const pre=`\nlet nativeBenchSeed=${seed};Math.random=()=>{nativeBenchSeed=(Math.imul(nativeBenchSeed,1664525)+1013904223)>>>0;return nativeBenchSeed/4294967296;};\n`;
const post=`\n// At most ${requireSavedModel?'30s waiting for the saved model, then ':''}56s: two cases, each with 14s warmup +14s sampling.
// Deliberate controller input aborts immediately and returns that input to the game.
const nativeBenchPad=gamepad,nativeBenchSim=sim,nativeBenchPaint=paint;
let nativeBenchStart=0,nativeBenchWaiting=0,nativeBenchFrames=0,nativeBenchDone=false,nativeBenchCase=-1;
let nativeBenchPads=null;
const nativeBenchInput=pad=>pad&&pad.connected!==false&&(
 pad.down?.length>0||Math.hypot(Number(pad.leftX)||0,Number(pad.leftY)||0)>.25||
 Math.hypot(Number(pad.rightX)||0,Number(pad.rightY)||0)>.25||
 (Number(pad.leftTrigger)||0)>.15||(Number(pad.rightTrigger)||0)>.15);
function nativeBenchAbort(reason){
 if(nativeBenchDone)return;
 nativeBenchDone=true;debugHitboxes=false;telemetry('BENCH_ABORT',reason);
}
gamepad=function(index=0){
 const pad=nativeBenchPads?.[index]??nativeBenchPad(index);
 if(!nativeBenchDone&&nativeBenchInput(pad))nativeBenchAbort('controller input');
 return nativeBenchDone?pad:{connected:index===0,down:[],leftX:0,leftY:0,rightX:0,rightY:0,leftTrigger:0,rightTrigger:0};
};
sim=function(){
 if(nativeBenchDone)return nativeBenchSim();
 nativeBenchPads=[nativeBenchPad(0),nativeBenchPad(1)];
 try{
 if(nativeBenchPads.some(nativeBenchInput)){nativeBenchAbort('controller input');return nativeBenchSim();}
 if(${requireSavedModel}&&!nativeBenchStart&&!globalThis.__oskiewarLocalPractice){
  if(!nativeBenchWaiting)nativeBenchWaiting=Date.now();
  if(Date.now()-nativeBenchWaiting>=30000)nativeBenchAbort('saved model unavailable');
  return nativeBenchSim();
 }
 if(!nativeBenchStart)nativeBenchStart=Date.now();
 const elapsed=Date.now()-nativeBenchStart;
 if(elapsed>=56000){if(!nativeBenchDone){nativeBenchDone=true;debugHitboxes=false;telemetry('BENCH_END',${JSON.stringify(label)});}return nativeBenchSim();}
 const phase=Math.floor(elapsed/28000);
 if(phase!==nativeBenchCase){nativeBenchCase=phase;debugHitboxes=phase===0;telemetry('BENCH_BEGIN',JSON.stringify({label:${JSON.stringify(label)},sourceHash:${JSON.stringify(hash)},seed:${seed},debug:debugHitboxes,savedModel:!!globalThis.__oskiewarLocalPractice,course:freeskateCourseNow()}));}
 nativeBenchSim();
 }finally{nativeBenchPads=null;}
};
paint=function(){nativeBenchPaint();
 if(nativeBenchDone||!nativeBenchStart||++nativeBenchFrames%120)return;
 const elapsed=Date.now()-nativeBenchStart;if(elapsed%28000<14000)return;
 const r=runtime();telemetry('BENCH_SAMPLE',JSON.stringify({label:${JSON.stringify(label)},debug:debugHitboxes,elapsedMs:elapsed,frameMs:r.frameMs,renderCpuMs:r.renderCpuMs,presentMs:r.presentMs,hz:r.refreshHz,x:players[0].x,z:players[0].z,triangles:'see AC_NATIVE_FRAME'}));
};\n`;
writeFileSync(output,'// @bundle-qr\n'+pre+source.replace('let freeskateLook = (Date.now()>>>0)||1;',`let freeskateLook = ${seed};`)+post);
console.log(JSON.stringify({output,label,sourceHash:hash,durationSeconds:56,waitTimeoutSeconds:requireSavedModel?30:0,maxDurationSeconds:requireSavedModel?86:56,controllerInputAborts:true,requireSavedModel,seed}));
