#!/usr/bin/env node
// Prepare a temporary, bounded console benchmark. Never deploys by itself.
import {readFileSync, writeFileSync} from 'node:fs';
import {createHash} from 'node:crypto';
import {resolve} from 'node:path';
const args=process.argv.slice(2),opt=(key,fallback)=>args.includes(key)?args[args.indexOf(key)+1]:fallback;
const input=resolve(opt('--source','xbox/live/oskiewar.js'));
const output=resolve(opt('--out','/tmp/oskiewar-native-benchmark.js'));
const label=opt('--label','candidate'),seed=7341;
const source=readFileSync(input,'utf8'),hash=createHash('sha256').update(source).digest('hex');
if(!source.includes('let freeskateLook = (Date.now()>>>0)||1;'))throw Error('Update benchmark appearance seed anchor');
const pre=`\nlet nativeBenchSeed=${seed};Math.random=()=>{nativeBenchSeed=(Math.imul(nativeBenchSeed,1664525)+1013904223)>>>0;return nativeBenchSeed/4294967296;};\n`;
const post=`\n// Controller input resumes after two 28-second cases (14s warmup +14s sample).
const nativeBenchPad=gamepad,nativeBenchSim=sim,nativeBenchPaint=paint;
let nativeBenchStart=0,nativeBenchFrames=0,nativeBenchDone=false,nativeBenchCase=-1;
gamepad=function(index){return nativeBenchDone?nativeBenchPad(index):{connected:index===0,down:[],leftX:0,leftY:0,rightX:0,rightY:0,leftTrigger:0,rightTrigger:0};};
sim=function(){
 if(!nativeBenchStart)nativeBenchStart=Date.now();
 const elapsed=Date.now()-nativeBenchStart;
 if(elapsed>=56000){if(!nativeBenchDone){nativeBenchDone=true;debugHitboxes=false;telemetry('BENCH_END',${JSON.stringify(label)});}return nativeBenchSim();}
 const phase=Math.floor(elapsed/28000);
 if(phase!==nativeBenchCase){nativeBenchCase=phase;debugHitboxes=phase===0;telemetry('BENCH_BEGIN',JSON.stringify({label:${JSON.stringify(label)},sourceHash:${JSON.stringify(hash)},seed:${seed},debug:debugHitboxes,course:freeskateCourseNow()}));}
 nativeBenchSim();
};
paint=function(){nativeBenchPaint();
 if(nativeBenchDone||++nativeBenchFrames%120)return;
 const elapsed=Date.now()-nativeBenchStart;if(elapsed%28000<14000)return;
 const r=runtime();telemetry('BENCH_SAMPLE',JSON.stringify({label:${JSON.stringify(label)},debug:debugHitboxes,elapsedMs:elapsed,frameMs:r.frameMs,renderCpuMs:r.renderCpuMs,presentMs:r.presentMs,hz:r.refreshHz,x:players[0].x,z:players[0].z,triangles:'see AC_NATIVE_FRAME'}));
};\n`;
writeFileSync(output,'// @bundle-qr\n'+pre+source.replace('let freeskateLook = (Date.now()>>>0)||1;',`let freeskateLook = ${seed};`)+post);
console.log(JSON.stringify({output,label,sourceHash:hash,durationSeconds:56,seed}));
