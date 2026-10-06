#!/usr/bin/env node
// Explicitly opt-in paid inference. Only synthetic prompts; reports omit text and credentials.
import {mkdtemp, mkdir, writeFile, rm, readFile} from 'node:fs/promises';
import {tmpdir,platform,arch,loadavg} from 'node:os';
import {join} from 'node:path';
import {createHash} from 'node:crypto';
import {execFileSync} from 'node:child_process';
import {parseArgs} from 'node:util';
import {performance} from 'node:perf_hooks';
import {AcServer} from '../src/ac-server.mjs';
import {AppServer} from '../src/app-server.mjs';
import {ACSession} from '../src/ac-session.mjs';
import {tasks} from './speed-tasks.mjs';

const {values}=parseArgs({options:{live:{type:'boolean'},runs:{type:'string',default:'3'},out:{type:'string'},timeout:{type:'string',default:'60'},targets:{type:'string',default:'ac,codex'},task:{type:'string',default:'ping'},'ac-model':{type:'string',default:''},'codex-model':{type:'string',default:''},'ac-reasoning':{type:'string'}}});
if(!values.live)throw Error('Pass --live to spend provider allowance on synthetic latency requests.');
const runs=Number(values.runs),timeout=Number(values.timeout)*1000,targets=values.targets.split(',');
if(!Number.isInteger(runs)||runs<1||runs>10||!Number.isFinite(timeout)||timeout<1000||timeout>180000||targets.some(t=>!['ac','codex'].includes(t)))throw Error('Invalid benchmark options');
if(values['ac-reasoning'] && !['none','minimal','low','medium','high'].includes(values['ac-reasoning']))throw Error('Invalid reasoning effort');
if(!Object.hasOwn(tasks,values.task))throw Error('Task must be ping, sequence, or coding');
const prompt='This is a response latency test. Do not use tools, read files, or change anything. '+tasks[values.task].prompt;
const instruction='This session benchmarks response latency with a synthetic prompt. Answer directly, without tools, file changes, planning, or publishing.';
const account=new ACSession();
const report={schema:1,task:values.task,measurement:'Real Aesel provider engines; fresh isolated working directory and thread per sample; existing account authentication; defaults unless overrides are recorded. Engine events, not screen pixels. Models and system prompts can differ.',startedAt:new Date().toISOString(),runs,samples:[]};
report.environment={platform:platform(),arch:arch(),node:process.version,loadAverage:loadavg(),...(targets.includes('codex')?{codex:execFileSync('codex',['--version'],{encoding:'utf8',timeout:10000}).trim()}:{})};
report.sourceSha256=Object.fromEntries(await Promise.all(['ac-server.mjs','app-server.mjs','provider-defaults.mjs','open-models.mjs'].map(async file=>[file,createHash('sha256').update(await readFile(new URL('../src/'+file,import.meta.url))).digest('hex')])));
const percentile=(a,p)=>a.length?[...a].sort((a,b)=>a-b)[Math.max(0,Math.ceil(a.length*p)-1)]:null;
async function measure(target){
 const cwd=await mkdtemp(join(tmpdir(),'aesel-provider-speed-'));
 const sample={target,status:'failed',requestedModel:values[target+'-model'],reasoning:target==='ac'?values['ac-reasoning']??'default':'configured default'};
 let engine,timer,first=null,text='',usage=null,started;
 try{
  engine=target==='ac'?new AcServer({cwd,workspace:true,model:values['ac-model'],token:()=>account.token(),developerInstructions:instruction,jev:null,rounds:1,preview:false,...(values['ac-reasoning']?{reasoning:{effort:values['ac-reasoning']}}:{})}):new AppServer({cwd,model:values['codex-model'],developerInstructions:instruction});
  let finish;
  const completed=new Promise(resolve=>{finish=resolve;});
  engine.on('request',r=>engine.reject(r.id,-32000,'Tools are disabled for this benchmark'));
  engine.on('notification',({method,params={}})=>{
   if(method==='model/reported')sample.reportedModel=params.reported;
   if(method==='item/agentMessage/delta'){if(first===null)first=performance.now();text+=params.delta??'';}
   if(method==='turn/usage'||method==='usage/updated'||method==='thread/tokenUsage/updated')usage=params.usage??params.tokenUsage;
   if(method==='turn/completed')finish(params.turn);
  });
  engine.on('error',()=>finish({status:'failed',error:{message:'Engine error'}}));
  engine.on('exit',()=>finish({status:'failed',error:{message:'Engine exited'}}));
  const begin=performance.now();
  const expired=new Promise((_,reject)=>{timer=setTimeout(()=>reject(Error('Sample timed out')),timeout);});
  await Promise.race([(async()=>{
   const connection=await engine.connect();sample.connectMs=performance.now()-begin;
   sample.configuredModel=connection?.model??engine.model;
   if(target==='codex'&&connection?.reasoningEffort)sample.reasoning=connection.reasoningEffort;
   started=performance.now();
   const start=engine.startTurn(prompt);
   await start;
   const turn=await completed;
   sample.totalMs=performance.now()-started;
   sample.firstTextMs=first===null?null:first-started;
   sample.outputCharacters=text.length;sample.matchesExpected=tasks[values.task].check(text);
   if(turn?.status!=='completed')throw Error(turn?.error?.message||'Turn failed');
   sample.status=sample.matchesExpected?'ok':'unexpected-output';
   if(usage)sample.usage=usage;
  })(),expired]);
 }catch(error){sample.error=String(error.message).slice(0,300);}
 finally{clearTimeout(timer);engine?.close();await rm(cwd,{recursive:true,force:true});}
 return sample;
}
for(let run=0;run<runs;run++)for(const target of run%2?[...targets].reverse():targets){const sample={run:run+1,...await measure(target)};report.samples.push(sample);console.log(JSON.stringify(sample));}
report.summary=Object.fromEntries(targets.map(target=>{
 const samples=report.samples.filter(s=>s.target===target),ok=samples.filter(s=>s.status==='ok');
 return [target,{passed:ok.length,attempted:samples.length,...Object.fromEntries(['connectMs','firstTextMs','totalMs'].map(metric=>{const a=ok.map(s=>s[metric]).filter(Number.isFinite);return [metric,{p50:percentile(a,.5),p95:percentile(a,.95)}];}))}];
}));
if(values.out){await mkdir(join(values.out,'..'),{recursive:true});await writeFile(values.out,JSON.stringify(report,null,2)+'\n');}
console.log(JSON.stringify(report.summary,null,2));
process.exitCode=report.samples.some(s=>s.status!=='ok')?1:0;
