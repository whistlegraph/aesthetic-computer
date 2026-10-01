// Replay one real Walkieware edit from the desktop through the same relay the
// phone uses, and time every round: request → headers → first delta → end,
// with the provider's usage block, stop reason and tool names per round.
// Usage: node Tests/replay-edit.mjs [model] [prompt]
//   REPLAY_SOURCE=./fixtures/other.mjs  REASONING_JSON='{"effort":"none"}'  REASONING=on (provider default)
// Reports land in Tests/audio/ (gitignored evidence).
import {ACSession,USER_AGENT} from '../../../aesel/src/ac-session.mjs';
import {AcServer} from '../../../aesel/src/ac-server.mjs';
import {GENERATION_INSTRUCTIONS,DEFAULT_MODEL} from '../Resources/Web/generation-policy.mjs';
import {inferenceRequest} from '../Resources/Web/inference-input.mjs';
import {mkdtemp,writeFile,readFile} from 'node:fs/promises';
import {tmpdir} from 'node:os';
import {join} from 'node:path';

const model=process.argv[2]||DEFAULT_MODEL;
const prompt=process.argv[3]||'Someone is eating them';
const initial=await readFile(new URL(process.env.REPLAY_SOURCE||'./fixtures/basket-v2.mjs',import.meta.url),'utf8');
const session=new ACSession();const token=await session.token();if(!token)throw Error('Sign in first');
const cwd=await mkdtemp(join(tmpdir(),'ww-replay-'));const file=join(cwd,'piece.mjs');await writeFile(file,initial);

const t0=performance.now();const now=()=>Math.round(performance.now()-t0);
const rounds=[];let round=null;let painted=initial;let feedback=null;
const log=(...a)=>console.log(String(now()).padStart(6)+'ms',...a);
const server=new AcServer({cwd,piece:{file,checkpoint:async()=>{painted=await readFile(file,'utf8');feedback={rendered:true,logs:[],updatedAt:new Date().toISOString()};}},
  model,token:()=>token,preview:true,rounds:12,outputContinuations:4,jev:null,reasoning:process.env.REASONING_JSON?JSON.parse(process.env.REASONING_JSON):process.env.REASONING==='on'?null:{effort:'none'},thinking:process.env.THINKING_JSON?JSON.parse(process.env.THINKING_JSON):process.env.REASONING==='on'?null:{type:'disabled'},frameCapture:false,layeredEdits:true,developerInstructions:GENERATION_INSTRUCTIONS,
  fetch:async(url,options)=>{
    round={n:rounds.length+1,request:now(),bodyBytes:Buffer.byteLength(options.body),tools:[],deltaBytes:0};rounds.push(round);
    const body=JSON.parse(options.body);round.messages=body.messages.length;round.systemBytes=JSON.stringify(body.system).length;round.toolDefs=body.tools?.length;
    const response=await fetch(url,{...options,headers:{...options.headers,'User-Agent':USER_AGENT}});
    round.headers=now();log(`round ${round.n} headers after ${round.headers-round.request}ms · body ${round.bodyBytes}B · ${round.messages} msgs`);
    return response;
  }});
server.runtimeFeedback=()=>feedback;
server.on('notification',({method,params})=>{
  if(!round)return;
  if(method==='item/modelCode/delta'){round.firstDelta??=now();round.deltaBytes+=params.delta.length;if(!round.tools.includes(params.tool||'write_piece'))round.tools.push(params.tool||'write_piece');}
  if(method==='item/agentMessage/delta'){round.firstDelta??=now();round.text=(round.text||'')+params.delta;}
  if(method==='item/reasoning/delta'){round.firstReasoning??=now();round.reasoningChars=(round.reasoningChars||0)+params.delta.length;}
  if(method==='item/started')round.tools.push(params.item?.tool||params.item?.type);
  if(method==='item/completed'){round.completed=(round.completed||[]);round.completed.push(params.item?.status||params.item?.path);log(`  tool done: ${params.item?.status||params.item?.summary||params.item?.path}`);}
  if(method==='turn/usage'){round.usage=params.usage;round.end=now();log(`  round ${round.n} end · ${round.end-round.request}ms · usage ${JSON.stringify(params.usage)}`);}
  if(method==='turn/progress'&&params.phase==='continuing')log('  CONTINUATION',params.continuation);
  if(method==='turn/progress'&&['generating','composing'].includes(params.phase)){round.firstByte??=now();round.phases=(round.phases||'')+(round.phases?.endsWith(params.phase[0])?'':params.phase[0]);}
  if(method==='turn/completed'){log('turn completed',params.turn.status,params.turn.error?.message||'');}
  if(method==='model/reported')round.reported=params.reported;
});
const deadline=setTimeout(()=>{log('DEADLINE interrupt');server.interrupt();},240000);
try{await server.startTurn(inferenceRequest(prompt));}catch(e){log('error',e.message);}finally{clearTimeout(deadline);server.close();}
const final=await readFile(file,'utf8');
console.log('\n=== SUMMARY',model,JSON.stringify(prompt),'reasoning',process.env.REASONING_JSON||(process.env.REASONING==='on'?'default':'none'));
console.log('total',now(),'ms · rounds',rounds.length,'· source',initial.length,'→',final.length,'chars · changed',final!==initial);
for(const r of rounds)console.log(JSON.stringify({n:r.n,ttHeaders:r.headers-r.request,ttFirstDelta:r.firstDelta?r.firstDelta-r.request:null,ttFirstByte:r.firstByte?r.firstByte-r.request:null,ttFirstReasoning:r.firstReasoning?r.firstReasoning-r.request:null,reasoningChars:r.reasoningChars||0,phases:r.phases,duration:(r.end||now())-r.request,tools:r.tools,deltaBytes:r.deltaBytes,usage:r.usage,text:(r.text||'').slice(0,120),bodyBytes:r.bodyBytes,msgs:r.messages}));
await writeFile(new URL(`./audio/replay-${model.replace(/\W/g,'_')}-${Date.now()}.json`,import.meta.url),JSON.stringify({model,prompt,rounds,total:now(),final},null,1));
