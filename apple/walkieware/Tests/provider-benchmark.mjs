import {musicalPrompt} from '../Resources/Web/musical-input.mjs';
import {instantPiece} from '../Resources/Web/instant-piece.mjs';
// Desktop relay comparison. This does not measure phone audio or painted frames.
import {ACSession,USER_AGENT} from '../../../aesel/src/ac-session.mjs';
import {AcServer} from '../../../aesel/src/ac-server.mjs';
import {GENERATION_INSTRUCTIONS} from '../Resources/Web/generation-policy.mjs';
import {mkdtemp,writeFile,readFile,mkdir} from 'node:fs/promises';
import {tmpdir} from 'node:os';
import {join} from 'node:path';
const session=new ACSession();
const token=await session.token();if(!token)throw Error('Sign in to AC on the desktop first.');
const models=process.argv.slice(2);
if(!models.length)models.push('deepseek/deepseek-v4.1-flash','qwen/qwen3.7-plus','z-ai/glm-5.3-flash');
let prompt='Make a pink circle.',initial='export function paint({wipe}) {wipe("black");}\n';
if(process.env.WALKIE_PROVIDER_INPUT){const receipt=JSON.parse(await readFile(process.env.WALKIE_PROVIDER_INPUT,'utf8'));const input=receipt.events.find(e=>e.event==='soundSubmitted').details;prompt=musicalPrompt(input);initial=instantPiece(input.transcript)||initial;}
const results=[];
const out=new URL('./audio/provider-'+Date.now()+'.json',import.meta.url);
console.log('Report: '+out.pathname);
for(const model of models){
 const cwd=await mkdtemp(join(tmpdir(),'walkieware-provider-'));
 const file=join(cwd,'piece.mjs');await writeFile(file,initial);
 const times={};const mark=name=>{times[name]??=performance.now();};
 let error=null;
 const server=new AcServer({cwd,piece:{file,checkpoint:async()=>{if((await readFile(file,'utf8')).trim()!==initial.trim())mark('checkpoint');}},model,token:()=>token,preview:true,rounds:6,jev:null,frameCapture:false,layeredEdits:true,developerInstructions:GENERATION_INSTRUCTIONS,
  fetch:async(url,options)=>{mark('request');const response=await fetch(url,{...options,headers:{...options.headers,'User-Agent':USER_AGENT}});mark('headers');return response;}});
 server.on('notification',({method,params})=>{
   if(method==='item/modelCode/delta'){mark('output');mark('code');}
   if(method==='item/agentMessage/delta')mark('output');
   if(method==='turn/completed'&&params.turn.error)error=params.turn.error.message;
 });
 const deadline=setTimeout(()=>server.interrupt(),45000);
 try {await server.startTurn(prompt);}finally{clearTimeout(deadline);server.close();}
 const ms=key=>times[key]===undefined?null:Math.round(times[key]-times.request);
 const source=await readFile(file,'utf8');
 const row={model,headersMs:ms('headers'),firstOutputMs:ms('output'),firstCodeMs:ms('code'),firstCheckpointMs:ms('checkpoint'),sourceChanged:source!==initial,hasPaint:/export\s+function\s+paint/.test(source),error};
 results.push(row);console.log(JSON.stringify(row));
 await writeFile(out,JSON.stringify({scope:'Desktop → same AC relay. First validated checkpoint, no rendered-frame timing. Six-round cap, 45-second deadline.',workload:process.env.WALKIE_PROVIDER_INPUT?'mixed audio with local starter':'simple text',results},null,2)+'\n');
}
