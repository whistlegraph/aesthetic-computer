import http from 'node:http';
import {readFile} from 'node:fs/promises';
import {createHash} from 'node:crypto';
import {networkInterfaces} from 'node:os';
import {performance} from 'node:perf_hooks';
import {makeSubScore,isLive} from './core.mjs';
let config={};try{config=JSON.parse(await readFile(new URL('./config.json',import.meta.url),'utf8'));}catch{}
const port=Number(process.env.PORT||config.port||8788),bind=process.env.SUB_BIND||config.bind||'0.0.0.0';
const sources=process.env.SUB_SOURCE?[process.env.SUB_SOURCE]:(config.sources||['192.168.1.241','192.168.1.242','192.168.1.238','192.168.1.236','192.168.1.237:8080','192.168.1.239']);
if(!sources.length||sources.some(s=>!/^[-\w.]+(?::\d+)?$/.test(s)))throw Error('Invalid source host');
let source=sources[0];
let raw;
if(process.env.SUB_SCORE) raw=await readFile(process.env.SUB_SCORE);
else {try{raw=await readFile(new URL('./score.nsscore',import.meta.url));}catch{raw=await readFile(new URL('../../scores/notespatial-native.nsscore',import.meta.url));}}
let score={...makeSubScore(JSON.parse(raw)),hash:createHash('sha256').update(raw).digest('hex')};
let trio=null;
let sample=null,changedAt=0,lastClock=null,receivedAt=0,sourceRtt=0;
const receivers=new Map();
async function readSource(host){
 const start=performance.now(),r=await fetch(`http://${host}/pieces/spatial-rehearsal-status.json`,{signal:AbortSignal.timeout(700)});
 if(!r.ok)throw Error('source');const s=await r.json();
 if(!Number.isFinite(s.audioTime)||s.scoreDuration!==score.dur)throw Error('Wrong score');
 return {s,host,rtt:performance.now()-start};
}
async function poll(){
 try {
  let result;try{result=await readSource(source);if(performance.now()-changedAt>2000&&result.s.audioTime===lastClock)throw Error('Stale');}
  catch{result=await Promise.any(sources.filter(h=>h!==source).map(readSource));}
  const {s,host,rtt}=result;sourceRtt=rtt;
  if(s.audioTime!==lastClock||host!==source){changedAt=performance.now();lastClock=s.audioTime;}
  source=host;sample=s;receivedAt=performance.now();
 }catch{}
 setTimeout(poll,150);
}poll();
async function advertise(){
 const clients=[...receivers.values()].filter(r=>Date.now()-r.seen<4000);
 const text=JSON.stringify({role:'SUB',at:Date.now(),name:config.name||'SUB',source,host:process.env.COMPUTERNAME||'controller',addresses:Object.values(networkInterfaces()).flat().filter(n=>n.family==='IPv4'&&!n.internal).map(n=>n.address),receivers:clients});
 if(config.advertise)await Promise.allSettled(sources.map(h=>fetch(`http://${h}/pieces/sub-receiver-status.json`,{method:'PUT',body:text,signal:AbortSignal.timeout(800)})));
 setTimeout(advertise,1500);
}advertise();
function state(){
 if(trio){
  if(['prepared','countdown','playing'].includes(trio.phase)&&performance.now()-trio.heartbeat>3000){trio.phase='error';trio.startMono=null;}
  const t=trio.startMono==null?null:(performance.now()-trio.startMono)/1000;
  if(trio.phase==='countdown'&&t>=0)trio.phase='playing';
  if(trio.phase==='playing'&&t>=score.dur)trio.phase='finished';
  return {source:'Trio conductor',live:['countdown','playing'].includes(trio.phase),phase:trio.phase,scoreTime:t,runId:trio.runId,scoreHash:score.hash,duration:score.dur,serverEpoch:Date.now()/1000,sourceRttMs:0};
 }
 const now=performance.now(),age=now-changedAt;return {source,live:isLive(sample,age),phase:age>2000?'offline':sample?.phase||'waiting',scoreTime:sample?.scoreTime==null?null:sample.scoreTime+(now-receivedAt+sourceRtt/2)/1000,runId:sample?.runId,scoreHash:score.hash,connectivity:sample?.connectivity,sourceAgeMs:Math.round(age),sourceRttMs:Math.round(sourceRtt),duration:score.dur};}
async function body(req){let s='';for await(const c of req){s+=c;if(s.length>1048576)throw Error('Too large');}return JSON.parse(s);}
const types={'.html':'text/html','.mjs':'text/javascript','.css':'text/css'};
http.createServer(async(req,res)=>{
 const path=new URL(req.url,'http://localhost').pathname;
 const send=(code,data,type='application/json')=>{res.writeHead(code,{'Content-Type':type,'Cache-Control':'no-store','X-Content-Type-Options':'nosniff'});res.end(type==='application/json'?JSON.stringify(data):data);};
 try {
  if(req.method==='GET'&&path==='/SUB-Display-Update.zip'&&process.env.SUB_UPDATE)return send(200,await readFile(process.env.SUB_UPDATE),'application/zip');
  if(req.method==='GET'&&path==='/Update-SUB.ps1'&&process.env.SUB_UPDATER)return send(200,await readFile(process.env.SUB_UPDATER),'text/plain');
  if(req.method==='GET'&&path==='/SUB-Windows.zip'&&process.env.SUB_PACKAGE){res.setHeader('Content-Disposition','attachment; filename="SUB-Windows.zip"');return send(200,await readFile(process.env.SUB_PACKAGE),'application/zip');}
  if(req.method==='GET'&&path==='/api/clock')return send(200,{epoch:Date.now()/1000});
  if(req.method==='POST'&&path.startsWith('/api/trio/')){
   if(!['127.0.0.1','::1','::ffff:127.0.0.1'].includes(req.socket.remoteAddress))return send(403,{error:'Local conductor only'});
   const b=await body(req);
   if(path==='/api/trio/load'){
    if(!b.hash||!Number.isFinite(b.dur)||b.dur<=0||b.dur>600||!Array.isArray(b.events)||!b.events.length||b.events.some(e=>![e.t,e.dur,e.hz,e.g].every(Number.isFinite)||e.t<0||e.dur<=0||e.t+e.dur>b.dur+.1||e.hz<20||e.hz>200||e.g<0||e.g>.25))return send(400,{error:'Invalid Trio bass score'});
    if(trio&&['prepared','countdown','playing'].includes(trio.phase))return send(409,{error:'Stop before loading another score'});
    score=b;raw=Buffer.from(JSON.stringify({name:b.name,dur:b.dur,lanes:[],geometry:'ring',center:5}));trio={phase:'ready',startMono:null,runId:null};return send(200,{loaded:true,hash:score.hash,duration:score.dur});
   }
   if(!trio)return send(409,{error:'Load Trio score first'});
   if(path==='/api/trio/keepalive'&&b.runId===trio.runId){trio.heartbeat=performance.now();return send(200,{alive:true});}
   if(path==='/api/trio/stop'){trio.phase='finished';trio.startMono=null;return send(200,{stopped:true});}
   if(path==='/api/trio/prepare'){
    if(b.hash!==score.hash||!b.runId||!Number.isFinite(b.startEpoch)||b.startEpoch<Date.now()/1000+2)return send(400,{error:'Invalid future cue'});
    trio={phase:'prepared',runId:b.runId,heartbeat:performance.now(),startMono:performance.now()+(b.startEpoch-Date.now()/1000)*1000};return send(200,{prepared:true,runId:trio.runId});
   }
   if(path==='/api/trio/play'&&trio.phase==='prepared'&&trio.runId===b.runId&&trio.startMono>performance.now()+500){trio.phase='countdown';return send(200,{armed:true,runId:trio.runId});}
   return send(409,{error:'Invalid Trio transition'});
  }
  if(req.method==='GET'&&path==='/api/state')return send(200,state());
  if(req.method==='GET'&&path==='/api/world')return send(200,JSON.parse(raw));
  if(req.method==='GET'&&path==='/api/score')return send(200,score);
  if(req.method==='GET'&&path==='/api/receivers')return send(200,[...receivers.values()].map(r=>({...r,online:Date.now()-r.seen<4000})));
  if(req.method==='POST'&&path==='/api/heartbeat'){
   if(req.headers.origin && req.headers.origin!==`http://${req.headers.host}`)return send(403,{error:'Origin'});
   const b=await body(req);if(typeof b.id!=='string'||!/^[-\w]{1,80}$/.test(b.id))return send(400,{error:'id'});
   receivers.set(b.id,{id:b.id,role:'SUB',ip:req.socket.remoteAddress,seen:Date.now(),armed:b.armed===true,audioState:String(b.audioState||'').slice(0,30),level:Math.max(0,Math.min(1,Number(b.level)||0)),route:['left','right','both'].includes(b.route)?b.route:'left',phase:String(b.phase||'').slice(0,30),scoreHash:String(b.scoreHash||'').slice(0,80),duration:Number(b.duration)||0,fullscreen:b.fullscreen===true});
   return send(200,{ok:true});
  }
  const file=path==='/'?'index.html':decodeURIComponent(path.slice(1));
  if(req.method==='GET'&&['index.html','app.mjs','core.mjs','style.css','display.mjs','font.mjs','spatial.mjs','Open SUB.url'].includes(file)){
   if(file==='Open SUB.url'){res.setHeader('Content-Disposition','attachment; filename="Open SUB.url"');return send(200,`[InternetShortcut]\r\nURL=http://${req.headers.host}/\r\n`,'application/octet-stream');}
   return send(200,await readFile(new URL(file,import.meta.url)),types[file.slice(file.lastIndexOf('.'))]);
  }
  send(404,{error:'Not found'});
 }catch(e){send(400,{error:'Invalid request'});}
}).listen(port,bind,()=>{console.log(`Open http://127.0.0.1:${port}/ on this PC`);for(const net of Object.values(networkInterfaces()).flat())if(net.family==='IPv4'&&!net.internal)console.log(`SUB receiver: http://${net.address}:${port}/`);console.log('Following '+source);});
