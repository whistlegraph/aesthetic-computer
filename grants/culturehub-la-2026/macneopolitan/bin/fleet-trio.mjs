#!/usr/bin/env node
// Preparation/readiness only. No command in this utility starts playback.
import {readFileSync,writeFileSync,mkdirSync,existsSync} from 'node:fs';
import {resolve,dirname} from 'node:path';
import {fileURLToPath} from 'node:url';
import {randomUUID} from 'node:crypto';
import {spawn} from 'node:child_process';
import {buildPlan,canonical,digest,members,noteName,readinessProblems} from './trio-fleet-plan.mjs';
const root=resolve(dirname(fileURLToPath(import.meta.url)),'..');
const args=process.argv.slice(2),command=args.shift();
if(!['plan','prepare','check'].includes(command))throw Error('Use fleet-trio.mjs plan|prepare|check [--out=DIR]. Playback remains held.');
const options=Object.fromEntries(args.map(a=>{if(!a.startsWith('--')||!a.includes('='))throw Error(`Invalid option ${a}`);const at=a.indexOf('=');return [a.slice(2,at),a.slice(at+1)];}));
for(const key of Object.keys(options))if(!['out','score','receipts'].includes(key))throw Error(`Unknown option ${key}`);
const out=resolve(options.out??'/Users/jas/Shelf/culturehub-trio');mkdirSync(out,{recursive:true});
const score=JSON.parse(readFileSync(resolve(options.score??resolve(root,'scores/trio-chorus-doowop.mbscore'))));
const profiles=Object.fromEntries(members.map(m=>[m,JSON.parse(readFileSync(resolve(root,`members/${m}/voice.json`)))]));
const fleet=JSON.parse(readFileSync('/Users/jas/.ac-os/culturehub/fleet.json'));
const plan=buildPlan(score,profiles,fleet);
const planPath=resolve(out,'plan.json');
if(command!=='check') {
 writeFileSync(planPath,JSON.stringify(plan,null,2)+'\n');
 // The Windows SUB's score: the bass layer alone, in the sub-receiver's own
 // event shape, hashed as the arrangement so its receipt can be matched.
 const subScore={name:plan.title,hash:plan.arrangementHash,dur:plan.duration,
  events:plan.events.filter(e=>e.layer==='sub').map(e=>({id:e.id,t:e.t,dur:e.dur,hz:e.frequency,g:e.gain,attack:e.attack,decay:e.release,wave:e.wave,note:noteName(e.note)}))};
 writeFileSync(resolve(out,'sub-score.json'),JSON.stringify(subScore)+'\n');
}
const quote=s=>"'"+String(s).replaceAll("'","'\\''")+"'";
function run(host,script,timeout=15000) {
 return new Promise((yes,no)=>{
  const child=host==='neo'?spawn('bash',['-s']):spawn('ssh',['-o','BatchMode=yes','-o','ConnectTimeout=8',host,'bash -s']);
  let stdout='',stderr='',done=false;
  const timer=setTimeout(()=>{child.kill();no(Error(`${host}: command timeout`));},timeout);
  child.stdout.on('data',d=>stdout+=d);child.stderr.on('data',d=>stderr+=d);
  child.on('error',e=>{clearTimeout(timer);no(e);});
  child.on('close',code=>{clearTimeout(timer);if(done)return;done=true;code===0?yes(stdout):no(Error(`${host}: ${stderr.trim()||'exit '+code}`));});
  child.stdin.end(script+'\n');
 });
}
async function post(member,name,info) {
 const kv=Object.entries(info).map(([k,v])=>`${k}=${v}`).join(';');
 await run(member,`test -x /tmp/mbpost && MB_NAME=${quote('computer.aestheticcomputer.menuband.'+name)} MB_KV=${quote(kv)} /tmp/mbpost`);
}
async function prepare(payload,id) {
 const {member,info}=payload,folder=`/tmp/menuband-trio/${id}`;
 await run(member,'pgrep -x MenuBand >/dev/null');
 await post(member,'fleetPrepare',{...info,prepareId:id});
 const deadline=Date.now()+120000;let status;
 while(Date.now()<deadline) {
  try {status=JSON.parse(await run(member,`/bin/cat ${quote(folder+'/status.json')}`));} catch {await new Promise(r=>setTimeout(r,250));continue;}
  if(status.id!==id)throw Error(`${member}: wrong preparation ID`);
  if(['error','cancelled','stopped'].includes(status.phase))throw Error(`${member}: ${status.reason??status.phase}`);
  if(status.phase==='ready')break;
  await new Promise(r=>setTimeout(r,250));
 }
 if(status?.phase!=='ready')throw Error(`${member}: preparation timed out; nothing played`);
 const local=resolve(out,'assets',member);mkdirSync(local,{recursive:true});
 await new Promise((yes,no)=>{
  const child=spawn('rsync',['-a',member==='neo'?folder+'/':`${member}:${folder}/`,local+'/']);
  child.on('error',no);child.on('close',c=>c===0?yes():no(Error(`${member}: copy failed`)));
 });
 const manifest=JSON.parse(readFileSync(resolve(local,'manifest.json')));
 if(manifest.id!==id||manifest.instance!==status.instance||manifest.fingerprint!==status.fingerprint)throw Error(`${member}: stale/mismatched manifest`);
 const actual={...manifest.payload};delete actual.prepareId;
 if(canonical(actual)!==canonical(info))throw Error(`${member}: payload mismatch`);
 for(const p of manifest.phrases) {
  for(const [file,hash] of [[p.file,p.sha256],[p.rawFile,p.rawSha256]]) {
   if(!/^phrase-\d+\.(wav|f32)$/.test(file)||digest(readFileSync(resolve(local,file)))!==hash)throw Error(`${member}: corrupted asset`);
  }
  if(p.notesUsed!==p.noteCount||p.channels!==1||!Number.isFinite(p.spanOffset)||p.spanOffset<0)throw Error(`${member}: incomplete render`);
 }
 console.log(`${member}: ${manifest.phrases.length} actual vocal phrases prepared silently`);
 return {member,prepareId:id,instance:status.instance,pid:status.pid,fingerprint:status.fingerprint,manifest:resolve(local,'manifest.json'),phrases:manifest.phrases};
}
if(command==='plan') {
 console.log(`${planPath}: ${plan.events.length} bass/bed/ornament/light events (${plan.layers.beatsPerBar} beats a bar); sub-score.json beside it; all six seats and three singers required. No devices cued.`);
} else if(command==='prepare') {
 const id='trio-'+randomUUID();
 writeFileSync(resolve(out,'preparation-state.json'),JSON.stringify({id,phase:'preparing',arrangementHash:plan.arrangementHash,playbackHeld:true}));
 const results=await Promise.allSettled(plan.payloads.map(p=>prepare(p,id)));
 const failed=results.filter(r=>r.status==='rejected');
 if(failed.length) {
  writeFileSync(resolve(out,'preparation-state.json'),JSON.stringify({id,phase:'error',arrangementHash:plan.arrangementHash,playbackHeld:true}));
  writeFileSync(resolve(out,'preparation-failed.json'),JSON.stringify({id,errors:failed.map(r=>String(r.reason)),playbackHeld:true},null,2));
  throw Error(failed.map(r=>String(r.reason)).join('\n'));
 }
 const singers=results.map(r=>r.value),rate=singers[0].phrases[0].sampleRate;
 const length=Math.max(...singers.flatMap(s=>s.phrases.map(p=>Math.round(p.spanOffset*rate)+p.frames)));
 const mix=new Float32Array(length);
 for(const s of singers)for(const p of s.phrases) {
  if(p.sampleRate!==rate)throw Error('Mixed sample rates need explicit resampling before relay');
  const raw=readFileSync(resolve(dirname(s.manifest),p.rawFile)),offset=Math.round(p.spanOffset*rate);
  if(raw.length!==p.frames*4)throw Error('PCM frame count mismatch');
  for(let n=0;n<p.frames;n++)mix[offset+n]+=raw.readFloatLE(n*4)*plan.levels.voice;
 }
 let peak=0;for(const sample of mix){if(!Number.isFinite(sample))throw Error('Non-finite relay audio');peak=Math.max(peak,Math.abs(sample));}
 if(peak>=1)throw Error('Center mix exceeds headroom; reduce voice layer gain');
 const bytes=Buffer.from(mix.buffer),file=resolve(out,'center-voices.f32');writeFileSync(file,bytes);
 const bundle={schema:'trio-fleet-assets-v1',id,arrangementHash:plan.arrangementHash,singers,
  centerMix:{file,sha256:digest(bytes),frames:mix.length,sampleRate:rate,channels:1,format:'float32-le',bakedGain:plan.levels.voice,playbackGain:1,peak,spanOffset:0},
  playbackHeld:true,receiverTransferAcknowledged:false};
 writeFileSync(resolve(out,'prepared.json'),JSON.stringify(bundle,null,2)+'\n');
 writeFileSync(resolve(out,'preparation-state.json'),JSON.stringify({id,phase:'ready',arrangementHash:plan.arrangementHash,playbackHeld:true}));
 console.log('All actual voices staged; Center mix built. No fleet receiver has been armed or started.');
} else {
 const prepared=existsSync(resolve(out,'prepared.json'))?JSON.parse(readFileSync(resolve(out,'prepared.json'))):null;
 const receipts=options.receipts?JSON.parse(readFileSync(resolve(options.receipts))):[];
 const state=existsSync(resolve(out,'preparation-state.json'))?JSON.parse(readFileSync(resolve(out,'preparation-state.json'))):null;
 const assets=state?.phase==='ready'&&state.id===prepared?.id&&prepared?.arrangementHash===plan.arrangementHash?prepared.singers.flatMap(s=>s.phrases.map(p=>p.rawSha256)):[];
 const problems=readinessProblems(plan,receipts,assets,Date.now()/1000,assets.length?prepared.centerMix.sha256:null);
 writeFileSync(resolve(out,'readiness.json'),JSON.stringify({arrangementHash:plan.arrangementHash,ready:problems.length===0,problems,playbackHeld:true},null,2));
 console.log(problems.length?'NOT READY\n'+problems.join('\n'):'Readiness receipts valid. Playback remains held.');
 if(problems.length)process.exitCode=1;
}
