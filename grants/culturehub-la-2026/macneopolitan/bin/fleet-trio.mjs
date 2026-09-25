#!/usr/bin/env node
// Preparation/readiness only. No command in this utility starts playback.
import {readFileSync,writeFileSync,mkdirSync,existsSync} from 'node:fs';
import {resolve,dirname} from 'node:path';
import {fileURLToPath} from 'node:url';
import {randomUUID} from 'node:crypto';
import {spawn,spawnSync} from 'node:child_process';
import {hostname} from 'node:os';
// The conductor is whichever Mac runs this: its own member name is local,
// the other two are reached over ssh. (Was hard-wired to neo.)
const localName=((spawnSync('scutil',['--get','LocalHostName'],{encoding:'utf8'}).stdout||'').trim()||hostname().split('.')[0]).toLowerCase();
const isLocal=h=>String(h).toLowerCase()===localName;
import {buildPlan,canonical,digest,members,noteName,readinessProblems} from './trio-fleet-plan.mjs';
import {announce} from './announce.mjs';
const root=resolve(dirname(fileURLToPath(import.meta.url)),'..');
const args=process.argv.slice(2),command=args.shift();
if(!['plan','prepare','check'].includes(command))throw Error('Use fleet-trio.mjs plan|prepare|check [--out=DIR]. Playback remains held.');
const options=Object.fromEntries(args.map(a=>{if(!a.startsWith('--')||!a.includes('='))throw Error(`Invalid option ${a}`);const at=a.indexOf('=');return [a.slice(2,at),a.slice(at+1)];}));
for(const key of Object.keys(options))if(!['out','score','receipts','announce','sing'].includes(key))throw Error(`Unknown option ${key}`);
const out=resolve(options.out??'/Users/jas/Shelf/culturehub-trio');mkdirSync(out,{recursive:true});
const scorePath=resolve(options.score??resolve(root,'scores/trio-chorus-doowop.mbscore'));
const score={...JSON.parse(readFileSync(scorePath)),slug:scorePath.split('/').pop().replace(/\.mbscore$/,'')};
const profiles=Object.fromEntries(members.map(m=>[m,JSON.parse(readFileSync(resolve(root,`members/${m}/voice.json`)))]));
const fleet=JSON.parse(readFileSync(process.env.TRIO_FLEET??'/Users/jas/.ac-os/culturehub/fleet.json'));   // TRIO_FLEET=… the seat map to use
const plan=buildPlan(score,profiles,fleet);
const planPath=resolve(out,'plan.json');
if(command!=='check') {
 writeFileSync(planPath,JSON.stringify(plan,null,2)+'\n');
 writeSubScore();
}
// The Windows SUB's score: the bass layer alone, in the sub-receiver's own
// event shape, hashed as the arrangement so its receipt can be matched.
function writeSubScore(){
 const subScore={name:plan.title,hash:plan.arrangementHash,dur:plan.duration,
  events:plan.events.filter(e=>e.layer==='sub').map(e=>({id:e.id,t:e.t,dur:e.dur,hz:e.frequency,g:e.gain,attack:e.attack,decay:e.release,wave:e.wave,note:noteName(e.note)}))};
 writeFileSync(resolve(out,'sub-score.json'),JSON.stringify(subScore)+'\n');
}
const quote=s=>"'"+String(s).replaceAll("'","'\\''")+"'";
function run(host,script,timeout=15000) {
 return new Promise((yes,no)=>{
  const child=isLocal(host)?spawn('bash',['-s']):spawn('ssh',['-o','BatchMode=yes','-o','ConnectTimeout=8',host,'bash -s']);
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
  const child=spawn('rsync',['-a',isLocal(member)?folder+'/':`${member}:${folder}/`,local+'/']);
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
 // The announcement: a line in Jeffrey's voice on all six speakers, then a
 // breath, then the downbeat. Everything timed in the plan moves by that
 // lead-in; the singers start after it (run-full-trio adds plan.leadIn).
 let lead=0,announced=null;
 if(options.announce){
  // --sing: the title sung on the piece's own opening — the first lead line's notes
  const firstLead=(plan.lyrics||[]).find(l=>l.role!=='hum'&&(l.syllables||[]).length);
  const melody=options.sing&&firstLead?firstLead.syllables.slice(0,8).map(y=>y.note):null;
  announced=await announce(options.announce,{sing:melody});lead=+(announced.seconds+.6).toFixed(3);
  const shift=e=>{e.t=+(e.t+lead).toFixed(4);};
  for(const e of plan.events)shift(e);
  for(const l of plan.lyrics||[]){shift(l);for(const y of l.syllables||[])shift(y);}
  for(const r of plan.routes||[])r.offsetSeconds=+(r.offsetSeconds+lead).toFixed(4);
  plan.duration=+(plan.duration+lead).toFixed(4);plan.leadIn=lead;plan.announce={text:options.announce,seconds:announced.seconds,hash:announced.hash};
  delete plan.arrangementHash;plan.arrangementHash=digest(canonical(plan));
  writeFileSync(planPath,JSON.stringify(plan,null,2)+'\n');writeSubScore();
  console.log(`announce: "${options.announce}" ${announced.seconds}s in Jeffrey's voice; lead-in ${lead}s; arrangement ${plan.arrangementHash.slice(0,12)}`);
 }
 // One stem per seat: every route (a member's phrase, a seat, an offset and
 // an absolute gain) mixed from that member's actual raw phrase. Seat 5's
 // stem doubles as `centerMix` for the older readiness checks.
 const phraseOf=(member,k)=>{const sg=singers.find(s=>s.member===member),p=sg?.phrases[k];if(!p)throw Error(`${member}: phrase ${k} not prepared`);return {p,dir:dirname(sg.manifest)};};
 const length=Math.max(Math.round((plan.duration+1)*rate),...plan.routes.map(r=>{const {p}=phraseOf(r.member,r.phrase);return Math.round((p.spanOffset+r.delay+lead)*rate)+p.frames;}));
 const seatMix={};for(const n of plan.nodes)seatMix[n.id]=new Float32Array(length);
 for(const r of plan.routes){
  const {p,dir}=phraseOf(r.member,r.phrase);
  if(p.sampleRate!==rate)throw Error('Mixed sample rates need explicit resampling before relay');
  const raw=readFileSync(resolve(dir,p.rawFile)),offset=Math.round((p.spanOffset+r.delay+lead)*rate),mix=seatMix[`seat-${r.seat}`];
  if(raw.length!==p.frames*4)throw Error('PCM frame count mismatch');
  for(let n=0;n<p.frames;n++)mix[offset+n]+=raw.readFloatLE(n*4)*r.gain;
 }
 // Bake every sounding event layer into the seat's stem too — sine beds,
 // harmonies, the music box and pad (approximated), the percussion — so
 // nothing on a seat is fired from a simulation frame: the deck plays it all
 // sample-locked from the downbeat. The events stay in the plan for the
 // notation and are marked `baked` so native staging keeps them off the
 // seat's live event list.
 const bake=(mix,e)=>{
  const start=Math.round(e.t*rate),len=Math.round(Math.min(e.dur,12)*rate)+Math.round((e.release??.12)*rate);
  const att=Math.max(1,Math.round((e.attack??.015)*rate)),rel=Math.max(1,Math.round((e.release??.12)*rate)),hold=Math.max(0,Math.round(e.dur*rate)-att);
  const f=e.frequency,g=e.gain,gm=e.gmProgram;let ph=0,ph2=0,seed=(start%9973)+1;
  for(let n=0;n<len&&start+n<mix.length;n++){
   const env=n<att?n/att:n<att+hold?1:Math.max(0,1-(n-att-hold)/rel);
   if(env<=0)break;
   let v;
   if(e.wave==='noise'){seed=(seed*1103515245+12345)&0x7fffffff;v=(seed/0x7fffffff*2-1)*.6;}
   else if(gm===11){ph+=2*Math.PI*f/rate;ph2+=2*Math.PI*f*4.1/rate;const dk=Math.exp(-n/rate*6);v=(Math.sin(ph)*.7+Math.sin(ph2)*.3*dk)*dk;}          // music box: bright partial, quick decay
   else if(gm===89){ph+=2*Math.PI*f/rate;ph2+=2*Math.PI*f*1.004/rate;v=(Math.sin(ph)+Math.sin(ph2)+.3*Math.sin(ph*2))/2.3;}                          // warm pad: two detuned sines
   else if(gm===116){const fr=f*(1+2.5*Math.exp(-n/rate*30));ph+=2*Math.PI*fr/rate;v=Math.sin(ph)*Math.exp(-n/rate*8);}                              // taiko: pitch drop, fast decay
   else if(gm===115){ph+=2*Math.PI*f/rate;v=(Math.sin(ph)*.6+Math.sin(ph*2.76)*.4)*Math.exp(-n/rate*40);}                                              // woodblock: inharmonic tick
   else{ph+=2*Math.PI*f/rate;v=e.wave==='triangle'?(2/Math.PI)*Math.asin(Math.sin(ph)):e.wave==='square'?Math.sign(Math.sin(ph)):Math.sin(ph);}
   mix[start+n]+=v*g*env;
  }
 };
 if(announced){const a=readFileSync(announced.f32);for(const mix of Object.values(seatMix))for(let n=0;n*4<a.length&&n<mix.length;n++)mix[n]+=a.readFloatLE(n*4)*.9;}
 const BAKED=['harmony','inst','perc','bed','ornament','drone'];
 for(const e of plan.events)if(BAKED.includes(e.layer)&&seatMix[e.receiver]){bake(seatMix[e.receiver],e);e.baked=true;}
 writeFileSync(planPath,JSON.stringify(plan,null,2)+'\n');   // the plan now says which events the stems carry
 const stems={};
 for(const [id,mix] of Object.entries(seatMix)){
  let peak=0;for(const sample of mix){if(!Number.isFinite(sample))throw Error('Non-finite relay audio');peak=Math.max(peak,Math.abs(sample));}
  if(peak>=1)throw Error(`${id}: stem exceeds headroom; reduce voice/echo levels`);
  const bytes=Buffer.from(mix.buffer),file=resolve(out,`${id}.f32`);writeFileSync(file,bytes);
  stems[id]={file,sha256:digest(bytes),frames:mix.length,sampleRate:rate,channels:1,format:'float32-le',peak,spanOffset:0,routes:plan.routes.filter(r=>`seat-${r.seat}`===id).length};
 }
 const centerStem=stems['seat-5'];
 const bundle={schema:'trio-fleet-assets-v2',id,arrangementHash:plan.arrangementHash,singers,stems,
  centerMix:{...centerStem,bakedGain:plan.levels.voice,playbackGain:1},
  playbackHeld:true,receiverTransferAcknowledged:false};
 writeFileSync(resolve(out,'prepared.json'),JSON.stringify(bundle,null,2)+'\n');
 writeFileSync(resolve(out,'preparation-state.json'),JSON.stringify({id,phase:'ready',arrangementHash:plan.arrangementHash,playbackHeld:true}));
 console.log(`All actual voices staged; ${Object.keys(stems).length} seat stems mixed from ${plan.routes.length} routes. No fleet receiver has been armed or started.`);
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
