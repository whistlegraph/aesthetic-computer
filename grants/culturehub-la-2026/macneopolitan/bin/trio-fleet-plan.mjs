import {createHash} from 'node:crypto';
import {singerPayload} from './trio-payload.mjs';
export const members=['neo','blueberry','frisbee'];
export const digest=data=>createHash('sha256').update(data).digest('hex');
export function canonical(value) {
 if(Array.isArray(value))return '['+value.map(canonical).join(',')+']';
 if(value&&typeof value==='object')return '{'+Object.keys(value).sort().map(k=>JSON.stringify(k)+':'+canonical(value[k])).join(',')+'}';
 return JSON.stringify(value);
}
export function notesOf(voice,bpm) {
 let beat=0,index=0,line=0,inLine=0;
 const counts=voice.lyrics.split(' / ').map(t=>t.trim().split(/\s+/).reduce((s,t)=>s+t.split('-').length,0));
 const result=voice.notes.split(',').flatMap(token=>{
  const [n,d]=token.split(':'),dur=Number(d);
  if(!Number.isFinite(dur)||dur<=0)throw Error('Invalid note duration');
  const at=beat;beat+=dur;if(n==='r')return [];
  const note=Number(n);if(!Number.isFinite(note)||note<0||note>127)throw Error('Invalid MIDI note');
  while(inLine>=counts[line]) {inLine-=counts[line];line++;}
  const gain=(voice.lineGains?.[line]??1)*(voice.noteGains?.[line]?.[inLine]??1);
  inLine++;
  return [{index:index++,line,beat:at,t:at*60/bpm,dur:dur*60/bpm,note,gain}];
 });
 if(result.length!==counts.reduce((a,b)=>a+b,0))throw Error('Notes and sung syllables must match');
 return result;
}
// Which member's part feeds each backing layer, and the bar length. A score
// may say so in `fleet` ({bass, bed, ornament, beatsPerBar}); the meter in
// `arrangement.meter` ("3/4") sets the bar when `fleet` does not. Defaults are
// the One Big Voice choices: bass from blueberry, bed from frisbee, ornaments
// from neo for four bars then frisbee.
import {readFileSync as _read} from 'node:fs';
import {resolve as _resolve,dirname as _dirname} from 'node:path';
import {fileURLToPath as _furl} from 'node:url';
const LAYER_FILE=_resolve(_dirname(_furl(import.meta.url)),'../scores/fleet-layers.json');
let LAYERS={};try{LAYERS=JSON.parse(_read(LAYER_FILE,'utf8'));}catch{}
export function layerSources(score) {
 const slug=String(score.slug??'').replace(/\.mbscore$/,'');
 const f=score.fleet??LAYERS[slug]??{};
 const meter=/^(\d+)\s*\/\s*\d+$/.exec(String(score.arrangement?.meter??''));
 const beatsPerBar=f.beatsPerBar??(meter?Number(meter[1]):4);
 if(!Number.isInteger(beatsPerBar)||beatsPerBar<1||beatsPerBar>12)throw Error('Invalid beats per bar');
 const idx=v=>{const i=typeof v==='number'?v:members.indexOf(v);if(!(i>=0&&i<3))throw Error('fleet layer sources must name a member');return i;};
 return {beatsPerBar,bass:idx(f.bass??1),bed:idx(f.bed??2),ornament:f.ornament==null?null:idx(f.ornament),ornamentLate:idx(f.ornamentLate??2)};
}
export const noteName=n=>['C','C#','D','D#','E','F','F#','G','G#','A','A#','B'][((n%12)+12)%12]+String(Math.floor(n/12)-1);
const hz=n=>440*2**((n-69)/12);
export const hexRgb=h=>{const m=/^#?([0-9a-f]{6})$/i.exec(String(h||''));const v=m?parseInt(m[1],16):0x8fd13f;return [(v>>16)&255,(v>>8)&255,v&255];};
// The seats a phrase bounces through: the lead seat walks the physical ring
// (0 left front … 5 held center) one phrase at a time; its echoes follow on
// the next two seats of the walk, half a beat and a beat later.
export const BOUNCE=[0,1,2,4,3,5];
export function buildPlan(score,profiles,fleet,levels={}) {
 if(score.voices?.length!==3||score.voices.some(v=>typeof v.lyrics!=='string'||!v.lyrics.trim()))throw Error('Expected three sung member parts');
 if(!Number.isFinite(score.bpm)||score.bpm<=0)throw Error('Invalid BPM');
 levels={voice:.35,echo1:.4,echo2:.2,harmony:.018,inst:.06,pad:.03,perc:.05,sub:.18,bed:.05,ornament:.07,lights:.28,mix:.25,...levels};
 if(Object.values(levels).some(x=>!Number.isFinite(x)||x<0||x>1))throw Error('Layer levels must be 0…1');
 if(levels.lights>.5)throw Error('The installed light bridge limits brightness to 128/255');
 const seats={'CENTER REAR':0,'RIGHT FRONT':1,'RIGHT REAR':2,'LEFT REAR':3,'LEFT FRONT':4,'HELD CENTER':5};
 const nodes=fleet.map(([host,port,label])=>({id:`seat-${seats[label]}`,seat:seats[label],host,port,label}));
 if(nodes.length!==6||new Set(nodes.map(n=>n.seat)).size!==6||nodes.some(n=>n.seat==null))throw Error('All six surround seats are required');
 const payloads=score.voices.map((voice,i)=>{
  const member=members[i];
  if(!voice.name.toLowerCase().startsWith(member)||! /\((Enhanced|Premium)\)$/.test(voice.singVoice))throw Error('Member/voice assignment must be explicit and Enhanced/Premium');
  const pairs=singerPayload({p:{voice,member},vj:profiles[member],score,bpm:score.bpm,epoch:0});
  const info=Object.fromEntries(pairs.map(p=>{const i=p.indexOf('=');return[p.slice(0,i),p.slice(i+1)];}));
  delete info.startEpoch;
  info.faceAlpha='1';info.captionSize='110';   // the room: a full, opaque face and big captions on every singer's screen
  return {member,info};
 });
 const parts=score.voices.map(v=>notesOf(v,score.bpm));
 const dur=Math.max(...parts.flat().map(e=>e.t+e.dur));
 const colors=members.map(m=>hexRgb(profiles[m]?.color));
 // Phrases: one per lyric line per member, in time order across the trio.
 const phrases=[];
 score.voices.forEach((v,i)=>{
  const lines=v.lyrics.split(' / ').map(l=>l.trim().split(/\s+/).filter(t=>t!=='/').map(t=>t.replace(/-/g,'')).join(' '));   // words, not syllables, for the notation
  lines.forEach((text,k)=>{const ns=parts[i].filter(n=>n.line===k);if(!ns.length)return;
   const toks=(v.lyrics.split(' / ')[k]||'').trim().split(/\s+/).filter(t=>t!=='/').flatMap(t=>t.split('-'));   // one syllable per note
   phrases.push({member:members[i],memberIndex:i,phrase:k,text,t:ns[0].t,dur:Math.max(...ns.map(n=>n.t+n.dur))-ns[0].t,notes:ns.map(n=>n.note),
    role:v.lineRoles?.[k]??'lead',   // the score's own word: lead or hum
    syllables:ns.map((n,j)=>({t:+n.t.toFixed(3),dur:+n.dur.toFixed(3),text:toks[j]||'',note:n.note}))});});
 });
 phrases.sort((a,b)=>a.t-b.t||a.memberIndex-b.memberIndex);
 // The answer: the line the room should show more. Named outright ("the
 // answer is four"), or the middle of a call and response — a lead line by
 // one member that follows another member's lead line within a bar and is
 // itself followed by a third member's line (question · answer · confirmation).
 const leads=phrases.filter(p=>p.role==='lead'),barS=layerSources(score).beatsPerBar*60/score.bpm;
 const named=leads.filter(p=>/^the answer\b/i.test(p.text));
 if(named.length)named.forEach(p=>p.answer=true);   // a piece that names its answers has them; nothing else is one
 else leads.forEach((p,i)=>{
  const prev=leads[i-1],next=leads[i+1];
  const middle=prev&&next&&prev.member!==p.member&&next.member!==p.member&&next.member!==prev.member&&p.t-(prev.t+prev.dur)<barS&&next.t-(p.t+p.dur)<barS;
  if(middle)p.answer=true;
 });
 const beat=60/score.bpm;
 const events=[];const emit=(layer,receiver,e,extra={})=>events.push({id:`${layer}-${events.length}`,layer,receiver,...e,...extra});
 // The voice bounce: every actual sung phrase is heard from a seat, then
 // echoed on the next two seats of the walk. `routes` is what the stems are
 // mixed from; `voice` events are the same thing on the timeline for notation
 // and lights. Gains are absolute (the vocal level times the echo fraction).
 const routes=[];
 phrases.forEach((ph,k)=>{
  [[0,0,1,'lead'],[1,beat/2,levels.echo1,'echo'],[2,beat,levels.echo2,'echo']].forEach(([step,delay,frac,role])=>{
   const seat=BOUNCE[(k+step)%BOUNCE.length],gain=levels.voice*frac;
   routes.push({member:ph.member,phrase:ph.phrase,seat,offsetSeconds:ph.t+delay,delay,duration:ph.dur,gain,role});
   emit('voice',`seat-${seat}`,{t:ph.t+delay,dur:ph.dur,member:ph.member,phrase:ph.phrase,text:ph.text,gain,role,delay,rgb:colors[ph.memberIndex]});
  });
 });
 // Harmony: a quiet sine a fifth above or an octave below each of neo's and
 // frisbee's notes, walking the ring, so the room hums the tune's shadow.
 let hk=0;
 for(const i of [0,2])for(const n of parts[i]){
  const up=hk%2===0,note=n.note+(up?7:-12),seat=hk%5;hk++;
  emit('harmony',`seat-${seat}`,{t:n.t,dur:n.dur,note,frequency:hz(note),gain:levels.harmony*n.gain,attack:.08,release:.18,wave:'sine'});
 }
 // Instruments: a music box (GM 11) doubling neo's line an octave up from the
 // seat that leads that phrase; a warm pad (GM 89) under blueberry's cradle
 // at center rear.
 for(const n of parts[0]){
  const k=phrases.findIndex(p=>p.memberIndex===0&&p.phrase===n.line),seat=k<0?1:BOUNCE[k%BOUNCE.length];
  emit('inst',`seat-${seat}`,{t:n.t,dur:Math.min(n.dur,1.2),note:n.note+12,frequency:hz(n.note+12),gain:levels.inst*n.gain,attack:.005,release:.35,wave:'sine',gmProgram:11,name:'music box'});
 }
 for(const n of parts[1])if(n.dur>=beat*1.5)emit('inst','seat-4',{t:n.t,dur:n.dur,note:n.note,frequency:hz(n.note),gain:levels.pad*n.gain,attack:.4,release:.5,wave:'sine',gmProgram:89,name:'warm pad'});
 // Percussion, in three: a soft taiko on the one (left rear / right front by
 // turns), a woodblock tick on two and three, a brush of noise on three.
 const src0=layerSources(score),bar=src0.beatsPerBar*beat;
 for(let b=0;b*bar<dur-.01;b++){
  const t=b*bar;
  emit('perc',`seat-${b%2?3:1}`,{t,dur:.35,note:33,frequency:55,gain:levels.perc,attack:.002,release:.3,wave:'sine',gmProgram:116,name:'taiko'});
  for(let q=1;q<src0.beatsPerBar;q++)emit('perc',`seat-${q%2?2:4}`,{t:t+q*beat,dur:.09,note:81,frequency:hz(81),gain:levels.perc*.45,attack:.001,release:.07,wave:'sine',gmProgram:115,name:'woodblock'});
  if(src0.beatsPerBar>2)emit('perc','seat-0',{t:t+(src0.beatsPerBar-1)*beat+beat*.5,dur:.06,note:0,frequency:8000,gain:.012,attack:.001,release:.05,wave:'noise',name:'brush'});
 }
 const src=layerSources(score);
 for(const e of parts[src.bass])emit('sub','sub',{t:e.t,dur:e.dur,note:e.note-12,frequency:440*2**((e.note-12-69)/12),gain:levels.sub*e.gain,attack:.015,release:.12,sourceNote:e.index,wave:'sine'});
 const barSeconds=src.beatsPerBar*60/score.bpm;
 for(let bar=0;bar*barSeconds<dur-.01;bar++) {
  const t=bar*barSeconds;
  const inner=parts[src.bed].find(e=>e.t>=t-1e-4&&e.t<t+barSeconds);
  if(inner)emit('bed','seat-0',{t,dur:Math.min(barSeconds*.92,dur-t),note:inner.note-12,frequency:440*2**((inner.note-12-69)/12),gain:levels.bed,attack:.2,release:.22,wave:'sine'});
  const lead=parts[src.ornament??(bar<4?0:src.ornamentLate)].find(e=>e.t>=t-1e-4&&e.t<t+barSeconds);
  if(lead&&bar%2===0)emit('ornament',`seat-${1+(bar/2)%4}`,{t:t+60/score.bpm*.5,dur:60/score.bpm*.65,note:lead.note+12,frequency:440*2**((lead.note+12-69)/12),gain:levels.ornament,attack:.01,release:.2,wave:'sine'});
 }
 const addresses=[1,11,21,31],palette=[[143,209,63],[90,87,211],[242,167,185]];
 const musical=[...events].filter(e=>e.layer==='sub'||e.layer==='ornament');
 musical.forEach((e,i)=>{
  const address=addresses[i%4],rgb=palette[((e.note%3)+3)%3].map(v=>Math.round(v*levels.lights));
  emit('dmx','dmx',{t:e.t,dur:e.dur,note:e.note,sourceEvent:e.id,address,rgb,level:Math.max(...rgb),attack:e.attack,release:e.release,
   command:{address,color:'rgb',rgb,level:Math.max(...rgb),duration:e.dur,envelope:{attack:e.attack,decay:e.release}}});
 });
 for(const e of events.filter(e=>e.layer==='voice'&&e.receiver==='seat-5'))
  emit('light','seat-5',{t:e.t,dur:e.dur,rgb:e.rgb.map(v=>Math.round(v*Math.min(1,e.gain/levels.voice))),sourceEvent:e.id});
 events.sort((a,b)=>a.t-b.t||a.id.localeCompare(b.id));
 const plan={schema:'trio-fleet-plan-v1',title:score.title,bpm:score.bpm,duration:dur,levels,payloads,nodes,events,
  requiredReceivers:[...members.map(m=>`singer-${m}`),...nodes.map(n=>n.id),'sub','dmx'],
  layers:src,routes,colors:Object.fromEntries(members.map((m,i)=>[m,colors[i]])),
  lyrics:phrases.map(p=>({t:p.t,dur:p.dur,text:p.text,member:p.member,rgb:colors[p.memberIndex],role:p.role,...(p.answer?{answer:true}:{}),syllables:p.syllables})),
  sections:(score.arrangement?.sections??[]).map(s=>({name:s.name,beat:s.beat})),arrangement:{total:score.arrangement?.total??null,meter:score.arrangement?.meter??null},
  dmx:{host:'192.168.1.235',port:8790,activeAddresses:addresses,inactiveAddresses:[41,511]},
  sub:{host:'192.168.1.67',port:8788,transport:'rustdesk-local-bridge'},
  center:{receiver:'seat-5',actualVocals:true,preSlideEffects:true},playbackHeld:true};
 return {...plan,arrangementHash:digest(canonical(plan))};
}
export function readinessProblems(plan,receipts,assets=[],now=Date.now()/1000,centerMixHash=null) {
 const problems=[];
 const expected=plan.payloads.reduce((sum,p)=>sum+p.info.lyrics.split(' / ').length,0);
 if(assets.length!==expected)problems.push(`voices: expected ${expected} prepared phrase hashes, got ${assets.length}`);
 if(new Set(receipts.map(a=>a.receiverId)).size!==receipts.length)problems.push('Duplicate receiver acknowledgments');
 for(const id of plan.requiredReceivers) {
  const a=receipts.find(a=>a.receiverId===id);
  if(!a){problems.push(`${id}: no acknowledgment`);continue;}
  if(a.schema!=='trio-fleet-ready-v1'||a.ready!==true||a.phase!=='ready')problems.push(`${id}: not ready`);
  if(a.arrangementHash!==plan.arrangementHash)problems.push(`${id}: wrong arrangement`);
  if(!a.instance||!Number.isFinite(a.observedAt)||Math.abs(now-a.observedAt)>15)problems.push(`${id}: stale/unidentified receiver`);
  if(!Number.isFinite(a.clockUncertaintyMs)||a.clockUncertaintyMs>20||a.clockUncertaintyMs<0)problems.push(`${id}: clock not verified`);
  const caps=a.capabilities??[];
  for(const cap of ['timestamped-start','cancel-queued'])if(!caps.includes(cap))problems.push(`${id}: missing ${cap}`);
  if(id.startsWith('seat-')&&(a.displayMode!=='concert'||a.pointerHidden!==true||a.fullscreen!==true))problems.push(`${id}: Concert display not ready`);
  if(id==='sub'&&a.fullscreen!==true)problems.push('sub: fullscreen output not ready');
  if(id==='seat-5') {
   if(!centerMixHash||a.centerMixHash!==centerMixHash)problems.push(`${id}: mixed vocal asset not acknowledged`);
   if(!caps.includes('actual-vocal-pcm'))problems.push(`${id}: voice relay not implemented`);
   for(const hash of assets)if(!a.assetHashes?.includes(hash))problems.push(`${id}: missing voice asset ${hash}`);
  }
  if(id==='sub'&&a.duration!==plan.duration)problems.push('sub: Trio score not loaded');
  if(id==='dmx'&&!plan.dmx.activeAddresses.every(x=>a.activeAddresses?.includes(x)))problems.push('dmx: required fixture routes not ready');
 }
 return problems;
}
