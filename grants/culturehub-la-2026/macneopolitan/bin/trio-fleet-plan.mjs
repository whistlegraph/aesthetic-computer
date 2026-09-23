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
  return [{index:index++,beat:at,t:at*60/bpm,dur:dur*60/bpm,note,gain}];
 });
 if(result.length!==counts.reduce((a,b)=>a+b,0))throw Error('Notes and sung syllables must match');
 return result;
}
export function buildPlan(score,profiles,fleet,levels={}) {
 if(score.voices?.length!==3||score.voices.some(v=>typeof v.lyrics!=='string'||!v.lyrics.trim()))throw Error('Expected three sung member parts');
 if(!Number.isFinite(score.bpm)||score.bpm<=0)throw Error('Invalid BPM');
 levels={voice:.35,sub:.18,bed:.05,ornament:.07,lights:.28,...levels};
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
  return {member,info};
 });
 const parts=score.voices.map(v=>notesOf(v,score.bpm));
 const dur=Math.max(...parts.flat().map(e=>e.t+e.dur));
 const events=[];const emit=(layer,receiver,e,extra={})=>events.push({id:`${layer}-${events.length}`,layer,receiver,...e,...extra});
 for(const e of parts[1])emit('sub','sub',{t:e.t,dur:e.dur,note:e.note-12,frequency:440*2**((e.note-12-69)/12),gain:levels.sub*e.gain,attack:.015,release:.12,sourceNote:e.index,wave:'sine'});
 const barSeconds=240/score.bpm;
 for(let bar=0;bar*barSeconds<dur-.01;bar++) {
  const t=bar*barSeconds;
  const inner=parts[2].find(e=>e.t>=t-1e-4&&e.t<t+barSeconds);
  if(inner)emit('bed','seat-0',{t,dur:Math.min(barSeconds*.92,dur-t),note:inner.note-12,frequency:440*2**((inner.note-12-69)/12),gain:levels.bed,attack:.2,release:.22,wave:'sine'});
  const lead=parts[bar<4?0:2].find(e=>e.t>=t-1e-4&&e.t<t+barSeconds);
  if(lead&&bar%2===0)emit('ornament',`seat-${1+(bar/2)%4}`,{t:t+60/score.bpm*.5,dur:60/score.bpm*.65,note:lead.note+12,frequency:440*2**((lead.note+12-69)/12),gain:levels.ornament,attack:.01,release:.2,wave:'sine'});
 }
 const addresses=[1,11,21,31],palette=[[143,209,63],[90,87,211],[242,167,185]];
 const musical=[...events].filter(e=>e.layer==='sub'||e.layer==='ornament');
 musical.forEach((e,i)=>{
  const address=addresses[i%4],rgb=palette[((e.note%3)+3)%3].map(v=>Math.round(v*levels.lights));
  emit('dmx','dmx',{t:e.t,dur:e.dur,note:e.note,sourceEvent:e.id,address,rgb,level:Math.max(...rgb),attack:e.attack,release:e.release,
   command:{address,color:'rgb',rgb,level:Math.max(...rgb),duration:e.dur,envelope:{attack:e.attack,decay:e.release}}});
 });
 events.sort((a,b)=>a.t-b.t||a.id.localeCompare(b.id));
 const plan={schema:'trio-fleet-plan-v1',title:score.title,bpm:score.bpm,duration:dur,levels,payloads,nodes,events,
  requiredReceivers:[...members.map(m=>`singer-${m}`),...nodes.map(n=>n.id),'sub','dmx'],
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
