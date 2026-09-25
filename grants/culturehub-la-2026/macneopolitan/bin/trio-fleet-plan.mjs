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
 const sung=typeof voice.lyrics==='string'&&voice.lyrics.trim().length>0;
 const counts=sung?voice.lyrics.split(' / ').map(t=>t.trim().split(/\s+/).reduce((s,t)=>s+t.split('-').length,0)):[Infinity];
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
 const want=counts.reduce((a,b)=>a+b,0);
 if(sung&&Math.abs(result.length-want)>2)throw Error(`Notes and sung syllables must match (${result.length} notes, ${want} syllables)`);   // a syllable or two adrift over a long ballad is forgiven
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
export function layerEntry(score) {
 const slug=String(score.slug??'').replace(/\.mbscore$/,'');
 return score.fleet??LAYERS[slug]??{};
}
export function layerSources(score) {
 const f=layerEntry(score);
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
// An authored arrangement. A fleet-layers entry may carry `sections`
// ([{beat,name,kind}]) and a `kinds` table (verse, refrain, bridge, last…):
// then every backing layer is shaped section by section instead of the flat
// defaults in buildPlan. The pulse grid restarts at each section so the
// downbeat lands with each entry; a kind says how loud and how busy each layer
// is (multipliers of the room's levels), which colours the four PARs breathe,
// chase or pulse, and when the held center's wedge lights. Pieces without
// `sections` are untouched: their plans stay byte for byte what they were.
function authoredSections(f,bpm,totalBeats) {
 const secs=f.sections.map((s,i)=>{
  const k=f.kinds?.[s.kind];if(!k)throw Error(`fleet section kind "${s.kind}" is not in kinds`);
  if(!Number.isFinite(s.beat)||s.beat<0)throw Error('fleet sections need a beat');
  const endBeat=f.sections[i+1]?.beat??f.arrangement?.total??totalBeats;
  return {name:s.name,beat:s.beat,kind:s.kind,k,t:s.beat*60/bpm,end:Math.min(endBeat,totalBeats)*60/bpm};
 });
 for(let i=1;i<secs.length;i++)if(secs[i].beat<=secs[i-1].beat)throw Error('fleet sections must be in beat order');
 if(secs[0].beat!==0)throw Error('fleet sections must start at beat 0');
 return secs;
}
const DMX_WALK=[1,11,31,21];   // left front, right front, right rear, left rear: around the ring
function authoredLayers({authored,parts,phrases,levels,beat,bar,dur,emit}) {
 const lead=parts[0];   // the ballad's line: neo's
 const secAt=t=>{let s=authored[0];for(const x of authored){if(t>=x.t-1e-4)s=x;else break;}return s;};
 const rootAt=t=>{const inBar=lead.find(e=>e.t>=t-1e-4&&e.t<t+bar);if(inBar)return inBar.note;let last=null;for(const e of lead){if(e.t<t)last=e.note;else break;}return last??lead[0].note;};
 const g=x=>Math.min(.25,+x.toFixed(5));   // no event past the receivers' gain ceiling
 const lamp=(rgb,k)=>rgb.map(v=>Math.max(0,Math.min(128,Math.round(v*levels.lights*k))));
 const bars=[];
 for(const sec of authored)for(let i=0,t=sec.t;t<sec.end-.05;i++,t+=bar)bars.push({t,len:Math.min(bar,sec.end-t),sec,i});
 // Harmony: the line's shadow walking the ring, sparser in verses, swelling through a refrain.
 let hk=0;
 for(const n of lead){
  const sec=secAt(n.t),h=sec.k.harmony;if(!h?.gain)continue;
  if(hk++%(h.every||1))continue;
  const swell=h.swell?(1-h.swell)+h.swell*Math.min(1,(n.t-sec.t)/(sec.end-sec.t)):1;
  const ivs=h.intervals||[7,-12],offs=h.both?ivs:[ivs[Math.floor(hk/(h.every||1))%ivs.length]];
  offs.forEach((off,j)=>{const note=n.note+off;emit('harmony',`seat-${(hk+j)%5}`,{t:n.t,dur:n.dur,note,frequency:hz(note),gain:g(levels.harmony*h.gain*swell*n.gain),attack:.08,release:.18,wave:'sine'});});
 }
 // Instruments: the music box doubling the line from its lead seat; the warm pad per bar from the bar's root.
 for(const n of lead){
  const m=secAt(n.t).k.inst;if(!m?.gain)continue;
  const k=phrases.findIndex(p=>p.memberIndex===0&&p.phrase===n.line),seat=k<0?1:BOUNCE[k%BOUNCE.length];
  emit('inst',`seat-${seat}`,{t:n.t,dur:Math.min(n.dur,1.2),note:n.note+12,frequency:hz(n.note+12),gain:g(levels.inst*m.gain*n.gain),attack:.005,release:.35,wave:'sine',gmProgram:11,name:'music box'});
 }
 for(const b of bars){
  const {pad,perc,sub,bed}=b.sec.k,root=rootAt(b.t),onBar=b.i%2?3:1;
  if(pad?.gain){
   emit('inst','seat-4',{t:b.t,dur:b.len*.95,note:root,frequency:hz(root),gain:g(levels.pad*pad.gain),attack:.4,release:.5,wave:'sine',gmProgram:89,name:'warm pad'});
   if(pad.fifth)emit('inst','seat-4',{t:b.t,dur:b.len*.95,note:root+7,frequency:hz(root+7),gain:g(levels.pad*pad.gain*pad.fifth),attack:.5,release:.5,wave:'sine',gmProgram:89,name:'warm pad'});
  }
  // Percussion: the heartbeat (a taiko and its softer second thump) on the one, woodblock ticks keeping time, a brush on the last and.
  if(perc?.gain){
   const thump=(t,k)=>{emit('perc',`seat-${onBar}`,{t,dur:.35,note:33,frequency:55,gain:g(levels.perc*perc.gain*k),attack:.002,release:.3,wave:'sine',gmProgram:116,name:'taiko'});
    emit('perc',`seat-${onBar}`,{t:t+.28,dur:.28,note:31,frequency:49,gain:g(levels.perc*perc.gain*k*.6),attack:.002,release:.25,wave:'sine',gmProgram:116,name:'taiko'});};
   thump(b.t,1);
   const beats=Math.max(1,Math.round(b.len/beat));
   if(perc.third&&beats>2)thump(b.t+2*beat,perc.third);
   if(perc.ticks)for(let q=1;q<beats;q++)emit('perc',`seat-${q%2?2:4}`,{t:b.t+q*beat,dur:.09,note:81,frequency:hz(81),gain:g(levels.perc*perc.gain*perc.ticks),attack:.001,release:.07,wave:'sine',gmProgram:115,name:'woodblock'});
   if(perc.brush)for(let q=(perc.brush>1?1:beats-1);q<beats;q+=2)emit('perc','seat-0',{t:b.t+q*beat+beat*.5,dur:.06,note:0,frequency:8000,gain:.012,attack:.001,release:.05,wave:'noise',name:'brush'});
  }
  // The SUB: a thump on the downbeat at the bar's root two octaves down, a soft sustain under it.
  if(sub){
   let low=root-24;while(low>40)low-=12;while(low<28)low+=12;
   if(sub.thump)emit('sub','sub',{t:b.t,dur:.45,note:low,frequency:hz(low),gain:g(levels.sub*sub.thump),attack:.01,release:.18,wave:'sine'});
   if(sub.third&&b.len>2.5*beat)emit('sub','sub',{t:b.t+2*beat,dur:.4,note:low,frequency:hz(low),gain:g(levels.sub*sub.third),attack:.01,release:.16,wave:'sine'});
   if(sub.sustain)emit('sub','sub',{t:b.t+.05,dur:b.len*.9,note:low,frequency:hz(low),gain:g(levels.sub*sub.sustain),attack:.3,release:.3,wave:'sine'});
  }
  // The bed at center rear: the root an octave down, a fifth beside it when the kind asks.
  if(bed?.gain){
   emit('bed','seat-0',{t:b.t,dur:Math.min(b.len*.92,dur-b.t),note:root-12,frequency:hz(root-12),gain:g(levels.bed*bed.gain),attack:bed.attack??.2,release:.22,wave:'sine'});
   if(bed.fifth)emit('bed','seat-0',{t:b.t,dur:Math.min(b.len*.92,dur-b.t),note:root-5,frequency:hz(root-5),gain:g(levels.bed*bed.gain*bed.fifth),attack:(bed.attack??.2)+.1,release:.22,wave:'sine'});
  }
 }
 // Ornaments answering the lead lines: a phrase's last notes an octave up, from across the ring, in the breath before the next line.
 const leadPhrases=phrases.filter(p=>p.memberIndex===0);
 leadPhrases.forEach((ph,k)=>{
  const o=secAt(ph.t).k.ornament;if(!o?.gain)return;
  const next=leadPhrases[k+1];if(!next)return;
  const step=beat*.5,gap=next.t-(ph.t+ph.dur),count=Math.min(o.notes||3,ph.notes.length,Math.floor((gap-.1)/step));   // as many notes as the breath holds
  if(count<1)return;
  const seat=BOUNCE[(k+3)%BOUNCE.length],tail=ph.notes.slice(-count);
  tail.forEach((note,j)=>emit('ornament',`seat-${seat}`,{t:ph.t+ph.dur+j*step,dur:step*.9,note:note+12,frequency:hz(note+12),gain:g(levels.ornament*o.gain*(1-j*.15)),attack:.01,release:.2,wave:'sine'}));
 });
 // neo's PARs: per section a colour arc — verses breathe one lamp a bar around the ring, refrains chase the beat, the last refrain pulses all four.
 const cue=(t,address,rgb,attack,decay,d0)=>{const d=Math.max(.05,Math.min(d0,dur-t));emit('dmx','dmx',{t,dur:d,note:0,address,rgb,level:Math.max(...rgb),attack,release:decay,
  command:{address,color:'rgb',rgb,level:Math.max(...rgb),duration:+d.toFixed(3),envelope:{attack:+attack.toFixed(3),decay:+decay.toFixed(3)}}});};
 for(const sec of authored){
  const d=sec.k.dmx;if(!d)continue;
  const colors=d.colors,level=d.level??1;
  if(d.entry)for(const a of DMX_WALK)cue(sec.t,a,lamp(colors[0],level*1.15),.02,1.5,1.8);
  bars.filter(b=>b.sec===sec).forEach(b=>{
   const rgb=lamp(colors[b.i%colors.length],level),beats=Math.max(1,Math.round(b.len/beat));
   if(d.mode==='breathe')cue(b.t,DMX_WALK[b.i%4],rgb,b.len*.45,b.len*.45,b.len*.95);
   else if(d.mode==='chase')for(let q=0;q<beats;q++)cue(b.t+q*beat,DMX_WALK[(b.i*beats+q)%4],rgb,.04,beat*.7,beat*.85);
   else if(d.mode==='pulse')for(let q=0;q<beats;q++){
    if(q===0)for(const a of DMX_WALK)cue(b.t,a,rgb,.02,beat*1.2,beat*1.4);
    else for(const a of [DMX_WALK[q%4],DMX_WALK[(q+2)%4]])cue(b.t+q*beat,a,lamp(colors[b.i%colors.length],level*.8),.03,beat*.6,beat*.8);
   }
  });
 }
 // The held center's wedge: lit through a refrain, breathing once a bar (or once a beat) with the pulse.
 for(const b of bars){
  const l=b.sec.k.light;if(!l)continue;
  const rgb=l.rgb.map(v=>Math.max(0,Math.min(255,Math.round(v*(l.gain??.35)))));
  if(l.per==='beat'){const beats=Math.max(1,Math.round(b.len/beat));for(let q=0;q<beats;q++)emit('light','seat-5',{t:b.t+q*beat,dur:Math.min(beat*.8,dur-b.t-q*beat),rgb,cue:b.sec.name});}
  else emit('light','seat-5',{t:b.t,dur:b.len*.85,rgb,cue:b.sec.name});
 }
}
export function buildPlan(score,profiles,fleet,levels={}) {
 if(score.voices?.length!==3||!score.voices.some(v=>typeof v.lyrics==='string'&&v.lyrics.trim()))throw Error('Expected three member parts, at least one sung');
 const isSung=v=>typeof v.lyrics==='string'&&v.lyrics.trim().length>0;
 if(!Number.isFinite(score.bpm)||score.bpm<=0)throw Error('Invalid BPM');
 levels={voice:.35,echo1:.4,echo2:.2,harmony:.018,inst:.06,pad:.03,perc:.05,sub:.18,bed:.05,ornament:.07,lights:.28,mix:.25,...levels};
 if(Object.values(levels).some(x=>!Number.isFinite(x)||x<0||x>1))throw Error('Layer levels must be 0…1');
 if(levels.lights>.5)throw Error('The installed light bridge limits brightness to 128/255');
 const seats={'CENTER REAR':0,'RIGHT FRONT':1,'RIGHT REAR':2,'LEFT REAR':3,'LEFT FRONT':4,'HELD CENTER':5};
 const nodes=fleet.map(([host,port,label])=>({id:`seat-${seats[label]}`,seat:seats[label],host,port,label}));
 if(nodes.length!==6||new Set(nodes.map(n=>n.seat)).size!==6||nodes.some(n=>n.seat==null))throw Error('All six surround seats are required');
 const payloads=score.voices.map((voice,i)=>[voice,i]).filter(([voice])=>isSung(voice)).map(([voice,i])=>{
  const member=members[i];
  // a bare cast name ("Noelle") resolves to the member's Enhanced/Premium voice from its profile
  if(!/\((Enhanced|Premium)\)$/.test(voice.singVoice||'')){const bare=(voice.singVoice||profiles[member]?.aesthetivox?.base_voice||'').replace(/\s*\(.*\)$/,'');const full={Noelle:'Noelle (Enhanced)',Aaron:'Aaron (Enhanced)',Tom:'Tom (Enhanced)',Zoe:'Zoe (Premium)',Samantha:'Samantha (Enhanced)'}[bare];if(full)voice={...voice,singVoice:full};}
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
  if(!isSung(v))return;
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
 const entry=layerEntry(score),src0=layerSources(score),bar=src0.beatsPerBar*beat;
 const authored=Array.isArray(entry.sections)?authoredSections(entry,score.bpm,dur*score.bpm/60):null;
 const droneGain=t=>{if(!authored)return 1;let s=authored[0];for(const x of authored){if(t>=x.t-1e-4)s=x;else break;}return s.k.drone??1;};
 score.voices.forEach((v,i)=>{if(isSung(v))return;const seat=i===1?4:i===2?0:2;
  for(const n of parts[i])emit('drone',`seat-${seat}`,{t:n.t,dur:n.dur,note:n.note,frequency:hz(n.note),gain:Math.min(.25,.045*n.gain*droneGain(n.t)),attack:.3,release:.45,wave:'sine',name:members[i]+' drone'});});
 const addresses=[1,11,21,31];   // neo's PARs: left front, right front, left rear, right rear
 if(authored)authoredLayers({authored,parts,phrases,levels,beat,bar,dur,emit});
 else {
 // Harmony: a quiet sine a fifth above or an octave below each of neo's and
 // frisbee's notes, walking the ring, so the room hums the tune's shadow.
 let hk=0;
 for(const i of [0,2].filter(i=>isSung(score.voices[i])))for(const n of parts[i]){
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
 for(let b=0;b*bar<dur-.01;b++){
  const t=b*bar;
  emit('perc',`seat-${b%2?3:1}`,{t,dur:.35,note:33,frequency:55,gain:levels.perc,attack:.002,release:.3,wave:'sine',gmProgram:116,name:'taiko'});
  for(let q=1;q<src0.beatsPerBar;q++)emit('perc',`seat-${q%2?2:4}`,{t:t+q*beat,dur:.09,note:81,frequency:hz(81),gain:levels.perc*.45,attack:.001,release:.07,wave:'sine',gmProgram:115,name:'woodblock'});
  if(src0.beatsPerBar>2)emit('perc','seat-0',{t:t+(src0.beatsPerBar-1)*beat+beat*.5,dur:.06,note:0,frequency:8000,gain:.012,attack:.001,release:.05,wave:'noise',name:'brush'});
 }
 const src=src0;
 for(const e of parts[src.bass])emit('sub','sub',{t:e.t,dur:e.dur,note:e.note-12,frequency:440*2**((e.note-12-69)/12),gain:levels.sub*e.gain,attack:.015,release:.12,sourceNote:e.index,wave:'sine'});
 const barSeconds=src.beatsPerBar*60/score.bpm;
 for(let bar=0;bar*barSeconds<dur-.01;bar++) {
  const t=bar*barSeconds;
  const inner=parts[src.bed].find(e=>e.t>=t-1e-4&&e.t<t+barSeconds);
  if(inner)emit('bed','seat-0',{t,dur:Math.min(barSeconds*.92,dur-t),note:inner.note-12,frequency:440*2**((inner.note-12-69)/12),gain:levels.bed,attack:.2,release:.22,wave:'sine'});
  const lead=parts[src.ornament??(bar<4?0:src.ornamentLate)].find(e=>e.t>=t-1e-4&&e.t<t+barSeconds);
  if(lead&&bar%2===0)emit('ornament',`seat-${1+(bar/2)%4}`,{t:t+60/score.bpm*.5,dur:60/score.bpm*.65,note:lead.note+12,frequency:440*2**((lead.note+12-69)/12),gain:levels.ornament,attack:.01,release:.2,wave:'sine'});
 }
 const palette=[[143,209,63],[90,87,211],[242,167,185]];
 const musical=[...events].filter(e=>e.layer==='sub'||e.layer==='ornament');
 musical.forEach((e,i)=>{
  const address=addresses[i%4],rgb=palette[((e.note%3)+3)%3].map(v=>Math.round(v*levels.lights));
  emit('dmx','dmx',{t:e.t,dur:e.dur,note:e.note,sourceEvent:e.id,address,rgb,level:Math.max(...rgb),attack:e.attack,release:e.release,
   command:{address,color:'rgb',rgb,level:Math.max(...rgb),duration:e.dur,envelope:{attack:e.attack,decay:e.release}}});
 });
 }
 for(const e of events.filter(e=>e.layer==='voice'&&e.receiver==='seat-5'))
  emit('light','seat-5',{t:e.t,dur:e.dur,rgb:e.rgb.map(v=>Math.round(v*Math.min(1,e.gain/levels.voice))),sourceEvent:e.id});
 events.sort((a,b)=>a.t-b.t||a.id.localeCompare(b.id));
 const plan={schema:'trio-fleet-plan-v1',title:score.title,bpm:score.bpm,duration:dur,levels,payloads,nodes,events,
  requiredReceivers:[...members.map(m=>`singer-${m}`),...nodes.map(n=>n.id),'sub','dmx'],
  layers:src0,routes,colors:Object.fromEntries(members.map((m,i)=>[m,colors[i]])),
  lyrics:phrases.map(p=>({t:p.t,dur:p.dur,text:p.text,member:p.member,rgb:colors[p.memberIndex],role:p.role,...(p.answer?{answer:true}:{}),syllables:p.syllables})),
  sections:score.arrangement?.sections?score.arrangement.sections.map(s=>({name:s.name,beat:s.beat})):authored?authored.map(s=>({name:s.name,beat:s.beat,kind:s.kind})):[],
  arrangement:{total:score.arrangement?.total??entry.arrangement?.total??null,meter:score.arrangement?.meter??entry.arrangement?.meter??null},
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
