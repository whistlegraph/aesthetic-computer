import {writeFileSync,mkdirSync,readFileSync} from 'node:fs';
import {dirname,resolve} from 'node:path';
import {fileURLToPath} from 'node:url';
import {createHash} from 'node:crypto';
import {BPM,TITLE,LINES,SECTIONS,VOCAL_GAIN} from '../score.mjs';
import {instrumentEvents,MIX_DB,OFFSET,DURATION,beat} from '../arrangement.mjs';
const lane=resolve(dirname(fileURLToPath(import.meta.url)),'..');
const out=resolve(lane,'out/live');mkdirSync(out,{recursive:true});
const members=['neo','blueberry','frisbee'];
// dB into each machine's master stage (engine.c). 0 is the record's own chain; the record's
// nonlinearities acted on the summed mix, so a subset needs a push to feel as dense.
const MASTER_DRIVE_DB=3;
// Each Mac's system output volume (0–100), set before every cue so three identical speakers
// start from one level. The members' balance lives in VOCAL_GAIN / MIX_DB, not here.
const SYSTEM_VOLUME={neo:75,blueberry:75,frisbee:75};
// The screen as a lamp: brightness (0–1) while the member rests, when it sings full out, and
// the house level every screen returns to afterwards (set, not read back: with auto-brightness
// on, restoring a reading ratchets the panel darker on every run).
const BRIGHTNESS={floor:.22,lit:1,house:1};
// Kick / perc sidechain on every machine's melodic bus and voice, Menu Band's keyboard-kit
// numbers (MenuBandSynth.sidechainDuck): depth, hold (s) and floor per drum; a trigger's
// target gain = max(floor, 1 - depth*(0.55+0.45*velocity)). amount scales the depths.
const SIDECHAIN={amount:1,kick:{depth:.85,hold:.16,floor:.10},tap:{depth:.62,hold:.11,floor:.18}};
const owner={drums:'frisbee',bass:'blueberry',pad:'blueberry',keys:'neo',whistle:'neo'};
const types=['kick','tap','tick','bass','pad','keys','whistle'];
const voices={neo:'Noelle (Enhanced)',blueberry:'Allison (Enhanced)',frisbee:'Junior'};
const events=instrumentEvents();let seed=8;
for(const e of events) {
 e.seed=seed;
 const release={bass:.05,pad:.7,keys:.5,whistle:.12}[e.type]??0;
 const n=Math.round((e.duration+release)*48000);
 const noiseN=e.type==='kick'?192:['tap','tick','whistle'].includes(e.type)?n:0;
 for(let i=0;i<noiseN;i++)seed=(Math.imul(seed,1664525)+1013904223)>>>0;
}
const payloads={};
for(const m of members) {
 const lines=LINES.filter(l=>l.m===m).sort((a,b)=>a.at-b.at);
 let pos=0;const notes=[],lyrics=[],gains=[];
 for(const l of lines) {
  if(l.at<pos-1e-6)throw Error(`${m}: overlap at ${l.at}`);
  if(l.at>pos)notes.push(`r:${(l.at-pos).toFixed(9)}`);
  const ns=l.n.split(/\s+/);const pitched=ns.filter(t=>!t.startsWith('r:')).length;
  if(pitched!==l.w.split(/\s+/).flatMap(w=>w.split('-')).length)throw Error('Syllable mismatch');
  notes.push(...ns);lyrics.push(l.w);pos=l.at+ns.reduce((a,t)=>a+Number(t.split(':')[1]),0);
  const role=l.w==='hmm'?'hum':m;gains.push(.9*10**(VOCAL_GAIN[role]/20));
 }
 const profile=JSON.parse(readFileSync(resolve(lane,'../../grants/culturehub-la-2026/macneopolitan/members',m,'voice.json')));
 const p=profile.aesthetivox;
 payloads[m]={bpm:String(BPM),program:'78',notes:notes.join(','),lyrics:lyrics.join(' / '),
  singVoice:voices[m],singVibratoHz:String(p.sing.vibrato_hz),singVibCents:String(p.sing.vibrato_depth_cents),
  singLock:String(p.sing.harmony_lock),singF0Floor:String(p.f0_floor),singLineGains:gains.join(','),
  face:m,captionColor:profile.color,title:TITLE,gaze:'0'};
 const mine=events.filter(e=>owner[e.bus]===m);
 const header=[DURATION,SECTIONS.find(s=>s.id==='r1').start*beat-OFFSET,SECTIONS.find(s=>s.id==='bridge').start*beat-OFFSET,SECTIONS.find(s=>s.id==='r3').start*beat-OFFSET,SECTIONS.find(s=>s.id==='outro').start*beat-OFFSET].join(' ');
 const rows=mine.map(e=>[types.indexOf(e.type),(e.t-OFFSET).toFixed(9),e.duration,e.midi??0,e.velocity??1,
  e.gain*10**(MIX_DB[e.bus]/20),e.pan,e.frequency??0,e.seed].join(' '));
 if(mine.some(e=>e.t<OFFSET))throw Error('Instrument before start');
 const triggers=events.filter(e=>e.type==='kick'||e.type==='tap').sort((a,b)=>a.t-b.t).map(e=>{
  const d=SIDECHAIN[e.type],target=Math.max(d.floor,1-SIDECHAIN.amount*d.depth*(.55+.45*(e.velocity??1)));
  return ['7',(e.t-OFFSET).toFixed(9),d.hold,target.toFixed(6),0,0,0,0,0].join(' ');});
 writeFileSync(resolve(out,`${m}.tsv`),header+'\n'+rows.concat(triggers).join('\n')+'\n');
 writeFileSync(resolve(out,`${m}.payload.json`),JSON.stringify(payloads[m],null,2)+'\n');
 const pan={neo:0,blueberry:.38,frisbee:-.38}[m];
 const performance={member:m,duration:DURATION,offset:OFFSET,outputGain:1.9152126546696906,driveDb:MASTER_DRIVE_DB,payload:payloads[m],speechVoice:voices[m].replace(/ \(Enhanced\)/,''),
  peers:members.filter(x=>x!==m),beat,systemVolume:SYSTEM_VOLUME[m],brightness:BRIGHTNESS,
  lines:lines.map((l,i)=>({at:l.at*beat-OFFSET,text:l.w.replaceAll('-',''),words:l.w.split(/\s+/),syllables:l.w.split(/\s+/).flatMap(w=>w.split('-')),notation:l.n,
    gain:gains[i],pan:pan*(l.w==='hmm'?1.6:1)}))};
 writeFileSync(resolve(out,`${m}.performance.json`),JSON.stringify(performance,null,2)+'\n');
}
const sourceFiles=['score.mjs','arrangement.mjs','live/plan.mjs','live/instruments.c','live/conduct.py','live/post.m','live/engine.c','live/engine.h','live/performer.swift','live/build.sh'];
const hash=createHash('sha256');for(const f of sourceFiles)hash.update(readFileSync(resolve(lane,f)));
const plan={title:TITLE,duration:DURATION,bpm:BPM,members,owner,payloads,volumes:SYSTEM_VOLUME,brightness:BRIGHTNESS,sidechain:SIDECHAIN,hash:hash.digest('hex'),sourceFiles,
 events:events.length,counts:Object.fromEntries(members.map(m=>[m,events.filter(e=>owner[e.bus]===m).length]))};
writeFileSync(resolve(out,'plan.json'),JSON.stringify(plan,null,2)+'\n');
console.log(JSON.stringify({duration:DURATION,events:plan.events,counts:plan.counts}));
