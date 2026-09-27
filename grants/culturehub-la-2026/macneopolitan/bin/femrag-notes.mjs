#!/usr/bin/env node
// femrag-notes.mjs — per-seat incoming-notes feeds for Femrag++ in the round.
//
// Reads the track's own timing receipt (pop/maytrax render → .events.json and
// .struct.json; its clock runs from -20 s, the audio from 0) and, for each
// seat, marks as `mine` the notes that seat's stem actually carries — matched
// against the cues blueberry staged with the stems (label + time). Writes
// notes/seat-N.json beside the receipts and, with --stage, merges `notes`,
// `sections`, `beatsPerBar` and pitch range into each seat's live
// trio-fleet-config.json and jumps it (stems and everything else untouched).
//
//   node bin/femrag-notes.mjs [--out=/Users/jas/Shelf/femrag-spatial] [--stage]
import {readFileSync,writeFileSync,mkdirSync,existsSync} from 'node:fs';
import {resolve,join} from 'node:path';
const args=Object.fromEntries(process.argv.slice(2).map(a=>{const[k,v]=a.replace(/^--/,'').split('=');return[k,v??true];}));
const OUT=resolve(args.out??'/Users/jas/Shelf/femrag-spatial'),OFFSET=20;
const receipt=JSON.parse(readFileSync(join(OUT,'femrag-plusplus.events.json')));
const struct=JSON.parse(readFileSync(join(OUT,'femrag-plusplus.struct.json')));
const plan=JSON.parse(readFileSync(join(OUT,'plan.json')));
const NAMES=['C','C#','D','D#','E','F','F#','G','G#','A','A#','B'];
const label=e=>e.midi!=null?NAMES[((e.midi%12)+12)%12]+String(Math.floor(e.midi/12)-1):({snare:'SNARE',hat:e.open?'OPENHAT':'HAT',kick:'KICK',boom:'BOOM',donk:'DONK',riser:'RISER',voice:'VOICE'})[e.i]||e.i.toUpperCase();
const notes=receipt.events.map(e=>({i:e.i,t:+(e.t+OFFSET).toFixed(4),dur:+(e.dur??e.length??.15).toFixed(4),midi:e.midi??null,gain:e.gain??.1,pan:e.pan??0,label:label(e),...(e.words?{text:e.words}:{})})).filter(n=>n.t>=0).sort((a,b)=>a.t-b.t);
const sections=struct.sections.map(s=>({name:s.name,startSec:s.startSec+OFFSET,endSec:s.endSec+OFFSET}));
sections.unshift({name:'intro',startSec:0,endSec:sections[0].startSec});
const out=join(OUT,'notes');mkdirSync(out,{recursive:true});
const configs=join(OUT,'blueberry-configs');
for(const node of plan.nodes){
 const seat=node.seat,cfgPath=join(configs,`seat-${seat}-config.json`);
 const cues=existsSync(cfgPath)?JSON.parse(readFileSync(cfgPath)).noteCues??[]:[];
 const byLabel=new Map();for(const c of cues){const k=c.label;if(!byLabel.has(k))byLabel.set(k,[]);byLabel.get(k).push(c.t);}
 for(const l of byLabel.values())l.sort((a,b)=>a-b);
 let mineCount=0;
 const seatNotes=notes.map(n=>{const ts=byLabel.get(n.label)||[];let lo=0,hi=ts.length;while(lo<hi){const m=(lo+hi)>>1;if(ts[m]<n.t-.03)lo=m+1;else hi=m;}const mine=ts[lo]!=null&&Math.abs(ts[lo]-n.t)<=.03;if(mine)mineCount++;return mine?{...n,mine:true}:n;});
 const feed={receiverId:node.id,seat,notes:seatNotes,sections,beatsPerBar:4,midiLow:30,midiHigh:110,title:plan.title};
 writeFileSync(join(out,`${node.id}.json`),JSON.stringify(feed));
 console.log(`${node.id}: ${seatNotes.length} notes, ${mineCount} mine (from ${cues.length} staged cues)`);
}
if(args.stage){
 for(const node of plan.nodes){
  const url=`http://${node.host}:${node.port}`,feed=JSON.parse(readFileSync(join(out,`${node.id}.json`)));
  const cfg=await (await fetch(`${url}/pieces/trio-fleet-config.json`)).json();
  if(cfg.arrangementHash!==plan.arrangementHash)throw Error(`${node.id}: seat holds ${cfg.arrangementHash?.slice(0,8)}, plan is ${plan.arrangementHash.slice(0,8)}`);
  Object.assign(cfg,{notes:feed.notes,sections:feed.sections,beatsPerBar:feed.beatsPerBar,midiLow:feed.midiLow,midiHigh:feed.midiHigh,title:feed.title});
  const r=await fetch(`${url}/pieces/trio-fleet-config.json`,{method:'PUT',body:JSON.stringify(cfg)});if(!r.ok)throw Error(`${node.id}: config PUT ${r.status}`);
  await fetch(`${url}/pieces/trio-fleet-command.json`,{method:'PUT',body:JSON.stringify({id:'notes-idle-'+Date.now(),action:'idle'})});
  await fetch(`${url}/jump/trio-fleet`,{method:'PUT',body:''});
  console.log(`${node.id}: notes merged, jumped`);
 }
 console.log('Notation staged on the six seats; stems untouched; no cue.');
}
