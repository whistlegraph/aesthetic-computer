// Extract a bounded sample bank from the original instrument functions; no full render.
import fs from 'node:fs';
import vm from 'node:vm';
import path from 'node:path';
import crypto from 'node:crypto';
import { fileURLToPath } from 'node:url';
const here = path.dirname(fileURLToPath(import.meta.url));
const root = path.resolve(here, '../../..');
const sourcePath = 'pop/maytrax/bin/render-femrag-plusplus.mjs';
const eventPath = 'pop/maytrax/out/femrag-plusplus.events.json';
const src = fs.readFileSync(path.join(root, sourcePath), 'utf8');
const original = JSON.parse(fs.readFileSync(path.join(root, eventPath)));
function fn(name) {
  const start = src.indexOf(`function ${name}(`);
    // Source functions close at column zero; ignore destructured argument braces.
  const end = start + src.slice(start).search(/^}$/m) + 1;
  if (start < 0 || end < start) throw Error(`Missing ${name}`);
  return src.slice(start, end);
}
const assets = path.join(here, 'assets'); fs.mkdirSync(assets, { recursive: true });
function wav(samples) {
  const b=Buffer.alloc(44+samples.length*2);
  b.write('RIFF');b.writeUInt32LE(b.length-8,4);b.write('WAVEfmt ',8);b.writeUInt32LE(16,16);
  b.writeUInt16LE(1,20);b.writeUInt16LE(1,22);b.writeUInt32LE(48000,24);b.writeUInt32LE(96000,28);
  b.writeUInt16LE(2,32);b.writeUInt16LE(16,34);b.write('data',36);b.writeUInt32LE(samples.length*2,40);
  for(let i=0;i<samples.length;i++)b.writeInt16LE(Math.round(Math.max(-1,Math.min(1,samples[i]))*32767),44+i*2);
  return b;
}
const recipes = {
  bell: ['bell(0,60,1,0,0.8)',1.1], rbell:['reverseBell(1,60,1,0,0.75)',1.05],
  boom:['kick(0,1)',0.4],snare:['snare(0,1)',0.2],hat:['hat(0,1,false)',0.05],
  openhat:['hat(0,1,true)',0.2],sub:['sub(0,2,36,1)',2],throat:['throatBass(0,2,36,1)',2],
  donk:['sub(0,0.15,57,1,{drive:5,slideTo:45,attack:0.0015,release:0.05})',0.2],
  riser:['riser(0,2,1)',2.1],
};
const bank={};
for(const [name,[call,secs]] of Object.entries(recipes)) {
  const context={result:null};vm.createContext(context);
  vm.runInContext(`const SR=48000, NS=Math.ceil(${secs}*SR), TAU=Math.PI*2; const out=[new Float32Array(NS),new Float32Array(NS)];let voices=0;const EVENTS=[];let seed=0x9e3779b9;const rand=()=>{seed^=seed<<13;seed^=seed>>>17;seed^=seed<<5;seed>>>=0;return seed/0xffffffff*2-1};const hz=m=>440*2**((m-69)/12);\n${['oscillator','noiseBurst','sub','throatBass','bell','reverseBell','boom','kick','snare','hat','riser'].map(fn).join('\n')}\n${call};result=out;`,context,{timeout:5000});
  const mono=context.result[0].map((v,i)=>(v+context.result[1][i])*.5);
  const gainScale=Math.max(1,mono.reduce((peak,v)=>Math.max(peak,Math.abs(v)),0));
  for(let i=0;i<mono.length;i++)mono[i]/=gainScale;
  fs.writeFileSync(path.join(assets,`${name}.wav`),wav(mono));
  bank[name]={gainScale,url:`assets/${name}.wav`,baseMidi:['bell','rbell'].includes(name)?60:['sub','throat'].includes(name)?36:null};
}
fs.copyFileSync(path.join(root,'pop/teknull/samples/prutti-aesthetic-dot-computer.wav'),path.join(assets,'voice.wav'));
bank.voice={url:'assets/voice.wav',baseMidi:null};
const bar=240/original.bpm;
const sections=JSON.parse(fs.readFileSync(path.join(root,'pop/maytrax/out/femrag-plusplus.struct.json'))).sections;
const events=[];let note=0;
for(const e of original.events) {
  if(e.t<0 || e.t>=original.seconds)continue;
  // donk() records both donk and nested sub: keep one pitched donk only.
  if(e.i==='sub' && original.events.some(d=>d.i==='donk'&&Math.abs(d.t-e.t)<1e-7))continue;
  const section=sections.findIndex(s=>e.t>=s.startSec&&e.t<s.endSec);
  let seat;
  if(e.i==='sub'||e.i==='boom')seat='sub';
  else if(['voice','throat','donk','riser'].includes(e.i))seat=5;
  else if(e.i==='bell'||e.i==='rbell')seat=((section>=6?-note:note++)%5+5)%5;
  else seat=(Math.floor(e.t/bar)+(e.i==='hat'?2:0))%5;
  if(section>=6 && (e.i==='bell'||e.i==='rbell'))note++;
  events.push({t:e.t,seat,sample:e.i==='hat'&&e.open?'openhat':e.i,midi:e.midi??(e.i==='donk'?57:null),gain:(e.gain??0.1)*(e.i==='voice'?2:1),duration:e.dur??e.length??(e.i==='boom'?.36:.18),section});
}
events.sort((a,b)=>a.t-b.t);
const manifest={version:1,title:'Femrag++ in the round — sample study',bpm:original.bpm,duration:original.seconds,master:0.25,source:{renderer:sourcePath,events:eventPath,eventSha256:crypto.createHash('sha256').update(fs.readFileSync(path.join(root,eventPath))).digest('hex')},seats:['left front','right front','right rear','left rear','center rear','held center'],samples:bank,sections,events,dmx:{addresses:[1,11,31,21,null,41],profile:'candlelight',roomCeiling:96,centerCeiling:192,attack:0.12,release:0.45,noBlackBetweenHits:true}};
fs.writeFileSync(path.join(here,'score.json'),JSON.stringify(manifest,null,2)+'\n');
console.log(JSON.stringify({events:events.length,duration:manifest.duration,samples:Object.keys(bank).length,bytes:fs.readdirSync(assets).reduce((n,f)=>n+fs.statSync(path.join(assets,f)).size,0)}));
