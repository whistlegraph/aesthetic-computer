import {readFileSync,writeFileSync,mkdirSync} from 'node:fs';
import {join} from 'node:path';
import {createHash} from 'node:crypto';
const input=process.argv[2],out=process.argv[3];if(!input||!out)throw Error('build.mjs input-folder output-folder');mkdirSync(out,{recursive:true});
const prepared=JSON.parse(readFileSync(join(input,'prepared-source.json'))),plan=JSON.parse(readFileSync(join(input,'plan-source.json')));
const sr=44100,frames=Math.ceil(plan.duration*sr),stems=Array.from({length:6},()=>new Float32Array(frames)),routes=[];
const sha=b=>createHash('sha256').update(b).digest('hex');
const phrases=prepared.singers.flatMap(s=>s.phrases.map(p=>({...p,member:s.member}))).sort((a,b)=>a.spanOffset-b.spanOffset);
for(let k=0;k<phrases.length;k++){
 const p=phrases[k],raw=readFileSync(join(input,'assets',p.member,p.rawFile));if(sha(raw)!==p.rawSha256)throw Error('Phrase checksum mismatch');
 // One primary phrase position, then two softer echoes around the physical ring.
 const route=[0,1,2,4,3,5];
 const destinations=[{seat:route[k%6],delay:0,gain:.35},{seat:route[(k+1)%6],delay:60/plan.bpm*.5,gain:.35*.4},{seat:route[(k+2)%6],delay:60/plan.bpm,gain:.35*.2}];
 for(const d of destinations){const offset=Math.round((p.spanOffset+d.delay)*sr);routes.push({member:p.member,phrase:p.index,offsetSeconds:offset/sr,duration:Math.min(p.duration,plan.duration-offset/sr),...d});
  for(let n=0;n<p.frames&&offset+n<frames;n++)stems[d.seat][offset+n]+=raw.readFloatLE(n*4)*d.gain;
 }
}
const harmony=[];let sequence=0;
for(const payload of plan.payloads){let beat=0;for(const token of payload.info.notes.split(',')){const [pitch,len]=token.split(':'),beats=Number(len);if(pitch!=='r'){
 const note=Number(pitch)-12,t=beat*60/plan.bpm,dur=Math.min(beats*60/plan.bpm*.85,plan.duration-t);if(dur>.04&&note>=36)harmony.push({id:`sine-harmony-${sequence}`,layer:'bed',receiver:`seat-${sequence++%6}`,t,dur,note,frequency:440*2**((note-69)/12),gain:.018,attack:.08,release:.18,wave:'sine'});
 if(dur>.04&&note>=36)harmony.push({id:`sine-fifth-${sequence}`,layer:'bed',receiver:`seat-${sequence++%6}`,t,dur,note:note+7,frequency:440*2**((note+7-69)/12),gain:.009,attack:.12,release:.2,wave:'sine'});
 }beat+=beats;}}
const manifest=[];
for(let seat=0;seat<6;seat++){
 const a=stems[seat];let peak=0,energy=0;for(let i=0;i<a.length;i++){a[i]*=Math.min(1,(a.length-1-i)/(sr*.35));if(!Number.isFinite(a[i]))throw Error('Nonfinite PCM');peak=Math.max(peak,Math.abs(a[i]));energy+=a[i]*a[i];}
 if(peak>1)throw Error('Stem exceeds unity');const raw=Buffer.from(a.buffer),wav=Buffer.alloc(44+raw.length);wav.write('RIFF',0);wav.writeUInt32LE(36+raw.length,4);wav.write('WAVEfmt ',8);wav.writeUInt32LE(16,16);wav.writeUInt16LE(3,20);wav.writeUInt16LE(1,22);wav.writeUInt32LE(sr,24);wav.writeUInt32LE(sr*4,28);wav.writeUInt16LE(4,32);wav.writeUInt16LE(32,34);wav.write('data',36);wav.writeUInt32LE(raw.length,40);raw.copy(wav,44);
 const file=`seat-${seat}.wav`;writeFileSync(join(out,file),wav);writeFileSync(join(out,`seat-${seat}.f32`),raw);manifest.push({seat,file,rawFile:`seat-${seat}.f32`,sha256:sha(wav),rawSha256:sha(raw),frames,sampleRate:sr,peak,rms:Math.sqrt(energy/frames)});
}
const colors={neo:[242,167,185],blueberry:[143,209,63],frisbee:[90,87,211]},addresses={0:1,1:11,2:31,3:21};
for(let i=0;i<routes.length;i++){const r=routes[i];if(r.duration<=0||!addresses[r.seat])continue;
 const rgb=colors[r.member].map(v=>Math.round(v/255*85*r.gain/.35));
 plan.events.push({id:`voice-light-${i}`,layer:'dmx',receiver:'dmx',t:r.offsetSeconds,dur:r.duration,command:{address:addresses[r.seat],color:'rgb',rgb,level:85,duration:r.duration,envelope:{attack:.12,decay:.3}}});
}
const hash=sha(JSON.stringify({base:plan.arrangementHash,manifest,harmony,routes,lights:plan.events.filter(e=>e.layer==='dmx')}));
plan.arrangementHash=hash;plan.events.push(...harmony);plan.events.sort((a,b)=>a.t-b.t);plan.echo={routes,manifest};
writeFileSync(join(out,'plan.json'),JSON.stringify(plan,null,2));writeFileSync(join(out,'manifest.json'),JSON.stringify({hash,manifest,routes,harmony},null,2));console.log(JSON.stringify({hash,phrases:phrases.length,harmonies:harmony.length,stems:manifest.map(({seat,peak,rms})=>({seat,peak,rms}))}));
