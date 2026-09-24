// Optional offline PCM stem bridge. Does not contact or play on the fleet.
import fs from 'node:fs';
import path from 'node:path';
import crypto from 'node:crypto';
import {fileURLToPath} from 'node:url';
const here=path.dirname(fileURLToPath(import.meta.url));
const root=path.resolve(here,'../../..');
const score=JSON.parse(fs.readFileSync(path.join(here,'score.json')));
const out=path.resolve(process.argv[2]??path.join(root,'.tmp/femrag-spatial-stems'));
fs.mkdirSync(out,{recursive:true});
const sr=48000,n=Math.ceil(score.duration*sr),samples={};
for(const [name,spec] of Object.entries(score.samples)){
 const b=fs.readFileSync(path.join(here,spec.url));let p=12;
 while(p+8<b.length){const length=b.readUInt32LE(p+4);if(b.toString('ascii',p,p+4)==='data'){const data=new Float32Array(length/2);for(let i=0;i<data.length;i++)data[i]=b.readInt16LE(p+8+i*2)/32768;samples[name]=data;break}p+=8+length+(length%2)}
 if(!samples[name])throw Error(`Missing PCM ${name}`);
}
const receipt={format:'PCM16 mono WAV',sampleRate:sr,duration:score.duration,masterBakedIn:1,playbackMaster:score.master,nativeTransport:"deck",sourceScoreSha256:crypto.createHash('sha256').update(fs.readFileSync(path.join(here,'score.json'))).digest('hex'),stems:[]};
for(const seat of [0,1,2,3,4,5,'sub']){
 const mix=new Float32Array(n);
 for(const e of score.events.filter(e=>e.seat===seat)){
  const data=samples[e.sample],spec=score.samples[e.sample];
  const rate=spec.baseMidi!=null&&e.midi!=null?2**((e.midi-spec.baseMidi)/12):1;
  const span=Math.min(e.duration,data.length/sr/rate),length=Math.floor(span*sr),start=Math.round(e.t*sr);
  const scale=e.gain*(spec.gainScale??1);
  for(let i=0;i<length&&start+i<n;i++){
   const f=i*rate,j=Math.floor(f),v=(data[j]??0)*(1-f+j)+(data[j+1]??0)*(f-j);
   const env=Math.min(1,i/(sr*.006),(length-i)/(sr*.04));mix[start+i]+=v*scale*Math.max(0,env);
  }
 }
 if(seat==='sub'){
  // Bilinear first-order 25Hz high-pass and two 80Hz low-pass stages.
  const h=Math.tan(Math.PI*25/sr),l=Math.tan(Math.PI*80/sr);let xp=0,hp=0,l1=0,l2=0,p1=0,p2=0;
  for(let i=0;i<n;i++){const x=mix[i],y=(x-xp+(1-h)*hp)/(1+h);xp=x;hp=y;const a=(l*(y+p1)+(1-l)*l1)/(1+l);p1=y;l1=a;const b=(l*(a+p2)+(1-l)*l2)/(1+l);p2=a;l2=b;mix[i]=b}
 }
 let peak=0;for(const v of mix)peak=Math.max(peak,Math.abs(v));if(peak>1)throw Error(`Clipping on ${seat}: ${peak}`);
 const b=Buffer.alloc(44+n*2);b.write('RIFF');b.writeUInt32LE(b.length-8,4);b.write('WAVEfmt ',8);b.writeUInt32LE(16,16);b.writeUInt16LE(1,20);b.writeUInt16LE(1,22);b.writeUInt32LE(sr,24);b.writeUInt32LE(sr*2,28);b.writeUInt16LE(2,32);b.writeUInt16LE(16,34);b.write('data',36);b.writeUInt32LE(n*2,40);
 for(let i=0;i<n;i++)b.writeInt16LE(Math.round(mix[i]*32767),44+i*2);
 const name=`${seat==='sub'?'sub':`seat-${seat+1}`}.wav`;fs.writeFileSync(path.join(out,name),b);
 receipt.stems.push({seat,file:name,peak,frames:n,sampleRate:sr,rawSha256:crypto.createHash('sha256').update(b.subarray(44)).digest('hex'),bytes:b.length,sha256:crypto.createHash('sha256').update(b).digest('hex')});
}
fs.writeFileSync(path.join(out,'manifest.json'),JSON.stringify(receipt,null,2)+'\n');console.log(JSON.stringify({out,stems:receipt.stems.length,bytes:receipt.stems.reduce((s,x)=>s+x.bytes,0),maxPeak:Math.max(...receipt.stems.map(s=>s.peak))}));
