// Offline config generation only. Upload/cue are separate explicit operations.
import fs from 'node:fs';import path from 'node:path';import crypto from 'node:crypto';import {fileURLToPath} from 'node:url';
const here=path.dirname(fileURLToPath(import.meta.url)),root=path.resolve(here,'../../..');
const built=path.resolve(process.argv[2]??path.join(root,'.tmp/femrag-spatial-stems'));
const score=JSON.parse(fs.readFileSync(path.join(here,'score.json'))),manifest=JSON.parse(fs.readFileSync(path.join(built,'manifest.json')));
if(manifest.masterBakedIn!==1)throw Error('Render unity-master stems first');
const sha=b=>crypto.createHash('sha256').update(b).digest('hex');
const code=fs.readFileSync(path.join(here,'native-femrag.mjs'));
const arrangementHash=sha(JSON.stringify({source:manifest.sourceScoreSha256,stems:manifest.stems,receiver:sha(code),master:.25,seatMap:[237,242,238,236,241,239]}));
const nodes=[237,242,238,236,241,239].map((ip,seat)=>({id:`seat-${seat}`,seat,host:`192.168.1.${ip}`,port:80,label:score.seats[seat].toUpperCase()}));
const roomAddresses=[1,11,31,21];
const events=[];
// Merge nearby notes into one gentle light envelope; bounded <=4 updates/sec/fixture.
for(let seat=0;seat<4;seat++){
 let last=-Infinity;
 for(const e of score.events.filter(e=>e.seat===seat))if(e.t-last>=.25){last=e.t;events.push({id:`femrag-light-${seat}-${events.length}`,layer:'dmx',receiver:'dmx',t:e.t,dur:.7,command:{address:roomAddresses[seat],color:'rgb',rgb:[90,42,8],level:90,duration:.7,envelope:{attack:.12,decay:.45}}})}
}
const plan={schema:'trio-fleet-plan-v1',title:'Femrag++ in the round',bpm:score.bpm,duration:score.duration,arrangementHash,nodes,payloads:[],events:events.sort((a,b)=>a.t-b.t),requiredReceivers:[...nodes.map(n=>n.id),'sub','dmx'],levels:{master:.25},dmx:{host:'192.168.1.235',port:8790,activeAddresses:roomAddresses,inactiveAddresses:[],heldCenter:41},sub:{transport:'sample-stem',file:'sub.wav',octaveShift:0},playbackHeld:true};
for(const node of nodes){
 const m=manifest.stems.find(s=>s.seat===node.seat),wave=fs.readFileSync(path.join(built,m.file));if(sha(wave)!==m.sha256)throw Error(`Stem mismatch ${node.id}`);
 const parts=Array.from({length:Math.ceil(wave.length/(4*1024*1024))},(_,i)=>`/pieces/trio-${m.sha256.slice(0,16)}-${i}.part`);
 const cues=score.events.filter(e=>e.seat===node.seat).map(e=>({t:e.t,dur:Math.max(.7,Math.min(2,e.duration)),rgb:[192,90,16]}));
 const config={schema:'trio-native-v1',title:plan.title,receiverId:node.id,seat:node.seat,arrangementHash,bpm:score.bpm,duration:score.duration,color:[240,156,60],events:[],heldCenter:node.seat===5,center:{file:`/pieces/trio-voices-${m.sha256.slice(0,16)}.wav`,parts,sha256:m.sha256,rawSha256:m.rawSha256,duration:m.frames/m.sampleRate,bytes:m.bytes,gainBakedIn:1},lightCues:node.seat===5?cues:[],noteCues:score.events.filter(e=>e.seat===node.seat).map(e=>({t:e.t,dur:e.duration,label:e.midi==null?e.sample.toUpperCase():['C','C#','D','D#','E','F','F#','G','G#','A','A#','B'][e.midi%12]+(Math.floor(e.midi/12)-1)}))};
 fs.writeFileSync(path.join(built,`${node.id}-config.json`),JSON.stringify(config));
}
fs.writeFileSync(path.join(built,'plan.json'),JSON.stringify(plan,null,2)+'\n');
const subStem=manifest.stems.find(s=>s.seat==='sub');
fs.writeFileSync(path.join(built,'sub-score.json'),JSON.stringify({hash:arrangementHash,
 name:plan.title,title:plan.title,dur:plan.duration,
 events:[{id:'stem',t:0,dur:plan.duration,hz:60,g:0}],
 stem:{url:'/femrag-sub.wav',sha256:subStem.sha256}},null,2)+'\n');
console.log(JSON.stringify({built,arrangementHash,nodes:6,roomLightCues:events.length,nativeSynthEvents:0,duration:score.duration}));
