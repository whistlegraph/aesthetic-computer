// Portable compositional Pop lane. No provider, samples, or executable score code.
import { readFile, writeFile, mkdir, lstat, rename, unlink } from 'node:fs/promises';
import { join } from 'node:path';
import { createHash, randomUUID } from 'node:crypto';
import { mixEventSinePower } from '../../media/sound/pop/sinepower.mjs';
import { applyWobble, applyBitcrush } from '../../media/sound/pop/fx.mjs';
import { bjorklund } from '../../media/sound/pop/necklace.mjs';
import { readWavMono } from '../../media/sound/pop/wav.mjs';
export const kind = 'sound';
const RATE = 24000;
const OUTPUTS = ['score.json', 'sound.wav', 'analysis.json', 'provenance.json'];
const number = (x, lo, hi, label) => { if (!Number.isFinite(x) || x < lo || x > hi) throw new Error(`${label} must be ${lo}..${hi}`); return x; };
const integer = (x,lo,hi,label) => { number(x,lo,hi,label); if(!Number.isInteger(x)) throw new Error(`${label} must be integer`); return x; };
export function validateScore(raw) {
  if (!raw || typeof raw !== 'object' || Array.isArray(raw)) throw new Error('score must be an object');
  const bpm = number(raw.bpm ?? 100, 30, 300, 'bpm');
  const beats = number(raw.beats ?? 8, .25, 64, 'beats');
  if (beats * 60 / bpm > 30) throw new Error('Sound clips are limited to 30 seconds');
  const preset = raw.preset ?? 'stab';
  if (!['lead', 'pad', 'stab'].includes(preset)) throw new Error('preset must be lead, pad, or stab');
  const instrument = raw.instrument ?? 'sinepower';
  if (instrument !== 'sinepower') throw new Error('Available instrument: sinepower');
  const loop = raw.loop ?? false;
  if(typeof loop !== 'boolean') throw new Error('loop must be boolean');
  if (!Array.isArray(raw.notes) || raw.notes.length > 128) throw new Error('notes must be an array of at most 128 events');
  let work = 0;
  const notes = raw.notes.map(n => {
    if(!n || typeof n !== 'object') throw new Error('Invalid note');
    const at = number(n.at, 0, beats, 'note.at');
    const duration = number(n.duration ?? .5, .01, beats, 'note.duration');
    if(at + duration > beats) throw new Error('Note extends beyond score beats');
    work += Math.max(duration * 60 / bpm, 2.2);
    return { at, duration, midi: integer(n.midi,24,96,'note.midi'), gain:number(n.gain ?? .3,0,1,'note.gain') };
  });
  if(work > 180) throw new Error('Too much overlapping audio; shorten or reduce notes');
  const fx = raw.effects ?? [];
  if(!Array.isArray(fx) || fx.length > 4) throw new Error('At most 4 effects');
  const effects = fx.map(e => {
    if(e?.type === 'wobble') return {type:e.type,rate:number(e.rate??2,.1,20,'wobble rate'),depth:number(e.depth??.6,0,1,'wobble depth')};
    if(e?.type === 'bitcrush') return {type:e.type,bits:integer(e.bits??8,2,16,'bits'),downsample:integer(e.downsample??2,1,32,'downsample'),mix:number(e.mix??.5,0,1,'mix')};
    throw new Error('Available effects: wobble, bitcrush');
  });
  return {format:1,bpm,beats,instrument,preset,loop,notes,effects};
}
export function renderScore(raw) {
  const score=validateScore(raw), length=Math.round(score.beats*60/score.bpm*RATE);
  const out=new Float32Array(length), tail = score.preset === 'pad' ? 2.2 : .5;
  // Wrap release tails into the start for a loop; preserve exact loop duration.
  const working = new Float32Array(length + Math.ceil(tail * RATE));
  for(const n of score.notes) mixEventSinePower({startSec:n.at*60/score.bpm,durSec:n.duration*60/score.bpm,midi:n.midi,gain:n.gain},working,{sampleRate:RATE,preset:score.preset});
  out.set(working.subarray(0,length));
  if(score.loop) for(let i=length;i<working.length;i++) out[i%length]+=working[i];
  for(const e of score.effects) {
    if(e.type==='wobble') applyWobble(out,{...e,target:'amp',sampleRate:RATE});
    else applyBitcrush(out,{...e,sampleRate:RATE});
  }
  let peak=0; for(const s of out) {if(!Number.isFinite(s)) throw new Error('Non-finite audio'); peak=Math.max(peak,Math.abs(s));}
  const attenuation=peak>.95?.95/peak:1;
  for(let i=0;i<out.length;i++) out[i]*=attenuation;
  if(!score.loop) {const fade=Math.min(240,out.length>>1); for(let i=0;i<fade;i++) {out[i]*=i/fade;out[out.length-1-i]*=i/fade;}}
  const wav=Buffer.alloc(44+out.length*2);
  wav.write('RIFF');wav.writeUInt32LE(wav.length-8,4);wav.write('WAVEfmt ',8);wav.writeUInt32LE(16,16);wav.writeUInt16LE(1,20);wav.writeUInt16LE(1,22);wav.writeUInt32LE(RATE,24);wav.writeUInt32LE(RATE*2,28);wav.writeUInt16LE(2,32);wav.writeUInt16LE(16,34);wav.write('data',36);wav.writeUInt32LE(out.length*2,40);
  for(let i=0;i<out.length;i++) wav.writeInt16LE(Math.round(out[i]*32767),44+i*2);
  return {score,wav,attenuation};
}
function analyze(samples,sampleRate) {
  let peak=0,sum=0;const waveform=[];const chunk=Math.max(1,Math.ceil(samples.length/256));
  for(let i=0;i<samples.length;i+=chunk){let min=1,max=-1;for(let j=i;j<Math.min(i+chunk,samples.length);j++){const s=samples[j];peak=Math.max(peak,Math.abs(s));sum+=s*s;min=Math.min(min,s);max=Math.max(max,s);}waveform.push([min,max]);}
  return {sampleRate,channels:1,frames:samples.length,duration:samples.length/sampleRate,peak,rms:Math.sqrt(sum/Math.max(1,samples.length)),waveform};
}
async function prepare(root) {
  await mkdir(root,{recursive:true});
  for(const file of OUTPUTS) {try{const stat=await lstat(join(root,file));if(!stat.isFile() || stat.isSymbolicLink()) throw new Error(`Unsafe artifact path: ${file}`);}catch(e){if(e.code!=='ENOENT')throw e;}}
}
async function atomic(root,file,bytes) {const tmp=join(root,`.${file}.${randomUUID()}.tmp`);try{await writeFile(tmp,bytes,{flag:'wx'});await rename(tmp,join(root,file));}finally{await unlink(tmp).catch(()=>{});}}
async function load(root) {await prepare(root);const bytes=await readFile(join(root,'score.json'));if(bytes.length>65536)throw new Error('Score exceeds 64 KiB');return validateScore(JSON.parse(bytes));}
async function save(root,raw) {
  const {score,wav,attenuation}=renderScore(raw);await prepare(root);
  const provenance=JSON.parse(await readFile(new URL('../../media/sound/provenance.json',import.meta.url),'utf8'));
  const scoreBytes=JSON.stringify(score,null,2)+'\n';
  provenance.render={sampleRate:RATE,pcm:'mono s16le',scoreSha256:createHash('sha256').update(scoreBytes).digest('hex'),wavSha256:createHash('sha256').update(wav).digest('hex'),attenuation};
  await atomic(root,'sound.wav',wav);
  const audio=readWavMono(join(root,'sound.wav'));
  const analysis=analyze(audio.samples,audio.sampleRate);
  await atomic(root,'analysis.json',JSON.stringify(analysis)+'\n');
  await atomic(root,'provenance.json',JSON.stringify(provenance,null,2)+'\n');
  await atomic(root,'score.json',scoreBytes);
  return {files:OUTPUTS,preview:{path:'sound.wav',mime:'audio/wav'},summary:`${score.notes.length} notes · ${analysis.duration.toFixed(2)}s · ${score.preset}${score.loop?' loop':''}`,analysis};
}
const noteSchema={type:'object',properties:{at:{type:'number'},midi:{type:'integer'},duration:{type:'number'},gain:{type:'number'}},required:['at','midi'],additionalProperties:false};
export const actions=[
  {name:'set_score',description:'Compose and render a short Pop sinepower phrase. Beats are quarter notes; MIDI 24–96, bpm 30–300, at most 30 seconds and 128 notes. Presets lead/pad/stab. Optional wobble or bitcrush effects.',inputSchema:{type:'object',properties:{score:{type:'object',properties:{bpm:{type:'number'},beats:{type:'number'},preset:{type:'string',enum:['lead','pad','stab']},loop:{type:'boolean'},notes:{type:'array',items:noteSchema},effects:{type:'array',items:{type:'object'}}},required:['notes']}},required:['score'],additionalProperties:false}},
  {name:'render',description:'Render current editable score.json into sound.wav.',inputSchema:{type:'object',properties:{},additionalProperties:false}},
  {name:'rhythm',description:'Replace notes with a maximally even Pop Bjorklund rhythm; preserve tempo, timbre and effects.',inputSchema:{type:'object',properties:{pulses:{type:'integer',minimum:1,maximum:64},hits:{type:'integer',minimum:0,maximum:64},midi:{type:'integer',minimum:24,maximum:96}},required:['pulses','hits'],additionalProperties:false}},
  {name:'analyze',description:'Measure the saved WAV: peak, RMS, duration and waveform. Does not regenerate audio.',inputSchema:{type:'object',properties:{},additionalProperties:false}},
];
export async function create({root}) {return save(root,{bpm:100,beats:8,preset:'stab',notes:[60,64,67,72].map((midi,i)=>({at:i*1.5,midi,duration:1,gain:.3}))});}
export async function run({root,action,input={}}) {
  if(action==='set_score') return save(root,input.score);
  if(action==='render') return save(root,await load(root));
  if(action==='rhythm') {const score=await load(root);const pulses=integer(input.pulses,1,64,'pulses'),hits=integer(input.hits,0,pulses,'hits'),midi=integer(input.midi??60,24,96,'midi');score.notes=bjorklund(hits,pulses).onsets.map(i=>({at:i*score.beats/pulses,duration:score.beats/pulses*.5,midi,gain:.3}));score.loop=true;return save(root,score);}
  if(action==='analyze') {await prepare(root);const stat=await lstat(join(root,'sound.wav'));if(stat.size>RATE*30*2+44)throw new Error('WAV exceeds sound budget');const wav=await readFile(join(root,'sound.wav'));if(wav.length<44 || wav.toString('ascii',0,4)!=='RIFF' || wav.toString('ascii',8,16)!=='WAVEfmt ' || wav.readUInt32LE(16)!==16 || wav.readUInt16LE(20)!==1 || wav.readUInt16LE(22)!==1 || wav.readUInt32LE(24)!==RATE || wav.readUInt16LE(34)!==16 || wav.toString('ascii',36,40)!=='data' || wav.readUInt32LE(40)!==wav.length-44 || (wav.length-44)%2)throw new Error('Expected an Easel mono PCM WAV; render score.json to repair');const audio=readWavMono(join(root,'sound.wav'));const analysis=analyze(audio.samples,audio.sampleRate);await atomic(root,'analysis.json',JSON.stringify(analysis)+'\n');return{files:OUTPUTS,preview:{path:'sound.wav',mime:'audio/wav'},summary:`${analysis.duration.toFixed(2)}s · peak ${analysis.peak.toFixed(3)}`,analysis};}
  throw new Error(`Unknown sound action: ${action}`);
}
