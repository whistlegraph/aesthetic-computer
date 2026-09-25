#!/usr/bin/env node
// announce.mjs — a line in Jeffrey's voice, as room audio.
//
// Renders `text` through aesthetic.computer's /api/say (provider "jeffrey",
// the ElevenLabs voice the readings use), caches the MP3 by text hash under
// ~/Shelf/announce-cache/, and decodes it to 44.1 kHz mono float32 raw for
// the seat stems. Prints JSON {text, mp3, f32, seconds, hash}.
//
//   node bin/announce.mjs "Good morning, Sophia."
import {createHash} from 'node:crypto';
import {existsSync,mkdirSync,readFileSync,writeFileSync,statSync} from 'node:fs';
import {join} from 'node:path';
import {homedir} from 'node:os';
import {spawnSync} from 'node:child_process';
export async function announce(text,{voice='neutral:0',speed=1.0}={}) {
 const dir=join(homedir(),'Shelf','announce-cache');mkdirSync(dir,{recursive:true});
 const body={from:text,provider:'jeffrey',voice,...(speed!==1?{speed}:{})};
 const hash=createHash('sha256').update(JSON.stringify(body)).digest('hex').slice(0,16);
 const mp3=join(dir,`${hash}.mp3`),f32=join(dir,`${hash}.f32`);
 if(!existsSync(mp3)||statSync(mp3).size<1000){
  const res=await fetch('https://aesthetic.computer/api/say',{method:'POST',headers:{'Content-Type':'application/json'},body:JSON.stringify(body),redirect:'follow'});
  if(!res.ok)throw Error(`/api/say ${res.status}: ${(await res.text()).slice(0,200)}`);
  writeFileSync(mp3,Buffer.from(await res.arrayBuffer()));
 }
 if(!existsSync(f32)){
  const r=spawnSync('ffmpeg',['-hide_banner','-loglevel','error','-y','-i',mp3,'-ac','1','-ar','44100','-f','f32le',f32]);
  if(r.status!==0)throw Error('ffmpeg failed: '+r.stderr);
 }
 const seconds=statSync(f32).size/4/44100;
 return {text,mp3,f32,seconds:+seconds.toFixed(3),hash};
}
if(process.argv[1]&&process.argv[1].endsWith('announce.mjs')&&process.argv[2]){
 announce(process.argv.slice(2).join(' ')).then(r=>console.log(JSON.stringify(r))).catch(e=>{console.error(String(e.message||e));process.exit(1);});
}
