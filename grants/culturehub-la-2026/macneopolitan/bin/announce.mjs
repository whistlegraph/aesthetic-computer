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
// `sing`: a melody (MIDI notes) — the spoken line is lifted onto it with WORLD
// f0 replacement on neo (bin/sing-proof.py under pop/.venv; the envelope stays
// Jeffrey's, so it is his voice singing the title as the piece begins).
export async function announce(text,{voice='neutral:0',speed=1.0,sing=null}={}) {
 const dir=join(homedir(),'Shelf','announce-cache');mkdirSync(dir,{recursive:true});
 const body={from:text,provider:'jeffrey',voice,...(speed!==1?{speed}:{})};
 const spokenHash=createHash('sha256').update(JSON.stringify(body)).digest('hex').slice(0,16);
 const hash=sing?createHash('sha256').update(spokenHash+'|'+sing.join(',')).digest('hex').slice(0,16):spokenHash;
 const mp3=join(dir,`${spokenHash}.mp3`),f32=join(dir,`${hash}.f32`);
 if(!existsSync(mp3)||statSync(mp3).size<1000){
  const res=await fetch('https://aesthetic.computer/api/say',{method:'POST',headers:{'Content-Type':'application/json'},body:JSON.stringify(body),redirect:'follow'});
  if(!res.ok)throw Error(`/api/say ${res.status}: ${(await res.text()).slice(0,200)}`);
  writeFileSync(mp3,Buffer.from(await res.arrayBuffer()));
 }
 if(!existsSync(f32)&&sing){
  const wav=join(dir,`${spokenHash}.wav`),sung=join(dir,`${hash}.sung.wav`);
  let r=spawnSync('ffmpeg',['-hide_banner','-loglevel','error','-y','-i',mp3,'-ac','1','-ar','44100','-sample_fmt','s16',wav]);if(r.status!==0)throw Error('ffmpeg failed: '+r.stderr);
  r=spawnSync('scp',['-q',wav,`neo:/tmp/announce-${spokenHash}.wav`]);if(r.status!==0)throw Error('scp to neo failed');
  const cmd=`cd ~/aesthetic-computer/grants/culturehub-la-2026/macneopolitan && ../../../pop/.venv/bin/python bin/sing-proof.py /tmp/announce-${spokenHash}.wav /tmp/announce-${hash}.sung.wav ${sing.map(n=>String(n)).join(' ')}`;
  r=spawnSync('ssh',['-o','BatchMode=yes','neo',`bash -lc ${JSON.stringify(cmd)}`],{encoding:'utf8'});if(r.status!==0)throw Error('sing-proof on neo failed: '+(r.stderr||r.stdout).slice(-300));
  r=spawnSync('scp',['-q',`neo:/tmp/announce-${hash}.sung.wav`,sung]);if(r.status!==0)throw Error('scp from neo failed');
  r=spawnSync('ffmpeg',['-hide_banner','-loglevel','error','-y','-i',sung,'-ac','1','-ar','44100','-f','f32le',f32]);if(r.status!==0)throw Error('ffmpeg failed: '+r.stderr);
 }
 if(!existsSync(f32)){
  const r=spawnSync('ffmpeg',['-hide_banner','-loglevel','error','-y','-i',mp3,'-ac','1','-ar','44100','-f','f32le',f32]);
  if(r.status!==0)throw Error('ffmpeg failed: '+r.stderr);
 }
 const seconds=statSync(f32).size/4/44100;
 return {text,mp3,f32,seconds:+seconds.toFixed(3),hash,sung:!!sing,melody:sing||null};
}
if(process.argv[1]&&process.argv[1].endsWith('announce.mjs')&&process.argv[2]){
 const args=process.argv.slice(2),singArg=args.find(a=>a.startsWith('--sing='));
 announce(args.filter(a=>!a.startsWith('--')).join(' '),{sing:singArg?singArg.slice(7).split(',').map(Number):null}).then(r=>console.log(JSON.stringify(r))).catch(e=>{console.error(String(e.message||e));process.exit(1);});
}
