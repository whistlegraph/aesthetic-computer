#!/usr/bin/env node
// Live control of MenuBand's own slide axes. Does not restart the singers.
import {spawnSync} from 'node:child_process';
import {hostname} from 'node:os';
const args=process.argv.slice(2), hosts=[], values={}; let dry=false;
for(let i=0;i<args.length;i++) {
  const a=args[i];
  if(a==='--dry') {dry=true;continue;}
  if(a==='--space'||a==='--echo'||a==='--pitch'||a==='--expression') {
    const value=Number(args[++i]);
    if(!Number.isFinite(value)) throw new Error(`${a} requires a finite number`);
    values[a.slice(2)]=value;
  } else if(/^[a-zA-Z0-9._-]+$/.test(a)&&!a.startsWith('-')) hosts.push(a);
  else throw new Error(`Unknown argument: ${a}`);
}
if(!Object.keys(values).length) throw new Error('Use --space 0…1, --echo 0…1, --pitch -24…24 (semitones), or --expression 0…1.');
if(values.space!=null&&(values.space<0||values.space>1)) throw new Error('Space must be 0…1.');
if(values.echo!=null&&(values.echo<0||values.echo>1)) throw new Error('Echo must be 0…1.');
if(values.echo!=null&&values.space!=null) throw new Error('Choose echo or space: these are opposite halves of the MenuBand slide.');
if(values.echo!=null) {values.x=values.echo;delete values.echo;}
if(values.pitch!=null&&Math.abs(values.pitch)>24) throw new Error('Pitch must be -24…24 semitones.');
if(values.expression!=null&&(values.expression<0||values.expression>1)) throw new Error('Expression must be 0…1.');
if(!hosts.length) hosts.push('neo','blueberry','frisbee');
const local=(spawnSync('scutil',['--get','LocalHostName'],{encoding:'utf8'}).stdout||hostname()).trim().toLowerCase();
const kv=Object.entries(values).map(([k,v])=>`${k}=${v}`).join(';');
const name='computer.aestheticcomputer.menuband.slide';
for(const host of hosts) {
  if(dry) {console.log(`${host}: ${name} ${kv}`);continue;}
  const isLocal=[local,'local','localhost'].includes(host.toLowerCase());
  const r=isLocal ? spawnSync('/tmp/mbpost',[],{env:{...process.env,MB_NAME:name,MB_KV:kv},encoding:'utf8'})
    : spawnSync('ssh',['-o','BatchMode=yes','-o','ConnectTimeout=8',host,`MB_NAME=${name} MB_KV='${kv}' /tmp/mbpost`],{encoding:'utf8'});
  if(r.error||r.status!==0) {console.error(`${host}: ${r.error?.message||r.stderr||'Slide command failed'}`);process.exitCode=1;}
  else console.log(`${host}: ${kv}`);
}
