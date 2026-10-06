#!/usr/bin/env node
import { createSocket } from 'node:dgram';
import { spawnSync } from 'node:child_process';
import { readFileSync, writeFileSync, mkdtempSync, rmSync } from 'node:fs';
import { homedir, tmpdir } from 'node:os';
import { join } from 'node:path';
import { randomBytes } from 'node:crypto';
import { createInterface } from 'node:readline';

// Same K/P/R stream as AC OS, plus mouse deltas and two mouse buttons.
// Device Portal credentials never leave this helper; input is never logged.
export function remoteSnapshot(keys, mouse = {}) {
  const axis=(positive,negative)=>Number(keys.has(positive))-Number(keys.has(negative));
  let lx=axis(32,30),ly=axis(17,31),rx=axis(38,36),ry=axis(23,37),buttons=0;
  const bindings=[[57,0],[42,1],[18,2],[16,3],[103,4],[108,5],[105,6],[106,7],[29,8],[33,9],[1,10],[15,11],[56,12],[46,13]];
  for(const [key,bit] of bindings)if(keys.has(key))buttons|=1<<bit;
  const bound=v=>Math.max(-1,Math.min(1,v));
  return [lx,ly,bound(rx+(mouse.x||0)/12),bound(ry-(mouse.y||0)/12),Number(keys.has(45)||!!mouse.right),Number(keys.has(19)||!!mouse.left),buttons];
}
async function main(){
  const env={};
  for(const raw of readFileSync(process.env.XBOX_DEVICE_PORTAL_ENV||join(homedir(),'aesthetic-computer/aesthetic-computer-vault/xbox/device-portal.env'),'utf8').split(/\r?\n/)){
    const m=raw.match(/^(?:export\s+)?([A-Z_]+)=(.*)$/);if(m)env[m[1]]=m[2].trim().replace(/^(['"])(.*)\1$/,'$2');
  }
  Object.assign(env,process.env);
  const host=env.XBOX_DEVICE_PORTAL_HOST,base=`https://${host}:${env.XBOX_DEVICE_PORTAL_PORT||11443}`;
  if(!host||!env.XBOX_DEVICE_PORTAL_USERNAME||!env.XBOX_DEVICE_PORTAL_PASSWORD)throw Error('Missing Xbox configuration');
  const auth=`${env.XBOX_DEVICE_PORTAL_USERNAME}:${env.XBOX_DEVICE_PORTAL_PASSWORD}`;
  function curl(args){const r=spawnSync('curl',['-ksSf','--connect-timeout','3','--max-time','5',...args],{encoding:'utf8'});if(r.status)throw Error('Xbox Device Portal unavailable');return r.stdout;}
  const packages=JSON.parse(curl(['-u',auth,`${base}/api/app/packagemanager/packages`]));
  const pkg=packages.InstalledPackages.filter(p=>p.PackageFamilyName==='AestheticComputer.NativeBios').sort((a,b)=>b.Version.Revision-a.Version.Revision)[0];
  if(!pkg||pkg.Version.Revision<68)throw Error('Xbox needs Native BIOS 68 or newer');
  const token=randomBytes(32).toString('hex'),dir=mkdtempSync(join(tmpdir(),'ac-remote-'));
  try{
    const path=join(dir,'ac-remote.txt');writeFileSync(path,`${token}\n${Math.floor(Date.now()/1000)+7200}\n`,{mode:0o600});
    const query=new URLSearchParams({knownfolderid:'LocalAppData',packagefullname:pkg.PackageFullName,path:'\\LocalState'});
    curl(['-u',`auto-${auth}`,'-X','POST','-F',`file=@${path}`,`${base}/api/filesystem/apps/file?${query}`]);
  }finally{rmSync(dir,{recursive:true,force:true});}
  const socket=createSocket('udp4'),keys=new Set(),mouse={x:0,y:0,left:false,right:false};
  let sequence=0,lastAck=0,ready=false,stopping=false,lastInput=Date.now(),started=Date.now(),lastAckSeq=0;
  const send=()=>{const values=remoteSnapshot(keys,mouse);mouse.x=mouse.y=0;socket.send(`ACR1 ${token} ${++sequence} ${values.join(' ')}`,51339,host);};
  const stop=()=>{if(stopping)return;stopping=true;clearInterval(timer);keys.clear();Object.assign(mouse,{x:0,y:0,left:false,right:false});send();setTimeout(()=>{socket.close();process.exit(0);},80);};
  socket.on('error',stop);
  socket.on('message',(message,peer)=>{
    if(peer.address!==host||peer.port!==51339)return;
    const m=message.toString().match(/^ACR1 (\d+)\n?$/);if(!m)return;
    const seq=Number(m[1]);if(seq<=lastAckSeq||seq>sequence)return;
    lastAckSeq=seq;lastAck=Date.now();
    if(!ready){ready=true;process.stdout.write('READY\n');}
  });
  const timer=setInterval(()=>{
    const now=Date.now();
    if((ready&&now-lastAck>700)||(!ready&&now-started>5000)||now-lastInput>3500){stop();return;}
    send();
  },33);
  const lines=createInterface({input:process.stdin});
  lines.on('line',line=>{
    if(line.length>100)return;
    const [kind,a,b]=line.split(' '),code=Number(a),value=Number(b);lastInput=Date.now();
    if(kind==='R'){stop();return;}
    if(kind==='P'&&/^\d+$/.test(a)&&Date.now()-lastAck<700)process.stdout.write(`P ${a}\n`);
    if(kind==='K'&&Number.isInteger(code)&&code>=0&&code<=200&&[0,1,2].includes(value)){if(value)keys.add(code);else keys.delete(code);}
    if(kind==='M'&&Number.isFinite(code)&&Number.isFinite(value)){mouse.x+=Math.max(-100,Math.min(100,code));mouse.y+=Math.max(-100,Math.min(100,value));}
    if(kind==='B'&&[0,1].includes(code)&&[0,1].includes(value))mouse[code?'right':'left']=!!value;
  });
  lines.on('close',stop);process.on('SIGTERM',stop);process.on('SIGINT',stop);
}
if(process.argv[1]&&import.meta.url===new URL(`file://${process.argv[1]}`).href)main().catch(()=>{console.error('Xbox remote could not connect. Check Xbox power, build, and Device Portal configuration.');process.exit(1);});
