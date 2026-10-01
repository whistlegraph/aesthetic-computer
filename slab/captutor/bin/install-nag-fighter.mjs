#!/usr/bin/env node
import {existsSync,mkdirSync,writeFileSync,readFileSync,copyFileSync,renameSync} from 'node:fs';
import {homedir} from 'node:os';
import {join} from 'node:path';
import {fileURLToPath} from 'node:url';
import {spawnSync,execFileSync} from 'node:child_process';
import {setTimeout as delay} from 'node:timers/promises';
if(process.platform!=='darwin')throw Error('Nag-fighter requires macOS Accessibility');
const home=homedir(),label='studio.fuser.captutor-nag-fighter';
const flags=process.argv.slice(2);
if(flags.some(flag=>!['--allow-remote-debugging','--deny-remote-debugging'].includes(flag))||flags.length>1)
 throw Error('Usage: install-nag-fighter.mjs [--allow-remote-debugging | --deny-remote-debugging]');
// This persistent policy is only changed by an explicit operator option.
if(flags.length){
 const policyPath=join(home,'.config/captutor/modal-police.json');
 mkdirSync(join(home,'.config/captutor'),{recursive:true});
 const previous=existsSync(policyPath)?readFileSync(policyPath,'utf8'):null;
 const policy=previous===null?{}:JSON.parse(previous);
 if(!policy||typeof policy!=='object'||Array.isArray(policy))throw Error('Invalid existing modal-police policy');
 const allowed=flags[0]==='--allow-remote-debugging';
 if(policy.allowRemoteDebugging!==allowed){
  if(previous!==null)writeFileSync(policyPath+'.before-'+Date.now(),previous,{mode:0o600});
  policy.allowRemoteDebugging=allowed;
  const tmp=policyPath+'.'+process.pid+'.tmp';
  writeFileSync(tmp,JSON.stringify(policy,null,2)+'\n',{mode:0o600});renameSync(tmp,policyPath);
 }
}
const directory=join(home,'.local/share/captutor/nag-fighter');
const agents=join(home,'Library/LaunchAgents');
mkdirSync(directory,{recursive:true});mkdirSync(agents,{recursive:true});
const plist=join(agents,label+'.plist');
if(existsSync(plist))copyFileSync(plist,plist+'.before-'+Date.now());
const xml=s=>s.replaceAll('&','&amp;').replaceAll('<','&lt;');
const node=process.env.CAPTUTOR_NODE||(existsSync('/opt/homebrew/bin/node')?'/opt/homebrew/bin/node':process.execPath);
const script=fileURLToPath(new URL('./nag-fighter.mjs',import.meta.url));
writeFileSync(plist,`<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0"><dict>
<key>Label</key><string>${label}</string>
<key>ProgramArguments</key><array><string>${xml(node)}</string><string>${xml(script)}</string></array>
<key>RunAtLoad</key><true/><key>KeepAlive</key><true/>
<key>ThrottleInterval</key><integer>10</integer>
<key>StandardOutPath</key><string>${xml(join(directory,'service.log'))}</string>
<key>StandardErrorPath</key><string>${xml(join(directory,'service.log'))}</string>
</dict></plist>\n`);
const domain=`gui/${process.getuid()}`;
if(spawnSync('/bin/launchctl',['print',`${domain}/${label}`],{stdio:'ignore'}).status===0)
 execFileSync('/bin/launchctl',['bootout',`${domain}/${label}`]);
// launchd can retain the booted-out label briefly (EIO on immediate bootstrap).
let loaded=false;
// A watcher already inside an AX scan can take up to 30s to wind down.
for(let attempt=0;attempt<450;attempt++){
 if(spawnSync('/bin/launchctl',['bootstrap',domain,plist],{stdio:'ignore'}).status===0){loaded=true;break;}
 await delay(100);
}
if(!loaded)execFileSync('/bin/launchctl',['bootstrap',domain,plist],{stdio:'inherit'});
console.log('Nag-fighter installed: '+plist+(flags.length?' ('+flags[0].slice(2)+')':''));
