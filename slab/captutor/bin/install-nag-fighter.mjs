#!/usr/bin/env node
import {existsSync,mkdirSync,writeFileSync} from 'node:fs';
import {homedir} from 'node:os';
import {join} from 'node:path';
import {fileURLToPath} from 'node:url';
import {spawnSync,execFileSync} from 'node:child_process';
if(process.platform!=='darwin')throw Error('Nag-fighter requires macOS Accessibility');
const home=homedir(),label='studio.fuser.captutor-nag-fighter';
const directory=join(home,'.local/share/captutor/nag-fighter');
const agents=join(home,'Library/LaunchAgents');
mkdirSync(directory,{recursive:true});mkdirSync(agents,{recursive:true});
const plist=join(agents,label+'.plist');
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
execFileSync('/bin/launchctl',['bootstrap',domain,plist],{stdio:'inherit'});
console.log('Nag-fighter installed: '+plist);
