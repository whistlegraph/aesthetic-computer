// Install the user-owned helper separately from the sandboxed Mac app.
import {cpSync,mkdirSync,realpathSync,writeFileSync,existsSync,rmSync,renameSync} from 'node:fs';
import {join,dirname,delimiter} from 'node:path';
import {homedir} from 'node:os';
import {fileURLToPath} from 'node:url';
import {spawnSync} from 'node:child_process';

if(process.platform!=='darwin')throw new Error('The native helper installer requires macOS');
const label='computer.aesthetic.aesel.host',domain=`gui/${process.getuid()}`;
const root=join(homedir(),'Library/Application Support/Aesel Host'),runtime=join(root,'runtime');
const source=dirname(dirname(fileURLToPath(import.meta.url))),staging=join(root,`runtime-${process.pid}`);
const node=realpathSync(process.execPath);
const xml=value=>String(value).replace(/[<>&"']/g,c=>({'<':'&lt;','>':'&gt;','&':'&amp;','"':'&quot;',"'":'&apos;'}[c]));
mkdirSync(root,{recursive:true,mode:0o700});mkdirSync(staging,{mode:0o700});
for(const folder of ['src','native','context','media'])cpSync(join(source,folder),join(staging,folder),{recursive:true});
const launchDirectory=join(homedir(),'Library/LaunchAgents'),plist=join(launchDirectory,`${label}.plist`);
mkdirSync(launchDirectory,{recursive:true});
// Resolve the current CLI directories so a temporary shell PATH is not required.
const commands={};
const directories=new Set([dirname(node),'/opt/homebrew/bin','/usr/local/bin','/usr/bin','/bin']);
for(const name of ['claude','codex'])for(const directory of (process.env.PATH||'').split(delimiter)){
 const path=join(directory,name);if(existsSync(path)){commands[`AESEL_${name.toUpperCase()}_COMMAND`]=realpathSync(path);directories.add(dirname(realpathSync(path)));break;}
}
spawnSync('launchctl',['bootout',`${domain}/${label}`],{stdio:'ignore'});
for(let attempt=0;attempt<50;attempt++){
 if(spawnSync('launchctl',['print',`${domain}/${label}`],{stdio:'ignore'}).status!==0)break;
 await new Promise(resolve=>setTimeout(resolve,100));
}
const previous=join(root,'runtime.previous');
if(existsSync(previous))rmSync(previous,{recursive:true});
if(existsSync(runtime))renameSync(runtime,previous);
renameSync(staging,runtime);
writeFileSync(plist,`<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0"><dict>
<key>Label</key><string>${label}</string>
<key>ProgramArguments</key><array><string>${xml(node)}</string><string>${xml(join(runtime,'native/host.mjs'))}</string></array>
<key>EnvironmentVariables</key><dict><key>PATH</key><string>${xml([...directories].join(':'))}</string>${Object.entries(commands).map(([key,value])=>`<key>${key}</key><string>${xml(value)}</string>`).join('')}</dict>
<key>RunAtLoad</key><true/><key>KeepAlive</key><true/>
<key>ProcessType</key><string>Background</string><key>ThrottleInterval</key><integer>10</integer>
<key>StandardOutPath</key><string>${xml(join(root,'host.log'))}</string>
<key>StandardErrorPath</key><string>${xml(join(root,'host-error.log'))}</string>
</dict></plist>`,{mode:0o600});
const result=spawnSync('launchctl',['bootstrap',domain,plist],{encoding:'utf8'});
if(result.status!==0)throw new Error(result.stderr.trim()||'Could not start Aesel Host');
console.log('Aesel Host installed. Open Settings in Aesel to see connected providers.');
