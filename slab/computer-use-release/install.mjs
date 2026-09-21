import { readFileSync, writeFileSync, mkdirSync, existsSync, renameSync, cpSync } from 'node:fs';
import { join, dirname, resolve } from 'node:path';
import { homedir } from 'node:os';
import { createHash } from 'node:crypto';
import { execFileSync, spawnSync } from 'node:child_process';
import { inspectMachineLeases } from '../lib/computer-use-lease.mjs';
import { createComputerUseClient } from '../lib/computer-use-client.mjs';

const root=resolve(import.meta.dirname,'../..'), home=homedir();
if(process.platform!=='darwin' || Number(process.versions.node.split('.')[0])<22) throw new Error('macOS and Node 22+ required');
const manifest=JSON.parse(readFileSync(join(root,'release.json'),'utf8'));
for(const [path,expected] of Object.entries(manifest.sha256)) {
 const actual=createHash('sha256').update(readFileSync(join(root,path))).digest('hex');
 if(actual!==expected) throw new Error('Release hash mismatch: '+path);
}
if(inspectMachineLeases().length) throw new Error('Computer-use input lease exists; wait for its owner before upgrading');
const nativeBinary=join(home,'Applications/SlabMenubar.app/Contents/MacOS/slab-menubar');
const nativeUUID=execFileSync('/usr/bin/dwarfdump',['--uuid',nativeBinary],{encoding:'utf8'}).split(/\s+/)[1];
if(nativeUUID!==manifest.nativeUUID)throw new Error('Install the matching tested native binary before this runtime');
const stamp=new Date().toISOString().replace(/[:.]/g,'-');
const backup=join(home,'.local/share/slab/computer-use/backups',stamp);
mkdirSync(backup,{recursive:true,mode:0o700});
const saved=new Set();
const captutor=[];
const hash=path=>createHash('sha256').update(readFileSync(path)).digest('hex');
function save(path) {
 if(saved.has(path))return;
 saved.add(path);
 if(existsSync(path)){const dest=join(backup,path.slice(home.length));mkdirSync(dirname(dest),{recursive:true});cpSync(path,dest,{verbatimSymlinks:true});}
}
function write(path,text,mode=0o600) {
 save(path);mkdirSync(dirname(path),{recursive:true});
 const tmp=path+'.computer-use-'+process.pid;writeFileSync(tmp,text,{mode});renameSync(tmp,path);
}
const shell=s=>"'"+s.replaceAll("'","'\\''")+"'";
const xml=s=>String(s).replaceAll('&','&amp;').replaceAll('<','&lt;').replaceAll('>','&gt;');
const node=process.env.SLAB_NODE || process.execPath;
const envPath=[dirname(node),join(home,'.local/bin'),'/opt/homebrew/bin','/usr/local/bin','/usr/bin','/bin','/usr/sbin','/sbin'].join(':');
const configPath=join(home,'.config/slab/puppet.json');
const config=existsSync(configPath)?JSON.parse(readFileSync(configPath,'utf8')):{machines:{}};
config.machines||={};config.machines.local||={local:true,cdpUrl:'http://127.0.0.1:9222',lazy:true};
if(!config.machines.local.local) throw new Error('Existing local machine entry is not local; inspect configuration');
write(configPath,JSON.stringify(config,null,2)+'\n');
for(const name of ['frame','puppet','computer-use']) {
 write(join(home,'.local/bin',name),'#!/bin/sh\nexec '+shell(node)+' '+shell(join(root,'slab/bin',name+'.mjs'))+' "$@"\n',0o755);
}
// Captutor imports this module in-process; direct CLI users still work.
const frameModule=join(root,'slab/bin/frame.mjs');
write(join(home,'.local/bin/frame.mjs'),
  'export * from '+JSON.stringify('file://'+frameModule)+';\n'+
  'import {fileURLToPath} from "node:url"; import {resolve} from "node:path";\n'+
  'if(process.argv[1] && resolve(process.argv[1])===fileURLToPath(import.meta.url)) {\n'+
  ' const {spawnSync}=await import("node:child_process");\n'+
  ' process.exitCode=spawnSync(process.execPath,['+JSON.stringify(frameModule)+',...process.argv.slice(2)],{stdio:"inherit"}).status ?? 1;\n}\n');
// Iris workers explicitly use this legacy entry instead of the global config.
for(const name of ['frame','puppet']) {
 const entry=join(home,'.hermes/mcp',name,name+'-mcp.mjs');
 if(existsSync(entry))write(entry,'import '+JSON.stringify('file://'+join(root,'slab/bin',name+'-mcp.mjs'))+';\n',0o755);
}
// Update only the known pre-optimization Captutor file, preserving other work.
for(const dir of [join(home,'aesthetic-computer/captutor'),join(home,'Developer/captutor')]) {
 const file=join(dir,'captutor.mjs');
 if(!existsSync(file))continue;
 const current=hash(file), expected=manifest.sha256['captutor/captutor.mjs'];
 if(current!==expected && current!==manifest.captutorPreviousHash) {
  captutor.push({path:dir,status:'custom version preserved'});continue;
 }
 const client=join(dir,'lib/frame-client.mjs');
 if(existsSync(client) && hash(client)!==manifest.sha256['captutor/lib/frame-client.mjs']) {
  throw new Error('Custom Captutor Frame client requires review: '+client);
 }
 if(current!==expected)write(file,readFileSync(join(root,'captutor/captutor.mjs'),'utf8'),0o755);
 write(client,readFileSync(join(root,'captutor/lib/frame-client.mjs'),'utf8'),0o644);
 captutor.push({path:dir,status:'updated'});
}
const servers=[['puppet-core','slab/bin/puppet.mjs',['daemon']],['frame-mcp','slab/bin/frame-mcp.mjs',['--http','7767']],['puppet-mcp','slab/bin/puppet-mcp.mjs',['--http','7769']]];
const domain='gui/'+process.getuid();
for(const [name,script,args] of servers) {
 const label='computer.aesthetic.'+name;
 const plist=join(home,'Library/LaunchAgents',label+'.plist');
 const strings=[node,join(root,script),...args].map(s=>'<string>'+xml(s)+'</string>').join('');
 const content='<?xml version="1.0" encoding="UTF-8"?>\n<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">\n<plist version="1.0"><dict><key>Label</key><string>'+label+'</string><key>ProgramArguments</key><array>'+strings+'</array><key>EnvironmentVariables</key><dict><key>HOME</key><string>'+xml(home)+'</string><key>PATH</key><string>'+xml(envPath)+'</string></dict><key>RunAtLoad</key><true/><key>KeepAlive</key><true/><key>ThrottleInterval</key><integer>5</integer><key>StandardOutPath</key><string>/tmp/'+label+'.out</string><key>StandardErrorPath</key><string>/tmp/'+label+'.err</string></dict></plist>\n';
 write(plist,content);execFileSync('/usr/bin/plutil',['-lint',plist],{stdio:'pipe'});
 spawnSync('/bin/launchctl',['bootout',domain+'/'+label],{stdio:'ignore'});
 let started=false;
 for(let i=0;i<20;i++) {
  if(spawnSync('/bin/launchctl',['bootstrap',domain,plist],{stdio:'ignore'}).status===0){started=true;break;}
  await new Promise(r=>setTimeout(r,200));
 }
 if(!started)throw new Error('Could not start '+label);
}
for(let i=0;i<30;i++) {
 try{await createComputerUseClient({timeoutMs:1500}).discover();break;}
 catch(error){if(i===29)throw error;await new Promise(r=>setTimeout(r,300));}
}
const endpoints={frame:'http://127.0.0.1:7767/mcp',puppet:'http://127.0.0.1:7769/mcp'};
const claudePath=join(home,'.claude.json');
const claude=existsSync(claudePath)?JSON.parse(readFileSync(claudePath,'utf8')):{};
claude.mcpServers||={};
for(const [name,url] of Object.entries(endpoints)) {
 const entry={type:'http',url};claude.mcpServers[name]=entry;
 for(const project of Object.values(claude.projects||{}))if(project.mcpServers?.[name])project.mcpServers[name]=entry;
}
write(claudePath,JSON.stringify(claude,null,2)+'\n');
const codexPath=join(process.env.CODEX_HOME||join(home,'.codex'),'config.toml');
const codex=existsSync(codexPath)?readFileSync(codexPath,'utf8'):'';
const preserved=codex.split(/(?=^\[)/m).filter(section=>!/^\[mcp_servers\.(frame|puppet)(?:\.[^\]]+)?\]/.test(section)).join('').trimEnd();
write(codexPath,preserved+'\n\n'+Object.entries(endpoints).map(([name,url])=>'[mcp_servers.'+name+']\nurl = '+JSON.stringify(url)+'\n').join('\n'));
write(join(home,'.local/share/slab/computer-use/installed.json'),JSON.stringify({revision:manifest.revision,runtime:root,installedAt:new Date().toISOString(),backup,node,nativeUUID:manifest.nativeUUID,nativeSources:manifest.nativeSources,captutor},null,2)+'\n');
console.log(JSON.stringify({installed:true,revision:manifest.revision,runtime:root,backup,clients:['Claude','Codex'],captutor,services:servers.map(s=>s[0])}));
