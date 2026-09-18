#!/usr/bin/env node
import fs from 'node:fs';
import path from 'node:path';
import os from 'node:os';
import {execFileSync} from 'node:child_process';
import {fileURLToPath} from 'node:url';
import {dependencyKey} from './sync.mjs';
const option=(name,fallback)=>{const at=process.argv.indexOf(name);return at<0?fallback:process.argv[at+1];};
const source=path.resolve(path.dirname(fileURLToPath(import.meta.url)),'..');
const home=path.join(os.homedir(),'.local/share/aesel-dev');
const repo=option('--repo',path.join(os.homedir(),'aesthetic-computer'));
const base=option('--base',path.join(os.homedir(),'Applications/aesel.app'));
const deps=option('--dependencies',path.join(source,'desktop/node_modules'));
const app=path.join(os.homedir(),'Applications/Aesel Dev.app');
const pkg=JSON.parse(fs.readFileSync(path.join(source,'desktop/package.json')));
if(process.platform!=='darwin')throw Error('The fleet dev installer currently supports macOS.');
if(fs.existsSync(app)&&execFileSync('/bin/ps',['-axo','command='],{encoding:'utf8'}).split('\n').some(line=>line.startsWith(path.join(app,'Contents/MacOS')+'/')))throw Error('Quit Aesel Dev before updating its Electron shell or dependencies.');
if(!fs.existsSync(base)||!fs.existsSync(deps))throw Error('Provide an installed Electron app and its extracted production node_modules.');
fs.mkdirSync(home,{recursive:true});fs.mkdirSync(path.join(home,'dependencies'),{recursive:true});
// Copy production Electron dependencies, including the matching node-pty binary.
execFileSync('/usr/bin/rsync',['-a',deps+'/',path.join(home,'dependencies/node_modules/')]);
const dependencyTarget=path.join(home,'dependencies/node_modules');
const helper=path.join(dependencyTarget,'node-pty/build/Release/spawn-helper');if(fs.existsSync(helper))fs.chmodSync(helper,0o755);
const executable=execFileSync('/usr/libexec/PlistBuddy',['-c','Print:CFBundleExecutable',path.join(base,'Contents/Info.plist')],{encoding:'utf8'}).trim();
const electron=path.join(base,'Contents/MacOS',executable);
const actual=execFileSync(electron,['-p','process.versions.electron'],{encoding:'utf8',env:{...process.env,ELECTRON_RUN_AS_NODE:'1'}}).trim();
if(actual!==pkg.devDependencies.electron)throw Error(`Electron ${pkg.devDependencies.electron} required; base has ${actual}.`);
execFileSync(electron,['-e',`require(${JSON.stringify(path.join(dependencyTarget,'node-pty'))})`],{env:{...process.env,ELECTRON_RUN_AS_NODE:'1'}});
fs.writeFileSync(path.join(home,'config.json'),JSON.stringify({repo,dependencyKey:dependencyKey(pkg)},null,2));
fs.copyFileSync(path.join(source,'dev/sync.mjs'),path.join(home,'sync-bootstrap.mjs'));
fs.writeFileSync(path.join(home,'sync-runner.mjs'),`import fs from 'node:fs';import {pathToFileURL} from 'node:url';const home=${JSON.stringify(home)};const current=home+'/current/easel/dev/sync.mjs';try{const module=await import(pathToFileURL(fs.existsSync(current)?current:home+'/sync-bootstrap.mjs'));console.log(JSON.stringify(module.sync({home})));}catch(error){console.error(error.message.split('\\n')[0]);process.exitCode=1;}\n`);
execFileSync(process.execPath,[path.join(home,'sync-runner.mjs')],{stdio:'inherit',timeout:240000});
const stage=app+`.stage-${process.pid}.app`;
execFileSync('/bin/cp',['-cR',base,stage]);
const resources=path.join(stage,'Contents/Resources');
for(const name of ['app.asar','app.asar.unpacked','easel','app-update.yml'])fs.rmSync(path.join(resources,name),{recursive:true,force:true});
fs.mkdirSync(path.join(resources,'app'),{recursive:true});
fs.writeFileSync(path.join(resources,'app/package.json'),JSON.stringify({name:'aesel-dev',version:pkg.version,main:'main.cjs'}));
fs.writeFileSync(path.join(resources,'app/main.cjs'),`const fs=require('node:fs'),path=require('node:path');const home=path.join(require('node:os').homedir(),'.local/share/aesel-dev');const root=path.join(fs.realpathSync(path.join(home,'current')),'easel');process.env.AESEL_DEV_HOME=home;process.env.AESEL_DEV_ROOT=root;require(path.join(root,'desktop/main.cjs'));\n`);
for(const [key,value] of Object.entries({CFBundleIdentifier:'computer.aesthetic.aesel.dev',CFBundleDisplayName:'Aesel Dev'})){
 try{execFileSync('/usr/libexec/PlistBuddy',['-c',`Set :${key} ${value}`,path.join(stage,'Contents/Info.plist')],{stdio:'pipe'});}catch{execFileSync('/usr/libexec/PlistBuddy',['-c',`Add :${key} string ${value}`,path.join(stage,'Contents/Info.plist')]);}
}
execFileSync('/usr/bin/codesign',['--force','--deep','--sign','-','--preserve-metadata=entitlements,requirements,flags',stage],{stdio:'inherit',timeout:120000});
if(fs.existsSync(app)){
 const live=execFileSync('/bin/ps',['-axo','command='],{encoding:'utf8'}).split('\n').some(line=>line.startsWith(path.join(app,'Contents/MacOS')+'/'));
 if(live)throw Error('Existing Dev app is running; staged shell kept. Quit Dev before replacing its Electron shell.');
 fs.renameSync(app,path.join(home,`previous-shell-${Date.now()}.app`));
}
fs.renameSync(stage,app);
const bin=path.join(os.homedir(),'.local/bin');fs.mkdirSync(bin,{recursive:true});
fs.writeFileSync(path.join(bin,'aesel-dev'),`#!/bin/bash\nexec "$HOME/.local/share/aesel-dev/current/easel/bin/easel-desktop" --dev "$@"\n`,{mode:0o755});
const xml=s=>s.replaceAll('&','&amp;').replaceAll('<','&lt;').replaceAll('>','&gt;');
const plist=path.join(os.homedir(),'Library/LaunchAgents/computer.aesthetic.aesel-dev-sync.plist');
fs.mkdirSync(path.dirname(plist),{recursive:true});
fs.writeFileSync(plist,`<?xml version="1.0" encoding="UTF-8"?><!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd"><plist version="1.0"><dict><key>Label</key><string>computer.aesthetic.aesel-dev-sync</string><key>ProgramArguments</key><array><string>${xml(fs.realpathSync(process.execPath))}</string><string>${xml(path.join(home,'sync-runner.mjs'))}</string></array><key>RunAtLoad</key><true/><key>StartInterval</key><integer>60</integer><key>ProcessType</key><string>Background</string><key>Nice</key><integer>10</integer><key>LowPriorityIO</key><true/><key>EnvironmentVariables</key><dict><key>PATH</key><string>/Users/jas/.local/bin:/opt/homebrew/bin:/usr/bin:/bin:/usr/sbin:/sbin</string></dict><key>StandardOutPath</key><string>${xml(path.join(home,'sync.log'))}</string><key>StandardErrorPath</key><string>${xml(path.join(home,'sync-error.log'))}</string></dict></plist>`);
try{execFileSync('/bin/launchctl',['bootout',`gui/${process.getuid()}`,plist],{stdio:'ignore'});}catch{}
execFileSync('/bin/launchctl',['bootstrap',`gui/${process.getuid()}`,plist]);
console.log('Installed Aesel Dev. It follows committed origin/main; release Aesel is separate.');
