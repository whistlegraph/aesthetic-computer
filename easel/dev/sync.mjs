#!/usr/bin/env node
// The shared dev channel is the committed easel tree on origin/main.
// Never checks out, resets, or edits a developer's working tree.
import fs from 'node:fs';
import path from 'node:path';
import os from 'node:os';
import crypto from 'node:crypto';
import {execFileSync} from 'node:child_process';
import {fileURLToPath} from 'node:url';
export const digest = value => crypto.createHash('sha256').update(value).digest('hex');
export function fingerprints(directory) {
  const files = {};
  function visit(relative = '') {
    for (const entry of fs.readdirSync(path.join(directory, relative), {withFileTypes:true}).sort((a,b)=>a.name.localeCompare(b.name))) {
      if (['node_modules','.git','dist','.build','.DS_Store'].includes(entry.name)) continue;
      const name = path.join(relative,entry.name);
      if (entry.isDirectory()) visit(name);
      else if(entry.isSymbolicLink()) files[name]=digest('symlink:'+fs.readlinkSync(path.join(directory,name)));
      else if (entry.isFile() && !['native/gamepad','desktop/native/credit-label'].includes(name)) files[name] = digest(fs.readFileSync(path.join(directory,name)));
    }
  }
  visit(); return files;
}
export function modifiedFiles(directory, manifest) {
  const actual=fingerprints(directory),expected=manifest.files;
  return [...new Set([...Object.keys(actual),...Object.keys(expected)])].filter(name=>actual[name]!==expected[name]);
}
export function dependencyKey(pkg) { return digest(JSON.stringify({dependencies:pkg.dependencies,electron:pkg.devDependencies?.electron})); }
export function pruneBuilds(home, retain=3) {
  // Keep recent builds, live PTY roots, and any locally edited snapshot.
  let processes;try{processes=execFileSync('/bin/ps',['-axo','command='],{encoding:'utf8',maxBuffer:4*1024*1024});}catch{return;}
  const current=fs.realpathSync(path.join(home,'current'));
  const versions=path.join(home,'versions');
  const builds=fs.readdirSync(versions).filter(name=>/^[a-f0-9]{40}$/.test(name)).map(name=>path.join(versions,name)).filter(dir=>fs.existsSync(path.join(dir,'build.json'))).sort((a,b)=>fs.statSync(b).mtimeMs-fs.statSync(a).mtimeMs);
  for(const dir of builds.slice(retain)){
    if(dir===current||processes.includes(dir))continue;
    try{const manifest=JSON.parse(fs.readFileSync(path.join(dir,'build.json')));if(!modifiedFiles(path.join(dir,'easel'),manifest).length)fs.rmSync(dir,{recursive:true});}catch{}
  }
}
function writeJSON(file,value){const temp=file+'.tmp';fs.writeFileSync(temp,JSON.stringify(value,null,2)+'\n');fs.renameSync(temp,file);}
export function sync({home=path.join(os.homedir(),'.local/share/aesel-dev'),fetch=true,build=source=>execFileSync('/bin/bash',[path.join(source,'desktop/scripts/build-credit-label.sh')],{timeout:120000,stdio:['ignore','pipe','pipe']})}={}) {
  const config=JSON.parse(fs.readFileSync(path.join(home,'config.json')));
  const stateFile=path.join(home,'status.json'),lock=path.join(home,'sync.lock');
  fs.mkdirSync(home,{recursive:true});
  try{fs.mkdirSync(lock);}catch{try{const pid=Number(fs.readFileSync(path.join(lock,'pid'),'utf8'));process.kill(pid,0);}catch(error){if(error.code==='ESRCH')fs.rmSync(lock,{recursive:true,force:true});}return {state:'busy'};}
  fs.writeFileSync(path.join(lock,'pid'),String(process.pid));
  let previous={};try{previous=JSON.parse(fs.readFileSync(stateFile));}catch{}
  const git=(...args)=>execFileSync('/usr/bin/git',['-C',config.repo,...args],{encoding:'utf8',timeout:120000,stdio:['ignore','pipe','pipe'],env:{...process.env,GIT_TERMINAL_PROMPT:'0'}}).trim();
  try {
    if(fetch)git('fetch','origin','main','--quiet');
    const revision=git('rev-parse','refs/remotes/origin/main'),tree=git('rev-parse',`${revision}:easel`);
    const target=path.join(home,'versions',tree),current=path.join(home,'current');
    let installed;try{installed=JSON.parse(fs.readFileSync(path.join(current,'build.json')));}catch{}
    const changed=installed?modifiedFiles(path.join(current,'easel'),installed):[];
    const state={channel:'dev',target:{revision,tree},checkedAt:new Date().toISOString(),online:true,installed:installed&&{revision:installed.revision,tree:installed.tree},modified:changed,state:changed.length?'modified':'checking'};
    if(changed.length){writeJSON(stateFile,state);return state;}
    if(installed?.tree!==tree){
      state.state='syncing';writeJSON(stateFile,state);
      if(!fs.existsSync(path.join(target,'build.json'))){
        const stage=target+`.stage-${process.pid}`;fs.mkdirSync(stage,{recursive:true});
        try {
          execFileSync('/usr/bin/git',['-C',config.repo,'archive','--format=tar','-o',path.join(stage,'source.tar'),revision,'easel'],{timeout:120000});
          execFileSync('/usr/bin/tar',['-xf',path.join(stage,'source.tar'),'-C',stage]);fs.unlinkSync(path.join(stage,'source.tar'));
          const source=path.join(stage,'easel'),pkg=JSON.parse(fs.readFileSync(path.join(source,'desktop/package.json')));
          if(dependencyKey(pkg)!==config.dependencyKey)throw Error('Dev dependencies changed; run install-dev again.');
          fs.symlinkSync(path.join(home,'dependencies/node_modules'),path.join(source,'desktop/node_modules'));
          build(source);
          const files=fingerprints(source);
          const hostFiles=Object.fromEntries(Object.entries(files).filter(([n])=>/^desktop\/.*\.cjs$/.test(n)||/^desktop\/native\//.test(n)||n==='desktop/package.json'||n==='native/gamepad.swift'));
          const uiFiles=Object.fromEntries(Object.entries(files).filter(([n])=>n.startsWith('desktop/')&&!n.endsWith('.cjs')));
          const manifest={ui:digest(JSON.stringify(uiFiles)),channel:'dev',revision,tree,version:pkg.version,builtAt:new Date().toISOString(),host:digest(JSON.stringify(hostFiles)),files};
          writeJSON(path.join(stage,'build.json'),manifest);fs.renameSync(stage,target);
        }catch(error){fs.rmSync(stage,{recursive:true,force:true});throw error;}
      }
      const manifest=JSON.parse(fs.readFileSync(path.join(target,'build.json')));
      if(modifiedFiles(path.join(target,'easel'),manifest).length)throw Error('Staged dev build was modified.');
      const next=path.join(home,`current-${process.pid}`);fs.symlinkSync(target,next);fs.renameSync(next,current);
      installed=manifest;
    }
    state.installed={revision:installed.revision,tree:installed.tree};state.state='current';writeJSON(stateFile,state);pruneBuilds(home);return state;
  } catch(error) {
    const state={...previous,channel:'dev',state:'error',online:false,attemptedAt:new Date().toISOString(),error:String(error.message).split('\n')[0].slice(0,180)};
    writeJSON(stateFile,state);throw error;
  } finally {fs.rmSync(lock,{recursive:true,force:true});}
}
if(process.argv[1]===fileURLToPath(import.meta.url)){
  try{console.log(JSON.stringify(sync({fetch:!process.argv.includes('--no-fetch')})));}catch(error){console.error(error.message.split('\n')[0]);process.exitCode=1;}
}
