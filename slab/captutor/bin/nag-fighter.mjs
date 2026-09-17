#!/usr/bin/env node
// Keep known Chrome nags off the idle filming desk; Stage owns its active take.
import {existsSync,readFileSync,mkdirSync,writeFileSync,renameSync} from 'node:fs';
import {homedir} from 'node:os';
import {join} from 'node:path';
import {setTimeout as sleep} from 'node:timers/promises';
import {createModalPolice,approvedDebuggingPolicy} from '../lib/modal-police.mjs';
const directory=join(homedir(),'.local/share/captutor/nag-fighter');
const stageState=join(homedir(),'.local/share/captutor/stage-mode.json');
const reelState=join(homedir(),'.local/share/slab/state/reel.state');
const recording=()=>{try{return JSON.parse(readFileSync(reelState,'utf8')).recording===true;}catch{return existsSync(stageState);}};
const statePath=join(directory,'status.json');
mkdirSync(directory,{recursive:true});
const status=data=>{const tmp=statePath+'.tmp';writeFileSync(tmp,JSON.stringify({pid:process.pid,at:new Date().toISOString(),...data},null,2));renameSync(tmp,statePath);};
const allowRemoteDebugging=approvedDebuggingPolicy();
const police=createModalPolice({directory,allowRemoteDebugging});
const controller=new AbortController();
for(const signal of ['SIGTERM','SIGINT'])process.on(signal,()=>controller.abort());
do {
 try {
  if(recording())status({phase:'stage-owned'});
  else {
   // Recheck ownership immediately before any action: a take can begin during AX scan.
   const result=await police.check(allowRemoteDebugging?'connecting':'preparing',{mayHandle:()=>!recording()&&!controller.signal.aborted});
   status({phase:'watching',...result});
  }
 } catch(error) {status({phase:'blocked',error:error.stderr?.trim()||error.message});}
 if(process.argv.includes('--once')||controller.signal.aborted)break;
 try{await sleep(1500,undefined,{signal:controller.signal});}catch{break;}
} while(!controller.signal.aborted);
