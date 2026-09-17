import {spawnSync} from 'node:child_process';
import {homedir} from 'node:os';
import {join} from 'node:path';
export function macPalStage({exec=spawnSync,uid=process.getuid(),home=homedir(),pause=ms=>new Promise(r=>setTimeout(r,ms))}={}) {
 const label=`gui/${uid}/computer.aesthetic.macpal`;
 const call=(bin,args)=>exec(bin,args,{encoding:'utf8'});
 const loaded=()=>call('/bin/launchctl',['print',label]).status===0;
 const running=()=>call('/usr/bin/pgrep',['-x','MacPal']).status===0;
 return {
  capture:()=>({loaded:loaded(),running:running()}),
  async hide(state){
   if(state.loaded){const r=call('/bin/launchctl',['bootout',label]);if(r.status!==0&&loaded())throw Error('Cannot unload MacPal for filming');}
   else if(state.running)call('/usr/bin/osascript',['-e','tell application "MacPal" to quit']);
   for(let i=0;i<20&&running();i++)await pause(100);
   if(loaded()||running())throw Error('MacPal remains active; refusing a contaminated recording');
  },
  restore(state){
   if(!state)return;
   if(state.loaded&&!loaded()){
    const r=call('/bin/launchctl',['bootstrap',`gui/${uid}`,join(home,'Library/LaunchAgents/computer.aesthetic.macpal.plist')]);
    if(r.status!==0&&!loaded())throw Error('MacPal launch agent restore failed');
   }else if(!state.loaded&&state.running&&!running()){
    const r=call('/usr/bin/open',['-a','MacPal']);if(r.status!==0)throw Error('MacPal app restore failed');
   }
  },
 };
}
