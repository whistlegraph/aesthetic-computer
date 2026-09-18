const fs=require('node:fs'),path=require('node:path');
function devStatus(running,state){
  const fresh=state?.checkedAt&&Date.now()-Date.parse(state.checkedAt)<180000;
  let status=!state?.online||!fresh?'unknown':state.state;
  if(state?.modified?.length)status='modified';
  else if(state?.online&&fresh&&state.installed?.tree!==running.tree)status='ready';
  else if(state?.online&&fresh&&state.target?.tree===running.tree)status='current';
  return {channel:'dev',version:running.version,revision:running.revision,tree:running.tree,status,latest:state?.target?.revision,checkedAt:state?.checkedAt||null};
}
function createBuildStatus({app,devHome,root,send,onDevReady,channel=app.isPackaged?'release':'local'}){
  let running=devHome?JSON.parse(fs.readFileSync(path.join(root,'../build.json'))):null;
  let latest={channel,version:app.getVersion(),status:'unknown'};
  let requested='',timer,pin='';
  if(devHome){const dir=path.join(devHome,'running');fs.mkdirSync(dir,{recursive:true});pin=path.join(dir,process.pid+'.json');fs.writeFileSync(pin,JSON.stringify({pid:process.pid,tree:running.tree}));process.once('exit',()=>{try{fs.unlinkSync(pin);}catch{}});}
  const emit=()=>send('build-status',latest);
  function poll(){
    if(!devHome)return emit();
    try{
      const state=JSON.parse(fs.readFileSync(path.join(devHome,'status.json')));latest=devStatus(running,state);emit();
      if(latest.status==='ready'&&requested!==state.installed.tree){
        const target=fs.realpathSync(path.join(devHome,'current'));
        const next=JSON.parse(fs.readFileSync(path.join(target,'build.json')));
        if(next.tree!==state.installed.tree)return;
        requested=next.tree;onDevReady(path.join(target,'easel'),{sameHost:next.host===running.host,sameUI:next.ui===running.ui});
      }
    }catch{latest={...latest,status:'unknown'};emit();}
  }
  if(devHome){timer=setInterval(poll,3000);timer.unref();}
  return {poll,emit,release(status,info={}){if(!devHome){latest={...latest,status,latest:info.version||latest.latest,checkedAt:new Date().toISOString()};emit();}},adopt(nextRoot){running=JSON.parse(fs.readFileSync(path.join(nextRoot,'../build.json')));requested='';poll();},close(){clearInterval(timer);if(pin)try{fs.unlinkSync(pin);}catch{}}};
}
module.exports={devStatus,createBuildStatus};
