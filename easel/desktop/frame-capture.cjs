const {join}=require('node:path');
const fs=require('node:fs/promises');
const {pathToFileURL}=require('node:url');
function matchesPreview(view,channel){
 try {
  const actual=new URL(view.getURL());
  if(!['https://aesthetic.computer','https://prompt.ac'].includes(actual.origin))return false;
  return actual.pathname==='/@'+channel || (view.aeselLiveChannel===channel && actual.pathname==='/'+channel.split('/').at(-1));
 }catch{return false;}
}
function startFrameCapture({workspace,root,context,guest}){
 let busy=false;
 const script=import(pathToFileURL(join(root,'src/frame-capture-script.mjs')).href);
 const timer=setInterval(async()=>{
  if(busy)return;busy=true;
  try{
   const base=join(workspace,'.easel'),dir=join(base,'frame-requests'),out=join(base,'frame-responses');
   for(const path of [base,dir,out]){const st=await fs.lstat(path);if(st.isSymbolicLink()||!st.isDirectory())return;}
   for(const name of (await fs.readdir(dir)).filter(n=>/^[a-f0-9-]{36}\.json$/.test(n)).slice(0,8)){
    const file=join(dir,name),response=join(out,name);const st=await fs.lstat(file);if(st.isSymbolicLink()||!st.isFile()||st.size>4096)continue;
    try{await fs.access(response);continue;}catch{}
    let request;try{request=JSON.parse(await fs.readFile(file,'utf8'));}catch{continue;}
    const current=context(),view=guest();if(!current||!view||view.isDestroyed()||request.id+'.json'!==name||request.channel!==current.channel||request.revision!==current.revision||Date.now()-request.createdAt>15000||Date.now()<request.createdAt)continue;
    let result;try{
     if(!matchesPreview(view,current.channel))throw new Error('Preview URL does not match the current piece channel; capture unavailable, do not repeatedly retry');
     const capture=await Promise.race([view.executeJavaScript((await script).CAPTURE_SCRIPT),new Promise((_,reject)=>setTimeout(()=>reject(new Error('Canvas capture timed out')),3000))]);
     if(context()?.revision!==current.revision||context()?.channel!==current.channel)throw new Error('Piece changed during capture');
     result={...capture,id:request.id,channel:current.channel,revision:current.revision,capturedAt:new Date().toISOString()};
    }catch(error){result={id:request.id,error:error.message};}
    const temp=response+'.tmp';await fs.writeFile(temp,JSON.stringify(result),{mode:0o600});await fs.rename(temp,response);
   }
  }catch{}finally{busy=false;}
 },200);timer.unref();return ()=>clearInterval(timer);
}
module.exports={startFrameCapture,matchesPreview};
