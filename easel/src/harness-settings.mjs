// A session-local control path shared by hosted tools and the CLI MCP bridges.
import {createServer} from 'node:http';
import {mkdtemp,chmod,rm} from 'node:fs/promises';
import {tmpdir} from 'node:os';
import {join} from 'node:path';

import {validateSettingsRequest} from './harness-contract.mjs';
export * from './harness-contract.mjs';
export {callSettings} from './harness-client.mjs';

export function createSettingsController({read,normalize,open,apply,isBusy}){
 let pending=null,chain=Promise.resolve();
 const serial=work=>{const operation=chain.then(work);chain=operation.catch(()=>{});return operation;};
 const context=()=>({...read(),pending});
 return {
  get pending(){return pending;},
  call(args){return serial(async()=>{
   validateSettingsRequest(args);
   if(args.action==='read')return {status:'current',...context()};
   if(args.action==='open'){const status=await open();return {status:status||'opened',...context()};}
   const {action,...patch}=args;
   const next=await normalize(patch,pending);
   if(isBusy()){pending=next;return {status:'queued',...context()};}
   await apply(next);return {status:'applied',...context()};
  });},
  flush({cancel=false}={}){return serial(async()=>{
   const next=pending;pending=null;
   if(!next||cancel)return {status:cancel?'cancelled':'unchanged',...context()};
   await apply(next);return {status:'applied',...context()};
  });},
 };
}
export async function serveSettings(call){
 // Short path also fits macOS's 104-byte Unix-socket limit.
 const root=await mkdtemp(join(tmpdir(),'aesel-')),socket=join(root,'settings.sock');
 const server=createServer(async(req,res)=>{
  const send=(status,value)=>{res.writeHead(status,{'Content-Type':'application/json','Cache-Control':'no-store'});res.end(JSON.stringify(value));};
  if(req.method!=='POST'||req.url!=='/settings'){send(404,{error:'Unknown control'});return;}
  try{
   let body='';for await(const chunk of req){body+=chunk;if(Buffer.byteLength(body)>4096)throw Error('Settings request too large');}
   const args=validateSettingsRequest(JSON.parse(body));
   send(200,await call(args));
  }catch(error){send(400,{error:error.message});}
 });
 server.requestTimeout=10000;server.headersTimeout=10000;
 try{await new Promise((resolve,reject)=>{server.once('error',reject);server.listen(socket,resolve);});await chmod(socket,0o600);}
 catch(error){server.close();await rm(root,{recursive:true,force:true});throw error;}
 server.unref();
 return {socket,async close(){server.closeAllConnections();await new Promise(resolve=>server.close(resolve));await rm(root,{recursive:true,force:true});}};
}
