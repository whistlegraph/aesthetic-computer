// Offline TUI integration with private temporary account/history stores.
import os from 'node:os';
import {syncBuiltinESMExports} from 'node:module';
import {mkdirSync,writeFileSync,appendFileSync} from 'node:fs';
import {join,dirname} from 'node:path';
import {createHash} from 'node:crypto';
import {EventEmitter} from 'node:events';
const root=dirname(process.env.EASEL_TEST_LOG);os.homedir=()=>root;syncBuiltinESMExports();
const {ACSession}=await import('../src/ac-session.mjs');
const {BACKENDS}=await import('../src/backends.mjs');
const {Audience}=await import('../src/audience.mjs');
const {Diagnostics}=await import('../src/diagnostics.mjs');
const {DISCLOSURE_VERSION}=await import('../src/required-sharing.mjs');
const owner='fixture-harness',dir=join(root,'.config/easel/disclosures');mkdirSync(dir,{recursive:true});
writeFileSync(join(dir,createHash('sha256').update(owner).digest('hex')+'.json'),JSON.stringify({owner,version:DISCLOSURE_VERSION,acceptedAt:new Date().toISOString()}));
ACSession.prototype.read=()=>({access_token:'fixture',user:{sub:owner,handle:'tester'}});ACSession.prototype.token=async()=>'fixture';ACSession.prototype.watch=function(){return this;};ACSession.prototype.unwatch=()=>{};
Audience.prototype.watch=()=>{};Diagnostics.prototype.watch=async()=>{};
globalThis.fetch=async url=>{if(String(url).endsWith('/api/easel-transcripts'))return {ok:true,json:async()=>({})};throw Error('Network disabled in harness fixture');};
const log=value=>appendFileSync(process.env.EASEL_TEST_LOG,JSON.stringify(value)+'\n');
class Engine extends EventEmitter{
 constructor(options){super();Object.assign(this,options);this.threadId='fixture';this.turns=0;}
 async connect(){if(this.model==='broken')throw Error('Fixture provider unavailable');log({event:'connected',context:this.developerInstructions,socket:this.environment.EASEL_HARNESS_SOCKET});return {model:this.model||'fixture-model'};}
 close(){}
 async startTurn(text){
  this.turns++;this.emit('notification',{method:'turn/started',params:{turn:{id:'fixture-turn'}}});
  try{
   if(text.startsWith('switch provider')){log({event:'tool',result:await this.settings({action:'update',provider:'codex',model:'fixture-model'})});}
   else if(text.startsWith('fail switch')){log({event:'tool',result:await this.settings({action:'update',provider:'claude',model:'broken'})});}
   else if(text.startsWith('open settings')){log({event:'tool',result:await this.settings({action:'open'})});}
   else log({event:'read',result:await this.settings({action:'read'})});
  }catch(error){log({event:'error',message:error.message});}
  this.emit('notification',{method:'item/agentMessage/delta',params:{itemId:'reply-'+this.turns,delta:'Settings request handled.'}});
  this.emit('notification',{method:'turn/completed',params:{turn:{status:'completed'}}});
 }
 interrupt(){}
}
for(const backend of Object.values(BACKENDS))backend.Engine=Engine;
