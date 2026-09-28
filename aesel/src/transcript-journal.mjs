import { randomUUID } from 'node:crypto';
import { constants } from 'node:fs';
import { mkdir, lstat, readFile, writeFile, rename, rm, open } from 'node:fs/promises';
import { join } from 'node:path';
import { validateHeader, validateRecord, serializeTranscript, parseTranscript, MAX_BATCH_BYTES } from './transcript-format.mjs';
import { USER_AGENT } from './ac-session.mjs';

export {TRANSCRIPT_DISCLOSURE} from './required-sharing.mjs';
export class TranscriptJournal {
  constructor({root,id=randomUUID(),metadata={},version='0.7',session,fetch=globalThis.fetch,now=()=>new Date().toISOString(),endpoint='https://aesthetic.computer/api/easel-transcripts'}={}) {
    this.root=root;this.session=session;this.fetch=fetch;this.now=now;this.endpoint=endpoint;this.chain=Promise.resolve();this.revoked=false;
    if(endpoint!=='https://aesthetic.computer/api/easel-transcripts')throw new Error('Transcript uploads only go to aesthetic.computer');
    this.header=validateHeader({type:'session',format:'aesthetic.easel',version:1,id,createdAt:now(),metadata,consent:{sharing:'private'},provenance:{application:'easel',version}});
    this.file=`${id}.easel`;this.settingsFile=`${id}.sharing.json`;this.queueFile=`${id}.queue.json`;
  }
  serial(work){const operation=this.chain.then(async()=>{await this.path(this.file);const lock=join(this.root,`${this.header.id}.lock`);try{await mkdir(lock,{mode:0o700});}catch(error){if(error.code==='EEXIST')throw new Error('Transcript journal is busy; another writer holds its lock');throw error;}try{return await work();}finally{await rm(lock,{recursive:true,force:true});}});this.chain=operation.catch(()=>{});return operation;}
  async path(name){await mkdir(this.root,{recursive:true,mode:0o700});if((await lstat(this.root)).isSymbolicLink())throw new Error('Transcript directory cannot be a symlink');const path=join(this.root,name);try{if(!(await lstat(path)).isFile())throw new Error('Unsafe transcript file');}catch(e){if(e.code!=='ENOENT')throw e;}return path;}
  async json(name,fallback){try{return JSON.parse(await readFile(await this.path(name),'utf8'));}catch(e){if(e.code==='ENOENT')return fallback;throw e;}}
  async atomic(name,value){const target=await this.path(name),tmp=join(this.root,`.${randomUUID()}.tmp`);try{await writeFile(tmp,JSON.stringify(value)+'\n',{flag:'wx',mode:0o600});await rename(tmp,target);}finally{await rm(tmp,{force:true});}}
  async init(){return this.serial(async()=>{const path=await this.path(this.file);try{await writeFile(path,serializeTranscript(this.header,[]),{flag:'wx',mode:0o600});}catch(e){if(e.code!=='EEXIST')throw e;this.header=parseTranscript(await readFile(path,'utf8')).header;}return this.status();});}
  async status(){const settings=await this.json(this.settingsFile,{sharing:false}),queue=await this.json(this.queueFile,[]);return{sharing:settings.sharing===true&&!this.revoked,pending:queue.length,label:settings.sharing===true&&!this.revoked?'Shared privately with AC':'Private on this computer'};}
  async enableSharing({userSub,acknowledged=false,disclosureVersion=1}={}){return this.serial(async()=>{
    if(!acknowledged||typeof userSub!=='string'||!userSub||userSub.length>200)throw new Error('Explicit signed-in user consent is required');
    if(this.session?.read()?.user?.sub!==userSub)throw new Error('Sign in as the consenting user first');
    const existing=await this.json(this.settingsFile,{});
    if(existing.sharing&&existing.userSub===userSub&&existing.consent?.disclosureVersion===disclosureVersion&&!this.revoked)return this.status();
    if(!existing.sharing || existing.userSub!==userSub)await this.atomic(this.queueFile,[]);
    await this.atomic(this.settingsFile,{sharing:true,userSub,consent:{sharing:'company',id:randomUUID(),acceptedAt:this.now(),disclosureVersion}});
    this.revoked=false;return this.status();
  });}
  async revoke(){this.revoked=true;this.abort?.abort();return this.serial(async()=>{await this.atomic(this.settingsFile,{sharing:false});await this.atomic(this.queueFile,[]);return this.status();});}
  async append(event){return this.serial(async()=>{
    const path=await this.path(this.file),document=parseTranscript(await readFile(path,'utf8'));
    const previous=event.id?document.records.find(record=>record.id===event.id):null;
    const record=validateRecord({...event,id:event.id??randomUUID(),seq:previous?.seq??(document.records.at(-1)?.seq??0)+1,at:previous?.at??this.now()});
    if(previous&&JSON.stringify(record)!==JSON.stringify(previous))throw new Error('Transcript entry ID already has different content');
    if(!previous){
    serializeTranscript(document.header,[...document.records,record]);
    const handle=await open(path,constants.O_WRONLY|constants.O_APPEND|constants.O_NOFOLLOW,0o600);
    try{await handle.writeFile(JSON.stringify(record)+'\n');await handle.sync();}finally{await handle.close();}
    }
    const settings=await this.json(this.settingsFile,{sharing:false});
    if(settings.sharing&&!this.revoked){if(this.session?.read()?.user?.sub!==settings.userSub)throw new Error('Transcript saved locally; sharing paused because account changed');const queue=await this.json(this.queueFile,[]);if(!queue.some(item=>item.id===record.id))queue.push(record);if(queue.length>500||Buffer.byteLength(JSON.stringify(queue))>2*1024*1024)throw new Error('Transcript saved locally; private-sharing queue is full. Reconnect before continuing.');await this.atomic(this.queueFile,queue);}
    return record;
  });}
  async export({redact=true}={}){return this.serial(async()=>{const document=parseTranscript(await readFile(await this.path(this.file),'utf8'));return serializeTranscript(document.header,document.records,{redact});});}
  async flush(){return this.serial(async()=>{
    const settings=await this.json(this.settingsFile,{sharing:false});
    if(!settings.sharing||this.revoked)return{sent:0,...await this.status()};
    if(this.session?.read()?.user?.sub!==settings.userSub)throw new Error('Transcript sharing paused: signed-in account changed');
    const queue=await this.json(this.queueFile,[]);if(!queue.length)return{sent:0,...await this.status()};
    const header={...this.header,consent:settings.consent},records=[];
    for(const record of queue){if(records.length>=100)break;try{serializeTranscript(header,[...records,record],{redact:true,maxBytes:MAX_BATCH_BYTES});records.push(record);}catch(e){if(!records.length)throw e;break;}}
    const token=await this.session.token();if(this.revoked)return{sent:0,...await this.status()};if(this.session?.read()?.user?.sub!==settings.userSub)throw new Error('Transcript sharing paused: signed-in account changed');
    const controller=new AbortController();this.abort=controller;
    const timeout=setTimeout(()=>controller.abort(),15000);timeout.unref?.();
    try{
      const response=await this.fetch(this.endpoint,{method:'POST',headers:{Authorization:`Bearer ${token}`,'Content-Type':'application/x-ndjson','User-Agent':USER_AGENT},body:serializeTranscript(header,records,{redact:true,maxBytes:MAX_BATCH_BYTES}),signal:controller.signal});
      if(!response.ok)throw new Error(`Private transcript upload failed (HTTP ${response.status}); retained for retry`);
      if(!this.revoked)await this.atomic(this.queueFile,queue.slice(records.length));
      return{sent:records.length,...await this.status()};
    }finally{clearTimeout(timeout);if(this.abort===controller)this.abort=null;}
  });}
  async deleteRemote(){const settings=await this.json(this.settingsFile,{});if(settings.userSub&&this.session?.read()?.user?.sub!==settings.userSub)throw new Error('Sign in as the sharing user to delete this transcript');await this.revoke();const token=await this.session.token();const response=await this.fetch(this.endpoint,{method:'DELETE',headers:{Authorization:`Bearer ${token}`,'Content-Type':'application/json','User-Agent':USER_AGENT},body:JSON.stringify({sessionId:this.header.id}),signal:AbortSignal.timeout(15000)});if(!response.ok)throw new Error(`Could not delete shared transcript (HTTP ${response.status})`);return{deleted:true};}
}
