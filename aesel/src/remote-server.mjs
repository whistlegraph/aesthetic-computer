// Closing a client detaches; accepted turns and their output stay on the relay.
import {EventEmitter} from 'node:events';
import {randomUUID} from 'node:crypto';
export class RemoteServer extends EventEmitter {
  constructor({token,resumeThreadId='',model='',effort='',developerInstructions='',
    url=process.env.AESEL_RELAY_URL||'https://help.aesthetic.computer',
    provider=process.env.AESEL_RELAY_PROVIDER||'claude',fetch=globalThis.fetch,pollMs=500}={}) {
    super();Object.assign(this,{token,model,effort,developerInstructions,provider,fetch,pollMs});
    this.url=url.replace(/\/$/,'');this.threadId=resumeThreadId;this.turnId=null;
    this.closed=false;this.cursor=0;this.seenApprovals=new Set();this.abort=new AbortController();
  }
  async call(path,body) {
    const token=await this.token?.();if(!token)throw Error('Sign in to Aesthetic Computer to use your private relay');
    const response=await this.fetch(`${this.url}/api/aesel/sessions${path}`,{
      method:body===undefined?'GET':'POST',headers:{authorization:`Bearer ${token}`,'content-type':'application/json'},
      ...(body===undefined?{}:{body:JSON.stringify(body)}),signal:AbortSignal.any([this.abort.signal,AbortSignal.timeout(20000)])});
    const result=await response.json();
    if(!response.ok)throw Object.assign(Error(result.error||`Relay returned ${response.status}`),{status:response.status});
    return result;
  }
  async connect() {
    const result=this.threadId?await this.call(`/${this.threadId}?after=0`):await this.call('',{
      provider:this.provider,model:this.model,effort:this.effort,
      instructions:`You are in a remote workspace on help.aesthetic.computer. Client paths are context only and do not exist on this host.\n\n${this.developerInstructions}`});
    this.threadId=result.thread.id;
    this.timer=setTimeout(()=>void this.poll(),0);
    return {thread:{id:this.threadId,turns:[]},model:this.model};
  }
  async poll() {
    if(this.closed)return;
    try {
      const data=await this.call(`/${this.threadId}?after=${this.cursor}`);
      for(const entry of data.events){
        if(entry.seq<=this.cursor)continue;this.cursor=entry.seq;
        if(entry.type==='notification'){
          if(entry.value.method==='turn/started')this.turnId=entry.value.params?.turn?.id;
          if(entry.value.method==='turn/completed')this.turnId=null;
          this.emit('notification',entry.value);
        }else if(entry.type==='fatal')this.emit('notification',{method:'turn/completed',params:{turn:{id:this.turnId,status:'failed',error:entry.value}}});
      }
      for(const request of data.pending)if(!this.seenApprovals.has(String(request.id))){
        this.seenApprovals.add(String(request.id));this.emit('request',request);
      }
      this.failed=false;
    }catch(error){
      if(this.closed)return;
      if(error.status===401||error.status===403){this.emit('fatal',error);this.close();return;}
      if(!this.failed)this.emit('log','Relay disconnected; reconnecting to saved turn');this.failed=true;
    }
    if(!this.closed)this.timer=setTimeout(()=>void this.poll(),this.failed?2000:this.pollMs);
  }
  async startTurn(text,{images=[],requestId=randomUUID()}={}) {
    const input={text,images,requestId};
    let request;
    try {request=await this.call(`/${this.threadId}/turn`,input);}
    catch(error){
      if(this.closed || (error.status && error.status<500))throw error;
      request=await this.call(`/${this.threadId}/turn`,input);
    }
    return {turn:{id:request.requestId,status:request.status==='running'?'inProgress':request.status}};
  }
  respond(id,result){void this.call(`/${this.threadId}/respond`,{id,result}).catch(error=>{
    this.seenApprovals.delete(String(id));this.emit('log',error.message);
  });}
  reject(id){this.respond(id,{decision:'decline'});}
  async interrupt(){return this.call(`/${this.threadId}/interrupt`,{});}
  async newThread(){clearTimeout(this.timer);this.threadId='';this.cursor=0;this.seenApprovals.clear();return this.connect();}
  close(){this.closed=true;clearTimeout(this.timer);this.abort.abort();}
}
