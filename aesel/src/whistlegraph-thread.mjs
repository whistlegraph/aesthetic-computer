// Stable local identity; the server atomically reserves its pronounceable code.
const ledgerText = ledger => ledger?JSON.stringify({format:ledger.format,head:ledger.head,versions:ledger.versions.map(v=>({id:v.id,parent:v.parent,source:v.source,request:v.request??null,createdAt:v.createdAt||'',layers:Number.isInteger(v.layers)?v.layers:0}))}):'null';
export async function verifyThreadRevision(command,state) {
  const before=state();if(before.busy)throw Error('Device busy');
  const digest=await crypto.subtle.digest('SHA-256',new TextEncoder().encode(before.source));
  const hash=[...new Uint8Array(digest)].map(b=>b.toString(16).padStart(2,'0')).join('');
  const after=state();
  if(after.busy||after.head!==before.head||after.source!==before.source||command.baseVersion!==after.head||command.baseHash!==hash)throw Error('Version changed; inspect before editing');
}
export function threadIdentity(storage,key,uuid=()=>crypto.randomUUID()) {
  const saved=storage.getItem(key+'-thread');
  if(saved){const value=JSON.parse(saved);if(typeof value.id!=='string')throw Error('Invalid thread identity');return value;}
  const value={id:uuid(),code:null};storage.setItem(key+'-thread',JSON.stringify(value));return value;
}
export class WhistlegraphThread {
  constructor({storage,key,token,ledger,state,onStatus,onCommand,onAdopt=null,onTurn=null,receipts=null,WebSocketImpl=globalThis.WebSocket,url='wss://aesthetic.computer/api/whistlegraph-stream',heartbeatMs=15000,maxIdleMs=45000,reconnectMs=3000}) {
    this.receipts=receipts;
    Object.assign(this,{storage,key,token,ledger,state,onStatus,onCommand,onAdopt,onTurn,WebSocketImpl,url,heartbeatMs,maxIdleMs,reconnectMs});
    this.identity=threadIdentity(storage,key);this.revision=Number(storage.getItem(key+'-cloud-revision')||0);this.last=storage.getItem(key+'-cloud-ledger')||'';
    this.active=false;this.sending=false;this.ready=false;this.connectURL=url;
  }
  async resume() {
    this.active=true;if(this.ws)return;
    const token=await this.token();if(!token||!this.active||this.ws)return;
    const ws=this.ws=new this.WebSocketImpl(this.connectURL);let opened=false;
    this.lastSeen=Date.now();
    this.heartbeat=setInterval(()=>{if(Date.now()-this.lastSeen>this.maxIdleMs)this.disconnect(ws);else if(this.ready)this.send({type:'ping'});},this.heartbeatMs);
    ws.onopen=()=>{opened=true;this.send({type:'authenticate',role:'device',token,id:this.identity.id});};
    ws.onmessage=async event=>{
      let m;try{m=JSON.parse(event.data);}catch{return;}
      if(this.ws!==ws)return;
      this.lastSeen=Date.now();
      if(m.type==='ready') {
        this.receiptSupport=m.capabilities?.includes('attempt-receipts-v1')===true;
        this.identity.code=m.thread.code;this.storage.setItem(this.key+'-thread',JSON.stringify(this.identity));
        const cloud=ledgerText(m.thread.ledger),local=ledgerText(this.ledger());
        // Never silently replace local work with another device's history. A
        // device with nothing unsynced (its local ledger is the last cloud
        // ledger it saw) follows the server forward: that is a turn that ran
        // off the phone, not another device's history.
        if(m.thread.ledger&&cloud!==local&&m.thread.revision!==this.revision){
          if(this.canFollow(local,m.thread)){await this.follow(m.thread);}
          else {this.onStatus(this.identity.code,'History conflict');return;}
        }
        this.revision=m.thread.revision;this.ready=true;
        if(cloud===local){this.last=local;this.storage.setItem(this.key+'-cloud-revision',String(this.revision));this.storage.setItem(this.key+'-cloud-ledger',local);}
        this.onStatus(this.identity.code,'Connected');this.sync();this.update();this.flushReceipts();
      }
      if(m.type==='turn'&&m.turn){try{await this.onTurn?.(m.turn);}catch{}}
      if(m.type==='updated'&&m.thread?.ledger) {
        if(this.sending)return;
        const cloud=ledgerText(m.thread.ledger),local=ledgerText(this.ledger());
        if(cloud===local){this.revision=m.thread.revision;this.last=cloud;this.storage.setItem(this.key+'-cloud-revision',String(this.revision));this.storage.setItem(this.key+'-cloud-ledger',cloud);return;}
        if(this.canFollow(local,m.thread)){await this.follow(m.thread);this.update();}
        else this.onStatus(this.identity.code,'History conflict');
      }
      if(m.type==='saved') {
        this.revision=m.thread.revision;this.last=ledgerText(m.thread.ledger);this.sending=false;
        this.storage.setItem(this.key+'-cloud-revision',String(this.revision));this.storage.setItem(this.key+'-cloud-ledger',this.last);this.sync();
      }
      if(m.type==='receiptSaved'&&m.id===this.receiptSending) {
        this.receipts?.acknowledge(m.id);this.receiptSending=null;
        this.receiptTimer=setTimeout(()=>this.flushReceipts(),150);
      }
      if(m.type==='receiptError'&&m.id===this.receiptSending) {
        this.receiptSending=null;this.receiptTimer=setTimeout(()=>this.flushReceipts(),5000);
      }
      if(m.type==='conflict'){this.ready=false;this.sending=false;this.onStatus(this.identity.code,'History conflict');}
      if(m.type==='error'){this.ready=false;this.sending=false;this.onStatus(this.identity.code,m.error);}
      if(m.type==='command') {
        try{if(m.action==='layout'){const result=await this.onCommand(m);this.send({type:'result',id:m.id,...result});return;}if(!this.ready||this.sending)throw Error('Thread is not synchronized');const result=await this.onCommand(m);this.sync();await this.flush();this.send({type:'result',id:m.id,...result});}
        catch(error){this.send({type:'result',id:m.id,ok:false,error:error.message});}
        this.sync();this.update();
      }
    };
    ws.onclose=()=>{
      // An installed phone can update before the service route is deployed.
      // Fall back only within the same first-party host, before authentication.
      if(this.ws===ws&&!opened&&this.connectURL==='wss://aesthetic.computer/api/whistlegraph-stream')this.connectURL='wss://aesthetic.computer/api/walkieware-stream';
      this.disconnect(ws);
    };
    ws.onerror=()=>{};
  }
  send(value){if(this.ws?.readyState===1)this.ws.send(JSON.stringify(value));}
  canFollow(local,thread){return !!this.onAdopt&&local===this.last&&Number.isSafeInteger(thread.revision)&&thread.revision>this.revision;}
  async follow(thread){
    this.revision=thread.revision;this.last=ledgerText(thread.ledger);
    this.storage.setItem(this.key+'-cloud-revision',String(this.revision));this.storage.setItem(this.key+'-cloud-ledger',this.last);
    await this.onAdopt(thread.ledger,thread);
    this.onStatus(this.identity.code,'Updated');
  }
  sync(){if(!this.ready||this.sending)return;const ledger=this.ledger(),next=ledgerText(ledger);if(next===this.last)return;this.sending=true;this.send({type:'sync',revision:this.revision,ledger});}
  update(){if(this.ready)this.send({type:'state',state:this.state()});}
  flushReceipts(){
    if(!this.ready||!this.receiptSupport||this.receiptSending)return;
    const receipt=this.receipts?.pending();if(!receipt)return;
    this.receiptSending=receipt.id;this.send({type:'receipt',receipt});
  }
  async flush(){for(let i=0;i<100;i++){if(!this.ready)throw Error('Connection lost; inspect history before retrying');if(!this.sending&&this.last===ledgerText(this.ledger()))return;await new Promise(r=>setTimeout(r,100));}throw Error('Version sync pending; inspect before retrying');}
  disconnect(ws){if(this.ws!==ws)return;this.ws=null;this.ready=false;this.sending=false;this.receiptSending=null;clearTimeout(this.receiptTimer);clearInterval(this.heartbeat);this.onStatus(this.identity.code,'Offline');try{ws.close();}catch{}if(this.active)this.timer=setTimeout(()=>this.resume(),this.reconnectMs);}
  suspend(){this.active=false;clearTimeout(this.timer);if(this.ws)this.disconnect(this.ws);}
}
