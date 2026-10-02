// Provider sidecar for the C terminal experiment. Reuse the production transport;
// the native process owns input and painting. No private checkpoints are written.
import {resolve} from 'node:path';
import {pathToFileURL} from 'node:url';
import {AppServer} from '../../src/app-server.mjs';
import {CONTINUE_INTERRUPTED_TURN} from '../../src/turn-recovery.mjs';

class ReadOnlyServer extends AppServer {
  constructor(options){
    super(options);
    // This prototype has no Aesel tool host. Do not register its media/MCP tools.
    this.args=['app-server','--listen','stdio://'];
  }
  async newThread(){
    const result=await this.request('thread/start',{
      cwd:this.cwd,approvalPolicy:'never',sandbox:'read-only',ephemeral:false,
      ...(this.model?{model:this.model}:{}),
      developerInstructions:'This is the experimental Aesel C terminal. Work read-only. It has no approval UI, artifact tools, or publishing support.',
    });
    this.threadId=result.thread.id;this.turnId=null;return result;
  }
  async resumeThread(threadId){
    const result=await this.request('thread/resume',{
      threadId,cwd:this.cwd,approvalPolicy:'never',sandbox:'read-only',
      ...(this.model?{model:this.model}:{}),
    });
    this.threadId=result.thread.id;this.turnId=null;return result;
  }
}

export class NativeBridge {
  constructor({send,create=options=>new ReadOnlyServer(options),cwd=process.cwd(),resume='',model='',retryTimeout=90000}){
    Object.assign(this,{send,create,cwd,model,retryTimeout});
    this.threadId=resume;this.engine=null;this.connecting=null;
    this.pending=null;this.generation=0;this.active=false;this.closed=false;this.watchdog=null;
  }
  state(text){this.send('S',text);}
  busy(value){this.active=value;this.send('B',value?'1':'0');}
  clearWatchdog(){clearTimeout(this.watchdog);this.watchdog=null;}
  fail(error){
    if(this.closed)return;
    this.clearWatchdog();this.busy(false);
    this.send('E',`${error?.message||error} · /retry continues the thread`);
  }
  async connect(){
    if(this.connecting)return this.connecting;
    if(this.engine?.threadId&&!this.engine.closed)return;
    if(this.engine)this.detach();
    const engine=this.engine=this.create({cwd:this.cwd,resumeThreadId:this.threadId,model:this.model});
    engine.on('request',({id})=>engine.reject(id,-32601,'The C prototype has no approval or tool UI.'));
    engine.on('fatal',error=>{if(this.engine===engine)this.fail(error);});
    engine.on('protocolError',error=>{if(this.engine===engine){engine.close();this.fail(error);}});
    engine.on('notification',({method,params={}})=>{
      if(this.engine!==engine||!this.active)return;
      if(method==='turn/started' && this.pending)this.pending.accepted=true;
      if(method==='item/agentMessage/delta'){
        this.clearWatchdog();this.send('D',params.delta||'');
      }else if(method==='error'){
        if(params.willRetry){
          this.state('Provider reconnecting · Ctrl-C stop');
          if(!this.watchdog)this.watchdog=setTimeout(()=>{engine.close();this.fail(new Error('Provider retry timed out'));},this.retryTimeout);
        }else this.fail(params.error||new Error('Provider failed'));
      }else if(method==='turn/completed'){
        this.clearWatchdog();
        if(params.turn?.status==='completed'){
          this.pending=null;this.busy(false);this.state('Ready · /quit exits');
        }else this.fail(params.turn?.error||new Error(`Turn ${params.turn?.status||'interrupted'}`));
      }
    });
    this.connecting=engine.connect().then(()=>{
      if(this.engine!==engine)return;
      this.threadId=engine.threadId;this.send('H',this.threadId);
      if(!this.active)this.state('Ready · /quit exits');
    }).finally(()=>{if(this.engine===engine)this.connecting=null;});
    return this.connecting;
  }
  detach(){
    const engine=this.engine;
    this.threadId=engine?.threadId||this.threadId;
    this.engine=null;this.connecting=null;engine?.close();this.clearWatchdog();
  }
  async submit(text,{retry=false}={}){
    if(this.closed||this.active)return;
    if(retry&&!this.pending){this.state('No interrupted request to retry');this.send('B','0');return;}
    if(!retry)this.pending={text,accepted:false,submitted:false};
    else this.detach();
    const pending=this.pending,generation=++this.generation;
    this.busy(true);this.state(retry?'Resuming the same thread':'Connecting to provider');
    try{
      await this.connect();
      if(generation!==this.generation||this.closed)return;
      const prompt=retry&&(pending.accepted||pending.submitted)
        ?`${CONTINUE_INTERRUPTED_TURN}${pending.accepted?'':`\nThe interrupted request was: ${pending.text}`}`:pending.text;
      pending.submitted=true;this.state('Waiting for reply · Ctrl-C stop');
      await this.engine.startTurn(prompt);
      if(generation===this.generation&&this.pending===pending)pending.accepted=true;
    }catch(error){if(generation===this.generation)this.fail(error);}
  }
  cancel(){
    this.generation++;this.pending=null;
    // Closing the bridge cancels startup too. Save the provider thread ID for
    // the next connection; never replay a cancelled prompt.
    this.detach();this.busy(false);this.state('Stopped · draft kept');
  }
  close(){this.closed=true;this.generation++;this.detach();}
}

export function frame(type,text=''){
  const body=Buffer.from(text);const header=Buffer.alloc(5);
  header[0]=type.charCodeAt(0);header.writeUInt32BE(body.length,1);
  return Buffer.concat([header,body]);
}
export function decodeInput(onFrame){
  let buffered=Buffer.alloc(0);
  return chunk=>{
    buffered=Buffer.concat([buffered,chunk]);
    while(buffered.length>=5){
      const length=buffered.readUInt32BE(1);
      if(length>=8192)throw Error('Native input frame exceeds 8191 bytes');
      if(buffered.length<length+5)break;
      const type=String.fromCharCode(buffered[0]);
      if(!['P','R','I','Q'].includes(type))throw Error('Unknown native input frame');
      onFrame(type,buffered.subarray(5,length+5).toString('utf8'));
      buffered=buffered.subarray(length+5);
    }
  };
}

if(process.argv[1]&&pathToFileURL(resolve(process.argv[1])).href===import.meta.url){
  const option=name=>{const index=process.argv.indexOf(name);return index<0?'':process.argv[index+1]||'';};
  const send=(type,text)=>{
    // A stalled terminal must not cause unbounded JS output buffering.
    if(process.stdout.writableLength>1048576){bridge.close();process.exitCode=1;process.stdin.destroy();process.stdout.destroy();return;}
    const body=Buffer.from(text);
    if(type==='D'&&body.length>60000){
      for(let offset=0;offset<body.length;){
        let end=Math.min(offset+60000,body.length);
        while(end<body.length&&(body[end]&0xc0)===0x80)end--;
        process.stdout.write(frame(type,body.subarray(offset,end)));offset=end;
      }
    }else process.stdout.write(frame(type,body.subarray(0,60000)));
  };
  const bridge=new NativeBridge({send,resume:option('--resume'),model:option('--model')});
  const close=()=>{bridge.close();process.exit(0);};
  process.once('SIGTERM',close);process.once('SIGINT',close);
  process.stdin.once('end',close);process.stdout.on('error',close);
  const decode=decodeInput((type,text)=>{
    if(type==='P')void bridge.submit(text);
    else if(type==='R')void bridge.submit('',{retry:true});
    else if(type==='I')bridge.cancel();else close();
  });
  process.stdin.on('data',chunk=>{try{decode(chunk);}catch(error){send('E',error.message);close();}});
  void bridge.connect().catch(error=>bridge.fail(error));
}
