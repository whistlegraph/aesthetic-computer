import {spawn} from 'node:child_process';
import {EventEmitter} from 'node:events';
import {Readable,Writable} from 'node:stream';
import {markStartup} from './startup-trace.mjs';

// macOS spawn synchronously waits for exec even with asynchronous stdio. Keep
// that wait off the editor thread, including when switching/restarting engines.
export function spawnBridge(command,args,options){
  if(process.platform!=='darwin')return spawn(command,args,options);
  return new BridgeProcess(command,args,options);
}

class BridgeProcess extends EventEmitter {
  constructor(command,args,options){
    super();this.pid=null;this.killed=false;this.worker=null;this.queue=[];
    this.writes=new Map();this.nextWrite=0;this.exited=false;
    const send=message=>{if(this.worker)this.worker.postMessage(message);else this.queue.push(message);};
    this.send=send;
    for(const channel of ['stdout','stderr']){
      this[channel]=new Readable({read(){send({type:'resume',channel});}});
    }
    this.stdin=new Writable({write:(data,encoding,done)=>{
      const id=++this.nextWrite;this.writes.set(id,done);
      send({type:'write',id,data});
    },final:done=>{send({type:'end'});done();}});
    void import('node:worker_threads').then(({Worker})=>{
      markStartup('bridge-worker-start');
      this.worker=new Worker(new URL('./bridge-process-worker.mjs',import.meta.url),{
        execArgv:[],workerData:{command,args,options},
      });
      markStartup('bridge-worker-created');
      this.worker.on('message',message=>this.receive(message));
      this.worker.once('error',error=>this.fail(error));
      this.worker.once('exit',()=>{
        if(!this.exited)this.fail(new Error('Engine process worker closed'));
      });
      for(const message of this.queue)this.worker.postMessage(message);
      this.queue=[];
    }).catch(error=>this.fail(error));
  }
  receive(message){
    const {type,channel}=message;
    if(type==='spawn'){this.pid=message.pid;this.emit('spawn');}
    else if(type==='data'){
      if(this[channel].push(Buffer.from(message.data)))this.send({type:'resume',channel});
    }else if(type==='end')this[channel].push(null);
    else if(type==='write'){
      const done=this.writes.get(message.id);this.writes.delete(message.id);
      done?.(message.error?Object.assign(new Error(message.error.message),message.error):null);
    }else if(type==='error')this.fail(Object.assign(new Error(message.error.message),message.error));
    else if(type==='exit'){
      this.exited=true;this.emit('exit',message.code,message.signal);
    }else if(type==='close'){
      this.exited=true;this.emit('close',message.code,message.signal);
      this.finishWrites();
    }
  }
  finishWrites(){
    for(const done of this.writes.values())done(new Error('Engine stdin closed'));
    this.writes.clear();
    this.stdin.destroy();
    this.stdout.push(null);this.stderr.push(null);
  }
  fail(error){
    if(this.exited)return;
    this.kill();
    this.exited=true;this.emit('error',error);this.finishWrites();
    void this.worker?.terminate();
  }
  kill(signal='SIGTERM'){
    if(this.exited)return false;
    this.killed=true;
    // A known PID can be signalled immediately, even if the UI is exiting.
    if(this.pid){try{process.kill(this.pid,signal);}catch{return false;}}
    else this.send({type:'kill',signal});
    return true;
  }
}
