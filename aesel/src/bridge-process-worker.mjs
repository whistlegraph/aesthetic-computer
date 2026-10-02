import {parentPort,workerData} from 'node:worker_threads';
import {spawn} from 'node:child_process';

const detail=error=>({message:error.message,code:error.code});
const child=spawn(workerData.command,workerData.args,workerData.options);
child.once('spawn',()=>parentPort.postMessage({type:'spawn',pid:child.pid}));
child.once('error',error=>parentPort.postMessage({type:'error',error:detail(error)}));
child.stdin.on('error',()=>{}); // Individual writes return their own error.
for(const channel of ['stdout','stderr']){
  child[channel].on('data',data=>{
    child[channel].pause();parentPort.postMessage({type:'data',channel,data});
  });
  child[channel].once('end',()=>parentPort.postMessage({type:'end',channel}));
}
parentPort.on('message',message=>{
  if(message.type==='write')child.stdin.write(message.data,error=>parentPort.postMessage({
    type:'write',id:message.id,...(error?{error:detail(error)}:{}),
  }));
  else if(message.type==='end')child.stdin.end();
  else if(message.type==='resume')child[message.channel].resume();
  else if(message.type==='kill')child.kill(message.signal);
});
child.once('exit',(code,signal)=>parentPort.postMessage({type:'exit',code,signal}));
child.once('close',(code,signal)=>{
  parentPort.postMessage({type:'close',code,signal});parentPort.close();
});
