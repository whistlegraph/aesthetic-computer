import './harness-tui-fixture.mjs';
import {appendFileSync} from 'node:fs';
import {BACKENDS} from '../src/backends.mjs';
import {TurnRecovery,CONTINUE_INTERRUPTED_TURN} from '../src/turn-recovery.mjs';
const log=value=>appendFileSync(process.env.AESEL_TEST_LOG,JSON.stringify(value)+'\n');
const schedule=TurnRecovery.prototype.schedule;
TurnRecovery.prototype.schedule=function(error){this.delays=[250,250,250];this.providerTimeout=100;return schedule.call(this,error);};
const retry=TurnRecovery.prototype.providerRetry;
TurnRecovery.prototype.providerRetry=function(){this.providerTimeout=100;return retry.call(this);};
const Engine=BACKENDS.codex.Engine;
let serial=0,mode='',failures=0;
Engine.prototype.connect=async function(){this.serial=++serial;log({event:'connect',serial,resume:this.resumeThreadId});return {thread:{id:this.threadId},model:'test'};};
Engine.prototype.close=function(){this.closed=true;log({event:'close',serial:this.serial});};
Engine.prototype.startTurn=async function(text){
 log({event:'prompt',text,serial:this.serial});this.turns++;
 if(!text.startsWith(CONTINUE_INTERRUPTED_TURN)){mode=text;failures=0;}
 if(mode==='before start'&&failures++===0)throw new TypeError('fetch failed');
 const id='turn-'+this.serial+'-'+this.turns;
 this.emit('notification',{method:'turn/started',params:{turn:{id}}});
 const done=()=>{if(this.closed)return;this.emit('notification',{method:'item/agentMessage/delta',params:{turnId:id,itemId:'reply-'+id,delta:'Recovered '+mode}});this.emit('notification',{method:'turn/completed',params:{turn:{id,status:'completed'}}});log({event:'done',mode});};
 const fail=()=>this.emit('notification',{method:'error',params:{turnId:id,error:{message:'stream disconnected before completion'},willRetry:false}});
 if(mode==='permanent'){this.emit('notification',{method:'error',params:{turnId:id,error:{message:'Invalid API key'},willRetry:false}});log({event:'permanent'});return;}
 if(mode==='exhaust'&&failures++<4){setTimeout(fail,10);return;}
 if(['terminal failure','cancel retry','quit during retry'].includes(mode)&&failures++===0){setTimeout(fail,10);return;}
 if(mode==='bridge dies'&&failures++===0){setTimeout(()=>this.emit('fatal',Object.assign(new Error('engine bridge closed'),{bridgeFailure:true})),10);return;}
 if(['provider retries','provider stalls'].includes(mode)&&failures++===0){
  this.emit('notification',{method:'error',params:{turnId:id,error:{message:'stream disconnected before completion'},willRetry:true}});
  if(mode==='provider retries')setTimeout(done,40);return;
 }
 setTimeout(done,20);
};
