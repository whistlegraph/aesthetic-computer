import '../../test/harness-tui-fixture.mjs';
import {appendFileSync} from 'node:fs';
import {BACKENDS} from '../../src/backends.mjs';
const Engine=BACKENDS.codex.Engine,original=Engine.prototype.startTurn;
const log=value=>appendFileSync(process.env.AESEL_TEST_LOG,JSON.stringify(value)+'\n');
Engine.prototype.startTurn=async function(text){
  log({event:'native-prompt',text});
  if(text!=='approval test')return original.call(this,text);
  this.emit('notification',{method:'turn/started',params:{turn:{id:'approval-turn'}}});
  this.emit('request',{id:77,method:'item/commandExecution/requestApproval',params:{command:'fixture-action'}});
};
Engine.prototype.respond=function(id,result){
  log({event:'native-approval',id,result});
  this.emit('notification',{method:'item/agentMessage/delta',params:{itemId:'answer',delta:'**Approval answered**'}});
  this.emit('notification',{method:'turn/completed',params:{turn:{status:'completed'}}});
};
