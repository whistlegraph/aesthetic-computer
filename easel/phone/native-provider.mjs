import {randomUUID} from './shim/crypto.mjs';
import {EventEmitter} from 'node:events';

// Transport is injected: native URLSession retains the host credential, and
// tests exercise the same reconnect/operation contract without WebKit.
export class NativeProvider extends EventEmitter {
  constructor({rpc,sessionID,provider,model,source,onSource,onOperation,context='',operation=null}) {
    super();Object.assign(this,{rpc,sessionID,provider,model,source,onSource,onOperation,context,operation});
    this.operation=operation?{...operation}:null;
    this.after=operation?.after || 0;this.threadId='';this.turns=0;
  }
  async startTurn(text) {
    if (!this.operation) {
      const configured=await this.rpc('configure',{sessionID:this.sessionID,provider:this.provider,model:this.model,source:this.source(),context:this.context});
      this.after=configured.sequence;
      this.operation={id:randomUUID(),after:this.after};
      this.onOperation(this.operation);
      try { await this.rpc('turn',{sessionID:this.sessionID,operationID:this.operation.id,text}); }
      catch(error) {
        // The request may have reached the host. Keep its ID for status lookup,
        // never manufacture another ID and accidentally submit the prompt twice.
        throw new Error(`Host connection interrupted. Reconnect to check this turn. ${error.message}`);
      }
    }
    return this.follow();
  }
  async follow() {
    let failures=0;
    while(this.operation) {
      let result;
      try {
        result=await this.rpc('events',{sessionID:this.sessionID,operationID:this.operation.id,after:this.after});
        failures=0;
      } catch(error) {
        if(++failures>=5)throw new Error('The Mac is unavailable. The turn may still be running; reconnect to check it.');
        await new Promise(resolve=>setTimeout(resolve,1000));continue;
      }
      if(result.storageError)throw new Error(result.storageError);
      if(this.after && result.oldest>this.after+1)throw new Error('Some host events are no longer available. Keep this draft and reconnect before continuing.');
      for(const event of result.events || []) {
        if(event.sequence<=this.after)continue;
        this.after=event.sequence;
        if(event.operation && event.operation!==this.operation.id)continue;
        if(event.type==='source')this.onSource(event.source);
        if(event.type==='notification')this.emit('notification',{method:event.method,params:event.params});
        if(event.type==='approval')this.emit('approval',event);
        if(event.type==='error')this.emit('notification',{method:'warning',params:{message:event.message}});
      }
      this.emit('approval',result.approvals?.[0] || null);
      this.operation.after=this.after;this.onOperation(this.operation);
      if(!result.operation)throw new Error('The host has no record of this turn. It was not automatically replayed.');
      if(result.operation.status!=='running') {
        const outcome=result.operation;this.operation=null;this.onOperation(null);
        this.emit('approval',null);
        if(outcome.status==='failed')throw new Error(outcome.error || 'Provider turn failed');
        if(outcome.status==='interrupted')this.emit('notification',{method:'warning',params:{message:outcome.error || 'Turn interrupted'}});
        return outcome;
      }
      await new Promise(resolve=>setTimeout(resolve,250));
    }
  }
  interrupt() {
    if(!this.operation)return;
    return this.rpc('interrupt',{sessionID:this.sessionID,operationID:this.operation.id});
  }
  respond(id,decision) {
    if(!this.operation)throw new Error('This approval has expired');
    return this.rpc('approval',{sessionID:this.sessionID,operationID:this.operation.id,id,decision});
  }
}
