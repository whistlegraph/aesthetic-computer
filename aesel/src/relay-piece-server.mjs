import {AcServer} from './ac-server.mjs';
import {runPersonalTurn} from './personal-relay.mjs';
export class RelayPieceServer extends AcServer {
 constructor(options){super(options);this.relayStorage=options.relayStorage;this.relayKey=options.relayKey;this.onRelayRequest=options.onRelayRequest||(()=>{});this.onRelayHeaders=options.onRelayHeaders||(()=>{});}
 // Revision labels detect stale edits; they are not authentication hashes.
 // Keep them synchronous and stable across WebKit relaunches and relay retries.
 revisionForSource(source){
  let a=0x811c9dc5,b=0x9e3779b9;
  for(let i=0;i<source.length;i++){
   const c=source.charCodeAt(i);
   a=Math.imul(a^c,0x01000193);b=Math.imul(b^c,0x85ebca6b);
  }
  return source.length.toString(16)+'-'+(a>>>0).toString(16).padStart(8,'0')+(b>>>0).toString(16).padStart(8,'0');
 }
 async startTurn(content){
  this.controller=new AbortController();
  const {instructions,tools}=this.relayContext();
  this.onRelayRequest();
  try{
   return await runPersonalTurn({token:this.token,model:this.model,instructions,content,tools,storage:this.relayStorage,key:this.relayKey,
    signal:this.controller.signal,fetch:this.fetch,onHeaders:this.onRelayHeaders,
    onTool:block=>this.runRelayTool(block),onEvent:event=>this.emit('notification',event)});
  }catch(error){
   const turn={id:this.turnId,status:error.name==='AbortError'?'interrupted':'failed',error:{message:error.message}};
   this.emit('notification',{method:'turn/completed',params:{turn}});return {turn};
  }
 }
}
