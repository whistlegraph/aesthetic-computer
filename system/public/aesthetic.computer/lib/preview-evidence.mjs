// Local native-preview observations. Never uploaded or enabled for ordinary runs.
export function createPreviewEvidence(send) {
  let current=null, generation=0, painted=false, active=false;
  const emit=(kind,extra={})=>current && send({type:'aesel-preview',content:{...current,kind,...extra}});
  return {
    async begin(source,identity) {
      emit('invalidated');
      const turn=++generation;current=null;painted=false;active=false;
      if(typeof source!=='string'||!identity||typeof identity.sessionID!=='string'||
        identity.sessionID.length>200||!Number.isSafeInteger(identity.revision)||
        !Number.isSafeInteger(identity.requestID))return null;
      const digest=await crypto.subtle.digest('SHA-256',new TextEncoder().encode(source));
      if(turn!==generation)return null;
      current={sessionID:identity.sessionID,revision:identity.revision,requestID:identity.requestID,
        sourceHash:Array.from(new Uint8Array(digest),n=>n.toString(16).padStart(2,'0')).join('')};
      emit('loading');return current;
    },
    activate(identity) { if(current && current===identity){active=true;emit('loaded');} },
    paint() { if(current&&active)painted=true; },
    frame() { return active&&painted?current:null; },
    record(level,message) { emit('console',{event:{level,message:String(message).slice(0,2000),at:Date.now()}}); },
  };
}
