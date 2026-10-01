// One foreground connection; existing HTTP path remains the cold/offline fallback.
export class MusicalInputSocket {
 constructor({token,onEvent=()=>{},WebSocketImpl=globalThis.WebSocket,url='wss://aesthetic.computer/api/easel-musical-stream',fetchImpl=(...args)=>globalThis.fetch(...args)}={}) {
  Object.assign(this,{token,onEvent,WebSocketImpl,url,fetchImpl});this.pending=new Map();this.ready=false;this.enabled=true;
 }
 connect(){
  const bearer=this.token?.();
  if(!this.enabled||!bearer||!this.WebSocketImpl)return;
  if(this.ws&&this.bearer===bearer)return;
  this.close();this.bearer=bearer;
  let ws;try{ws=new this.WebSocketImpl(this.url);}catch{return;}
  this.ws=ws;const started=performance.now();
  const timer=setTimeout(()=>{if(this.ws===ws&&!this.ready)ws.close();},4000);
  ws.onopen=()=>ws.send(JSON.stringify({type:'authenticate',token:bearer}));
  ws.onmessage=event=>{
   if(this.ws!==ws)return;
   let m;try{m=JSON.parse(event.data);}catch{return;}
   if(m.type==='ready'){clearTimeout(timer);this.ready=true;this.onEvent('inputSocketReady',{connectMs:Math.round(performance.now()-started)});return;}
   const key=`${m.sessionId}:${m.sequence}`,p=this.pending.get(key);if(!p)return;
   if(m.type==='received'){p.transportMs=Math.round(performance.now()-p.started);this.onEvent('inputSocketAck',{transportMs:p.transportMs});return;}
   if(m.type==='decision'){this.pending.delete(key);p.cleanup();p.resolve({ok:true,json:async()=>({...m,transport:'socket',transportMs:p.transportMs})});}
   else if(m.type==='error'){this.pending.delete(key);p.cleanup();p.resolve({ok:false,status:m.status});}
  };
  ws.onerror=()=>{};
  ws.onclose=()=>{clearTimeout(timer);if(this.ws!==ws)return;this.ws=null;this.ready=false;this.rejectPending();if(this.enabled)this.reconnect=setTimeout(()=>this.connect(),2000);};
 }
 rejectPending(){for(const p of this.pending.values()){p.cleanup();p.reject(Error('socket_closed'));}this.pending.clear();}
 close(){clearTimeout(this.reconnect);const ws=this.ws;this.ws=null;this.ready=false;this.rejectPending();ws?.close();}
 suspend(){this.enabled=false;this.close();}
 resume(){this.enabled=true;this.connect();}
 fetch=async(endpoint,options)=>{
  this.connect();
  if(!this.ready){this.onEvent('inputHttpFallback',{reason:'socket_not_ready'});return this.fetchImpl(endpoint,options);}
  const ws=this.ws,body=JSON.parse(options.body),key=`${body.sessionId}:${body.sequence}`;
  // Once sent, never replay a possibly billed decision over HTTP.
  return new Promise((resolve,reject)=>{
   const cancel=()=>{
    this.pending.delete(key);options.signal?.removeEventListener('abort',cancel);
    if(ws.readyState===1)ws.send(JSON.stringify({type:'cancel',sessionId:body.sessionId}));
    reject(Error('cancelled'));
   };
   if(options.signal?.aborted){reject(Error('cancelled'));return;}
   const cleanup=()=>options.signal?.removeEventListener('abort',cancel);
   this.pending.set(key,{resolve,reject,cleanup,started:performance.now()});options.signal?.addEventListener('abort',cancel,{once:true});
   try{ws.send(JSON.stringify({type:'observation',body}));}catch{this.pending.delete(key);cleanup();reject(Error('socket_send_failed'));}
  });
 };
}
