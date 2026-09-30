import {WebSocketServer, WebSocket} from 'ws';

// Same decision service and durable allowance as HTTP. No audio or tokens in URLs.
export function attachMusicalSocket(server, {authenticate, decide, authMs=5000, lifetimeMs=600000}={}) {
 const wss=new WebSocketServer({noServer:true,maxPayload:8192,perMessageDeflate:false});
 const accounts=new Map();
 const upgrade=(req,socket,head)=>{
  if(req.url!=='/api/easel-musical-stream'||wss.clients.size>=64){socket.end('HTTP/1.1 503 Service Unavailable\r\nConnection: close\r\n\r\n');return;}
  wss.handleUpgrade(req,socket,head,ws=>wss.emit('connection',ws));
 };
 server.on('upgrade',upgrade);
 wss.on('connection',ws=>{
  let subject,authenticating=false,active=null,queued=null,closed=false,alive=true;
  let messages=0,windowAt=Date.now();
  const send=value=>{if(ws.readyState===WebSocket.OPEN){if(ws.bufferedAmount>16384)ws.close(1008,'Slow reader');else ws.send(JSON.stringify(value));}};
  const authTimer=setTimeout(()=>ws.close(1008,'Authenticate first'),authMs);
  const lifeTimer=setTimeout(()=>ws.close(1000,'Renew session'),lifetimeMs);
  const pingTimer=setInterval(()=>{if(!alive){ws.terminate();return;}alive=false;ws.ping();},20000);
  for(const t of [authTimer,lifeTimer,pingTimer])t.unref?.();
  ws.on('pong',()=>{alive=true;});
  const fail=(body,status)=>send({type:'error',sessionId:body?.sessionId,sequence:body?.sequence,status});
  async function run(body){
   const task={body,controller:new AbortController()};active=task;
   const started=performance.now();
   try{
    const r=await decide({httpMethod:'POST',headers:{authorization:'socket-session'},body:JSON.stringify(body)},{subject,signal:task.controller.signal});
    if(!closed&&!task.controller.signal.aborted){
     if(r.statusCode===200)send({...JSON.parse(r.body),type:'decision',serverMs:Math.round(performance.now()-started)});
     else fail(body,r.statusCode);
    }
   }catch{if(!closed&&!task.controller.signal.aborted)fail(body,503);}
   finally{active=null;if(queued&&!closed){const next=queued;queued=null;void run(next);}}
  }
  ws.on('message',async(data,binary)=>{
   if(binary){ws.close(1003,'JSON only');return;}
   if(Date.now()-windowAt>=1000){messages=0;windowAt=Date.now();}
   if(++messages>30){ws.close(1008,'Input rate exceeded');return;}
   let m;try{m=JSON.parse(data.toString());}catch{ws.close(1008,'Invalid message');return;}
   if(!m||typeof m!=='object'){ws.close(1008,'Invalid message');return;}
   if(!subject){
    if(authenticating||m.type!=='authenticate'||typeof m.token!=='string'||m.token.length>7000){ws.close(1008,'Authenticate first');return;}
    authenticating=true;
    try{
     const user=await authenticate({authorization:`Bearer ${m.token}`});
     if(closed)return;
     if(!user||(accounts.get(user)||0)>=2){ws.close(1008,'Account unavailable');return;}
     subject=user;accounts.set(subject,(accounts.get(subject)||0)+1);clearTimeout(authTimer);send({type:'ready'});
    }catch{ws.close(1011,'Account unavailable');}
    return;
   }
   if(m.type==='ping'){send({type:'pong'});return;}
   if(m.type==='cancel'){
    if(active?.body.sessionId===m.sessionId)active.controller.abort();
    if(queued?.sessionId===m.sessionId)queued=null;
    return;
   }
   if(m.type!=='observation'||!m.body||JSON.stringify(m.body).length>2048){ws.close(1008,'Invalid observation');return;}
   const body=m.body;
   if(!body||typeof body.sessionId!=='string'||!Number.isInteger(body.sequence)){ws.close(1008,'Invalid observation');return;}
   send({type:'received',sessionId:body.sessionId,sequence:body.sequence});
   if(active){if(queued)fail(queued,409);queued=body;}else void run(body);
  });
  ws.on('error',()=>{});
  ws.on('close',()=>{closed=true;clearTimeout(authTimer);clearTimeout(lifeTimer);clearInterval(pingTimer);active?.controller.abort();queued=null;if(subject){const n=(accounts.get(subject)||1)-1;if(n)accounts.set(subject,n);else accounts.delete(subject);}});
 });
 return {close(){server.off('upgrade',upgrade);for(const ws of wss.clients)ws.terminate();wss.close();},wss};
}
