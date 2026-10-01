import {WebSocketServer,WebSocket} from 'ws';
import {randomUUID} from 'node:crypto';
import {publicThread,sourceHash} from '../system/backend/walkieware.mjs';

export function attachWalkiewareSocket(server,{authenticate,store,authMs=5000,lifetimeMs=600000}={}) {
  const wss=new WebSocketServer({noServer:true,maxPayload:8_100_000,perMessageDeflate:false});
  const rooms=new Map();
  const send=(ws,value)=>{if(ws?.readyState===WebSocket.OPEN){if(ws.bufferedAmount>8_100_000)ws.close(1008,'Slow reader');else ws.send(JSON.stringify(value));}};
  const upgrade=(req,socket,head)=>{
    if(req.url!=='/api/walkieware-stream')return;
    if(wss.clients.size>=64){socket.destroy();return;}
    wss.handleUpgrade(req,socket,head,ws=>wss.emit('connection',ws));
  };
  server.on('upgrade',upgrade);
  wss.on('connection',ws=>{
    let owner,row,room,role,closed=false,chain=Promise.resolve(),pending=0,alive=true,messages=0,windowAt=Date.now();
    const authTimer=setTimeout(()=>ws.close(1008,'Authenticate first'),authMs);
    const lifeTimer=setTimeout(()=>ws.close(1000,'Renew session'),lifetimeMs);
    const heartbeat=setInterval(()=>{if(!alive)return ws.terminate();alive=false;ws.ping();},20000);
    for(const timer of [authTimer,lifeTimer,heartbeat])timer.unref?.();
    ws.on('pong',()=>alive=true);
    const broadcast=value=>{for(const client of room?.clients||[])send(client,value);};
    async function message(m) {
      if(closed)return;
      if(!owner) {
        if(m.type!=='authenticate'||typeof m.token!=='string'||m.token.length>7000||!['device','agent'].includes(m.role))throw Error('Authenticate first');
        const user=await authenticate({authorization:`Bearer ${m.token}`});
        if(!user||closed)throw Error('Authentication failed');
        const db=await store();role=m.role;
        row=role==='device'?await db.open(user,m.id):await db.read(user,m.code);
        if(!row||closed)throw Error('Thread unavailable');
        owner=user;clearTimeout(authTimer);
        room=rooms.get(row._id);
        if(!room){room={clients:new Set(),device:null,state:null,commands:new Map()};rooms.set(row._id,room);}
        if(role==='device'&&room.device)throw Error('This thread is running on another device');
        room.clients.add(ws);if(role==='device')room.device=ws;
        send(ws,{type:'ready',thread:publicThread(row),online:!!room.device,state:room.state});
        if(role==='device')broadcast({type:'presence',online:true});
        return;
      }
      if(m.type==='sync'&&role==='device') {
        const saved=await (await store()).save(owner,row._id,m.revision,m.ledger);
        if(!saved){send(ws,{type:'conflict',thread:publicThread(await (await store()).read(owner,row.code))});return;}
        row=saved;broadcast({type:'saved',thread:publicThread(row)});return;
      }
      if(m.type==='ping'){send(ws,{type:'pong',online:!!room.device,role,peers:room.clients.size,attached:rooms.get(row._id)===room});return;}
      if(m.type==='state'&&role==='device') {
        const state=m.state;
        if(!state||JSON.stringify(state).length>525000)throw Error('Invalid state');
        room.state={busy:!!state.busy,phase:String(state.phase||'').slice(0,200),head:state.head,sourceHash:typeof state.source==='string'?sourceHash(state.source):null,source:typeof state.source==='string'?state.source.slice(0,500000):'',errors:Array.isArray(state.errors)?state.errors.slice(-20).map(e=>String(e).slice(0,1000)):[],attempt:state.attempt&&typeof state.attempt==='object'?{request:String(state.attempt.request||'').slice(0,20000),status:String(state.attempt.status||'').slice(0,40),error:String(state.attempt.error||'').slice(0,1000),parent:state.attempt.parent,startedAt:state.attempt.startedAt,finishedAt:state.attempt.finishedAt}:null,updatedAt:new Date().toISOString()};
        broadcast({type:'state',state:room.state});
        await (await store()).state?.(owner,row._id,room.state);return;
      }
      if(m.type==='command'&&role==='agent') {
        const id=typeof m.id==='string'&&/^[a-zA-Z0-9-]{1,80}$/.test(m.id)?m.id:randomUUID();
        if(!room.device){send(ws,{type:'result',id,ok:false,error:'Device offline; saved versions remain available'});return;}
        if(room.commands.size||room.state?.busy){send(ws,{type:'result',id,ok:false,error:'Device busy'});return;}
        if(!['ask','undo','edit'].includes(m.action)||!Number.isSafeInteger(m.baseVersion)||typeof m.baseHash!=='string'||(m.action==='ask'&&(typeof m.text!=='string'||!m.text.trim()||m.text.length>20000))||(m.action==='edit'&&(typeof m.source!=='string'||Buffer.byteLength(m.source)>500000||!m.source.trim())))throw Error('Invalid command');
        const timer=setTimeout(()=>{room.commands.delete(id);send(ws,{type:'result',id,ok:false,error:'Timed out; inspect history before retrying'});},180000);timer.unref?.();
        room.commands.set(id,{ws,timer});
        send(room.device,{type:'command',id,action:m.action,text:m.text,source:m.source,baseVersion:m.baseVersion,baseHash:m.baseHash});
        send(ws,{type:'accepted',id});return;
      }
      if(m.type==='result'&&role==='device') {
        const command=room.commands.get(m.id);if(!command)return;
        clearTimeout(command.timer);room.commands.delete(m.id);
        send(command.ws,{type:'result',id:m.id,ok:m.ok===true,head:m.head,error:String(m.error||'').slice(0,1000)});return;
      }
      throw Error('Unsupported message');
    }
    ws.on('message',(data,binary)=>{
      if(Date.now()-windowAt>=1000){messages=0;windowAt=Date.now();}
      if(++messages>30){ws.close(1008,'Message rate exceeded');return;}
      if(binary||++pending>16){ws.close(1008,'Message limit');return;}
      chain=chain.then(async()=>{try{await message(JSON.parse(data.toString()));}catch(error){send(ws,{type:'error',error:error.message});if(!room?.clients.has(ws))ws.close(1008,'Session unavailable');}finally{pending--;}});
    });
    ws.on('error',()=>{});
    ws.on('close',()=>{
      closed=true;clearTimeout(authTimer);clearTimeout(lifeTimer);clearInterval(heartbeat);
      room?.clients.delete(ws);
      if(room?.device===ws){room.device=null;room.state=null;broadcast({type:'presence',online:false});for(const [id,c] of room.commands){clearTimeout(c.timer);send(c.ws,{type:'result',id,ok:false,error:'Device disconnected; inspect history before retrying'});}room.commands.clear();}
      if(room&&!room.clients.size)rooms.delete(row._id);
    });
  });
  return {wss,close(){server.off('upgrade',upgrade);for(const ws of wss.clients)ws.terminate();wss.close();}};
}
