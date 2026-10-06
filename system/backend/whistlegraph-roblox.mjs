import {timingSafeEqual} from 'node:crypto';
import {validateRoom} from '../../shared/roblox-room.mjs';

export function validRobloxLaunch(value){
  try{const u=new URL(value);return u.protocol==='https:'&&u.hostname==='www.roblox.com'&&u.pathname==='/share'&&!u.username&&!u.password&&!u.port&&!!u.searchParams.get('code');}catch{return false;}
}
function sameSecret(a,b){return typeof a==='string'&&typeof b==='string'&&b.length>=32&&Buffer.byteLength(a)===Buffer.byteLength(b)&&timingSafeEqual(Buffer.from(a),Buffer.from(b));}
export function mongoRoomStore(collection){return {
  read:owner=>collection.findOne({_id:owner}),
  async save(owner,expectedRevision,room,requestId){
    const previous=await collection.findOne({_id:owner});
    if(previous?.requestId===requestId)return previous;
    const revision=previous?.revision||0;
    if(revision!==expectedRevision)return null;
    const next={_id:owner,revision:revision+1,room,requestId,updatedAt:new Date().toISOString()};
    if(!previous){try{await collection.insertOne(next);return next;}catch(e){if(e.code===11000)return null;throw e;}}
    const result=await collection.replaceOne({_id:owner,revision},next);
    return result.modifiedCount?next:null;
  },
  async applied(owner,revision){return collection.updateOne({_id:owner,revision},{$set:{appliedRevision:revision,appliedAt:new Date().toISOString()}});},
};}

// A private, operator-configured pilot. It is NOT public OAuth account linking.
// Only the configured AC subject can write; only the configured Roblox user can
// receive the room from an authenticated game server. No ID comes from the app.
export function createRoomHandler({authenticate,store,config}){
  return async event=>{
    const headers={'Content-Type':'application/json','Cache-Control':'no-store','Access-Control-Allow-Origin':'*','Access-Control-Allow-Headers':'Authorization, Content-Type','Access-Control-Allow-Methods':'GET, POST, OPTIONS'};
    const reply=(statusCode,value)=>({statusCode,headers,body:JSON.stringify(value)});
    if(event.httpMethod==='OPTIONS')return reply(204,{});
    if(!['GET','POST'].includes(event.httpMethod))return reply(405,{error:'Use GET or POST'});
    if(typeof event.body==='string'&&Buffer.byteLength(event.body)>65536)return reply(413,{error:'Room is too large'});
    const settings=config();
    const available=!!settings.owner&&/^[1-9]\d{0,19}$/.test(settings.playerId||'')&&validRobloxLaunch(settings.launchURL)&&typeof settings.bridgeKey==='string'&&settings.bridgeKey.length>=32;
    const incoming=Object.fromEntries(Object.entries(event.headers||{}).map(([k,v])=>[k.toLowerCase(),v]));
    try {
      if(incoming['x-whistlegraph-bridge-key']!==undefined){
        if(!available||!sameSecret(incoming['x-whistlegraph-bridge-key'],settings.bridgeKey))return reply(401,{error:'Game server authorization required'});
        if(event.httpMethod==='GET'){
          if(event.queryStringParameters?.playerId!==settings.playerId)return reply(404,{error:'No room for this player'});
          const row=await (await store()).read(settings.owner);
          return row?reply(200,{revision:row.revision,room:row.room}):reply(404,{error:'No room saved yet'});
        }
        let body;try{body=JSON.parse(event.body||'');}catch{return reply(400,{error:'Invalid JSON'});}
        if(body.action!=='applied'||body.playerId!==settings.playerId||!Number.isSafeInteger(body.revision)||body.revision<1)return reply(400,{error:'Invalid acknowledgment'});
        await (await store()).applied(settings.owner,body.revision);return reply(200,{ok:true});
      }
      const owner=await authenticate(incoming);
      if(!owner)return reply(401,{error:'Sign in to Whistlegraph'});
      if(!available)return reply(200,{available:false});
      if(owner!==settings.owner)return reply(403,{error:'This Roblox connection is a private pilot.'});
      if(event.httpMethod==='GET'){
        const row=await (await store()).read(owner);
        return reply(200,{available:true,revision:row?.revision||0,appliedRevision:row?.appliedRevision||null});
      }
      let body,room;
      try{
        body=JSON.parse(event.body||'');
        if(body.action!=='save'||Object.keys(body).some(k=>!['action','room','requestId','expectedRevision'].includes(k))||!Number.isSafeInteger(body.expectedRevision)||body.expectedRevision<0||typeof body.requestId!=='string'||!/^[a-f0-9-]{36}$/i.test(body.requestId))throw Error();
        room=validateRoom(body.room);
      }catch{return reply(400,{error:'Invalid room save'});}
      const row=await (await store()).save(owner,body.expectedRevision,room,body.requestId);
      return row?reply(200,{revision:row.revision,launchURL:settings.launchURL}):reply(409,{error:'The saved Roblox room changed on another session. Export your local room before reconnecting.'});
    }catch{return reply(503,{error:'Room service unavailable. Your local room is still saved.'});}
  };
}
