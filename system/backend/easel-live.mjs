import {createHash} from 'node:crypto';
export const MAX_FRAME_BYTES=8*1024*1024;
export const LIVE_TTL=3600;
const ID=/^[a-f0-9]{32}$/;
const TYPES={picture:['image/png','image/jpeg','image/webp'],sound:['audio/wav','audio/mpeg','audio/ogg'],paper:['application/pdf','text/plain'],gameboy:['application/x-gameboy-rom','text/plain'],piece:['text/javascript','text/plain']};
const FIELDS=['kind','version','mime','sequence','status','updatedAt','expiresAt'];
const headers={'Access-Control-Allow-Origin':'*','Access-Control-Allow-Methods':'GET, POST, DELETE, OPTIONS','Access-Control-Allow-Headers':'Content-Type, Authorization','Cache-Control':'no-store','X-Content-Type-Options':'nosniff'};
const reply=(statusCode,value)=>({statusCode,headers:{...headers,'Content-Type':'application/json'},body:JSON.stringify(value)});

// One Redis transaction owns identity, ordering, rate limits, and frame lifetime.
export const WRITE_SCRIPT=`
local owner=redis.call('HGET',KEYS[1],'owner')
if owner and owner~=ARGV[1] then return -1 end
local seq=tonumber(redis.call('HGET',KEYS[1],'sequence') or '0')
if tonumber(ARGV[2])<=seq then return -2 end
if ARGV[3]=='stopped' and not owner then return -5 end
local requests=tonumber(redis.call('GET',KEYS[3]) or '0')
if requests>=120 then return -4 end
if ARGV[3]=='live' then
  redis.call('ZREMRANGEBYSCORE',KEYS[2],'-inf',ARGV[8])
  if not redis.call('ZSCORE',KEYS[2],KEYS[1]) and redis.call('ZCARD',KEYS[2])>=64 then return -3 end
  local bytes=tonumber(redis.call('GET',KEYS[4]) or '0')
  if requests>=120 or bytes+tonumber(ARGV[11])>67108864 then return -4 end
  redis.call('INCRBY',KEYS[4],ARGV[11]);redis.call('EXPIRE',KEYS[4],60)
  redis.call('ZADD',KEYS[2],ARGV[9],KEYS[1]);redis.call('EXPIRE',KEYS[2],ARGV[10])
else redis.call('ZREM',KEYS[2],KEYS[1]) end
redis.call('INCR',KEYS[3]);redis.call('EXPIRE',KEYS[3],60)
redis.call('HSET',KEYS[1],'owner',ARGV[1],'sequence',ARGV[2],'status',ARGV[3],'updatedAt',ARGV[8],'expiresAt',ARGV[9])
if ARGV[3]=='live' then
 redis.call('HSET',KEYS[1],'kind',ARGV[4],'version',ARGV[5],'mime',ARGV[6],'data',ARGV[7])
else redis.call('HDEL',KEYS[1],'data') end
redis.call('EXPIRE',KEYS[1],ARGV[10])
return 1`;
export function redisLiveStore(redis,{prefix='easel-live:'}={}) {
 return {
  async read(id,frame=false){const fields=frame?[...FIELDS,'data']:FIELDS;const values=await redis.hmGet(prefix+id,fields);if(values[0]===null)return null;return Object.fromEntries(fields.map((key,i)=>[key,values[i]]));},
  async write(id,owner,document,now){const ownerKey=createHash('sha256').update(owner).digest('hex');const minute=Math.floor(now/60000);return Number(await redis.eval(WRITE_SCRIPT,{keys:[prefix+id,prefix+'owner:'+ownerKey,prefix+'rate:'+ownerKey+':'+minute,prefix+'bytes:'+ownerKey+':'+minute],arguments:[owner,String(document.sequence),document.status,document.kind||'',String(document.version||''),document.mime||'',document.data||'',String(now),String(now+LIVE_TTL*1000),String(LIVE_TTL),String(document.bytes||0)]}));}
 };
}
function metadata(id,row){return {id,route:`https://aesthetic.computer/watch/?id=${id}`,kind:row.kind,version:Number(row.version),mime:row.mime,sequence:Number(row.sequence),status:row.status,updatedAt:new Date(Number(row.updatedAt)).toISOString(),expiresAt:new Date(Number(row.expiresAt)).toISOString()};}
export function createLiveHandler({authorize,getHandleOrEmail,store,now=Date.now}={}) {
 return async event=>{
  const method=event.httpMethod;
  if(method==='OPTIONS')return {statusCode:204,headers,body:''};
  if(!['GET','POST','DELETE'].includes(method))return reply(405,{error:'Use GET, POST or DELETE'});
  try {
   if(method==='GET'){
    const {id,frame}=event.queryStringParameters||{};
    if(!ID.test(id||'')||(frame!==undefined&&!/^[1-9][0-9]{0,14}$/.test(frame)))return reply(400,{error:'Invalid draft ID or frame sequence'});
    const row=await store.read(id,frame!==undefined);if(!row)return reply(404,{error:'Draft expired or not found'});
    if(frame===undefined)return reply(200,metadata(id,row));
    if(row.status!=='live'||!row.data)return reply(410,{error:'Broadcast stopped'});
    if(Number(frame)!==Number(row.sequence))return reply(409,{error:'Frame superseded; refresh metadata'});
    return {statusCode:200,headers:{...headers,'Content-Type':row.mime,'Content-Security-Policy':"sandbox; default-src 'none'",'Content-Disposition':'inline'},body:row.data,isBase64Encoded:true};
   }
   if(!event.headers?.authorization && !event.headers?.Authorization)return reply(401,{error:'Authentication required'});
   const maxBody=method==='DELETE'?1024:Math.ceil(MAX_FRAME_BYTES/3)*4+2048;
   if(event.isBase64Encoded||typeof event.body!=='string'||Buffer.byteLength(event.body)>maxBody)return reply(413,{error:'Draft exceeds 8 MiB'});
   let doc;try{doc=JSON.parse(event.body);}catch{return reply(400,{error:'Invalid JSON'});}
   if(!doc||Array.isArray(doc)||!ID.test(doc.id||'')||!Number.isSafeInteger(doc.sequence)||doc.sequence<1)return reply(400,{error:'ID and positive sequence required'});
   const allowed=method==='DELETE'?['id','sequence']:['id','sequence','kind','version','mime','data','status'];
   if(Object.keys(doc).some(k=>!allowed.includes(k)))return reply(400,{error:'Unknown draft fields'});
   if(method==='POST') {
    if(!TYPES[doc.kind]?.includes(doc.mime)||!Number.isSafeInteger(doc.version)||doc.version<1||doc.status!=='live'||typeof doc.data!=='string'||!doc.data.length||doc.data.length%4!==0||!/^[A-Za-z0-9+/]*={0,2}$/.test(doc.data))return reply(400,{error:'Invalid media, version, status or base64'});
    const bytes=Buffer.from(doc.data,'base64');if(bytes.length>MAX_FRAME_BYTES)return reply(413,{error:'Draft exceeds 8 MiB'});
    if(bytes.toString('base64')!==doc.data)return reply(400,{error:'Invalid base64'});doc.bytes=bytes.length;
   }else doc.status='stopped';
   let user;try{user=await authorize(event.headers||{});}catch{return reply(503,{error:'Authentication unavailable'});}
   if(!user?.sub)return reply(401,{error:'Authentication required'});
   const handle=await getHandleOrEmail(user.sub);if(typeof handle!=='string'||!handle.startsWith('@'))return reply(403,{error:'An AC handle is required to broadcast'});
   const result=await store.write(doc.id,user.sub,doc,now());
   if(result===-2) {const row=await store.read(doc.id);return reply(409,{error:'Stale sequence',sequence:Number(row?.sequence||0)});}
   if(result!==1)return reply(result===-1?403:result===-2?409:result===-5?404:429,{error:result===-1?'Draft belongs to another account':result===-2?'Stale sequence':result===-5?'Draft not found':'Draft limit reached; coalesce updates and retry'});
   const row=await store.read(doc.id);return reply(200,metadata(doc.id,row));
  }catch{return reply(503,{error:'Draft storage unavailable'});}
 };
}
