// Company-private Easel transcripts. Authentication is always server verified;
// a configured staff subject allowlist is required for raw reads.
import { createHash } from 'node:crypto';
import { parseTranscript, serializeTranscript, MAX_BATCH_BYTES } from '../../easel/src/transcript-format.mjs';
const COLLECTION='easel-transcripts-private';
const DAY=86400000;
const ID=/^[a-zA-Z0-9_-]{1,80}$/;
const reply=(statusCode,value)=>({statusCode,headers:{'Content-Type':'application/json; charset=utf-8','Cache-Control':'no-store, private'},body:JSON.stringify(value)});
export async function ensureTranscriptIndexes(db) {
  const collection=db.collection(COLLECTION);
  await collection.createIndex({expiresAt:1},{expireAfterSeconds:0,name:'easel_transcript_expiry'});
  await collection.createIndex({owner:1,sessionId:1,'record.seq':1},{name:'easel_transcript_owner_session',unique:true,partialFilterExpression:{'record.seq':{$gt:0}}});
}
export function createTranscriptHandler({authorize,connect,staffSubs=()=>process.env.EASEL_TRANSCRIPT_STAFF_SUBS||'',now=()=>new Date()}={}) {
  const initialized=new WeakSet();
  return async event=>{
    if(!['POST','GET','DELETE'].includes(event.httpMethod))return reply(405,{error:'POST, GET or DELETE required'});
    let user;
    try{user=await authorize({authorization:event.headers?.authorization??event.headers?.Authorization});}catch{return reply(503,{error:'Could not verify authentication'});}
    if(typeof user?.sub!=='string'||!user.sub)return reply(401,{error:'Authentication required'});
    if(event.httpMethod==='GET'&&!String(staffSubs()).split(',').map(s=>s.trim()).filter(Boolean).includes(user.sub))return reply(403,{error:'Company staff authorization required'});
    let database;
    try{
      // Validate the complete request before connecting or writing anything.
      let document,query;
      if(event.httpMethod==='POST'){
        if(event.isBase64Encoded||typeof event.body!=='string'||Buffer.byteLength(event.body)>MAX_BATCH_BYTES)return reply(413,{error:'Transcript batch exceeds 256 KiB'});
        try{document=parseTranscript(event.body,{maxBytes:MAX_BATCH_BYTES});if(document.header.consent.sharing!=='company')throw new Error('Explicit sharing consent required');if(document.records.length<1||document.records.length>100)throw new Error('Batch requires 1–100 records');}
        catch{return reply(400,{error:'Invalid transcript batch or sharing consent'});}
        // Apply credential redaction again to direct clients; never trust a client flag.
        document=parseTranscript(serializeTranscript(document.header,document.records,{redact:true,maxBytes:MAX_BATCH_BYTES}));
      }else if(event.httpMethod==='GET'){
        const args=event.queryStringParameters||{};
        if(typeof args.owner!=='string'||!args.owner||args.owner.length>200||!ID.test(args.sessionId||''))return reply(400,{error:'Explicit owner and sessionId required'});
        const after=Number(args.afterSeq??0);if(!Number.isSafeInteger(after)||after<0)return reply(400,{error:'Invalid sequence cursor'});
        query={owner:args.owner,sessionId:args.sessionId,expiresAt:{$gt:now()},'record.seq':{$gt:after}};
      }else{
        let args;try{if(Buffer.byteLength(event.body||'')>1024)throw new Error();args=JSON.parse(event.body||'{}');if(!args||typeof args!=='object'||Array.isArray(args)||Object.keys(args).some(k=>!['sessionId','all'].includes(k)))throw new Error();}catch{return reply(400,{error:'Invalid delete request'});}
        if(!ID.test(args.sessionId||'')&&args.all!==true)return reply(400,{error:'Provide sessionId or explicit all:true'});
        query={owner:user.sub,...(ID.test(args.sessionId||'')?{sessionId:args.sessionId}:{})};
      }
      database=await connect();const db=database.db;
      if(!initialized.has(db)){await ensureTranscriptIndexes(db);initialized.add(db);}
      const collection=db.collection(COLLECTION);
      if(event.httpMethod==='DELETE'){const result=await collection.deleteMany(query);return reply(200,{deleted:result.deletedCount});}
      if(event.httpMethod==='GET'){
        const records=await collection.find(query,{projection:{_id:0,record:1}}).sort({'record.seq':1}).limit(100).toArray();
        return reply(200,{records:records.map(row=>row.record),nextAfterSeq:records.at(-1)?.record.seq??null});
      }
      const receivedAt=now(),expiresAt=new Date(receivedAt.getTime()+30*DAY);
      const records=[{...document.header,seq:0},...document.records];
      const rows=records.map(record=>{
        const recordId=record.type==='session'?`session:${record.consent.id}`:record.id;
        const _id=createHash('sha256').update(JSON.stringify([user.sub,document.header.id,recordId])).digest('hex');
        const hash=createHash('sha256').update(JSON.stringify(record)).digest('hex');
        return{_id,hash,owner:user.sub,sessionId:document.header.id,recordId,record,receivedAt,expiresAt};
      });
      // Retry cannot mutate an accepted event or renew its retention deadline.
      await collection.bulkWrite(rows.map(row=>({updateOne:{filter:{_id:row._id,hash:row.hash},update:{$setOnInsert:row},upsert:true}})),{ordered:false});
      return reply(200,{accepted:document.records.length,sessionId:document.header.id,retentionDays:30});
    }catch(error){return reply(error?.code===11000?409:503,{error:error?.code===11000?'Transcript ID already contains different content':'Private transcript storage unavailable'});}
    finally{if(database?.disconnect)await database.disconnect().catch(()=>{});}
  };
}
