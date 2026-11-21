// Company-private Easel transcripts. Authentication is always server verified;
// a configured staff subject allowlist is required for raw reads.
import { createHash } from 'node:crypto';
import { parseTranscript, serializeTranscript, MAX_BATCH_BYTES } from '../../aesel/src/transcript-format.mjs';
const COLLECTION='easel-transcripts-private';
const DAY=86400000;
const ID=/^[a-zA-Z0-9_-]{1,80}$/;
const reply=(statusCode,value)=>({statusCode,headers:{'Content-Type':'application/json; charset=utf-8','Cache-Control':'no-store, private'},body:JSON.stringify(value)});
export async function ensureTranscriptIndexes(db) {
  const collection=db.collection(COLLECTION);
  // Expiration is a soft marker. Remove the legacy TTL before accepting records.
  try{await collection.dropIndex('easel_transcript_expiry');}
  catch(error){if(![26,27].includes(error?.code))throw error;} // Missing collection/index is safe.
  await collection.createIndex({expiresAt:1},{name:'easel_transcript_expiration_marker'});
  await collection.createIndex({owner:1,sessionId:1,'record.seq':1},{name:'easel_transcript_owner_session',unique:true,partialFilterExpression:{'record.seq':{$gt:0}}});
}
export function createTranscriptHandler({authorize,connect,staffSubs=()=>process.env.AESEL_TRANSCRIPT_STAFF_SUBS||process.env.EASEL_TRANSCRIPT_STAFF_SUBS||'',now=()=>new Date()}={}) {
  const initialized=new WeakSet();
  return async event=>{
    if(!['POST','GET','DELETE'].includes(event.httpMethod))return reply(405,{error:'POST, GET or DELETE required'});
    let user;
    try{user=await authorize({authorization:event.headers?.authorization??event.headers?.Authorization});}catch{return reply(503,{error:'Could not verify authentication'});}
    if(typeof user?.sub!=='string'||!user.sub)return reply(401,{error:'Authentication required'});
    const args=event.queryStringParameters || {};
    const mine=event.httpMethod==='GET' && args.scope==='mine';
    if(mine && Object.keys(args).some(key=>!['scope','sessionId','afterSeq','before','beforeId'].includes(key)))return reply(400,{error:'Unexpected account history filter'});
    const listing=mine && args.sessionId===undefined;
    if(listing && (args.afterSeq!==undefined || ((args.before!==undefined || args.beforeId!==undefined) && (!/^\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}\.\d{3}Z$/.test(args.before||'') || !ID.test(args.beforeId||'')))))return reply(400,{error:'Invalid thread cursor'});
    if(event.httpMethod==='GET'&&!mine&&!String(staffSubs()).split(',').map(s=>s.trim()).filter(Boolean).includes(user.sub))return reply(403,{error:'Company staff authorization required'});
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
        const owner=mine?user.sub:args.owner;
        if(!listing && (typeof owner!=='string'||!owner||owner.length>200||!ID.test(args.sessionId||'')))return reply(400,{error:'Explicit owner and sessionId required'});
        const after=Number(args.afterSeq??0);if(!Number.isSafeInteger(after)||after<0)return reply(400,{error:'Invalid sequence cursor'});
        query={owner,sessionId:args.sessionId,'record.seq':{$gt:after}};
      }else{
        let args;try{if(Buffer.byteLength(event.body||'')>1024)throw new Error();args=JSON.parse(event.body||'{}');if(!args||typeof args!=='object'||Array.isArray(args)||Object.keys(args).some(k=>!['sessionId','all'].includes(k)))throw new Error();}catch{return reply(400,{error:'Invalid delete request'});}
        if(!ID.test(args.sessionId||'')&&args.all!==true)return reply(400,{error:'Provide sessionId or explicit all:true'});
        query={owner:user.sub,...(ID.test(args.sessionId||'')?{sessionId:args.sessionId}:{})};
      }
      database=await connect();const db=database.db;
      if(!initialized.has(db)){await ensureTranscriptIndexes(db);initialized.add(db);}
      const collection=db.collection(COLLECTION);
      if(listing){
        // Renewed sharing consent may produce several headers for one session.
        const pipeline=[
          {$match:{owner:user.sub,'record.type':'session'}},{$sort:{receivedAt:-1}},
          {$group:{_id:'$sessionId',title:{$first:'$record.metadata.title'},medium:{$first:'$record.metadata.medium'},createdAt:{$first:'$record.createdAt'}}},
        ];
        if(args.before)pipeline.push({$match:{$or:[{createdAt:{$lt:args.before}},{createdAt:args.before,_id:{$lt:args.beforeId}}]}});
        pipeline.push({$sort:{createdAt:-1,_id:-1}},{$limit:51},
          {$lookup:{from:COLLECTION,let:{thread:'$_id'},pipeline:[
            {$match:{owner:user.sub,'record.role':'user','record.seq':{$gt:0},$expr:{$eq:['$sessionId','$$thread']}}},
            {$sort:{'record.seq':1}},{$limit:1},{$project:{_id:0,text:{$substrCP:['$record.text',0,90]}}},
          ],as:'opening'}});
        const rows=await collection.aggregate(pipeline).toArray();
        const threads=rows.slice(0,50).map(row=>({id:row._id,title:row.title||row.opening[0]?.text,medium:row.medium,createdAt:row.createdAt}));
        const last=threads.at(-1);
        return reply(200,{threads,nextBefore:rows.length>50?{before:last.createdAt,beforeId:last.id}:null});
      }
      if(event.httpMethod==='DELETE'){const result=await collection.deleteMany(query);return reply(200,{deleted:result.deletedCount});}
      if(event.httpMethod==='GET'){
        const records=await collection.find(query,{projection:{_id:0,record:1,expiresAt:1}}).sort({'record.seq':1}).limit(100).toArray();
        if(mine)return reply(200,{records:records.map(row=>row.record),nextAfterSeq:records.length===100?records.at(-1).record.seq:null});
        const readAt=now();
        const expiredSeqs=records.filter(row=>row.expiresAt&&row.expiresAt<=readAt).map(row=>row.record.seq);
        return reply(200,{records:records.map(row=>row.record),expiredSeqs,nextAfterSeq:records.at(-1)?.record.seq??null});
      }
      const receivedAt=now(),expiresAt=new Date(receivedAt.getTime()+30*DAY);
      const records=[{...document.header,seq:0},...document.records];
      const rows=records.map(record=>{
        const recordId=record.type==='session'?`session:${record.consent.id}`:record.id;
        const _id=createHash('sha256').update(JSON.stringify([user.sub,document.header.id,recordId])).digest('hex');
        const hash=createHash('sha256').update(JSON.stringify(record)).digest('hex');
        return{_id,hash,owner:user.sub,sessionId:document.header.id,recordId,record,receivedAt,expiresAt};
      });
      // Retry cannot mutate an accepted event or move its expiration marker.
      await collection.bulkWrite(rows.map(row=>({updateOne:{filter:{_id:row._id,hash:row.hash},update:{$setOnInsert:row},upsert:true}})),{ordered:false});
      return reply(200,{accepted:document.records.length,sessionId:document.header.id,retentionDays:null,expirationDays:30});
    }catch(error){return reply(error?.code===11000?409:503,{error:error?.code===11000?'Transcript ID already contains different content':'Private transcript storage unavailable'});}
    finally{if(database?.disconnect)await database.disconnect().catch(()=>{});}
  };
}
