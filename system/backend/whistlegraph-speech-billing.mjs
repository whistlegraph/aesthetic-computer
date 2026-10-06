import {createHash,randomUUID} from 'node:crypto';
import {DAILY_TOKEN_BUDGET,dayKey} from './ai-budget.mjs';
import {BRAINCELLS_PER_USD,INFERENCE_MARKUP,reserve,settle} from './easel-paid-credits.mjs';
// https://developers.openai.com/api/docs/models/whisper-1 — $0.006/minute.
export const WHISPER_USD_PER_MINUTE=0.006;
export const speechCost=ms=>Math.ceil(Math.round(ms/60000*WHISPER_USD_PER_MINUTE*BRAINCELLS_PER_USD*INFERENCE_MARKUP*1e6)/1e6);
const fail=(status,message)=>Object.assign(Error(message),{status});
const uuid=/^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$/i;
export const speechRequestKey=(user,id)=>createHash('sha256').update(user+'\0'+id.toLowerCase()).digest('hex');
const walletIn=(db,session)=>({
  updateOne:(filter,update,options={})=>db.collection('ac-credit-wallets').updateOne(filter,update,{...options,session}),
});
export function speechBilling(db,{now=()=>new Date(),limit=DAILY_TOKEN_BUDGET}={}) {
  const receipts=db.collection('whistlegraph-speech-requests'),usage=db.collection('ai-usage');
  async function transaction(fn) {
    const session=db.client.startSession();
    try{return await session.withTransaction(()=>fn(session),{readConcern:{level:'snapshot'},writeConcern:{w:'majority'}});}
    finally{await session.endSession();}
  }
  async function finish(id,success) {
    return transaction(async session=>{
      const r=await receipts.findOne({_id:id},{session});
      if(!r||r.status!=='pending')return false;
      if(r.hold)await settle(r.hold,success?r.paid:0,walletIn(db,session),{now:now()});
      if(!success&&r.free)await usage.updateOne({_id:r.usageId},{$inc:{tokens:-r.free}},{session});
      if(success)await usage.updateOne({_id:r.usageId},{$inc:{asks:1},$set:{last:now(),lastModel:'openai/whisper-1'}},{session});
      await receipts.updateOne({_id:id,status:'pending'},{$set:{status:success?'complete':'failed',charged:success?r.braincells:0,finishedAt:now()}},{session});
      return true;
    });
  }
  async function reconcile(user) {
    const query={status:'pending',expiresAt:{$lte:now()},...(user?{user}:{})};
    let count=0;for await(const r of receipts.find(query,{projection:{_id:1}}))if(await finish(r._id,false))count++;
    return count;
  }
  async function begin({user,handle,requestId,audio,durationMs}) {
    if(!uuid.test(requestId||''))throw fail(400,'Recording ID must be a UUID');
    const id=speechRequestKey(user,requestId),hash=createHash('sha256').update(audio).digest('hex');
    const braincells=speechCost(durationMs);
    if(!Number.isSafeInteger(braincells)||braincells<1||durationMs>46000)throw fail(400,'Invalid recording duration');
    await reconcile(user);
    const at=now(),day=dayKey(at),usageId=handle+':'+day,holdId=randomUUID();
    return transaction(async session=>{
      const previous=await receipts.findOne({_id:id},{session});
      if(previous) {
        if(previous.hash!==hash)throw fail(409,'Recording ID was already used for different audio');
        throw fail(409,previous.status==='pending'?'Transcription is still processing':'Recording already processed; use the saved or device transcript');
      }
      await usage.updateOne({_id:usageId},{$setOnInsert:{handle,day,first:at,tokens:0}},{upsert:true,session});
      const spent=await usage.findOne({_id:usageId},{session});
      const free=Math.min(braincells,Math.max(0,limit-(Number(spent.tokens)||0))),paid=braincells-free;
      const hold=paid?await reserve(user,paid,walletIn(db,session),{id:holdId,now:at}):null;
      if(paid&&!hold)throw fail(402,'Not enough braincells for OpenAI speech; using device speech');
      if(free)await usage.updateOne({_id:usageId},{$inc:{tokens:free}},{session});
      await receipts.insertOne({_id:id,user,hash,usageId,day,free,paid,hold,braincells,status:'pending',startedAt:at,expiresAt:new Date(+at+120000)},{session});
      return {id,braincells,free,paid,durationMs};
    });
  }
  return {begin,finish,reconcile};
}
