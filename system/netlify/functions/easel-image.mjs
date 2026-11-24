import { authorize, getHandleOrEmail } from '../../backend/authorization.mjs';
import { connect } from '../../backend/database.mjs';
import { respond } from '../../backend/http.mjs';
import { imageRequest, generateEaselImage } from '../../backend/easel-images.mjs';
import { imageHoldBraincells, imageBraincells } from '../../backend/easel-image-credits.mjs';
import { inferenceBudgetFailure } from '../../backend/easel-policy.mjs';
const headers={'Cache-Control':'no-store'};
// A coarse client label for the Braincell ledger, as easel-inference keeps it.
const spendClient=(h={})=>String(h['x-ac-client']||String(h['user-agent']||'').split(/[\s/]/)[0]||'').toLowerCase().replace(/[^a-z0-9._-]/g,'').slice(0,40);

// One hosted image, paid for like hosted text: today's free braincells first,
// then the signed-in account's bought braincells (held before the paid call,
// settled at what the image used, refunded if it fails). A printer runs every
// job on its owner's account, so its owner pays for what is printed to it.
export async function handler(event) {
  if(event.httpMethod!=='POST')return respond(405,{error:'POST required.'},headers);
  const user=await authorize(event.headers);
  if(!user?.sub)return respond(401,{error:'Sign in to use hosted image tools.'},headers);
  const handleOrEmail=await getHandleOrEmail(user.sub);
  if(!handleOrEmail?.startsWith('@'))return respond(403,{error:'Claim an AC handle first.'},headers);
  const handle=handleOrEmail.slice(1);
  let input;
  try{input=imageRequest(JSON.parse(event.body));}catch(error){return respond(400,{error:error.message},headers);}
  const key=process.env[input.provider === 'fal' ? 'FAL_KEY' : 'OPENAI_API_KEY'];
  if(!key)return respond(503,{error:`Hosted ${input.provider} image tools are unavailable.`},headers);
  const {db}=await connect();
  const jobs=db.collection('easel-image-jobs');
  await jobs.createIndex({expiresAt:1},{expireAfterSeconds:0});
  // The same job id always refers to one request, including when the client
  // loses its response: it is never generated or charged twice.
  const id=`${user.sub}:${input.jobId}`;
  const existing=await jobs.findOne({_id:id});
  if(existing)return existing.result?respond(200,existing.result,headers):respond(409,{error:'This image job was already submitted. It will not be charged twice.',status:existing.status},headers);
  try{await jobs.insertOne({_id:id,status:'reserved',expiresAt:new Date(Date.now()+7*86400000)});}catch(error){if(error.code===11000)return respond(409,{error:'This image job is already running.'},headers);throw error;}
  // Refused before the provider is called: nothing was spent, so the job id is
  // released and the same job can be sent again.
  const refuse=async(status,error)=>{await jobs.deleteOne({_id:id,status:'reserved'});return respond(status,{error},headers);};

  let budget=null;
  try{const {checkBudget}=await import('../../backend/ai-budget.mjs');budget=await checkBudget(handle);}
  catch(error){console.log('🪙 easel image: budget unavailable —',error.message);}
  const budgetFailure=inferenceBudgetFailure(budget,handle);
  let paidHold=null;
  if(budgetFailure){
    if(budgetFailure.statusCode!==429)return refuse(budgetFailure.statusCode,budgetFailure.message);
    try{
      const {authorizePaidAmount}=await import('../../backend/easel-paid-credits.mjs');
      paidHold=await authorizePaidAmount({user:user.sub,amount:imageHoldBraincells(input),model:input.model,product:'aesel'});
    }catch(error){return refuse(error.statusCode||503,error.message);}
  }
  const settlePaid=async braincells=>{
    if(!paidHold)return;
    const {withWallets,settleDurably}=await import('../../backend/easel-paid-credits.mjs');
    await withWallets(w=>settleDurably(paidHold,braincells,w));
  };

  let result;
  try {
    await jobs.updateOne({_id:id},{$set:{status:'submitted'}});
    result=await generateEaselImage(input,{key});
  }catch(error){
    await jobs.updateOne({_id:id},{$set:{status:'failed-or-unknown'}});
    await settlePaid(0).catch(e=>console.error('[easel-image] refund pending recovery',paidHold?.id,e.message));
    console.warn('Easel image failed:',error.message);
    return respond(502,{error:error.message},headers);
  }
  const braincells=imageBraincells(input,result.provenance?.usage);
  try{
    if(paidHold)await settlePaid(braincells);
    else{const {recordUsage}=await import('../../backend/ai-budget.mjs');await recordUsage(handle,braincells,{model:input.model});}
    // Reporting only, after billing: which model, for whom, paid or free.
    const {recordSpend}=await import('../../backend/braincell-ledger.mjs');
    await recordSpend({product:'aesel',client:spendClient(event.headers),handle,user:user.sub,model:input.model,braincells,paid:Boolean(paidHold)});
  }catch(error){console.error('[easel-image] settlement pending recovery',paidHold?.id||'free-allowance',error.message);}
  result={...result,braincells};
  await jobs.updateOne({_id:id},{$set:{status:'complete',result}});
  console.log(`🖼️ easel image @${handle} — ${input.model} ${input.quality} · ${braincells} braincells${paidHold?' (bought)':''}`);
  return respond(200,result,headers);
}
