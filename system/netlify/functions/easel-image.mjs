import { authorize, getHandleOrEmail } from '../../backend/authorization.mjs';
import { connect } from '../../backend/database.mjs';
import { respond } from '../../backend/http.mjs';
import { imageRequest, generateEaselImage } from '../../backend/easel-images.mjs';
const headers={'Cache-Control':'no-store'};
export async function handler(event) {
  if(event.httpMethod!=='POST')return respond(405,{error:'POST required.'},headers);
  const user=await authorize(event.headers);
  if(!user?.sub)return respond(401,{error:'Sign in to use hosted image tools.'},headers);
  const handle=await getHandleOrEmail(user.sub);
  if(!handle?.startsWith('@'))return respond(403,{error:'Claim an AC handle first.'},headers);
  let input;
  try{input=imageRequest(JSON.parse(event.body));}catch(error){return respond(400,{error:error.message},headers);}
  const key=process.env[input.provider === 'fal' ? 'FAL_KEY' : 'OPENAI_API_KEY'];
  if(!key)return respond(503,{error:`Hosted ${input.provider} image tools are unavailable.`},headers);
  const {db}=await connect();
  const jobs=db.collection('easel-image-jobs');
  await jobs.createIndex({expiresAt:1},{expireAfterSeconds:0});
  const id=`${user.sub}:${input.jobId}`;
  const existing=await jobs.findOne({_id:id});
  if(existing)return existing.result?respond(200,existing.result,headers):respond(409,{error:'This image job was already submitted. It will not be charged twice.',status:existing.status},headers);
  // Reserve a bounded daily allowance before the paid request. The same job id
  // always refers to one request, including when the client loses its response.
  const budget=db.collection('easel-image-budget');
  await budget.createIndex({expiresAt:1},{expireAfterSeconds:0});
  const day=new Date().toISOString().slice(0,10), budgetId=`${user.sub}:${day}`;
  await budget.updateOne({_id:budgetId},{$setOnInsert:{count:0,expiresAt:new Date(Date.now()+2*86400000)}},{upsert:true});
  try{await jobs.insertOne({_id:id,status:'reserved',expiresAt:new Date(Date.now()+7*86400000)});}catch(error){if(error.code===11000)return respond(409,{error:'This image job is already running.'},headers);throw error;}
  const allowed=await budget.updateOne({_id:budgetId,count:{$lt:5}},{$inc:{count:1}});
  if(!allowed.modifiedCount){await jobs.updateOne({_id:id},{$set:{status:'limited'}});return respond(429,{error:'Today’s five hosted images have been used. Try tomorrow or use your own provider key.'},headers);}
  try {
    await jobs.updateOne({_id:id},{$set:{status:'submitted'}});
    const result=await generateEaselImage(input,{key});
    await jobs.updateOne({_id:id},{$set:{status:'complete',result}});
    return respond(200,result,headers);
  }catch(error){
    await jobs.updateOne({_id:id},{$set:{status:'failed-or-unknown'}});
    console.warn('Easel image failed:',error.message);
    return respond(502,{error:error.message},headers);
  }
}
