// Purchased credits are separate from the resetting free allowance.
import { randomUUID } from 'node:crypto';
import { connect } from './database.mjs';
export const CREDIT_PACK = Object.freeze({ id:'braincells-1m-v1', amount:500, currency:'usd', credits:1_000_000, unit:'braincells' });
// The same pack sold through Apple in-app purchase; Apple keeps the price.
export const IAP_PRODUCTS = Object.freeze({ 'computer.aesthetic.easel.braincells.1m': Object.freeze({ credits:1_000_000, unit:'braincells' }) });
const COLLECTION='ac-credit-wallets';
export async function withWallets(fn) {
  const connection=await connect();
  try { return await fn(connection.db.collection(COLLECTION)); }
  finally { await connection.disconnect(); }
}
export async function balance(user, wallets) {
  const value=await wallets.findOne({_id:user},{projection:{balance:1}});
  return Math.max(0,Number(value?.balance)||0);
}
export async function ensureWallet(user,wallets) {
  try { await wallets.updateOne({_id:user},{$setOnInsert:{balance:0,grants:[],createdAt:new Date()}},{upsert:true}); }
  catch(error) { if(error.code!==11000)throw error; }
}
export function paidCheckout(session, {live}={}) {
  const m=session?.metadata;
  if(m?.type!=='ac-credits')return null;
  if(session.mode!=='payment'||session.payment_status!=='paid')return null;
  if(![CREDIT_PACK.id,'luna-1m-v1'].includes(m.pack)||!m.userSub||session.client_reference_id!==m.userSub||session.amount_total!==CREDIT_PACK.amount||session.currency!==CREDIT_PACK.currency||typeof session.id!=='string'||!session.id.startsWith('cs_')||(live!==undefined&&session.livemode!==live))throw new Error('Credit checkout does not match the server offer');
  return {user:m.userSub,id:session.id,credits:CREDIT_PACK.credits};
}
// A single-document conditional increment makes webhook retries and a
// simultaneous checkout-return reconciliation safe across server processes.
export async function fulfillGrant(grant,wallets) {
  if(!grant?.user||typeof grant.id!=='string'||!Number.isSafeInteger(grant.credits)||grant.credits<1)return false;
  await ensureWallet(grant.user,wallets);
  const result=await wallets.updateOne({_id:grant.user,grants:{$ne:grant.id}},{$inc:{balance:grant.credits},$addToSet:{grants:grant.id},$set:{updatedAt:new Date()}});
  return result.modifiedCount===1;
}
export async function fulfillCheckout(session,wallets,options) {
  const grant=paidCheckout(session,options); if(!grant)return false;
  return fulfillGrant(grant,wallets);
}
// Take back a granted pack (an Apple refund). Cumulative per grant, so a
// redelivered notification converges; spent credit becomes debt.
export async function revokeGrant(grant,wallets) {
  if(!grant?.user||typeof grant.id!=='string'||!Number.isSafeInteger(grant.credits)||grant.credits<1)return false;
  await fulfillGrant(grant,wallets);
  const path=`refunds.${grant.id}`;
  const result=await wallets.updateOne({_id:grant.user},[{$set:{balance:{$subtract:['$balance',{$max:[0,{$subtract:[grant.credits,{$ifNull:[`$${path}`,0]}]}]}]},[path]:{$max:[grant.credits,{$ifNull:[`$${path}`,0]}]},updatedAt:'$$NOW'}}]);
  return result.modifiedCount===1;
}
export function reservationSize(body,maxTokens,{validateMedia=true}={}) {
  // UTF-8 bytes upper-bound text tokens, including JSON tool definitions.
  // Unsupported media is rejected below; it cannot hide unbounded token cost.
  const input=JSON.stringify({system:body.system,messages:body.messages,tools:body.tools});
  if(validateMedia && /"type"\s*:\s*"(?:image|document|input_audio|video)"/.test(input))throw new Error('Braincells currently support hosted text and code requests.');
  return Math.ceil(Buffer.byteLength(input,'utf8')*1.25)+maxTokens+4096;
}
export async function reserve(user,amount,wallets,{id=randomUUID()}={}) {
  if(!Number.isSafeInteger(amount)||amount<1)throw new Error('Invalid credit reservation');
  const result=await wallets.updateOne({_id:user,balance:{$gte:amount},[`holds.${id}`]:{$exists:false}},{$inc:{balance:-amount},$set:{[`holds.${id}`]:{amount,at:new Date()}}});
  return result.modifiedCount===1?{user,id,amount}:null;
}
export async function settle(hold,spent,wallets) {
  if(!hold)return;
  if(!Number.isFinite(spent)||spent<0)throw new Error('Invalid metered usage');
  const charged=Math.min(hold.amount,Math.ceil(spent*(hold.rate||1)));
  await wallets.updateOne({_id:hold.user,[`holds.${hold.id}.amount`]:hold.amount},{$inc:{balance:hold.amount-charged,spent:charged},$unset:{[`holds.${hold.id}`]:''},$set:{updatedAt:new Date()}});
}
// Fixed braincell tariff, checked against OpenRouter's model catalog 2026-09-17.
// One balance works across models; these are consumption rates, not separate packs.
export const BRAINCELL_RATES=Object.freeze({
 'openai/gpt-5.6-luna':1, 'z-ai/glm-4.6':3, 'qwen/qwen3-coder':2,
 'deepseek/deepseek-chat-v3.1':2, 'anthropic/claude-sonnet-4.6':15,
 'anthropic/claude-opus-5':25, 'openai/gpt-5.4':13,
});
export function braincellRate(model,inputBound=0){
 const base=BRAINCELL_RATES[model];if(!base)throw Error('This model has no braincell rate yet');
 if(inputBound>=272000 && model==='openai/gpt-5.6-luna')return 2;
 if(inputBound>=272000 && model==='openai/gpt-5.4')return 25;
 return base;
}
export async function authorizePaidRequest({user,model,body,maxTokens}) {
  const inputBound=reservationSize(body,0);
  const rate=braincellRate(model,inputBound);
  const amount=reservationSize(body,maxTokens)*rate;
  const hold=await withWallets(wallets=>reserve(user,amount,wallets));
  if(!hold)throw Object.assign(new Error('Not enough braincells for this request. Open the braincell selector in Aesel to buy more, or start a shorter thread. Free braincells reset at midnight UTC.'),{statusCode:402});
  return {...hold,rate};
}

// Cumulative refund amounts make partial refunds and out-of-order redelivery
// converge. A spent refund becomes debt, preventing refunded credit reuse.
export async function refundCheckout(session,refundedCents,wallets,options){
 const grant=paidCheckout(session,options);if(!grant)return false;
 if(!Number.isSafeInteger(refundedCents)||refundedCents<0||refundedCents>CREDIT_PACK.amount)throw Error('Invalid refund amount');
 await fulfillCheckout(session,wallets,options);
 const amount=Math.floor(grant.credits*refundedCents/CREDIT_PACK.amount);
 const path=`refunds.${grant.id}`;
 const result=await wallets.updateOne({_id:grant.user},[{$set:{balance:{$subtract:['$balance',{$max:[0,{$subtract:[amount,{$ifNull:[`$${path}`,0]}]}]}]},[path]:{$max:[amount,{$ifNull:[`$${path}`,0]}]},updatedAt:'$$NOW'}}]);
 return result.modifiedCount===1;
}
