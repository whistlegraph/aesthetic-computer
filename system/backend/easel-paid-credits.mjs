// Purchased credits are separate from the resetting free allowance. Both are
// counted in the same braincell, and a hosted request draws on the free
// allowance first; only once today's is spent does it reserve from the wallet.
import { randomUUID } from 'node:crypto';
import { connect } from './database.mjs';
import { dayKey } from './ai-budget.mjs';
import { imageInputBound } from './easel-input-images.mjs';
export const CREDIT_PACK = Object.freeze({ id:'braincells-1m-v1', amount:500, currency:'usd', credits:1_000_000, unit:'braincells' });
// What a braincell is worth: the pack's price, so $5 buys 1,000,000.
export const BRAINCELLS_PER_USD = CREDIT_PACK.credits / (CREDIT_PACK.amount / 100);
// Hosted inference sells at twice what OpenRouter charges AC for it.
export const INFERENCE_MARKUP = 2;
// The most one handle may spend from its wallet in a UTC day ($10 at pack
// price). A runaway loop stops here rather than draining a balance overnight.
export const DAILY_PAID_BRAINCELL_CAP = 2_000_000;
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
export function reservationSize(body,maxTokens) {
  // UTF-8 bytes upper-bound text tokens, including JSON tool definitions.
  // Static PNG pixels have a separate conservative bound. Other media remains
  // unsupported; it cannot hide unbounded token cost.
  const input=JSON.stringify({system:body.system,messages:body.messages,tools:body.tools});
  const images=imageInputBound(body);
  return Math.ceil(Buffer.byteLength(input,'utf8')*1.25)+images+maxTokens+4096;
}
// Pending work counts until settled, including work crossing midnight. This
// conservative bound avoids spending the same daily capacity concurrently.
export const INFERENCE_DEADLINE_MS = 5 * 60_000;
export const HOLD_LIFETIME_MS = 10 * 60_000;
const outstanding = wallet => Object.values(wallet?.holds || {}).reduce((n,h)=>n+h.amount,0);
export async function reserve(user,amount,wallets,{id=randomUUID(),now=new Date(),cap=DAILY_PAID_BRAINCELL_CAP}={}) {
  if(!Number.isSafeInteger(amount)||amount<1)throw new Error('Invalid credit reservation');
  const day=dayKey(now);
  const result=await wallets.updateOne({_id:user,balance:{$gte:amount},[`holds.${id}`]:{$exists:false},
    $expr:{$lte:[{$add:[{$ifNull:[`$daily.${day}`,0]},
      {$sum:{$map:{input:{$objectToArray:{$ifNull:['$holds',{}]}},as:'hold',in:'$$hold.v.amount'}}},amount]},cap]}},
    {$inc:{balance:-amount},$set:{[`holds.${id}`]:{amount,at:now,day,expiresAt:new Date(+now+HOLD_LIFETIME_MS)}}});
  return result.modifiedCount===1?{user,id,amount,day}:null;
}
// Charge a finished request `braincells` against its hold and refund the rest.
// Keyed by the hold's id: the update only matches while that hold exists and it
// removes the hold, so a retried or duplicated settle changes nothing.
export async function settle(hold,braincells,wallets,{now=new Date()}={}) {
  if(!hold)return false;
  if(!Number.isFinite(braincells)||braincells<0)throw new Error('Invalid metered usage');
  const charged=Math.min(hold.amount,Math.ceil(braincells));
  const result=await wallets.updateOne({_id:hold.user,[`holds.${hold.id}.amount`]:hold.amount},{$inc:{balance:hold.amount-charged,spent:charged,[`daily.${hold.day||dayKey(now)}`]:charged},$unset:{[`holds.${hold.id}`]:''},$set:{updatedAt:now}});
  return result.modifiedCount===1;
}
// Persist the intended settlement before applying it. A crash between these
// writes is recovered by Lith's runner; removal of the hold fences late charges.
export async function settleDurably(hold,braincells,wallets,{now=new Date()}={}) {
  if(!hold)return false;
  if(!Number.isFinite(braincells)||braincells<0)throw new Error('Invalid metered usage');
  const charged=Math.min(hold.amount,Math.ceil(braincells));
  await wallets.updateOne({_id:hold.user,[`holds.${hold.id}.amount`]:hold.amount,
    [`holds.${hold.id}.settlement`]:{$exists:false}},
    {$set:{[`holds.${hold.id}.settlement`]:charged}});
  const wallet=await wallets.findOne({_id:hold.user});
  const pending=wallet?.holds?.[hold.id];
  if(!pending)return false;
  return settle({...hold,day:pending.day||dayKey(new Date(pending.at))},pending.settlement,wallets,{now});
}

export async function reconcileWallet(wallet,wallets,{now=new Date()}={}) {
  let recovered=0;
  for(const [id,pending] of Object.entries(wallet?.holds || {})) {
    const hold={user:wallet._id,id,amount:pending.amount,day:pending.day||dayKey(new Date(pending.at))};
    if(Number.isFinite(pending.settlement)) {
      if(await settle(hold,pending.settlement,wallets,{now}))recovered++;
    } else if(+new Date(pending.expiresAt || +new Date(pending.at)+HOLD_LIFETIME_MS)<=+now) {
      // The server deadline has passed. Unknown usage is AC's expense. Use
      // the same first-writer settlement claim as normal completion.
      if(await settleDurably(hold,0,wallets,{now}))recovered++;
    }
  }
  return recovered;
}

export async function reconcilePaidHolds(wallets,{now=new Date()}={}) {
  let recovered=0;
  for await(const wallet of wallets.find({holds:{$exists:true,$ne:{}}})) {
    recovered+=await reconcileWallet(wallet,wallets,{now});
  }
  return recovered;
}

// OpenRouter's reported cost, as braincells: twice the price, at pack value.
// Rounded to a millionth first so float noise (0.001 × 400,000) cannot tip a
// whole braincell.
export function braincellsFromCost(usd) {
  return Math.ceil(Math.round(usd*INFERENCE_MARKUP*BRAINCELLS_PER_USD*1e6)/1e6);
}
// What a finished request costs in braincells. The provider's own cost when it
// reported one; otherwise the model's fixed rate over the weighted tokens.
export function usageBraincells({model,tokens=0,cost,inputBound=0}) {
  if(Number.isFinite(cost)&&cost>=0)return braincellsFromCost(cost);
  return Math.ceil(tokens*braincellRate(model,inputBound));
}
// OpenRouter's prices on 2026-09-28, USD per million tokens as input / output.
// Only the hold and the no-cost fallback read these; the bill is the cost the
// provider reports.
const OPEN_PRICES={
 'deepseek/deepseek-v4.1-flash':[0.30,1.20], 'deepseek/deepseek-v4-pro':[0.78,1.57],
 'moonshotai/kimi-k3':[3.00,15.0], 'qwen/qwen3.7-plus':[0.32,1.28],
 'minimax/minimax-m3':[0.30,1.20], 'z-ai/glm-5.3-flash':[0.15,0.50],
 // OpenRouter 2026-10-08. Whistlegraph's default (Sonnet) and premium (Opus).
 'anthropic/claude-sonnet-5.5':[2.00,10.0], 'anthropic/claude-opus-5.5':[4.00,20.0],
};
// Braincells per token, worst case: every token priced as output, marked up,
// rounded up to a hundredth. Flash comes to 0.48, Kimi K3 to 6.
const worstRate=([input,output])=>Math.ceil(Math.max(input,output)*INFERENCE_MARKUP*BRAINCELLS_PER_USD/1e4)/100;
// Fixed braincell tariff. The first seven were checked against OpenRouter's
// catalog 2026-09-17, relative to Luna; the open models derive from the prices
// above. One balance works across models; these are consumption rates.
export const BRAINCELL_RATES=Object.freeze({
 'openai/gpt-5.6-luna':1, 'z-ai/glm-4.6':3, 'qwen/qwen3-coder':2,
 'deepseek/deepseek-chat-v3.1':2, 'anthropic/claude-sonnet-4.6':15,
 'anthropic/claude-opus-5':25, 'openai/gpt-5.4':13,
 ...Object.fromEntries(Object.entries(OPEN_PRICES).map(([model,price])=>[model,worstRate(price)])),
});
export function braincellRate(model,inputBound=0){
 const base=BRAINCELL_RATES[model];if(!base)throw Error('This model has no braincell rate yet');
 if(inputBound>=272000 && model==='openai/gpt-5.6-luna')return 2;
 if(inputBound>=272000 && model==='openai/gpt-5.4')return 25;
 return base;
}
export const OUT_OF_BRAINCELLS='Out of braincells — buy more from the braincell meter in Aesel, or switch provider with /provider. Free braincells reset at midnight UTC.';
// Hold the worst case this request could cost: its input and its whole output
// allowance at the model's rate, so a larger max_tokens holds more.
export async function authorizePaidRequest({user,model,body,maxTokens,now=new Date(),withWallets:using=withWallets}) {
  const inputBound=reservationSize(body,0);
  const rate=braincellRate(model,inputBound);
  const amount=Math.ceil(reservationSize(body,maxTokens)*rate);
  return using(async wallets=>{
    const prior=await wallets.findOne({_id:user});
    await reconcileWallet(prior,wallets,{now});
    const hold=await reserve(user,amount,wallets,{now});
    if(hold)return {...hold,rate,inputBound};
    const wallet=await wallets.findOne({_id:user},{projection:{daily:1,holds:1}});
    if((Number(wallet?.daily?.[dayKey(now)])||0)+outstanding(wallet)+amount>DAILY_PAID_BRAINCELL_CAP){
      const minutes=Math.ceil((Date.parse(dayKey(now)+'T00:00:00Z')+86400000-now)/60000);
      throw Object.assign(new Error(`This request and pending work would exceed today's limit of ${DAILY_PAID_BRAINCELL_CAP.toLocaleString('en-US')} bought braincells. It resets at midnight UTC, in ${Math.floor(minutes/60)}h ${minutes%60}m.`),{statusCode:429});
    }
    throw Object.assign(new Error(OUT_OF_BRAINCELLS),{statusCode:402});
  });
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
