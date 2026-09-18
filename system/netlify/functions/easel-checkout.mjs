import Stripe from 'stripe';
import { authorize, getHandleOrEmail } from '../../backend/authorization.mjs';
import { CREDIT_PACK, withWallets, fulfillCheckout, refundCheckout } from '../../backend/easel-paid-credits.mjs';
const origin='https://aesthetic.computer';
const headers={'Content-Type':'application/json','Cache-Control':'no-store','Access-Control-Allow-Origin':'*','Access-Control-Allow-Headers':'Authorization, Content-Type','Access-Control-Allow-Methods':'GET, POST, OPTIONS'};
const reply=(statusCode,value)=>({statusCode,headers,body:JSON.stringify(value)});
export function createHandler({stripe,verifyUser=authorize,handleFor=getHandleOrEmail,fulfill=session=>withWallets(w=>fulfillCheckout(session,w,{live:process.env.CONTEXT!=='dev'})),refund=(session,amount)=>withWallets(w=>refundCheckout(session,amount,w,{live:process.env.CONTEXT!=='dev'})),secret,enabled=true}={}) {
 return async event=>{
  if(event.httpMethod==='OPTIONS')return reply(204,null);
  const signature=event.headers?.['stripe-signature'];
  if(signature){
   if(event.httpMethod!=='POST'||!secret)return reply(400,{error:'Webhook unavailable'});
   let hook;try{hook=stripe.webhooks.constructEvent(event.isBase64Encoded?Buffer.from(event.body,'base64'):event.body,signature,secret);}catch{return reply(400,{error:'Invalid Stripe signature'});}
   try{
    if(['checkout.session.completed','checkout.session.async_payment_succeeded'].includes(hook.type))await fulfill(hook.data.object);
    if(hook.type==='charge.refunded'){
      const sessions=await stripe.checkout.sessions.list({payment_intent:hook.data.object.payment_intent,limit:10});
      for(const session of sessions.data)if(session.metadata?.type==='ac-credits')await refund(session,hook.data.object.amount_refunded);
    }
    return reply(200,{received:true});
   }catch(error){console.error('Credit fulfillment failed:',error.message);return reply(500,{error:'Credit fulfillment pending'});}
  }
  // The landing page never grants credit. Signed webhooks and an authenticated
  // server-to-server session lookup perform the same idempotent fulfillment.
  if(event.httpMethod==='GET')return {statusCode:200,headers:{'Content-Type':'text/html; charset=utf-8','Cache-Control':'no-store','Content-Security-Policy':"default-src 'none'; style-src 'unsafe-inline'; base-uri 'none'; frame-ancestors 'none'"},body:'<!doctype html><html lang="en"><meta charset="utf-8"><meta name="viewport" content="width=device-width"><title>AC braincells</title><style>body{background:#18121c;color:#fff;font:24px system-ui;max-width:32rem;margin:15vh auto;padding:24px}a{color:#ffaadd}</style><h1>Return to Aesel</h1><p>Your balance updates after Stripe confirms payment. If you canceled, nothing was charged.</p><a href="https://aesthetic.computer">Aesthetic Computer</a></html>'};
  if(event.httpMethod!=='POST')return reply(405,{error:'POST only'});
  let user;try{user=await verifyUser(event.headers||{});}catch{return reply(401,{error:'Sign in to buy credits'});}
  if(!user?.sub)return reply(401,{error:'Sign in to buy credits'});
  let body;try{body=JSON.parse(event.body||'{}');}catch{return reply(400,{error:'Invalid request'});}
  try{
   if(body.sessionId){
    if(typeof body.sessionId!=='string'||!/^cs_(test_|live_)?[A-Za-z0-9]+$/.test(body.sessionId))return reply(400,{error:'Invalid checkout'});
    const session=await stripe.checkout.sessions.retrieve(body.sessionId);
    if(session.metadata?.userSub!==user.sub||session.metadata?.type!=='ac-credits')return reply(404,{error:'Checkout not found'});
    await fulfill(session);return reply(200,{status:session.status,paid:session.payment_status==='paid'});
   }
   if(!enabled)return reply(503,{error:'Credit purchases are not available yet'});
   const handle=await handleFor(user.sub);
   if(typeof handle!=='string'||!handle.startsWith('@'))return reply(403,{error:'Claim an AC handle before buying credits'});
   if(![CREDIT_PACK.id,'luna-1m-v1'].includes(body.pack))return reply(400,{error:'Unknown credit pack'});
   if(typeof body.requestId!=='string'||! /^[a-f0-9-]{36}$/.test(body.requestId))return reply(400,{error:'Missing checkout request ID'});
   const session=await stripe.checkout.sessions.create({mode:'payment',payment_method_types:['card'],client_reference_id:user.sub,
    success_url:origin+'/api/easel-checkout?result=success',cancel_url:origin+'/api/easel-checkout?result=canceled',
    line_items:[{price_data:{currency:CREDIT_PACK.currency,unit_amount:CREDIT_PACK.amount,product_data:{images:[origin+'/aesthetic.computer/braincell.png'],name:'AC braincells',description:'1,000,000 braincells for AC hosted text and code generation. One balance across supported models; consumption varies by model and context. Free allowance first. Purchased braincells never expire.'}},quantity:1}],
    metadata:{type:'ac-credits',pack:CREDIT_PACK.id,userSub:user.sub,handle}},
    {idempotencyKey:`ac-credits:${user.sub}:${body.requestId}`});
   return reply(200,{url:session.url,sessionId:session.id});
  }catch(error){console.error('Credit checkout failed:',error.message);return reply(503,{error:'Could not complete checkout. Try again shortly.'});}
 };
}
export async function handler(event){
 const dev=process.env.CONTEXT==='dev';const key=dev?process.env.STRIPE_API_TEST_PRIV_KEY:process.env.STRIPE_API_PRIV_KEY;
 if(!key)return reply(503,{error:'Payments unavailable'});
 return createHandler({stripe:new Stripe(key),secret:process.env.STRIPE_AC_CREDITS_WEBHOOK_SECRET,enabled:process.env.AC_CREDITS_CHECKOUT_ENABLED==='true'})(event);
}
