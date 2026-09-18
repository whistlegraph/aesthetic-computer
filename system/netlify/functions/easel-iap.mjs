// Apple in-app purchases of braincells for Aesel on iPhone.
//
// The app sends the StoreKit 2 signed transaction (`jwsRepresentation`); we
// verify Apple's signature and certificate chain here, never the client's
// word, and credit the same wallet the Stripe checkout fills. The grant id is
// the Apple transaction id, so a retried redemption or a duplicate
// notification cannot credit twice. App Store Server Notifications (REFUND)
// arrive at the same address with a `signedPayload` and take the pack back.
import { SignedDataVerifier, Environment } from '@apple/app-store-server-library';
import { authorize } from '../../backend/authorization.mjs';
import { appleRootCertificates } from '../../backend/apple-roots.mjs';
import { IAP_PRODUCTS, withWallets, fulfillGrant, revokeGrant } from '../../backend/easel-paid-credits.mjs';

export const BUNDLE_ID='computer.aesthetic.easel';
export const APP_APPLE_ID=6812823093;
const headers={'Content-Type':'application/json','Cache-Control':'no-store','Access-Control-Allow-Origin':'*','Access-Control-Allow-Headers':'Authorization, Content-Type','Access-Control-Allow-Methods':'POST, OPTIONS'};
const reply=(statusCode,value)=>({statusCode,headers,body:JSON.stringify(value)});

export function makeVerifiers({roots=appleRootCertificates(),sandbox=process.env.AC_IAP_ALLOW_SANDBOX==='true',online=true}={}){
  const list=[new SignedDataVerifier(roots,online,Environment.PRODUCTION,BUNDLE_ID,APP_APPLE_ID)];
  if(sandbox)list.push(new SignedDataVerifier(roots,online,Environment.SANDBOX,BUNDLE_ID,APP_APPLE_ID));
  return list;
}
async function decodeWith(verifiers,method,jws){
  let failure;
  for(const verifier of verifiers){
    try{return {payload:await verifier[method](jws),environment:verifier.environment};}
    catch(error){failure=error;}
  }
  throw failure||new Error('Unverifiable');
}
// What a verified transaction is worth, or why it is worth nothing.
export function grantFor(transaction,user){
  const product=IAP_PRODUCTS[transaction?.productId];
  if(!product)return {error:'Unknown product'};
  if(transaction.bundleId!==BUNDLE_ID)return {error:'Wrong app'};
  if(transaction.type!=='Consumable')return {error:'Not a consumable'};
  if(transaction.revocationDate)return {error:'Refunded'};
  const quantity=Number.isSafeInteger(transaction.quantity)&&transaction.quantity>0?transaction.quantity:1;
  if(quantity>10)return {error:'Quantity too large'};
  if(typeof transaction.transactionId!=='string'||!/^\d{1,30}$/.test(transaction.transactionId))return {error:'Invalid transaction id'};
  return {user,id:`apple:${transaction.transactionId}`,credits:product.credits*quantity,quantity,transactionId:transaction.transactionId};
}

export function createHandler({verifiers=makeVerifiers(),verifyUser=authorize,wallets=fn=>withWallets(fn),purchases=null,now=()=>new Date()}={}){
  const record=async doc=>{ if(purchases)await purchases(doc); };
  return async event=>{
    if(event.httpMethod==='OPTIONS')return reply(204,null);
    if(event.httpMethod!=='POST')return reply(405,{error:'POST only'});
    let body;try{body=JSON.parse(event.body||'{}');}catch{return reply(400,{error:'Invalid request'});}

    // App Store Server Notifications V2: unauthenticated, signed by Apple.
    if(typeof body.signedPayload==='string'){
      let note;try{note=(await decodeWith(verifiers,'verifyAndDecodeNotification',body.signedPayload));}catch{return reply(401,{error:'Invalid notification'});}
      const {payload,environment}=note;
      try{
        if(['REFUND','REVOKE'].includes(payload.notificationType)&&payload.data?.signedTransactionInfo){
          const {payload:transaction}=await decodeWith(verifiers,'verifyAndDecodeTransaction',payload.data.signedTransactionInfo);
          const owner=purchases?await purchases({find:`apple:${transaction.transactionId}`}):null;
          const grant=grantFor({...transaction,revocationDate:undefined},owner?.user);
          if(grant.error)return reply(200,{received:true,ignored:grant.error});
          if(!owner?.user)return reply(200,{received:true,ignored:'Unknown purchase'});
          await wallets(w=>revokeGrant(grant,w));
          await record({id:grant.id,user:grant.user,refundedAt:now(),environment,notificationUUID:payload.notificationUUID});
        }
        return reply(200,{received:true});
      }catch(error){console.error('IAP notification failed:',error.message);return reply(500,{error:'Notification pending'});}
    }

    // A purchase the app just made, redeemed for the signed-in account.
    let user;try{user=await verifyUser(event.headers||{});}catch{return reply(401,{error:'Sign in to add braincells'});}
    if(!user?.sub)return reply(401,{error:'Sign in to add braincells'});
    if(typeof body.jws!=='string'||body.jws.length>16384)return reply(400,{error:'Missing transaction'});
    let verified;try{verified=await decodeWith(verifiers,'verifyAndDecodeTransaction',body.jws);}catch{return reply(401,{error:'Apple could not verify this purchase'});}
    const grant=grantFor(verified.payload,user.sub);
    if(grant.error)return reply(400,{error:grant.error});
    try{
      const credited=await wallets(w=>fulfillGrant(grant,w));
      await record({id:grant.id,user:user.sub,environment:verified.environment,productId:verified.payload.productId,quantity:grant.quantity,credits:grant.credits,
        originalTransactionId:verified.payload.originalTransactionId,purchaseDate:verified.payload.purchaseDate,appAccountToken:verified.payload.appAccountToken,redeemedAt:now()});
      return reply(200,{credited,transactionId:grant.transactionId,credits:grant.credits,environment:verified.environment});
    }catch(error){console.error('IAP fulfillment failed:',error.message);return reply(503,{error:'Could not add braincells yet. The purchase is kept and will be retried.'});}
  };
}

// Purchases are remembered by grant id so a refund can find its owner.
async function purchaseLedger(doc){
  const { connect } = await import('../../backend/database.mjs');
  const connection=await connect();
  try{
    const purchases=connection.db.collection('ac-credit-purchases');
    if(doc.find)return purchases.findOne({_id:doc.find});
    const {id,...rest}=doc;
    await purchases.updateOne({_id:id},{$set:rest,$setOnInsert:{createdAt:new Date()}},{upsert:true});
  }finally{await connection.disconnect();}
}
export async function handler(event){
  return createHandler({purchases:purchaseLedger})(event);
}
