import {connect} from '../../backend/database.mjs';
import {authenticateMusical} from './easel-musical-jev.mjs';
import {mongoWalkiewareStore,publicThread} from '../../backend/walkieware.mjs';
export {authenticateMusical as authenticateWalkieware};
let pending;
export const walkiewareStore=()=>pending??=(async()=>{const {db}=await connect();return mongoWalkiewareStore(db.collection('walkieware-threads'));})().catch(error=>{pending=null;throw error;});
export async function handler(event) {
  const headers={'Content-Type':'application/json','Cache-Control':'no-store','Access-Control-Allow-Origin':'*','Access-Control-Allow-Headers':'Authorization, Content-Type','Access-Control-Allow-Methods':'GET, OPTIONS'};
  const reply=(statusCode,value)=>({statusCode,headers,body:JSON.stringify(value)});
  if(event.httpMethod==='OPTIONS')return reply(204,{});
  if(event.httpMethod!=='GET')return reply(405,{error:'Use GET; edits use the authenticated live socket'});
  try {
    const owner=await authenticateMusical(event.headers||{});
    if(!owner)return reply(401,{error:'Sign in to your AC account'});
    const store=await walkiewareStore(),code=event.queryStringParameters?.code;
    if(!code)return reply(200,{threads:(await store.list(owner)).map(publicThread)});
    const row=await store.read(owner,code);
    return row?reply(200,publicThread(row)):reply(404,{error:'Thread unavailable'});
  }catch{return reply(503,{error:'Thread storage unavailable'});}
}
