// A turn worker finished a turn: tell the thread's room and push the owner.
// POST {owner, code, versionID, displayText}  x-ac-worker: <secret>
// Workers only; the phone never calls this.
import {timingSafeEqual} from 'node:crypto';
import {connect} from '../../backend/database.mjs';
import {whistlegraphStore} from './whistlegraph.mjs';
import {publicThread} from '../../backend/whistlegraph.mjs';
import {threadUpdated} from '../../backend/whistlegraph-live.mjs';
export async function handler(event) {
  const headers={'Content-Type':'application/json','Cache-Control':'no-store'};
  const reply=(statusCode,value)=>({statusCode,headers,body:JSON.stringify(value)});
  if(event.httpMethod!=='POST')return reply(405,{error:'Use POST'});
  const secret=process.env.WHISTLEGRAPH_WORKER_SECRET||'',presented=String(event.headers?.['x-ac-worker']||'');
  if(secret.length<32||presented.length!==secret.length||!timingSafeEqual(Buffer.from(presented),Buffer.from(secret)))return reply(401,{error:'Workers only'});
  let body;try{body=JSON.parse(event.body||'{}');}catch{return reply(400,{error:'Send JSON'});}
  const {owner,code,versionID,displayText=''}=body;
  if(typeof owner!=='string'||!owner||typeof code!=='string'||!Number.isSafeInteger(versionID))return reply(400,{error:'Specify owner, code and versionID'});
  try{
    const store=await whistlegraphStore();
    const row=await store.read(owner,code);
    if(!row)return reply(404,{error:'Thread unavailable'});
    const listeners=threadUpdated(row._id,{thread:publicThread(row),versionID,source:'worker'});
    let push=null;
    try{
      const {db}=await connect();
      const {sendToTarget}=await import('../../../shared/push.mjs');
      push=await sendToTarget(db,{user:owner,app:'whistlegraph'},{title:'Your piece is ready',body:(String(displayText).trim()||'/'+code).slice(0,120)+' · v'+versionID,data:{url:'/'+code},thread:'whistlegraph-'+code,collapse:'whistlegraph-'+code});
    }catch(error){push={error:error.message};}
    return reply(200,{told:listeners,push});
  }catch(error){return reply(503,{error:'Could not tell the thread: '+error.message});}
}
