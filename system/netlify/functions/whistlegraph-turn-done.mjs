// A turn worker finished a turn: tell the thread's room and push the owner.
// POST {owner, code, versionID, displayText, still?}  x-ac-worker: <secret>
// still: base64 PNG of the saved version's last painted frame; kept in
// whistlegraph-stills and shown on the push (see whistlegraph-still.mjs).
// Workers only; the phone never calls this.
import {timingSafeEqual,randomBytes} from 'node:crypto';
import {connect} from '../../backend/database.mjs';
import {whistlegraphStore} from './whistlegraph.mjs';
import {publicThread} from '../../backend/whistlegraph.mjs';
import {threadUpdated} from '../../backend/whistlegraph-live.mjs';
const SITE=process.env.AC_SITE||'https://aesthetic.computer';
const MAX_STILL=600_000; // base64 chars, ~450 KB; the worker's frames are ≤384 px
const PNG=Buffer.from([0x89,0x50,0x4e,0x47,0x0d,0x0a,0x1a,0x0a]);
export async function handler(event) {
  const headers={'Content-Type':'application/json','Cache-Control':'no-store'};
  const reply=(statusCode,value)=>({statusCode,headers,body:JSON.stringify(value)});
  if(event.httpMethod!=='POST')return reply(405,{error:'Use POST'});
  const secret=process.env.WHISTLEGRAPH_WORKER_SECRET||'',presented=String(event.headers?.['x-ac-worker']||'');
  if(secret.length<32||presented.length!==secret.length||!timingSafeEqual(Buffer.from(presented),Buffer.from(secret)))return reply(401,{error:'Workers only'});
  let body;try{body=JSON.parse(event.body||'{}');}catch{return reply(400,{error:'Send JSON'});}
  const {owner,code,versionID,displayText='',still=''}=body;
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
      // Named the way the app's header names it: @jeffrey/wgVupos is ready.
      let handle='';
      try{const {getHandleOrEmail}=await import('../../backend/authorization.mjs');const found=await getHandleOrEmail(owner);if(typeof found==='string'&&found.startsWith('@'))handle=found;}catch{}
      // The piece's own picture rides on the push; a missing or bad still only costs the image.
      let image;
      try{const png=typeof still==='string'&&still.length<=MAX_STILL?Buffer.from(still,'base64'):null;
        if(png?.length>8&&png.subarray(0,8).equals(PNG)){const id=randomBytes(16).toString('hex');
          await db.collection('whistlegraph-stills').insertOne({_id:id,owner,code,versionID,png,createdAt:new Date()});
          image=SITE+'/api/whistlegraph-still?id='+id;}}catch{}
      push=await sendToTarget(db,{user:owner,app:'whistlegraph'},{title:handle+'/'+code+' is ready',body:(String(displayText).trim()||'/'+code).slice(0,120)+' · v'+versionID,image,data:{url:'/'+code,...(image?{image}:{})},thread:'whistlegraph-'+code,collapse:'whistlegraph-'+code});
    }catch(error){push={error:error.message};}
    return reply(200,{told:listeners,push});
  }catch(error){return reply(503,{error:'Could not tell the thread: '+error.message});}
}
