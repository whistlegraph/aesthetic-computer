// Publish or unpublish one of your mimes on the mime.ac feed.
// POST /api/whistlegraph-publish  Authorization: Bearer <AC token>
// {code:'wgXxxxx', published:true|false} → {published, mime}
import {whistlegraphStore} from './whistlegraph.mjs';
import {authenticateMusical} from './easel-musical-jev.mjs';
import {getHandleOrEmail} from '../../backend/authorization.mjs';
import {feedEntry} from '../../backend/whistlegraph.mjs';
export async function handler(event) {
  const headers={'Content-Type':'application/json','Cache-Control':'no-store','Access-Control-Allow-Origin':'*','Access-Control-Allow-Headers':'Authorization, Content-Type','Access-Control-Allow-Methods':'POST, OPTIONS'};
  const reply=(statusCode,value)=>({statusCode,headers,body:JSON.stringify(value)});
  if(event.httpMethod==='OPTIONS')return reply(204,{});
  if(event.httpMethod!=='POST')return reply(405,{error:'Use POST'});
  try {
    const owner=await authenticateMusical(event.headers||{});
    if(!owner)return reply(401,{error:'Sign in to your AC account'});
    let body;try{body=JSON.parse(event.body||'{}');}catch{return reply(400,{error:'Send JSON'});}
    if(typeof body.code!=='string'||typeof body.published!=='boolean')return reply(400,{error:'Specify code and published'});
    const store=await whistlegraphStore();
    const handle=body.published?await getHandleOrEmail(owner):undefined;
    const row=await store.publish(owner,body.code,body.published,handle);
    if(!row)return reply(404,{error:'Thread unavailable'});
    return reply(200,{published:row.published===true,mime:row.published?feedEntry(row):null});
  }catch{return reply(503,{error:'Thread storage unavailable'});}
}
