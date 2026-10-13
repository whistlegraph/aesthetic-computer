// mime.ac: the public feed of published Whistlegraph mimes, newest first.
// GET /api/mime-feed?limit=20&before=<publishedAt>  → {mimes:[…], next}
// GET /api/mime-feed?code=wgXxxxx                   → one published mime
// Only threads their owner published appear; nothing here needs an account.
import {whistlegraphStore} from './whistlegraph.mjs';
import {feedEntry} from '../../backend/whistlegraph.mjs';
export function createMimeFeedHandler(storeForRequest=whistlegraphStore) { return async function handler(event) {
  const headers={'Content-Type':'application/json','Cache-Control':'public, max-age=30','Access-Control-Allow-Origin':'*'};
  const reply=(statusCode,value)=>({statusCode,headers,body:JSON.stringify(value)});
  if(event.httpMethod==='OPTIONS')return reply(204,{});
  if(event.httpMethod!=='GET')return reply(405,{error:'Use GET'});
  try {
    const store=await storeForRequest(),q=event.queryStringParameters||{};
    if(q.code){
      const row=await store.readPublished(q.code);if(!row)return reply(404,{error:'No such mime'});
      const entry=feedEntry(row);
      if(q.version!==undefined&&String(entry.version)!==String(q.version))return {...reply(409,{error:'This piece has a newer published version. Refresh the feed to remix it.'}),headers:{...headers,'Cache-Control':'no-store'}};
      return reply(200,entry);
    }
    const rows=await store.feed({before:q.before,limit:Number(q.limit)||20});
    return reply(200,{mimes:rows.map(feedEntry),next:rows.length?rows.at(-1).publishedAt:null});
  }catch{return reply(503,{error:'Feed unavailable'});}
}; }
export const handler=createMimeFeedHandler();
