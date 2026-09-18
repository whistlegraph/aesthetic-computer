// Register a verified @handle piece once; saves update its bytes, not its feed position.
import {createHash} from 'node:crypto';
import {ObjectId} from 'mongodb';
import {authorize} from '../../backend/authorization.mjs';
import {connect} from '../../backend/database.mjs';
import {respond} from '../../backend/http.mjs';
import {generateUniqueCode} from '../../backend/generate-short-code.mjs';
export function createHandler({authenticate=authorize,database=connect,request=globalThis.fetch,makeCode=generateUniqueCode}={}){
 return async event=>{
  if(event.httpMethod!=='POST')return respond(405,{error:'POST required'});
  let db;
  try{
   const user=await authenticate(event.headers||{});if(!user?.sub)return respond(401,{error:'Sign in to register a piece'});
   const {slug,ext,revision}=JSON.parse(event.body||'{}');
   if(typeof slug!=='string'||! /^[a-zA-Z0-9_-]{1,100}$/.test(slug)||!['mjs','lisp','lua'].includes(ext)||! /^[a-f0-9]{64}$/.test(revision||''))return respond(400,{error:'Invalid piece identity'});
   const key=`${user.sub}/piece/${slug}.${ext}`;
   const upload=await request('https://user-aesthetic-computer.sfo3.digitaloceanspaces.com/'+key.split('/').map(encodeURIComponent).join('/'),{redirect:'error',signal:AbortSignal.timeout(8000)});
   if(!upload.ok)return respond(409,{error:'Published source is not readable yet'});
   if(Number(upload.headers.get('content-length'))>400000)return respond(413,{error:'Piece is too large'});
   const chunks=[];let size=0;for await(const chunk of upload.body){size+=chunk.length;if(size>400000)return respond(413,{error:'Piece is too large'});chunks.push(chunk);}
   const bytes=Buffer.concat(chunks);if(createHash('sha256').update(bytes).digest('hex')!==revision)return respond(409,{error:'Published source changed before registration'});
   db=await database();const pieces=db.db.collection('pieces');const mediaSlug=`piece/${slug}`;
   const existing=await pieces.findOne({user:user.sub,slug:{$in:[mediaSlug,`${user.sub}/${mediaSlug}`]}});
   const id=existing?._id||new ObjectId(createHash('sha256').update(`named-piece\0${user.sub}\0${slug}`).digest('hex').slice(0,24));
   const when=new Date(),code=existing?.code||await makeCode(pieces,{mode:'random',type:'piece'});
   // Atomic deterministic identity also protects concurrent first publications.
   await pieces.updateOne({_id:id},{$setOnInsert:{code,slug:mediaSlug,user:user.sub,when,bucket:'user-aesthetic-computer',type:'piece',name:slug,origin:'aesel'},$set:{ext,size:bytes.length,revision,updatedAt:when}},{upsert:true});
   const saved=await pieces.findOne({_id:id});
   return respond(200,{code:saved.code,id:String(saved._id),discussion:`https://mime.ac/#/t/piece_${saved._id}`});
  }catch(error){return respond(500,{error:'Piece registration unavailable'});}finally{await db?.disconnect?.();}
 };
}
export const handler=createHandler();
