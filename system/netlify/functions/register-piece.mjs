// Register a verified @handle piece once; saves update its bytes, not its feed position.
import {createHash} from 'node:crypto';
import {authorize} from '../../backend/authorization.mjs';
import {connect} from '../../backend/database.mjs';
import {respond} from '../../backend/http.mjs';
import {generateUniqueCode} from '../../backend/generate-short-code.mjs';
import {registerPiece, PIECE_BUCKET} from '../../backend/piece-registry.mjs';
export function createHandler({authenticate=authorize,database=connect,request=globalThis.fetch,makeCode=generateUniqueCode}={}){
 return async event=>{
  if(event.httpMethod!=='POST')return respond(405,{error:'POST required'});
  let db;
  try{
   const user=await authenticate(event.headers||{});if(!user?.sub)return respond(401,{error:'Sign in to register a piece'});
   const {slug,ext,revision,version}=JSON.parse(event.body||'{}');
   if(typeof slug!=='string'||! /^[a-zA-Z0-9_-]{1,100}$/.test(slug)||!['mjs','lisp','lua'].includes(ext)||! /^[a-f0-9]{64}$/.test(revision||''))return respond(400,{error:'Invalid piece identity'});
   if(!Number.isInteger(version)||version<2)return respond(200,{eligible:false});
   const key=`${user.sub}/piece/${slug}.${ext}`;
   const upload=await request(`https://${PIECE_BUCKET}.sfo3.digitaloceanspaces.com/`+key.split('/').map(encodeURIComponent).join('/'),{redirect:'error',signal:AbortSignal.timeout(8000)});
   if(!upload.ok)return respond(409,{error:'Published source is not readable yet'});
   if(Number(upload.headers.get('content-length'))>400000)return respond(413,{error:'Piece is too large'});
   const chunks=[];let size=0;for await(const chunk of upload.body){size+=chunk.length;if(size>400000)return respond(413,{error:'Piece is too large'});chunks.push(chunk);}
   const bytes=Buffer.concat(chunks);if(createHash('sha256').update(bytes).digest('hex')!==revision)return respond(409,{error:'Published source changed before registration'});
   db=await database();
   return respond(200,await registerPiece(db.db.collection('pieces'),{user:user.sub,slug,ext,bytes,makeCode}));
  }catch(error){return respond(500,{error:'Piece registration unavailable'});}finally{await db?.disconnect?.();}
 };
}
export const handler=createHandler();
