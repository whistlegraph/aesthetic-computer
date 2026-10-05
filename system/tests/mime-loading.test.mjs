import test from 'node:test';
import assert from 'node:assert/strict';
import {createHandler} from '../netlify/functions/mime.mjs';
import {publicPosts, publicMediaUrl, mediaFile} from '../backend/mime-media.mjs';
const code='tape_6aa83d4ef78ff6472a0a64b2';
test('tape file and poster reads each validate current visibility with one lookup',async()=>{
 let calls=0,visible=true;
 const db={collection(name){assert.ok(['mimechan','tapes'].includes(name));return {async findOne(query){calls++;assert.deepEqual(query.slug,{$type:"string",$ne:""});assert.equal(query.private.$ne,true);assert.equal(query.deleted.$ne,true);assert.equal(query.hidden.$ne,true);assert.deepEqual(query.visibility.$in,[null,'public']);return visible?{slug:'tape',kind:'mp4',thumbnailUrl:'https://art.aesthetic.computer/tape.jpg'}:null;}}}};
 const handler=createHandler(async()=>({db}));
 for(const key of ['file','poster']){
  calls=0;const r=await handler({httpMethod:'GET',queryStringParameters:{[key]:code}});
  assert.equal(r.statusCode,302);assert.equal(calls,1);assert.equal(r.headers['Cache-Control'],'no-store');
 }
 visible=false;
 for(const key of ['file','poster'])assert.equal((await handler({httpMethod:'GET',queryStringParameters:{[key]:code}})).statusCode,404);
});
test('poster listing exposes a checked endpoint and rejects arbitrary storage hosts',async()=>{
 const [post]=await publicPosts({},[{code,parent:null,board:'video/mp4',_media:{kind:'tape',id:'id',code:'sample',hasPoster:true}}]);
 assert.equal(post.file.poster,`/api/mime?poster=${code}`);
 assert.ok(!JSON.stringify(post).includes('thumbnailUrl'));
 for(const url of ['https://evil.example/a','http://art.aesthetic.computer/a','https://u:p@art.aesthetic.computer/a'])assert.throws(()=>publicMediaUrl(url));
});
test('feed-only reads skip the global MIME census, normal inventory retains it',async()=>{
 let census=0;
 const db={collection(){return {aggregate(pipeline){if(pipeline.some(s=>s.$unionWith))census++;return {async toArray(){return []}}}}}};
 const handler=createHandler(async()=>({db}));
 let r=await handler({httpMethod:'GET',queryStringParameters:{feed:'1'}});assert.equal(r.statusCode,200);assert.equal(census,0);
 r=await handler({httpMethod:'GET',queryStringParameters:{}});assert.equal(r.statusCode,200);assert.equal(census,1);
});
test('Teia directory packs retain a standalone HTML preview and download in Mime',async()=>{
 const htmlUri='ipfs://Qm'+'a'.repeat(44),artifactUri='ipfs://Qm'+'b'.repeat(44);
 const [post]=await publicPosts({},[{code:'whistlegraph_'+'a'.repeat(64),parent:null,board:'text/html',_media:{kind:'whistlegraph',id:'id',code:'wgDefen',version:7,htmlUri,artifactUri,tokenId:'885470'}}]);
 assert.equal(post.file.name,'wgDefen-v7.html');assert.equal(post.file.type,'text/html');
 assert.equal(post.media.url,'https://ipfs.aesthetic.computer/ipfs/'+htmlUri.slice(7));
 assert.equal(mediaFile('whistlegraph',{htmlUri,artifactUri}).url,post.media.url);
});
