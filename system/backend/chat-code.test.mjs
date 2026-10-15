import test from 'node:test';
import assert from 'node:assert/strict';
import { ObjectId } from 'mongodb';
import { ensureChatCode } from './chat-code.mjs';
import { feedOnlyOp, currentChatPosts } from './mime-feed-engine.mjs';
import { createHandler } from '../netlify/functions/mime.mjs';

function fixture(instance = 'clock') {
  const codes = new Map();
  const message = {_id:new ObjectId(),user:'person',text:'testing gyp',when:new Date()};
  const key = 'chat_' + instance + '_' + message._id;
  const cursor = rows => ({sort(){return this},toArray:async()=>rows});
  const db = {collection(name){
    if(name==='chat-codes')return {
      createIndex:async()=>{},
      findOne:async q=>q._id?codes.get(q._id):[...codes.values()].find(r=>r.code===q.code),
      insertOne:async row=>{if(codes.has(row._id)||[...codes.values()].some(r=>r.code===row.code))throw Object.assign(Error('duplicate'),{code:11000});codes.set(row._id,row);},
    };
    if(name==='chat-'+instance)return {
      findOne:async q=>{assert.equal(q.deleted.$ne,true);return String(q._id)===String(message._id)&&!message.deleted?message:null;},
      find:q=>{assert.equal(q.deleted.$ne,true);return cursor(message.deleted?[]:[message]);},
    };
    if(['chat-system','chat-clock'].includes(name))return {findOne:async()=>null,find:()=>cursor([])};
    if(name==='@handles')return {find:()=>cursor(message.user==='person'?[{_id:'person',handle:'tester'}]:[])};
    if(name==='mime-media-threads')return {findOne:async()=>({replies:1})};
    if(name==='mimechan')return {find:q=>cursor(q.parent===key?[{code:'reply',parent:key,text:'hello'}]:[]),findOne:async()=>null};
    if(name==='mime-engagement')return {aggregate:()=>cursor([])};
    throw Error('Unexpected collection '+name);
  }};
  return {db,codes,message,key};
}

test('chat identity is short, persistent, concurrency safe and scoped to its channel',async()=>{
  const f=fixture();
  const codes=await Promise.all(Array.from({length:8},()=>ensureChatCode(f.db,'clock',f.message._id)));
  assert.equal(new Set(codes).size,1);assert.match(codes[0],/^[a-z2-9]{3}$/);
  const other=await ensureChatCode(f.db,'system',f.message._id);assert.notEqual(other,codes[0]);
  assert.equal(await ensureChatCode(f.db,'clock',f.message._id),codes[0]);
  await assert.rejects(()=>ensureChatCode(f.db,'private',f.message._id));
});

test('room addresses keep the same message, comments and legacy alias, and reject the wrong room',async()=>{
 for(const [instance,room,other] of [['clock','laer-klokken','laklok'],['system','laklok','laer-klokken']]) {
  const f=fixture(instance),code=await ensureChatCode(f.db,instance,f.message._id);
  const address=room+'~'+code;
  const post=await feedOnlyOp(f.db,address);
  assert.equal(post.code,f.key);assert.equal(post.media.address,address);
  assert.equal(post.origin.name,room);assert.equal(post.origin.url,'https://aesthetic.computer/'+room);
  assert.deepEqual(await feedOnlyOp(f.db,'chat~'+code),post);
  assert.equal(await feedOnlyOp(f.db,other+'~'+code),null);
  const handler=createHandler(async()=>({db:f.db}));
  const response=await handler({httpMethod:'GET',queryStringParameters:{thread:address}});
  assert.equal(response.statusCode,200,response.body);
  assert.equal(JSON.parse(response.body).replies[0].parent,f.key);
  f.message.deleted=true;
  assert.equal(await feedOnlyOp(f.db,address),null);
 }
});

test('short chat links retain thread replies and read edits and deletes through cached cards',async()=>{
  const f=fixture(),code=await ensureChatCode(f.db,'clock',f.message._id);
  const op=await feedOnlyOp(f.db,'chat~'+code);
  assert.equal(op.code,f.key);assert.equal(op.origin.name,'laer-klokken');
  assert.deepEqual(await feedOnlyOp(f.db,f.key),op);
  const handler=createHandler(async()=>({db:f.db}));
  const response=await handler({httpMethod:'GET',queryStringParameters:{thread:'chat~'+code}});
  assert.equal(response.statusCode,200,response.body);assert.equal(JSON.parse(response.body).replies[0].code,'reply');
  f.message.text='edited';
  assert.equal((await feedOnlyOp(f.db,'chat~'+code)).text,'edited');
  assert.equal((await currentChatPosts(f.db,[op]))[0].text,'edited');
  f.message.deleted=true;
  assert.equal(await feedOnlyOp(f.db,'chat~'+code),null);
  assert.deepEqual(await currentChatPosts(f.db,[op]),[]);
  assert.equal(await feedOnlyOp(f.db,'chat~missing'),null);
  assert.equal(await feedOnlyOp(f.db,'chat_private_'+f.message._id),null);
});
