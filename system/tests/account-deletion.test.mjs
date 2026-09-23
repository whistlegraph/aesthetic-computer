// node --experimental-vm-modules --test system/tests/account-deletion.test.mjs
import test from 'node:test';
import assert from 'node:assert/strict';
import vm from 'node:vm';
import {readFile} from 'node:fs/promises';

async function fixture({listError=false,objectErrors=false,identity=true,pds={deleted:true},authorized=true}={}) {
 const calls=[];
 class ListObjectsV2Command {}
 class DeleteObjectsCommand {}
 const context=vm.createContext({process:{env:{}},console:{log(){},error(){}}});
 const collection={deleteMany:async()=>{},deleteOne:async()=>{},updateMany:async()=>{}};
 const mocks={
  '../../backend/authorization.mjs':{
   authorize:async()=>authorized?{sub:'test-user'}:null,
   getHandleOrEmail:async()=>null,userIDFromEmail:async()=>null,
   deleteUser:async()=>{calls.push('identity');return {success:identity};}
  },
  '../../backend/database.mjs':{connect:async()=>({db:{collection:()=>collection},disconnect:async()=>calls.push('disconnect')})},
  '../../backend/http.mjs':{respond:(statusCode,body)=>({statusCode,body})},
  '@aws-sdk/client-s3':{ListObjectsV2Command,DeleteObjectsCommand,S3Client:class{async send(command){
   if(command instanceof ListObjectsV2Command){if(listError)throw Error('offline');return {Contents:[{Key:'test-user/art'}]};}
   return objectErrors?{Errors:[{Code:'AccessDenied'}]}:{};
  }}},
  '../../backend/kv.mjs':{},'../../backend/shell.mjs':{shell:{log(){}}},
  '../../backend/at.mjs':{deleteAtprotoAccount:async()=>pds},
 };
 const module=new vm.SourceTextModule(await readFile(new URL('../netlify/functions/delete-erase-and-forget-me.mjs',import.meta.url),'utf8'),{context});
 await module.link(name=>new vm.SyntheticModule(Object.keys(mocks[name]),function(){for(const [key,value]of Object.entries(mocks[name]))this.setExport(key,value);},{context}));
 await module.evaluate();
 return {calls,run:()=>module.namespace.handler({httpMethod:'POST',headers:{}})};
}

test('storage listing and partial object failures never report successful deletion',async()=>{
 for(const options of [{listError:true},{objectErrors:true}]){
  const f=await fixture(options);assert.equal((await f.run()).statusCode,500);assert.ok(!f.calls.includes('identity'));
 }
});
test('identity deletion failure is retryable and always closes the database',async()=>{
 const f=await fixture({identity:false});const result=await f.run();assert.equal(result.statusCode,500);assert.match(result.body.message,/registration/);assert.deepEqual(f.calls,['identity','disconnect']);
});
test('linked-account failures stop before deleting the sign-in identity',async()=>{
 for(const reason of ['request-failed','missing-admin-password']){
  const f=await fixture({pds:{deleted:false,reason}});assert.equal((await f.run()).statusCode,500);assert.deepEqual(f.calls,['disconnect']);
 }
});
test('accounts with or without a linked identity complete and close the database',async()=>{
 for(const pds of [{deleted:true},{deleted:false,reason:'missing-did'}]){
  const f=await fixture({pds});assert.equal((await f.run()).statusCode,200);assert.deepEqual(f.calls,['identity','disconnect']);
 }
});
test('unauthenticated deletion cannot reach identity or database operations',async()=>{
 const f=await fixture({authorized:false});assert.equal((await f.run()).statusCode,401);assert.deepEqual(f.calls,[]);
});
