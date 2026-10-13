import test from 'node:test';
import assert from 'node:assert/strict';
import {createHash} from 'node:crypto';
import {createMimeFeedHandler} from '../netlify/functions/mime-feed.mjs';

test('remixing fetches the displayed public version and refuses a changed publication',async()=>{
 const source='export function paint(){}';
 const row={code:'wgOriginal',owner:'private-owner',handle:'@artist',ledger:{head:1,versions:[{id:1,source:'old'},{id:7,source,request:'private prompt'}]}};
 const handler=createMimeFeedHandler(async()=>({readPublished:async code=>code===row.code?row:null}));
 const get=version=>handler({httpMethod:'GET',queryStringParameters:{code:row.code,version}});
 const exact=await get('7'),body=JSON.parse(exact.body);
 assert.equal(exact.statusCode,200);assert.equal(body.source,source);assert.equal(body.version,7);
 assert.equal(body.sourceHash,createHash('sha256').update(source).digest('hex'));
 assert.equal(body.ledger,undefined);assert.equal(body.owner,undefined);
 for(const version of ['1','8','07','bad']){const stale=await get(version);assert.equal(stale.statusCode,409);assert.equal(stale.headers['Cache-Control'],'no-store');assert.equal(JSON.parse(stale.body).source,undefined);}
 const missing=await handler({httpMethod:'GET',queryStringParameters:{code:'private',version:'7'}});assert.equal(missing.statusCode,404);
});
