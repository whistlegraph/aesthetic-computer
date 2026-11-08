import test from 'node:test';
import assert from 'node:assert/strict';
import { Readable } from 'node:stream';
import { memoryDb } from './memory-db.mjs';
import { createGameStore } from '../../backend/oskie-store.mjs';
import { createGameObjects, OSKIE_TYPE } from '../../backend/oskie-objects.mjs';
import { starterGame } from '../../public/oskie/format.mjs';
import { createGameAPI } from '../../netlify/functions/oskie.mjs';

const jeffrey = {sub:'auth0|test-jeffrey', handle:'@jeffrey'};
const other = {sub:'auth0|test-other', handle:'@other'};
function fixture() {
  const files = new Map(), calls = [], db = memoryDb();
  let failDelete = false, failPut = false;
  const client = {async send(command) {
    const {input} = command, kind = command.constructor.name;
    calls.push({kind, ...input});
    assert.equal(input.Bucket, 'test-user-bucket');
    if (kind === 'PutObjectCommand') {
      if (failPut) throw Error('upload failed');
      assert.equal(input.ACL, 'private');
      assert.equal(input.ContentType, OSKIE_TYPE);
      files.set(input.Key, Buffer.from(input.Body));
      return {};
    }
    if (kind === 'GetObjectCommand') {
      const bytes = files.get(input.Key);
      if (!bytes) throw Error('object missing');
      return {ContentLength: bytes.length, Body: Readable.from([bytes])};
    }
    if (kind === 'DeleteObjectCommand') {
      if (failDelete) throw Error('delete failed');
      files.delete(input.Key); return {};
    }
    throw Error(kind);
  }};
  const objects = createGameObjects({client, bucket:'test-user-bucket'});
  const store = createGameStore(db, {objects});
  return {files,calls,db,objects,store, failDeletion(value) {failDelete=value;}, failUpload(value) {failPut=value;}};
}

test('private object-backed revisions load, download, restore and publish exact bytes', async () => {
  const f = fixture(), p = await f.store.create(jeffrey, {bundle:starterGame('first-word','Public title')});
  const r = await f.db.collection('oskie-revisions').findOne({_id:p.revision});
  assert.equal(r.bundle, undefined);
  assert.match(r.object.key, /^auth0\|test-jeffrey\/oskie\/[a-f0-9]{32}\/[a-f0-9]{32}\.oskie$/);
  assert.equal(f.files.size,1);
  assert.equal((await f.store.read(jeffrey,p.id)).bundle.manifest.title,'Public title');
  await assert.rejects(f.store.read(other,p.id), {status:404});
  await assert.rejects(f.store.readPublic(p.id), {status:404});
  await f.store.publish(jeffrey,p.id,p.revision);
  const change = await f.store.update(jeffrey,p.id,starterGame('secret-title','SECRET DRAFT'),p.revision);
  const catalog = await f.store.listPublished();
  assert.equal(catalog.games[0].title,'Public title');
  assert.equal(catalog.games[0].revision,p.revision);
  assert.equal(catalog.games[0].file.type,OSKIE_TYPE);
  const serialized = JSON.stringify(catalog);
  for (const privateValue of ['SECRET DRAFT', jeffrey.sub, 'object', 'bundle', change.revision]) assert.ok(!serialized.includes(privateValue),privateValue);
  const api = createGameAPI({store:async()=>f.store});
  const response = await api({path:`/api/oskie/${p.id}/download`,httpMethod:'GET'});
  assert.equal(response.statusCode,200);
  assert.equal(JSON.parse(response.body).manifest.title,'Public title');
  await f.store.restore(jeffrey,p.id,p.revision,change.revision);
  assert.equal((await f.store.read(jeffrey,p.id)).bundle.manifest.title,'Public title');
  await f.store.unpublish(jeffrey,p.id);
  assert.equal((await f.store.listPublished()).games.length,0);
  assert.equal((await api({path:`/api/oskie/${p.id}/download`,httpMethod:'GET'})).statusCode,404);
});

test('catalog filters authors, pages without duplicates and rejects malformed queries', async () => {
  const {store} = fixture();
  for (let i=0;i<6;i++) {
    const owner = i%2 ? other : jeffrey;
    const p = await store.create(owner);
    await store.publish(owner,p.id,p.revision);
  }
  await store.create(jeffrey); // private game must never appear
  assert.equal((await store.listPublished({author:'jeffrey'})).games.length,3);
  assert.equal((await store.listPublished({author:'@other'})).games.length,3);
  const seen = new Set(); let cursor;
  do {
    const page = await store.listPublished({limit:2,cursor});
    for (const g of page.games) {assert.ok(!seen.has(g.id)); seen.add(g.id);}
    cursor=page.nextCursor;
  } while(cursor);
  assert.equal(seen.size,6);
  const api = createGameAPI({store:async()=>store});
  const get = queryStringParameters => api({path:'/api/oskie/catalog',httpMethod:'GET',queryStringParameters});
  assert.equal(JSON.parse((await get({author:'@jeffrey'})).body).games.length,3);
  for (const q of [{author:{$ne:''}},{cursor:'../../bad'},{limit:'999'},{limit:'nope'}]) assert.equal((await get(q)).statusCode,400);
  assert.equal((await api({path:'/api/oskie/catalog',httpMethod:'POST'})).statusCode,405);
});

test('object failure cannot advance a revision; checksum failures cannot publish', async () => {
  const f=fixture(), p=await f.store.create(jeffrey);
  f.failUpload(true);
  await assert.rejects(f.store.update(jeffrey,p.id,starterGame(),p.revision), /upload failed/);
  assert.equal((await f.store.read(jeffrey,p.id)).revision,p.revision);
  const r=await f.db.collection('oskie-revisions').findOne({_id:p.revision});
  f.files.set(r.object.key,Buffer.from('{}'));
  await assert.rejects(f.store.publish(jeffrey,p.id,p.revision), /checksum/);
  assert.deepEqual((await f.store.listPublished()).games,[]);
});

test('concurrent edits clean losing objects; deletion remains retryable after storage failure', async () => {
  const f=fixture(), p=await f.store.create(jeffrey);
  const results=await Promise.allSettled([1,2].map(()=>f.store.update(jeffrey,p.id,starterGame(),p.revision)));
  assert.equal(results.filter(r=>r.status==='fulfilled').length,1);
  assert.equal(f.files.size,2);
  const next=results.find(r=>r.status==='fulfilled').value;
  await f.store.publish(jeffrey,p.id,next.revision);
  f.failDeletion(true);
  await assert.rejects(f.store.remove(jeffrey,p.id,next.revision),/delete failed/);
  await assert.rejects(f.store.readPublic(p.id),{status:404});
  await assert.rejects(f.store.update(jeffrey,p.id,starterGame(),next.revision),{status:404});
  assert.equal((await f.store.listPublished()).games.length,0);
  f.failDeletion(false);
  await f.store.remove(jeffrey,p.id,next.revision);
  assert.equal(f.files.size,0);
  assert.equal(await f.db.collection('oskie-revisions').countDocuments({owner:jeffrey.sub}),0);
});

test('legacy inline bundles remain playable and discoverable without a migration', async () => {
  const f=fixture(), old=createGameStore(f.db);
  const p=await old.create(jeffrey,{bundle:starterGame('old-game','Old game')});
  await old.publish(jeffrey,p.id,p.revision);
  assert.equal((await f.store.readPublic(p.id)).bundle.manifest.title,'Old game');
  assert.equal((await f.store.listPublished()).games[0].title,'Old game');
  const next=await f.store.update(jeffrey,p.id,starterGame(),p.revision);
  assert.equal(f.files.size,1);
  await f.store.restore(jeffrey,p.id,p.revision,next.revision);
  assert.equal(f.files.size,2);
});

test('migration verifies old bundles, is repeatable, and preserves publication and ownership', async () => {
  const {migrateGameObjects}=await import('../../backend/oskie-migrate-objects.mjs');
  const f=fixture(), old=createGameStore(f.db), p=await old.create(jeffrey);
  await old.publish(jeffrey,p.id,p.revision);
  assert.deepEqual(await migrateGameObjects(f.db,f.objects),{apply:false,scanned:1,migrated:0,skipped:0});
  assert.equal(f.files.size,0);
  assert.equal((await migrateGameObjects(f.db,f.objects,{apply:true})).migrated,1);
  const r=await f.db.collection('oskie-revisions').findOne({_id:p.revision});
  assert.equal(r.bundle,undefined);
  assert.equal((await f.store.readPublic(p.id)).revision,p.revision);
  assert.equal((await migrateGameObjects(f.db,f.objects,{apply:true})).scanned,0);
  await assert.rejects(f.store.read(other,p.id),{status:404});
});

test('account export includes portable object-backed bundles rather than unusable private links', async () => {
  const {exportAccount}=await import('../../backend/account-deletion.mjs');
  const f=fixture(), p=await f.store.create(jeffrey);
  const data=await exportAccount({db:f.db,gameObjects:f.objects,storage:{list:async()=>[
    {key:`${jeffrey.sub}/oskie/${p.id}/${p.revision}.oskie`,url:'private'},
    {key:`${jeffrey.sub}/painting.png`,url:'public'},
  ]}},{user:jeffrey});
  assert.equal(data.records['oskie-revisions'][0].bundle.manifest.author,'@jeffrey');
  assert.equal(data.files.length,1);
  assert.equal(data.files[0].url,'public');
});

test('revision polling checks access but does not repeatedly read unchanged objects', async () => {
  const f=fixture(), p=await f.store.create(jeffrey);
  await f.store.publish(jeffrey,p.id,p.revision);
  const before=f.calls.filter(c=>c.kind==='GetObjectCommand').length;
  const api=createGameAPI({store:async()=>f.store});
  const req={path:`/api/oskie/${p.id}`,httpMethod:'GET',queryStringParameters:{if_revision:p.revision}};
  assert.deepEqual(JSON.parse((await api(req)).body),{id:p.id,revision:p.revision,unchanged:true});
  assert.equal(f.calls.filter(c=>c.kind==='GetObjectCommand').length,before);
  await f.store.unpublish(jeffrey,p.id);
  assert.equal((await api(req)).statusCode,404);
});
