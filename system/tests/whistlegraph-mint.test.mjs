import test from 'node:test';
import assert from 'node:assert/strict';
import { randomBytes } from 'node:crypto';
import { InMemorySigner } from '@taquito/signer';
import { b58Encode, PrefixV2 } from '@taquito/utils';
import sharp from 'sharp';
import { whistlegraphMints, hash, mintPayload, verifyMintWallet, matchingMint, mintOperation, HEN_MINTER, HEN_OBJKTS } from '../backend/whistlegraph-mint.mjs';
import { normalizeMintCover, pinMintDirectory } from '../backend/whistlegraph-pack.mjs';
import { createServer } from 'node:http';
import { teiaPackage, TEIA_FORMAT } from '../backend/whistlegraph-teia.mjs';
import AdmZip from 'adm-zip';
import { createHandler } from '../netlify/functions/whistlegraph-mint.mjs';
function collection() {
  const docs = new Map();
  const matches = (doc, filter) => Object.entries(filter).every(([key, value]) => {
    if (key === '$or') return value.some(f => matches(doc, f));
    if (value && typeof value === 'object' && !(value instanceof Date)) {
      if ('$exists' in value) return (doc[key] !== undefined) === value.$exists;
      if ('$gt' in value) return doc[key] > value.$gt;
      if ('$ne' in value) return doc[key] !== value.$ne;
      if ('$lt' in value) return doc[key] < value.$lt;
    }
    return doc[key] === value;
  });
  return { docs,
    async findOne(filter) { const value = [...docs.values()].find(doc => matches(doc, filter)); return value ? structuredClone(value) : null; },
    async countDocuments(filter) { return [...docs.values()].filter(doc => matches(doc, filter)).length; },
    async insertOne(doc) { if (docs.has(doc._id)) throw Object.assign(new Error('duplicate'), { code:11000 }); docs.set(doc._id, structuredClone(doc)); },
    async updateOne(filter, update, opts = {}) {
      let doc = [...docs.values()].find(doc => matches(doc, filter));
      if (!doc && opts.upsert) { doc = { _id:filter._id, ...structuredClone(update.$setOnInsert) }; docs.set(doc._id, doc); }
      if (!doc) return { modifiedCount:0 };
      Object.assign(doc, structuredClone(update.$set || {}));
      for (const key of Object.keys(update.$unset || {})) delete doc[key];
      for (const [key, val] of Object.entries(update.$inc || {})) doc[key] = (doc[key] || 0) + val;
      for (const [key, val] of Object.entries(update.$addToSet || {})) if (!doc[key].includes(val)) doc[key].push(val);
      return { modifiedCount:1 };
    },
  };
}
const address = 'tz1inPpZMzFUv5mkmqDMEC8sYxmEq53vxRhw';
const other = 'tz1gkf8EexComFBJvjtT1zdsisdah791KwBE';
const operationHash = 'o' + 'a'.repeat(50);
const time = new Date('2026-10-05T23:00:00Z');
async function fixture() {
  const intents = collection(), threads = collection(), pins = [], state = { txs:[], transfers:[], packs:0,
    head:{ chain:'mainnet', chainId:'NetXdQprcVkpaWU', synced:true, level:102 } };
  const source = 'export function paint({wipe}) { wipe("red"); }';
  await threads.insertOne({ _id:'piece', owner:'user', code:'wgDefen', codeKey:'wgdefen', ledger:{ head:7, versions:[{ id:7, source }] } });
  const mints = whistlegraphMints({ intents, threads, now:() => time, cover:async () => Buffer.from('cover'),
    pack:async () => { state.packs++; return { html:'<html><head></head>packed</html>', version:'test', preview:{ gif:Buffer.from('gif'), thumbnail:Buffer.from('png'), frames:48 } }; },
    pinDirectory:async files => { state.files = files; return 'ipfs://Qm' + 'a'.repeat(44); },
    pin:async (name, mime, bytes) => { pins.push({ name, mime, bytes }); return 'ipfs://Qm' + hash(bytes).slice(0,44); },
    verify:(_i, p) => p.signature === 'valid',
    chain:async path => path === '/head' ? state.head : path.startsWith('/tokens/') ? state.transfers : state.txs });
  const input = { secret:'a'.repeat(64), code:'wgDefen', version:7, sourceHash:hash(source), density:1,
    title:'Spinning tree', description:'Test', editions:1, royalties:150, cover:'cover' };
  return { mints, intents, threads, pins, state, input, secret:input.secret };
}
async function ready() {
  const f = await fixture();
  await f.mints.create('user', '@test', f.input);
  await f.mints.prepare(f.secret);
  await f.mints.bind(f.secret, { address, signature:'valid' });
  return f;
}
async function requested() {
  const f = await ready(); await f.mints.begin(f.secret);
  const i = [...f.intents.docs.values()][0];
  const tx = { id:123, level:100, counter:41, hash:operationHash, status:'applied', sender:{ address }, target:{ address:HEN_MINTER }, amount:0,
    timestamp:'2026-10-05T23:00:01Z', parameter:{ entrypoint:'mint_OBJKT', value:{ address, amount:'1', royalties:'150', metadata:Buffer.from(i.metadataUri).toString('hex') } } };
  const transfer = { from:null, to:{ address }, amount:'1', token:{ contract:{ address:HEN_OBJKTS }, tokenId:'900001' } };
  return { ...f, i, tx, transfer };
}

test('only the owner can snapshot a saved exact version; a retry preserves its settings', async () => {
  const f = await fixture();
  await assert.rejects(f.mints.create('stranger', '@other', f.input), /Save/);
  await assert.rejects(f.mints.create('user', '@test', { ...f.input, sourceHash:'f'.repeat(64) }), /differs/);
  for (const patch of [{ code:'../../private' }, { editions:0 }, { editions:1.5 }, { royalties:251 }, { density:0 }, { title:'' }]) {
    await assert.rejects(f.mints.create('user', '@test', { ...f.input, ...patch }), /Invalid/);
  }
  const created = await f.mints.create('user', '@test', f.input);
  assert.equal(created.version, 7); assert.equal(created.source, undefined); assert.equal(created.cover, undefined);
  assert.equal((await f.mints.create('user', '@test', { ...f.input, title:'Changed' })).title, 'Spinning tree');
  await assert.rejects(f.mints.create('stranger', '@other', f.input), /another account/);
  assert.equal(f.intents.docs.size, 1);
});
test('packing is claimed once and publishes a Teia directory, ZIP, HTML, GIF and thumbnail', async () => {
  const f = await fixture(); await f.mints.create('user', '@test', f.input);
  await Promise.all([f.mints.prepare(f.secret), f.mints.prepare(f.secret)]);
  assert.equal(f.state.packs, 1);
  assert.deepEqual(f.pins.map(p => p.mime), ['text/html','image/gif','image/png','application/zip']);
  assert.deepEqual(f.state.files.map(p => p.name), ['index.html','cover.gif','thumbnail.png']);
  const zip = new AdmZip(f.pins.find(p => p.mime === 'application/zip').bytes);
  assert.match(zip.readAsText('index.html'), /property="og:image" content="cover.gif"/);
  assert.match(zip.readAsText('index.html'), /Content-Security-Policy/);
  const i = [...f.intents.docs.values()][0];
  assert.equal(i.status, 'packed'); assert.equal(i.source, undefined); assert.equal(i.cover, undefined);
  assert.equal(i.artifactHash, hash('<html><head></head>packed</html>'));
  assert.equal(i.artifactMimeType, TEIA_FORMAT);
  assert.equal(i.previewFrames, 48);
});
test('real signed ownership proof binds account, source, version and mint nonce', async () => {
  const signer = new InMemorySigner(b58Encode(randomBytes(32), PrefixV2.Ed25519Seed));
  const i = { _id:'mint', handle:'test', code:'wgDefen', version:7, sourceHash:'hash', nonce:'nonce' };
  const signature = (await signer.sign(mintPayload(i))).prefixSig;
  const proof = { address:await signer.publicKeyHash(), publicKey:await signer.publicKey(), signature };
  assert.equal(verifyMintWallet(i, proof), true);
  for (const patch of [{ version:8 }, { sourceHash:'changed' }, { handle:'other' }, { _id:'other' }, { nonce:'other' }]) assert.equal(verifyMintWallet({ ...i, ...patch }, proof), false);
});
test('wallet ownership is required, metadata immutable and mint dispatch happens once', async () => {
  const f = await ready();
  await assert.rejects(f.mints.bind(f.secret, { address:other, signature:'valid' }), /different wallet/);
  const metadata = JSON.parse(f.pins.find(p => p.mime === 'application/json').bytes);
  assert.equal(metadata.whistlegraph.version, 7); assert.equal(metadata.whistlegraph.sourceHash, f.input.sourceHash);
  assert.deepEqual(metadata.creators, [address]);
  assert.deepEqual(metadata.formats.map(f => f.mimeType), [TEIA_FORMAT, 'image/gif', 'image/png']);
  assert.notEqual(metadata.thumbnailUri, metadata.displayUri);
  const first = await f.mints.begin(f.secret);
  assert.deepEqual(first.operation, mintOperation([...f.intents.docs.values()][0]));
  assert.equal(first.operation.destination, HEN_MINTER); assert.equal(first.operation.amount, '0');
  await assert.rejects(f.mints.begin(f.secret), /already pending/);
});
test('missing previews and legacy HTML-only intents cannot mint again', async () => {
  assert.throws(() => teiaPackage('<html><head></head></html>', { frames:1 }), /required/);
  const f = await ready();
  const i = [...f.intents.docs.values()][0];
  delete i.artifactMimeType;
  await assert.rejects(f.mints.begin(f.secret), /corrected Teia package/);
  await assert.rejects(f.mints.bind(f.secret, { address, signature:'valid' }), /corrected Teia package/);
  assert.equal(i.status, 'ready');
});
test('IPFS packages use the wrapping directory CID, with all three required files', async () => {
  const cid = 'Qm' + 'a'.repeat(44);
  let seen;
  const server = createServer(async (req,res) => {
    let body = ''; for await (const part of req) body += part;
    if (req.url.startsWith('/api/v0/add')) {
      seen = { url:req.url, body };
      res.end(JSON.stringify({ Name:'index.html', Hash:'Qm' + 'b'.repeat(44) }) + '\n' + JSON.stringify({ Name:'', Hash:cid }) + '\n');
    } else res.end('{}');
  });
  await new Promise(resolve => server.listen(0,'127.0.0.1',resolve));
  const old = { api:process.env.IPFS_API_URL, seed:process.env.IPFS_SEEDER_URL };
  process.env.IPFS_API_URL = process.env.IPFS_SEEDER_URL = 'http://127.0.0.1:' + server.address().port;
  try {
    const { files } = teiaPackage('<html><head></head></html>', { gif:Buffer.from('gif'), thumbnail:Buffer.from('png'), frames:48 });
    assert.equal(await pinMintDirectory(files), 'ipfs://' + cid);
    assert.match(seen.url, /wrap-with-directory=true/);
    for (const name of ['index.html','cover.gif','thumbnail.png']) assert.ok(seen.body.includes(`filename="${name}"`));
    await assert.rejects(pinMintDirectory(files.slice(0,2)), /Incomplete/);
  } finally {
    for (const [key,value] of [['IPFS_API_URL',old.api],['IPFS_SEEDER_URL',old.seed]]) if (value === undefined) delete process.env[key]; else process.env[key] = value;
    await new Promise(resolve => server.close(resolve));
  }
});
test('lost callbacks recover the unique applied mint and wait for chain confirmations and token transfer', async () => {
  const f = await requested();
  assert.equal((await f.mints.confirm(f.secret)).status, 'requested');
  f.state.txs = [f.tx]; f.state.head.level = 101;
  assert.equal((await f.mints.confirm(f.secret)).status, 'confirming');
  f.state.head.level = 102;
  assert.equal((await f.mints.confirm(f.secret)).status, 'confirming');
  f.state.transfers = [f.transfer];
  const result = await f.mints.confirm(f.secret);
  assert.equal(result.status, 'minted'); assert.equal(result.tokenId, '900001'); assert.equal(result.operationHash, operationHash);
  assert.deepEqual(await f.mints.confirm(f.secret), result);
});
test('backtracking, another wallet or artwork, wrong contract and internal mints cannot produce success', async () => {
  const f = await requested();
  for (const patch of [{ status:'backtracked' }, { sender:{ address:other } }, { target:{ address:HEN_OBJKTS } }, { nonce:0 }, { amount:1 },
    { parameter:{ ...f.tx.parameter, value:{ ...f.tx.parameter.value, metadata:'abcd' } } },
    { parameter:{ ...f.tx.parameter, value:{ ...f.tx.parameter.value, amount:'2' } } }]) {
    assert.equal(Boolean(matchingMint(f.i, { ...f.tx, ...patch })), false);
    f.state.txs = [{ ...f.tx, ...patch }];
    await assert.rejects(f.mints.confirm(f.secret, operationHash), /not an applied mint/);
  }
  f.state.txs = [f.tx]; f.state.transfers = [{ ...f.transfer, to:{ address:other } }];
  assert.equal((await f.mints.confirm(f.secret)).status, 'confirming');
  f.state.head.chainId = 'other'; await assert.rejects(f.mints.confirm(f.secret), /syncing/);
});
test('API create requires AC auth and pilot access; malformed capability stays denied', async () => {
  const f = await fixture();
  const event = { httpMethod:'POST', body:JSON.stringify({ ...f.input, action:'create' }), headers:{} };
  const handler = options => createHandler({ mints:f.mints, authorize:async () => ({ sub:'user' }), handleFor:async () => '@jeffrey', pilot:true, ...options });
  assert.equal((await handler({ authorize:async () => null })(event)).statusCode, 401);
  assert.equal((await handler({ pilot:false })(event)).statusCode, 403);
  assert.equal((await handler({ handleFor:async () => '@other' })(event)).statusCode, 403);
  assert.equal((await handler({})(event)).statusCode, 200);
  assert.equal((await handler({})({ ...event, body:JSON.stringify({ action:'status' }) })).statusCode, 401);
});
test('cover validation strips metadata, bounds dimensions, and rejects HTML and oversized input', async () => {
  const input = await sharp({ create:{ width:1000, height:500, channels:3, background:'red' } }).png().toBuffer();
  const png = await normalizeMintCover(input.toString('base64'));
  const info = await sharp(png).metadata(); assert.equal(info.width, 768); assert.equal(info.height, 384); assert.equal(info.exif, undefined);
  await assert.rejects(normalizeMintCover(Buffer.from('<script>bad</script>').toString('base64')), /could not be read/);
  await assert.rejects(normalizeMintCover('x'.repeat(1_400_001)), /Invalid/);
});
test('discard closes an unsigned preview, but never clears a pending wallet request', async () => {
  const f = await ready();
  assert.equal((await f.mints.cancel(f.secret)).status, 'cancelled');
  await assert.rejects(f.mints.begin(f.secret), /pending/);
  const pending = await requested();
  await assert.rejects(pending.mints.cancel(pending.secret), /wallet request/);
  assert.equal((await pending.mints.status(pending.secret)).status, 'requested');
});
