import assert from 'node:assert/strict';
import test, { mock } from 'node:test';
import { generateKeyPairSync, sign, createHash } from 'node:crypto';
import sharp from 'sharp';
import { verifyGenerationCapability, practiceScopeHash, generateAppearance, validateAppearance, RECIPE } from '../backend/oskiewar-generation.mjs';
import { fighterMesh, validateFighter } from '../../xbox/live/oskiewar-fighter.mjs';
const { privateKey, publicKey } = generateKeyPairSync('ed25519');
const jwks = { keys: [{ ...publicKey.export({ format: 'jwk' }), kid: 'test-key' }] };
let subject = 'subject';
const token = (patch = {}) => {
  const payload = { typ: 'regarde-generation-capability', v: 1, sub: subject, purpose: 'oskiewar_fighter_generation',
    sources: ['appearance'], outputs: ['fighter_mesh'], receipt: 'a'.repeat(64), scope_hash: practiceScopeHash(['appearance']),
    iat: Math.floor(Date.now()/1000), exp: Math.floor(Date.now()/1000)+600, jti: 'one-job', ...patch };
  const data = [ { alg: 'EdDSA', kid: 'test-key' }, payload ].map(v => Buffer.from(JSON.stringify(v)).toString('base64url')).join('.');
  return data + '.' + sign(null, Buffer.from(data), privateKey).toString('base64url');
};
const appearance = { skin: '#d7a079', hair: '#392819', shirt: '#446688', pants: '#223344', shoes: '#eeeeee', hairStyle: 'short', beard: true, glasses: true, sleeves: 'short' };
test('generation requires a valid, live subject/output/distribution-bound capability', () => {
  assert.equal(verifyGenerationCapability(token(), jwks, subject).sub, subject);
  for (const patch of [{ sub: 'someone-else' }, { exp: 1 }, { outputs: ['portrait'] }, { sources: ['voice'] }, { scope_hash: 'sha256:'+'b'.repeat(64) }])
    assert.throws(() => verifyGenerationCapability(token(patch), jwks, subject));
  const tampered = token().split('.'); tampered[1] = Buffer.from('{}').toString('base64url');
  assert.throws(() => verifyGenerationCapability(tampered.join('.'), jwks, subject));
});
test('malformed pictures never reach the model; appearance has no programmable code or combat values', async () => {
  let calls = 0;
  await assert.rejects(generateAppearance(Buffer.from('bad image'), { key: 'fixture', fetchImpl: async () => { calls++; } }), /readable/);
  assert.equal(calls, 0);
  assert.deepEqual(validateAppearance({ ...appearance, damage: 999, javascript: 'bad' }), appearance);
  assert.throws(() => validateAppearance({ ...appearance, hair: 'url(secret)' }));
  const a = validateFighter({ version: 1, recipe: RECIPE, hash: 'a'.repeat(64), appearance });
  assert.ok(fighterMesh(a).every(face => face.points.flat().every(Number.isFinite)));
});
const stores = new Map();
function collection(name) {
  if (!stores.has(name)) stores.set(name, new Map());
  const rows = stores.get(name);
  return {
    createIndex: async () => {}, findOne: async q => rows.get(q._id),
    insertOne: async row => { if(rows.has(row._id)) throw Object.assign(Error('duplicate'),{code:11000}); rows.set(row._id, row); },
    updateOne: async (q, update, options={}) => {
      let row = rows.get(q._id);
      if (!row && options.upsert) { row={_id:q._id,...update.$setOnInsert}; rows.set(q._id,row); }
      if (!row || (q.status && q.status !== row.status) || (q.count && !(row.count < q.count.$lt))) return {modifiedCount:0};
      Object.assign(row,update.$set); for(const [k,v] of Object.entries(update.$inc||{})) row[k]=(row[k]||0)+v;
      for(const k of Object.keys(update.$unset||{})) delete row[k];
      return {modifiedCount:1};
    },
  };
}
mock.module('../backend/authorization.mjs', { exports: { authorize: async headers => headers?.authorization ? { sub: 'auth0|fixture' } : null } });
mock.module('../backend/database.mjs', { exports: { connect: async () => ({ db: { collection } }) } });
const { handler } = await import('../netlify/functions/oskiewar-generation.mjs');
const { pseudonym } = await import('../netlify/functions/oskiewar-consent.mjs');
test('real bridge releases one result, refuses revoked grants, and never regenerates duplicate jobs', async () => {
  const saved = {...process.env}, originalFetch=globalThis.fetch;
  Object.assign(process.env,{REGARDE_GATEWAY_URL:'https://gate.invalid/v0/gateway',REGARDE_SUBJECT_SALT:'fixture',REGARDE_GATEWAY_TOKEN:'deployer',OPENAI_API_KEY:'fixture'});
  subject=pseudonym('auth0|fixture','fixture');
  const photo=await sharp({create:{width:8,height:8,channels:3,background:'#ddaa99'}}).png().toBuffer();
  const hash=createHash('sha256').update(photo).digest('hex');
  let modelCalls=0, mediaCalls=0, revoked=false, revokeDuringModel=false;
  globalThis.fetch=async(url,options)=>{
    if(url.endsWith('jwks.json')) return Response.json(jwks);
    if(url.endsWith('/media')) {
      mediaCalls++; const body=JSON.parse(options.body);
      assert.equal(body.requireSource,'appearance'); assert.equal(body.requireOutput,'fighter_mesh');
      return revoked ? Response.json({}, {status:403}) : new Response(photo,{headers:{'x-regarde-source':'appearance'}});
    }
    assert.equal(url,'https://api.openai.com/v1/responses'); modelCalls++;
    const body=JSON.parse(options.body); assert.equal(body.store,false); assert.equal(body.text.format.strict,true);
    if(revokeDuringModel)revoked=true;
    return Response.json({model:'test-vision',output:[{content:[{type:'output_text',text:JSON.stringify(appearance)}]}]});
  };
  const event=(action='generate',capability=token())=>({httpMethod:'POST',headers:{authorization:'Bearer fixture'},body:JSON.stringify({action,hash,capability})});
  try {
    assert.equal((await handler({...event(),headers:{}})).statusCode,401);
    assert.equal((await handler(event('generate',token({outputs:['portrait']})))).statusCode,403);
    assert.equal(mediaCalls,0); assert.equal(modelCalls,0);
    const generated=await handler(event()); assert.equal(generated.statusCode,200);
    assert.equal(JSON.parse(generated.body).fighter.appearance.hairStyle,'short');
    assert.equal((await handler(event())).statusCode,200); assert.equal(modelCalls,1);
    revoked=true; assert.equal((await handler(event('status'))).statusCode,403); assert.equal(modelCalls,1);
    revoked=false; revokeDuringModel=true;
    assert.equal((await handler(event('generate',token({receipt:'b'.repeat(64)})))).statusCode,403);
    const jobs=[...stores.get('oskiewar-generation-jobs').values()];
    assert.equal(jobs.filter(j=>j.status==='complete').length,1);
    assert.equal(jobs.filter(j=>j.status==='failed' && !j.fighter).length,1);
  } finally { globalThis.fetch=originalFetch; for(const key of ['REGARDE_GATEWAY_URL','REGARDE_SUBJECT_SALT','REGARDE_GATEWAY_TOKEN','OPENAI_API_KEY']) { if(saved[key]===undefined)delete process.env[key];else process.env[key]=saved[key]; } }
});

test('generated appearance is local practice presentation only, and expires', async () => {
  const { readFile } = await import('node:fs/promises');
  const source = await readFile(new URL('../../xbox/live/oskiewar.js', import.meta.url), 'utf8');
  const code = source.slice(source.indexOf('function generatedAppearance('), source.indexOf('function generatedPartColor('));
  const read = Function('globalThis','netSession','roundViewer','versusLane','survivalActive','shellMode', code+';return generatedAppearance;');
  const selected={__oskiewarFighterAppearance:{appearance,validUntil:Date.now()+60000}};
  assert.equal(read(selected,null,false,()=>false,()=>false,'GAME')({pad:0}),appearance);
  for(const [net,viewer,versus,survival,mode] of [[{},false,false,false,'GAME'],[null,true,false,false,'GAME'],[null,false,true,false,'GAME'],[null,false,false,true,'GAME'],[null,false,false,false,'MENU']])
    assert.equal(read(selected,net,viewer,()=>versus,()=>survival,mode)({pad:0}),null);
  assert.equal(read(selected,null,false,()=>false,()=>false,'GAME')({pad:1}),null);
  selected.__oskiewarFighterAppearance.validUntil=1;
  assert.equal(read(selected,null,false,()=>false,()=>false,'GAME')({pad:0}),null);
});

test('withdrawal uses the signed-in subject and works without a generation token or model key', async () => {
  const saved={...process.env}, originalFetch=globalThis.fetch;
  Object.assign(process.env,{REGARDE_GATEWAY_URL:'https://gate.invalid/v0/gateway',REGARDE_SUBJECT_SALT:'fixture',REGARDE_GATEWAY_TOKEN:'deployer'});
  delete process.env.OPENAI_API_KEY;
  let sent;
  globalThis.fetch=async(url,options)=>{assert.equal(url,'https://gate.invalid/v0/withdraw');sent=JSON.parse(options.body);return Response.json({outcome:'withdrawn'});};
  try {
    const result=await handler({httpMethod:'POST',headers:{authorization:'Bearer fixture'},body:JSON.stringify({action:'withdraw',subject:'another-person'})});
    assert.equal(result.statusCode,200);
    assert.equal(sent.subject,pseudonym('auth0|fixture','fixture'));
    assert.equal(sent.frozen_fields.operation_kind,'WITHDRAW_CONSENT');
  }finally{globalThis.fetch=originalFetch;for(const key of ['REGARDE_GATEWAY_URL','REGARDE_SUBJECT_SALT','REGARDE_GATEWAY_TOKEN','OPENAI_API_KEY']){if(saved[key]===undefined)delete process.env[key];else process.env[key]=saved[key];}}
});
