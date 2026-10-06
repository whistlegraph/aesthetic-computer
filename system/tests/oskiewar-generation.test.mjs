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
stores.set('@handles', new Map([['auth0|fixture', { _id: 'auth0|fixture', handle: 'fixture' }],
  ['auth0|other', { _id: 'auth0|other', handle: 'other' }]]));
function collection(name) {
  if (!stores.has(name)) stores.set(name, new Map());
  const rows = stores.get(name);
  return {
    createIndex: async () => {}, findOne: async q => [...rows.values()].find(row => Object.entries(q).every(([key,value]) => row[key] === value)),
    deleteOne: async q => { const row = rows.get(q._id); if (row && (!q['fighter.hash'] || row.fighter?.hash === q['fighter.hash'])) rows.delete(q._id); },
    deleteMany: async q => { for (const [id, row] of rows) if (row.owner === q.owner) rows.delete(id); },
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
const aliases = new Map();
mock.module('../backend/authorization.mjs', { exports: {
  handleFor: async sub => aliases.get(sub),
  userEmailFromID: async sub => ({ email: sub === 'auth0|other' ? 'other@example.test' : 'fixture@example.test', email_verified: true }),
  authorize: async headers => headers?.authorization ? { sub: headers.authorization === 'Bearer other' ? 'auth0|other' : headers.authorization === 'Bearer otp' ? 'email|fixture' : 'auth0|fixture' } : null,
} });
mock.module('../backend/account-lock.mjs', { exports: { accountLocked: async () => false } });
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
    if(url.endsWith('/resume')) {
      assert.deepEqual(JSON.parse(options.body), { subject, receipt: 'a'.repeat(64) });
      return revoked ? Response.json({}, {status:403}) : Response.json({capability:{jws:token()}});
    }
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
    assert.equal((await handler({...event(),headers:{authorization:'Bearer other'}})).statusCode,403, 'a capability cannot be used by another AC account');
    const profile = stores.get('@handles').get('auth0|fixture');
    stores.get('@handles').delete('auth0|fixture');
    assert.equal((await handler(event())).statusCode,409, 'choose a handle before generation');
    stores.get('@handles').set('auth0|fixture',profile);
    assert.equal(mediaCalls,0); assert.equal(modelCalls,0);
    const generated=await handler(event()); assert.equal(generated.statusCode,200);
    const fighter = JSON.parse(generated.body).fighter;
    assert.equal(fighter.appearance.hairStyle,'short');
    assert.equal(JSON.parse(generated.body).handle, '@fixture');
    const accept = event('accept');
    accept.body = JSON.stringify({ ...JSON.parse(accept.body), handle: '@other', fighter: { damage: 999 } });
    const accepted = await handler(accept);
    assert.equal(accepted.statusCode, 200);
    assert.equal(JSON.parse(accepted.body).status, 'accepted');
    assert.deepEqual(JSON.parse(accepted.body).fighter, fighter);
    const savedUntil = JSON.parse(accepted.body).savedUntil;
    assert.equal(JSON.parse((await handler(event('accept'))).body).savedUntil, savedUntil, 'repeat acceptance does not extend retention');
    const accountEvent = {httpMethod:'POST', headers:{authorization:'Bearer fixture'}, body:JSON.stringify({action:'account'})};
    assert.equal(JSON.parse((await handler({...accountEvent,headers:{authorization:'Bearer other'}})).body).status, 'empty', 'another account cannot recover this fighter');
    stores.get('@handles').get('auth0|fixture').handle = 'renamed';
    const restored = JSON.parse((await handler(accountEvent)).body);
    assert.equal(restored.handle, '@renamed');
    assert.equal(restored.fighter.hash, fighter.hash, 'handle rename preserves ownership');
    assert.equal(modelCalls, 1, 'restoring an accepted fighter never calls the model');
    aliases.set('email|fixture', 'renamed');
    const aliasRestored = JSON.parse((await handler({...accountEvent, headers:{authorization:'Bearer otp'}})).body);
    assert.equal(aliasRestored.handle, '@renamed', 'handle lookup matches the sign-in shell');
    assert.equal(aliasRestored.fighter.hash, fighter.hash, 'alias retains the same consent owner');
    assert.equal((await handler({...event('status'), headers:{authorization:'Bearer otp'}})).statusCode, 200, 'generation status shares the same consent identity');
    aliases.set('auth0|other', 'renamed');
    const otherProfile = stores.get('@handles').get('auth0|other');
    stores.get('@handles').delete('auth0|other');
    assert.equal((await handler({...accountEvent,headers:{authorization:'Bearer other'}})).statusCode, 403, 'a shared handle label never shares a fighter');
    stores.get('@handles').set('auth0|fixture', profile);
    stores.get('@handles').set('auth0|other', otherProfile);
    aliases.clear();
    delete process.env.OPENAI_API_KEY;
    assert.equal((await handler(accountEvent)).statusCode, 200, 'recovery needs no model credentials');
    process.env.OPENAI_API_KEY = 'fixture';
    stores.get('@handles').get('auth0|fixture').handle = 'fixture';
    assert.equal((await handler(event())).statusCode,200); assert.equal(modelCalls,1);
    stores.get('oskiewar-fighters').get(subject).expiresAt = new Date(1);
    assert.equal(JSON.parse((await handler(accountEvent)).body).status, 'empty', 'expired saved fighter is unavailable before TTL cleanup');
    assert.equal((await handler(event('accept'))).statusCode,200);
    revoked=true; assert.equal((await handler(event('status'))).statusCode,403); assert.equal(modelCalls,1);
    assert.equal(JSON.parse((await handler(accountEvent)).body).status, 'empty');
    assert.equal(stores.get('oskiewar-fighters').size, 0, 'revoked fighter is removed');
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
  const selected={__oskiewarLocalPractice:true,__oskiewarFighterAppearance:{appearance,validUntil:Date.now()+60000}};
  assert.equal(read(selected,null,false,()=>false,()=>false,'GAME')({pad:0}),appearance);
  for(const [net,viewer,versus,survival,mode] of [[{},false,false,false,'GAME'],[null,true,false,false,'GAME'],[null,false,true,false,'GAME'],[null,false,false,true,'GAME'],[null,false,false,false,'MENU']])
    assert.equal(read(selected,net,viewer,()=>versus,()=>survival,mode)({pad:0}),null);
  assert.equal(read(selected,null,false,()=>false,()=>false,'GAME')({pad:1}),null);
  selected.__oskiewarLocalPractice=false;
  assert.equal(read(selected,null,false,()=>false,()=>false,'GAME')({pad:0}),null,'online park never uses the private fighter');
  selected.__oskiewarLocalPractice=true;
  selected.__oskiewarFighterAppearance.validUntil=1;
  assert.equal(read(selected,null,false,()=>false,()=>false,'GAME')({pad:0}),null);
});

test('withdrawal uses the signed-in subject and works without a generation token or model key', async () => {
  const saved={...process.env}, originalFetch=globalThis.fetch;
  Object.assign(process.env,{REGARDE_GATEWAY_URL:'https://gate.invalid/v0/gateway',REGARDE_SUBJECT_SALT:'fixture',REGARDE_GATEWAY_TOKEN:'deployer'});
  delete process.env.OPENAI_API_KEY;
  const sent=[];
  globalThis.fetch=async(url,options)=>{assert.equal(String(url),'https://gate.invalid/v0/withdraw');sent.push(JSON.parse(options.body));return Response.json({outcome:'withdrawn'});};
  try {
    const result=await handler({httpMethod:'POST',headers:{authorization:'Bearer fixture'},body:JSON.stringify({action:'withdraw',subject:'another-person'})});
    assert.equal(result.statusCode,200);
    assert.equal(sent[0].subject,pseudonym('auth0|fixture','fixture'));
    assert.equal(sent[0].frozen_fields.operation_kind,'WITHDRAW_CONSENT');
    aliases.set('email|fixture','fixture');
    const linked=await handler({httpMethod:'POST',headers:{authorization:'Bearer otp'},body:JSON.stringify({action:'withdraw'})});
    assert.equal(linked.statusCode,200);
    assert.deepEqual(sent.slice(1).map(r=>r.subject),['auth0|fixture','email|fixture'].map(sub=>pseudonym(sub,'fixture')),'withdrawal covers both the handle owner and legacy email-code material');
  }finally{aliases.clear();globalThis.fetch=originalFetch;for(const key of ['REGARDE_GATEWAY_URL','REGARDE_SUBJECT_SALT','REGARDE_GATEWAY_TOKEN','OPENAI_API_KEY']){if(saved[key]===undefined)delete process.env[key];else process.env[key]=saved[key];}}
});

test('shared character follows the game bones without mutating physics or restoring missing limbs', async () => {
  const { fighterParts, poseFighter, previewPose } = await import('../../xbox/live/oskiewar-fighter.mjs');
  const a = validateFighter({version:1,recipe:RECIPE,hash:'a'.repeat(64),appearance});
  const model = fighterParts(a), world = previewPose(), before = structuredClone(world);
  // Combat changes role prefixes while preserving the anatomical part.
  world.segments.find(b=>b.role==='left-thigh').role='lead-thigh';
  world.segments.find(b=>b.role==='right-thigh').role='rear-thigh';
  world.segments.find(b=>b.role==='left-upper-arm').role='attack-upper-arm';
  before.segments = structuredClone(world.segments);
  const instances = poseFighter(model, world);
  assert.equal(instances.length, 20);
  assert.equal(instances.find(i=>i.name==='head').faces,model.parts.head);
  assert.deepEqual(world,before);
  const missing = poseFighter(model,world,{headless:true,hasPart:part=>part!=='left-arm'});
  assert.equal(missing.length,15);
  assert.ok(!missing.some(i=>i.name==='head'));
  const arm = world.segments.find(b=>b.role==='right-forearm');
  Object.assign(arm,{x2:arm.x1,y2:arm.y1,z2:arm.z1+32});
  const posed = poseFighter(model,world,{yaw:0});
  for(const {axes,faces,origin} of posed) {
    assert.ok(axes.every(v=>Object.values(v).every(Number.isFinite)));
    assert.ok(faces.every(f=>f.points.flat().every(Number.isFinite)));
    assert.ok([origin.x,origin.y,origin.z].every(Number.isFinite));
  }
  const rightForearm = posed.filter(i=>i.name==='forearm')[1];
  assert.deepEqual(rightForearm.axes[1],{x:0,y:0,z:32});
  assert.ok(Math.hypot(...Object.values(rightForearm.axes[0]))>.99);
  assert.equal(fighterParts(a),model,'geometry is cached across animation frames');
});
