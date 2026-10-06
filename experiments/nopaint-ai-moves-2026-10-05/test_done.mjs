import test from 'node:test';
import assert from 'node:assert/strict';
import {mkdtemp, mkdir, readFile, writeFile, rm} from 'node:fs/promises';
import {tmpdir} from 'node:os';
import {join} from 'node:path';
import {randomUUID, createHash} from 'node:crypto';
import {finishPainting} from './done.mjs';
import {createPaintingWipService} from '../../system/backend/painting-wips.mjs';
import {memoryWipRepository} from '../../system/tests/painting-wip-fixture.mjs';
import {JSZip} from '../../aesel/media/picture/ac-tools.mjs';
const digest = b=>createHash('sha256').update(b).digest('hex');
async function setup(t) {
  const root=await mkdtemp(join(tmpdir(),'nopaint-done-'));t.after(()=>rm(root,{recursive:true,force:true}));
  const history=[];
  for(const name of ['blank','base']) {
    const path=new URL(`./starts/${name}.png`,import.meta.url).pathname;
    history.push({path,sha256:digest(await readFile(path))});
  }
  async function operation() {
    const id=randomUUID(),folder=join(root,id);await mkdir(folder);
    await writeFile(join(folder,'manifest.json'),JSON.stringify({id,account_id:'owner',createdAt:'2026-10-06T00:00:00Z',history}));
    return folder;
  }
  const repo=memoryWipRepository(),service=createPaintingWipService(repo),user={sub:'auth0|owner'};
  const objects=new Map(),calls=[];let lost=false,loseTrack=false;
  const fetch=async(url,opts={})=>{
    url=String(url);calls.push({url,opts});
    if(new URL(url).host==='ac.example' && (opts.method==='POST'||url.includes('/presigned-upload-url/')))assert.equal(opts.headers.Authorization,'Bearer private-test-token');
    if(url.endsWith('/api/painting-wip')){const b=JSON.parse(opts.body);return Response.json(await service[b.action](b,user));}
    if(url.includes('/presigned-upload-url/')){const name=url.split('/').at(-2);return Response.json({slug:'auth0|owner/'+name,uploadURL:'https://storage.example/'+name+'?signature=private'});}
    if(opts.method==='PUT'){assert(!opts.headers.Authorization);objects.set(url.split('?')[0],Buffer.from(opts.body));return new Response('');}
    if(url.endsWith('/api/track-media')){
      const b=JSON.parse(opts.body),value=await service.seal(b.wip,user,b.slug);
      if(loseTrack&&!lost){lost=true;throw Error('lost response');}return Response.json(value);
    }
    if(url.includes('/api/painting-code?')){const code=new URL(url).searchParams.get('code'),p=await repo.find({code});return Response.json({code,slug:p.slug,handle:'tester'});}
    if(url.includes('/media/paintings/')){const code=url.split('/').at(-1).slice(0,-4),p=await repo.find({code});return new Response(objects.get('https://storage.example/'+p.slug.split('/').at(-1)+'.png'));}
    if(url.startsWith('https://storage.example/'))return new Response(objects.get(url));
    throw Error('Unexpected mock route');
  };
  const finish=folder=>finishPainting({folder,accountId:'owner',handle:'tester',token:'private-test-token',fetch,site:'https://ac.example'});
  return {operation,finish,repo,objects,calls,history,lose:()=>{loseTrack=true;}};
}
test('each Done publishes a distinct painting, its accepted pixels, and all accepted steps',async t=>{
  const api=await setup(t),first=await api.operation(),a=await api.finish(first);
  assert(a.verified);assert.equal(a.code,'test');
  const bytes=await readFile(api.history.at(-1).path),png=[...api.objects].find(([url])=>url.endsWith('.png'))[1];assert.deepEqual(png,bytes);
  const zip=await JSZip.loadAsync([...api.objects].find(([url])=>url.endsWith('.zip'))[1]);
  assert.equal(JSON.parse(await zip.file('painting.json').async('string')).length,2);
  const before=api.objects.size;await api.finish(first);assert.equal(api.objects.size,before);
  const b=await api.finish(await api.operation());assert.notEqual(a.code,b.code);assert.equal(api.repo.paintings.size,2);
  const manifest=JSON.parse(await readFile(join(first,'manifest.json')));
  const receipt=await readFile(join(first,'publications',manifest.id,'v2.json'),'utf8');
  assert(!receipt.includes('private-test-token'));assert(!receipt.includes('signature=private'));
});
test('lost seal response retries same code without uploading again',async t=>{
  const api=await setup(t),folder=await api.operation();api.lose();
  await assert.rejects(api.finish(folder),/lost response/);
  const puts=api.calls.filter(c=>c.opts.method==='PUT').length;
  const done=await api.finish(folder);assert(done.verified);assert.equal(api.repo.paintings.size,1);
  assert.equal(api.calls.filter(c=>c.opts.method==='PUT').length,puts);
});
test('account mismatch stops before any network request',async t=>{
  const api=await setup(t),folder=await api.operation();
  await assert.rejects(finishPainting({folder,accountId:'other',fetch:()=>{throw Error('network called');}}),/account that started/);
  assert.equal(api.calls.length,0);
});
