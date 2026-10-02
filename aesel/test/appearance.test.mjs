import assert from 'node:assert/strict';
import test from 'node:test';
import {mkdtempSync,writeFileSync,rmSync} from 'node:fs';
import {tmpdir} from 'node:os';
import {join} from 'node:path';
import {Appearance} from '../src/appearance.mjs';

test('cached appearance is immediate while one shared refresh is pending',async t=>{
  const root=mkdtempSync(join(tmpdir(),'aesel-appearance-'));
  t.after(()=>rmSync(root,{recursive:true,force:true}));
  const file=join(root,'appearance');writeFileSync(file,'light\n');
  let calls=0,answer;
  const appearance=new Appearance({file,platform:'darwin',query:()=>{
    calls++;return new Promise(resolve=>{answer=resolve;});
  }});
  assert.equal(appearance.value,'light');
  const first=appearance.refresh(),second=appearance.refresh();
  assert.equal(first,second);
  await Promise.resolve();assert.equal(calls,1);
  assert.equal(appearance.value,'light','an unfinished query does not change the first frame');
  answer('dark');assert.equal(await first,'dark');
  assert.equal(appearance.value,'dark');
});

test('failed appearance queries keep the cached theme',async t=>{
  const root=mkdtempSync(join(tmpdir(),'aesel-appearance-'));
  t.after(()=>rmSync(root,{recursive:true,force:true}));
  const file=join(root,'appearance');writeFileSync(file,'light\n');
  for(const query of [async()=>null,async()=>{throw Error('unavailable');}]){
    const appearance=new Appearance({file,platform:'darwin',query});
    assert.equal(await appearance.refresh(),'light');
  }
  const linux=new Appearance({file,platform:'linux',query:()=>assert.fail('must not query macOS')});
  assert.equal(await linux.refresh(),'dark');
});
