import assert from 'node:assert/strict';
import test from 'node:test';
import {once} from 'node:events';
import {spawnBridge} from '../src/bridge-process.mjs';

test('bridge transports queued input and drains both output streams before closing',async()=>{
  const child=spawnBridge(process.execPath,['-e',
    "process.stdin.on('data',d=>process.stdout.write(d));process.stdin.on('end',()=>process.stderr.write('done'));"],
    {stdio:['pipe','pipe','pipe']});
  const out=[],err=[];
  child.stdout.on('data',chunk=>out.push(chunk));
  child.stderr.on('data',chunk=>err.push(chunk));
  const closed=once(child,'close');
  const data=Buffer.alloc(1024*1024,65);
  child.stdin.end(data);
  assert.equal((await closed)[0],0);
  assert.deepEqual(Buffer.concat(out),data);
  assert.equal(Buffer.concat(err).toString(),'done');
});

test('a process killed before it starts cannot remain running',async()=>{
  const child=spawnBridge(process.execPath,['-e','setInterval(()=>{},1000)'],{stdio:['pipe','pipe','pipe']});
  const closed=once(child,'close');
  child.kill('SIGTERM');
  const [code,signal]=await closed;
  assert.ok(signal==='SIGTERM'||code!==0);
  if(child.pid)assert.throws(()=>process.kill(child.pid,0));
});

test('a missing executable reports an error and rejects pending writes',async()=>{
  const child=spawnBridge('/does-not-exist/aesel-test',[],{stdio:['pipe','pipe','pipe']});
  child.stdin.on('error',()=>{});
  const failed=new Promise(resolve=>child.once('error',resolve));
  const writeFailed=new Promise(resolve=>child.stdin.write('request',error=>resolve(error)));
  assert.equal((await failed).code,'ENOENT');
  assert.ok(await writeFailed);
});
