import test from 'node:test';
import assert from 'node:assert/strict';
import {mkdtemp,writeFile,rm} from 'node:fs/promises';
import {join} from 'node:path';
import {tmpdir} from 'node:os';
import {fork} from 'node:child_process';
import {fileURLToPath} from 'node:url';

test('Store terminal bridge carries input, dimensions, and checkpoint signals without a PTY',async t=>{
  const dir=await mkdtemp(join(tmpdir(),'aesel-mas-terminal-'));t.after(()=>rm(dir,{recursive:true,force:true}));
  const shim=join(dir,'port.cjs'),entry=join(dir,'app.mjs');
  await writeFile(shim,"const {EventEmitter}=require('events');process.parentPort=new EventEmitter();process.on('message',data=>process.parentPort.emit('message',{data}));");
  await writeFile(entry,`process.stdin.setRawMode(true);process.stdin.on('data',data=>process.send({input:data.toString(),tty:process.stdin.isTTY,raw:process.stdin.isRaw}));process.stdout.on('resize',()=>process.send({cols:process.stdout.columns,rows:process.stdout.rows}));process.on('SIGUSR2',()=>process.exit(75));process.send({ready:true,entry:process.argv[1]});`);
  const child=fork(fileURLToPath(new URL('../desktop/mas-terminal-worker.cjs',import.meta.url)),[entry],{silent:true,execArgv:['--require',shim],env:{...process.env,EASEL_COLUMNS:'80',EASEL_ROWS:'24'}});
  t.after(()=>child.kill());
  const next=()=>new Promise((resolve,reject)=>{const timer=setTimeout(()=>reject(Error('Worker response timed out')),5000);child.once('message',data=>{clearTimeout(timer);resolve(data);});});
  assert.deepEqual(await next(),{ready:true,entry});
  let received=next();child.send({type:'input',data:'é🫏'});assert.deepEqual(await received,{input:'é🫏',tty:true,raw:true});
  received=next();child.send({type:'resize',cols:120,rows:40});assert.deepEqual(await received,{cols:120,rows:40});
  const exit=new Promise(resolve=>child.once('exit',resolve));child.send({type:'signal',signal:'SIGUSR2'});assert.equal(await exit,75);
});
