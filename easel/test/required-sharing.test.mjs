import test from 'node:test';import assert from 'node:assert/strict';import {EventEmitter} from 'node:events';import {mkdtemp,rm,writeFile} from 'node:fs/promises';import {tmpdir} from 'node:os';import {join} from 'node:path';
import {createHash} from 'node:crypto';
import {requireSharing,readAcknowledgment,DISCLOSURE_VERSION} from '../src/required-sharing.mjs';
async function fixture(t){const root=await mkdtemp(join(tmpdir(),'easel-policy-'));t.after(()=>rm(root,{recursive:true,force:true}));const input=new EventEmitter();Object.assign(input,{isTTY:true,setRawMode(value){this.isRaw=value},resume(){},pause(){}});let text='';const output={isTTY:true,write(value){text+=value}};const session={read:()=>({user:{sub:'owner-a'}})};return{root,input,output,session,text:()=>text};}
async function waitInput(input){for(let i=0;i<100&&!input.listenerCount('data');i++)await new Promise(r=>setTimeout(r,1));assert(input.listenerCount('data'));}
test('first use shows required disclosure; quitting stores no acceptance',async t=>{const f=await fixture(t);const waiting=requireSharing(f);await waitInput(f.input);assert.match(f.text(),/requires sharing/);assert.match(f.text(),/retained\s+indefinitely\s+until\s+you\s+delete\s+them/);f.input.emit('data',Buffer.from('q'));assert.equal(await waiting,null);assert.equal(await readAcknowledgment(f.root,'owner-a'),null);});
test('only explicit agreement records account-bound version; restart skips and another account does not inherit it',async t=>{const f=await fixture(t);const waiting=requireSharing(f);await waitInput(f.input);f.input.emit('data',Buffer.from('\r'));assert.equal(await readAcknowledgment(f.root,'owner-a'),null);f.input.emit('data',Buffer.from('a'));const receipt=await waiting;assert.equal(receipt.version,DISCLOSURE_VERSION);assert.equal(receipt.owner,'owner-a');assert.deepEqual(await requireSharing(f),receipt);assert.equal(await readAcknowledgment(f.root,'owner-b'),null);});
test('noninteractive runs cannot silently accept disclosure',async t=>{const f=await fixture(t);f.input.isTTY=false;await assert.rejects(requireSharing(f),/interactively/);});

test('an old retention acknowledgment requires agreement to the new disclosure',async t=>{
 const f=await fixture(t),owner='owner-a';
 const file=join(f.root,createHash('sha256').update(owner).digest('hex')+'.json');
 await writeFile(file,JSON.stringify({owner,version:3,acceptedAt:new Date().toISOString()}));
 assert.equal(await readAcknowledgment(f.root,owner),null);
 const waiting=requireSharing(f);await waitInput(f.input);
 assert.match(f.text(),/retained\s+indefinitely/);
 f.input.emit('data',Buffer.from('a'));
 assert.equal((await waiting).version,4);
});
