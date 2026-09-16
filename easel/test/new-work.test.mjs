import test from 'node:test';
import assert from 'node:assert/strict';
import {mkdtemp,readFile,rm,stat} from 'node:fs/promises';
import {tmpdir} from 'node:os';
import {join} from 'node:path';
import {LivePiece} from '../src/live.mjs';
import {replaceWork,archiveThread} from '../src/new-work.mjs';
import {desktopSnapshot,readDesktopSession} from '../src/desktop-session.mjs';
async function fixture(t){const cwd=await mkdtemp(join(tmpdir(),'easel-new-'));t.after(()=>rm(cwd,{recursive:true,force:true}));return cwd;}
test('fresh piece preserves old blank and revisions while replacing name, QR and channel',async t=>{
 const cwd=await fixture(t);const live=new LivePiece({cwd});live.create();await live.checkpoint();
 const file=live.file,source=live.source(),channel=live.channel,url=live.scanUrl,history=await live.history.list();
 await live.fresh();assert.notEqual(live.file,file);assert.notEqual(live.channel,channel);assert.notEqual(live.scanUrl,url);
 assert.equal(await readFile(file,'utf8'),source);assert.equal(live.revision.version,1);assert.equal(live.pristine,true);
 const old=new LivePiece({cwd,slug:file.split('/').at(-1).replace('.mjs','')});assert.deepEqual(await old.history.list(),history);
 live.sending=true;const next=live.file;await assert.rejects(live.fresh(),/upload/);assert.equal(live.file,next);
});
test('failed replacement restores work and never accepts or closes previous thread',async()=>{
 let selected='old',accepted=false,discarded=false;
 await assert.rejects(replaceWork({archive:async()=>'/private/archive',prepare:async()=>{selected='new';},connect:async()=>{throw Error('offline');},restore:async()=>{selected='old';},discard:async()=>{discarded=true;},accept:async()=>{accepted=true;}}),/offline/);
 assert.equal(selected,'old');assert.equal(accepted,false);assert.equal(discarded,true);
 let prepared=false;await assert.rejects(replaceWork({archive:async()=>{throw Error('disk full');},prepare:async()=>{prepared=true;}}),/disk full/);assert.equal(prepared,false);
});
test('private archive retains exact resumable history and draft without credential fields',async t=>{
 const cwd=await fixture(t),live=new LivePiece({cwd});live.create();
 const snapshot=desktopSnapshot({cwd,backend:'ac',model:'test',live,state:{entries:[{id:'u',kind:'user',text:'hello'}],input:'draft',cursor:5,history:[],queued:[]},options:{},engine:{threadId:'thread-previous',messages:[{role:'user',content:'hello'}],token:'SECRET'},handoff:'',archivedConversation:[]});
 const file=await archiveThread(snapshot);const restored=await readDesktopSession(file,cwd);
 assert.deepEqual(restored.engine,snapshot.engine);assert.equal(restored.ui.input,'draft');assert.equal((await stat(file)).mode&0o777,0o600);assert.doesNotMatch(await readFile(file,'utf8'),/SECRET/);
});
