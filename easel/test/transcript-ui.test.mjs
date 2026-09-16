import test from 'node:test';
import assert from 'node:assert/strict';
import {mkdtemp,rm} from 'node:fs/promises';
import {join} from 'node:path';
import {tmpdir} from 'node:os';
import {TranscriptJournal} from '../src/transcript-journal.mjs';
import {transcriptMessages} from '../src/transcript-ui.mjs';
import {parseTranscript} from '../src/transcript-format.mjs';
test('UI transcript includes only conversational text with stable safe IDs',()=>{
 const entry={id:'entry-123-0.12345',kind:'assistant',text:'Final answer',token:'DO-NOT-COPY',providerState:{secret:'SECRET'}};
 const a=transcriptMessages(entry,{backend:'ac',model:'glm'});
 assert.equal(a[0].id,transcriptMessages(entry,{backend:'codex',model:'other'})[0].id);
 assert.equal(a[0].role,'assistant');assert.doesNotMatch(JSON.stringify(a),/DO-NOT-COPY|SECRET|providerState|token/);
 for(const kind of ['command','change','notice','error'])assert.deepEqual(transcriptMessages({...entry,kind}),[]);
});
test('long messages preserve complete Unicode content in bounded transcript records',()=>{
 const text='🟣'.repeat(30000);const records=transcriptMessages({id:'large',kind:'assistant',text});
 assert.equal(records.map(r=>r.text).join(''),text);assert.ok(records.every(r=>r.text.length<=32768));
 assert.equal(new Set(records.map(r=>r.id)).size,records.length);
});
test('final messages deduplicate across journal reopen and backend switches without upload',async t=>{
 const root=await mkdtemp(join(tmpdir(),'easel-ui-journal-'));t.after(()=>rm(root,{recursive:true,force:true}));
 let requests=0;const fetch=async()=>{requests++;throw new Error('no development uploads');};
 const first=new TranscriptJournal({root,fetch});await first.init();
 const a=transcriptMessages({id:'user.1',kind:'user',text:'keep dots purple'},{backend:'ac',model:'glm'})[0];
 await first.append(a);await first.append(a);
 const reopened=new TranscriptJournal({root,id:first.header.id,fetch});await reopened.init();await reopened.append(a);
 await reopened.append(transcriptMessages({id:'assistant.2',kind:'assistant',text:'done'},{backend:'codex',model:'new'})[0]);
 const parsed=parseTranscript(await reopened.export({redact:true}));
 assert.equal(parsed.records.length,2);assert.deepEqual(parsed.records.map(r=>r.backend),['ac','codex']);
 await reopened.flush();assert.equal(requests,0);assert.equal((await reopened.status()).sharing,false);
});
