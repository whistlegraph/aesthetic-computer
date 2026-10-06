import test from 'node:test';
import assert from 'node:assert/strict';
import {createHash} from 'node:crypto';
import {SourceEditor} from '../Resources/Web/source-editor.mjs';
import {PieceVersions} from '../Resources/Web/piece-versions.mjs';
import {sourceChecks} from '../../../aesel/src/edit-contract.mjs';

const hash = async source => createHash('sha256').update(source).digest('hex');
const base = 'export function paint({wipe}) { wipe("navy"); }';
const edited = 'export function paint({wipe}) { wipe("pink"); }';

function fixture(options = {}) {
  const values = new Map();
  const storage = {getItem: key => values.get(key), setItem: (key, value) => values.set(key, value)};
  const ledger = new PieceVersions(storage, 'versions', options.source ?? base);
  const state = {piece: 'piece-a', code: 'wgSource', busy: false, recording: false};
  let clock = 0, proof, preview = ledger.head.source, requestID = 0, restores = 0, finishes = 0;
  const readState = () => ({...state, version: ledger.head.id, source: ledger.head.source});
  const editor = new SourceEditor({
    state: readState, checks: sourceChecks, hash,
    begin() { state.busy = true; },
    render(source) {
      preview = source;
      proof = {sourceHash: createHash('sha256').update(source).digest('hex'), requestID: ++requestID,
        rendered: true, logs: []};
      options.onRender?.(proof);
      return requestID;
    },
    inspect: () => proof,
    commit: command => ledger.commit(command),
    restore() { preview = ledger.head.source; restores++; },
    finish() { state.busy = false; finishes++; },
    now: () => clock,
    wait: async ms => { clock += ms; await options.onWait?.({proof, clock, ledger, state, editor}); },
    timeout: 200, settle: 50,
  });
  return {editor, ledger, storage, state, get preview() { return preview; }, get restores() { return restores; },
    get finishes() { return finishes; }, get requestID() { return requestID; }};
}

test('reads complete source beyond ticker limit and applies a durable child with no inference dependency', async () => {
  const source = base + '\n//' + 'whole source '.repeat(2000) + '\n// final sentinel';
  const f = fixture({source});
  const doc = await f.editor.read();
  assert.equal(doc.source, source);
  assert.ok(doc.source.length > 6000);
  const saved = await f.editor.apply({...doc, source: edited});
  assert.equal(saved.version, 1);
  assert.equal(saved.sourceHash, await hash(edited));
  assert.equal(f.ledger.head.parent, 0);
  assert.equal(f.ledger.value.versions[0].source, source);
  assert.equal(new PieceVersions(f.storage, 'versions').head.source, edited);
  assert.equal(f.preview, edited);
  assert.equal(f.state.busy, false);
  assert.equal(f.restores, 0);
});

test('edits the selected historical version without deleting later branches', async () => {
  const f = fixture();
  f.ledger.commit({source: edited, request: 'Earlier edit'});
  f.ledger.checkout(0);
  const saved = await f.editor.apply({...await f.editor.read(), source: base + '\n// branch'});
  assert.equal(saved.version, 2);
  assert.equal(f.ledger.head.parent, 0);
  assert.equal(f.ledger.value.versions[1].source, edited);
});

for (const [name, source, message] of [
  ['syntax', 'export function paint(', /complete JavaScript module/],
  ['API misuse', 'export function paint({ink}) { ink.box(1,2,3); }', /standalone drawing function/],
  ['empty source', ' ', /complete JavaScript piece/],
  ['oversize edit', '//'+ 'x'.repeat(499998), /500 KB/],
]) test(`rejects ${name} before rendering or changing history`, async () => {
  const f = fixture();
  await assert.rejects(f.editor.apply({...await f.editor.read(), source}), message);
  assert.equal(f.ledger.head.id, 0);
  assert.equal(f.requestID, 0);
  assert.equal(f.state.busy, false);
});

test('an empty starter can be read and replaced with a complete piece', async () => {
  const f = fixture({source: ''});
  const doc = await f.editor.read();
  assert.equal(doc.source, '');
  await f.editor.apply({...doc, source: edited});
  assert.equal(f.ledger.head.source, edited);
});

test('identical source does not create a redundant version', async () => {
  const f = fixture();
  const result = await f.editor.apply(await f.editor.read());
  assert.equal(result.changed, false);
  assert.equal(f.requestID, 0);
  assert.equal(f.ledger.value.versions.length, 1);
});

for (const field of ['piece', 'version', 'sourceHash']) test(`rejects stale ${field}`, async () => {
  const f = fixture(), doc = await f.editor.read();
  await assert.rejects(f.editor.apply({...doc, [field]: field === 'version' ? 99 : 'stale', source: edited}), /selected version changed/);
  assert.equal(f.requestID, 0);
});

for (const flag of ['busy', 'recording']) test(`rejects source application while ${flag}`, async () => {
  const f = fixture(), doc = await f.editor.read();
  f.state[flag] = true;
  await assert.rejects(f.editor.apply({...doc, source: edited}), /current request or recording/);
  assert.equal(f.requestID, 0);
});

for (const [name, change, message] of [
  ['runtime error', proof => proof.logs.push({level: 'error', text: 'Paint failure: broken'}), /Paint failure/],
  ['stale source hash', proof => proof.sourceHash = 'old', /does not match/],
  ['stale render request', proof => proof.requestID = -1, /preview changed/],
  ['unpainted timeout', proof => proof.rendered = false, /did not render/],
  ['cancel', proof => proof.cancelled = true, /stopped/],
]) test(`restores previous version after ${name}`, async () => {
  const f = fixture({onRender: change});
  await assert.rejects(f.editor.apply({...await f.editor.read(), source: edited}), message);
  assert.equal(f.preview, base);
  assert.equal(f.ledger.head.id, 0);
  assert.equal(f.restores, 1);
  assert.equal(f.finishes, 1);
  assert.equal(f.state.busy, false);
});

test('waits after first paint and rejects a delayed runtime failure', async () => {
  const f = fixture({onWait({clock, proof}) { if (clock === 25) proof.runtimeFailed = true; }});
  await assert.rejects(f.editor.apply({...await f.editor.read(), source: edited}), /could not run/);
  assert.equal(f.ledger.head.id, 0);
  assert.equal(f.preview, base);
});

test('a failed durable write restores preview and leaves all saved versions intact', async () => {
  const f = fixture(), doc = await f.editor.read();
  f.storage.setItem = () => { throw Error('Storage full'); };
  await assert.rejects(f.editor.apply({...doc, source: edited}), /Storage full/);
  assert.equal(f.ledger.head.id, 0);
  assert.equal(f.preview, base);
  assert.equal(f.state.busy, false);
});

test('stale completion cannot overwrite a newly selected version', async () => {
  const f = fixture({onWait({clock, ledger}) {
    if (clock === 25) ledger.commit({source: base + '\n// external update'});
  }});
  await assert.rejects(f.editor.apply({...await f.editor.read(), source: edited}), /selected version changed/);
  assert.equal(f.ledger.head.source, base + '\n// external update');
  assert.equal(f.preview, f.ledger.head.source);
});

test('duplicate application is rejected while the first preview is being checked', async () => {
  let duplicateChecked = false, doc;
  const f = fixture({async onWait({editor}) {
    if (duplicateChecked) return;
    duplicateChecked = true;
    await assert.rejects(editor.apply({...doc, source: edited}), /already being checked/);
  }});
  doc = await f.editor.read();
  await f.editor.apply({...doc, source: edited});
  assert.equal(f.ledger.value.versions.length, 2);
  assert.ok(duplicateChecked);
});
