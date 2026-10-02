import test from 'node:test';
import assert from 'node:assert/strict';
import {readFileSync} from 'node:fs';
import {initializeBasePiece, isBasePiece} from '../Resources/Web/base-piece.mjs';
import {PieceVersions} from '../Resources/Web/piece-versions.mjs';
const read = path => readFileSync(new URL('../' + path, import.meta.url), 'utf8');

test('Whistlegraph updates the existing app, Keychain service and WebView origin', () => {
  const project = read('project.yml'), plist = read('Info.plist');
  assert.match(project, /^name: Whistlegraph$/m);
  assert.match(project, /PRODUCT_NAME: Whistlegraph/);
  assert.match(project, /PRODUCT_BUNDLE_IDENTIFIER: computer\.aesthetic\.walkieware\s/);
  assert.match(plist, /<key>CFBundleDisplayName<\/key><string>Whistlegraph<\/string>/);
  assert.match(read('Sources/WhistlegraphAccount.swift'), /kSecAttrService as String: "computer\.aesthetic\.walkieware"/);
  assert.match(read('Sources/WhistlegraphApp.swift'), /walkieware:\/\/app\/index\.html\?walkie=1/);
  assert.match(read('Sources/WhistlegraphApp.swift'), /forURLScheme: "walkieware"/);
  assert.match(read('Resources/Web/shell.html'), /<title>Whistlegraph<\/title>/);
});

test('saved Walkieware work, branch history and cloud identity survive unchanged', () => {
  const base = '// Walkieware v0 — base color.\nexport function paint({wipe}) { wipe("pink"); }';
  const source = 'export function paint({wipe}) { wipe("navy"); }';
  const ledger = {format: 1, head: 1, versions: [
    {id: 0, parent: null, source: base, request: null},
    {id: 1, parent: 0, source, request: 'make it navy'}
  ]};
  const entries = new Map([
    ['walkieware-source', source],
    ['walkieware-source-versions', JSON.stringify(ledger)],
    ['walkieware-source-thread', JSON.stringify({id: 'existing-thread', code: 'wwSaved'})],
    ['walkieware-source-cloud-revision', '7'],
    ['walkieware-archive-older', '{"source":"older piece"}']
  ]);
  const before = [...entries];
  const storage = {getItem: key => entries.get(key) ?? null, setItem: (key, value) => entries.set(key, value), removeItem: key => entries.delete(key)};
  assert.equal(initializeBasePiece(storage, 'walkieware-source'), false);
  assert.equal(isBasePiece(base), true);
  const versions = new PieceVersions(storage, 'walkieware-source-versions');
  assert.equal(versions.head.source, source);
  assert.deepEqual([...entries], before);
  assert.equal(versions.undo().source, base);
  assert.equal(versions.checkout(1).source, source);
  assert.deepEqual(versions.value.versions, ledger.versions);
});
