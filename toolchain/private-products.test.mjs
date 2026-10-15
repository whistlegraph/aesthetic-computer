import test from 'node:test';
import assert from 'node:assert/strict';
import fs from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import { execFileSync } from 'node:child_process';
import { pathToFileURL } from 'node:url';
import { workspace, migrate, linkProducts, checkProducts, productFiles, buildBundle, installBundle, snapshotInstalled, composeSource, verifyCoreRevision } from './private-products.mjs';
import { optionalProduct } from '../shared/private-product.mjs';

function fixture(t) {
  const root = fs.mkdtempSync(path.join(os.tmpdir(), 'ac-private-test-'));
  t.after(() => fs.rmSync(root, { recursive: true, force: true }));
  const put = (p, text) => { fs.mkdirSync(path.dirname(path.join(root, p)), { recursive: true }); fs.writeFileSync(path.join(root, p), text); };
  const git = (args, cwd = root) => execFileSync('git', ['-C', cwd, ...args], { encoding: 'utf8' });
  for (const dir of [root, path.join(root, 'vault')]) {
    fs.mkdirSync(dir, { recursive: true });
    git(['init', '-q'], dir);
    git(['config', 'user.email', 'fixture@example.invalid'], dir);
    git(['config', 'user.name', 'Fixture'], dir);
    git(['config', 'commit.gpgsign', 'false'], dir);
    git(['config', 'core.hooksPath', path.join(root, 'no-hooks')], dir);
  }
  put('.gitignore', '/vault/\n/node_modules/\n');
  put('.gitattributes', '*.mjs text\n');
  put('private-products.json', JSON.stringify({ version: 1, vault: 'vault', products: {
    diary: { include: ['system/diary/**'] }, mime: { include: ['system/mime/**'] },
  }, localArtifacts: ['legacy/**'] }));
  put('system/shared.mjs', 'export const color = "pink";\n');
  put('system/diary/page.mjs', 'export {color} from "../shared.mjs"; export {lines} from "../mime/photo.mjs";\n');
  put('system/mime/photo.mjs', 'export const lines = 5;\n');
  put('unrelated.mjs', 'before\n');
  git(['add', '.']); git(['commit', '-qm', 'fixture baseline']);
  put('system/mime/photo.mjs', 'export const lines = 7;\n');
  put('system/diary/new.mjs', 'export const added = true;\n');
  put('unrelated.mjs', 'user edits\n'); git(['add', 'unrelated.mjs']);
  put('vault/secret.env', 'never include this');
  return { root, put, git, w: workspace(root) };
}

test('migration preserves edits and staged work, resolves shared and sibling imports, and relinks idempotently', async t => {
  const { root, w, git } = fixture(t);
  const receipt = migrate(w);
  assert.equal(receipt.files.length, 3);
  assert.deepEqual(checkProducts(w), { diary: 2, mime: 1 });
  assert.equal(git(['show', ':unrelated.mjs']), 'user edits\n');
  assert.equal(git(['diff', '--cached', '--name-only', '--diff-filter=D']).trim().split('\n').length, 2);
  const page = await import(pathToFileURL(path.join(root, 'system/diary/page.mjs')));
  assert.equal(page.color, 'pink'); assert.equal(page.lines, 7);
  linkProducts(w); linkProducts(w);
  assert.equal(productFiles(w).length, 3);
  assert.equal(fs.lstatSync(path.join(w.vault, 'products/diary/.gitattributes')).isSymbolicLink(), false);
  assert.equal(fs.readFileSync(path.join(w.vault, 'products/diary/.gitattributes'), 'utf8'), '*.mjs text\n');
  assert.equal(git(['check-ignore', 'system/diary/new.mjs']).trim(), 'system/diary/new.mjs');
  git(['add', '-f', 'system/diary/new.mjs']);
  assert.throws(() => checkProducts(w), /public Git index/);
});

test('preflight refuses staged product changes without moving files', t => {
  const { w, git, root } = fixture(t);
  git(['add', 'system/mime/photo.mjs']);
  assert.throws(() => migrate(w), /already staged/);
  assert.equal(fs.lstatSync(path.join(root, 'system/mime/photo.mjs')).isSymbolicLink(), false);
  assert.equal(fs.existsSync(path.join(w.vault, 'products')), false);
});

test('bundle contains only product sources; install verifies bytes, prunes removed sources, and preserves public files', t => {
  const { root, w, put } = fixture(t); migrate(w);
  assert.throws(() => buildBundle(w, path.join(root, 'release.tgz')), /Commit the private/);
  const bundle = path.join(root, 'local.tgz');
  const manifest = buildBundle(w, bundle, { worktree: true });
  assert.equal(manifest.files.length, 3);
  const contents = execFileSync('tar', ['-tzf', bundle], { encoding: 'utf8' });
  assert.doesNotMatch(contents, /secret|shared|\.gitignore|migration|node_modules/);
  const input = path.join(root, 'unpacked'); fs.mkdirSync(input);
  execFileSync('tar', ['-xzf', bundle, '-C', input]);
  put('deployed/private-products.json', fs.readFileSync(path.join(root, 'private-products.json')));
  put('deployed/system/shared.mjs', 'public source');
  const deployed = workspace(path.join(root, 'deployed'));
  installBundle(deployed, input);
  assert.equal(fs.readFileSync(path.join(deployed.root, 'system/mime/photo.mjs'), 'utf8'), 'export const lines = 7;\n');
  assert.equal(fs.readFileSync(path.join(deployed.root, 'system/public/.products-ref'), 'utf8'), 'local-test\n');
  const photo = path.join(input, 'system/mime/photo.mjs'); fs.appendFileSync(photo, 'tampered');
  assert.throws(() => installBundle(deployed, input), /Invalid bundle bytes/);
  fs.writeFileSync(photo, 'export const lines = 7;\n');
  manifest.files = manifest.files.filter(f => f.path !== 'system/diary/new.mjs');
  fs.writeFileSync(path.join(input, '.private-products-installed.json'), JSON.stringify(manifest));
  installBundle(deployed, input);
  assert.equal(fs.existsSync(path.join(deployed.root, 'system/diary/new.mjs')), false);
  assert.equal(fs.readFileSync(path.join(deployed.root, 'system/shared.mjs'), 'utf8'), 'public source');
  assert.throws(() => installBundle(w, input), /symlink/);
  manifest.files[0].path = '../escape';
  fs.writeFileSync(path.join(input, '.private-products-installed.json'), JSON.stringify(manifest));
  assert.throws(() => installBundle(deployed, input), /Invalid bundle path/);
});

test('a committed pair composes without the vault, and an installed snapshot restores the old product bytes', t => {
  const { root, w, git, put } = fixture(t);
  assert.throws(() => verifyCoreRevision(w, 'HEAD'), /cutover/);
  migrate(w);
  git(['commit', '-qam', 'public cutover']);
  git(['add', 'products'], w.vault); git(['commit', '-qm', 'private import'], w.vault);
  const branch = git(['branch', '--show-current'], w.vault).trim();
  git(['config', 'remote.origin.url', '.'], w.vault);
  git(['config', 'remote.origin.fetch', '+refs/heads/*:refs/remotes/origin/*'], w.vault);
  git(['config', `branch.${branch}.remote`, 'origin'], w.vault);
  git(['config', `branch.${branch}.merge`, `refs/heads/${branch}`], w.vault);
  git(['update-ref', `refs/remotes/origin/${branch}`, 'HEAD'], w.vault);
  const revision = git(['rev-parse', 'HEAD']).trim();
  const output = path.join(root, 'composed');
  const pair = composeSource(w, output, revision, ['system', 'private-products.json']);
  assert.equal(pair.publicCommit, revision);
  const deployed = workspace(output);
  const before = snapshotInstalled(deployed, path.join(root, 'before'));
  assert.equal(before.files.length, 3);
  put('composed/system/mime/photo.mjs', 'new broken source');
  installBundle(deployed, path.join(root, 'before'));
  assert.equal(fs.readFileSync(path.join(output, 'system/mime/photo.mjs'), 'utf8'), 'export const lines = 7;\n');
  assert.equal(fs.existsSync(path.join(output, 'vault')), false);
});

test('optional products omit absent entry points but report broken installed modules', async t => {
  const { root, put } = fixture(t);
  assert.equal(await optionalProduct(pathToFileURL(path.join(root, 'missing.mjs'))), null);
  put('valid.mjs', 'export const present = true;');
  assert.equal((await optionalProduct(pathToFileURL(path.join(root, 'valid.mjs')))).present, true);
  put('broken.mjs', 'import "./missing.mjs";');
  await assert.rejects(optionalProduct(pathToFileURL(path.join(root, 'broken.mjs'))), /Cannot find module/);
});
