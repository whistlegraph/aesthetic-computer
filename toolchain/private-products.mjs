#!/usr/bin/env node
// Public/private workspace composition. Source stays in the vault; local
// compatibility links and deployed overlays are always ignored by public Git.
import { execFileSync } from 'node:child_process';
import { createHash } from 'node:crypto';
import fs from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import { fileURLToPath, pathToFileURL } from 'node:url';

export const coreRoot = fileURLToPath(new URL('../', import.meta.url));
const marker = '# private-products: generated boundary';
const hash = bytes => createHash('sha256').update(bytes).digest('hex');
const exists = p => { try { fs.lstatSync(p); return true; } catch { return false; } };
const git = (root, args, options = {}) => execFileSync('git', ['-C', root, ...args], { maxBuffer: 64 * 1024 * 1024, ...options });
const names = (root, args) => git(root, args).toString().split('\0').filter(Boolean);
const safe = p => typeof p === 'string' && p.length && !p.includes('\\') && !p.startsWith('/') && p.split('/').every(x => x && x !== '.' && x !== '..');
const glob = p => new RegExp('^' + p.split('**').map(s => s.split('*').map(x => x.replace(/[.*+?^${}()|[\]\\]/g, '\\$&')).join('[^/]*')).join('.*') + '$');

export function workspace(root = coreRoot, vault) {
  root = path.resolve(root);
  const manifest = JSON.parse(fs.readFileSync(process.env.AC_PRODUCTS_MANIFEST || path.join(root, 'private-products.json')));
  vault = path.resolve(vault || path.join(root, manifest.vault));
  const rules = Object.entries(manifest.products).map(([id, spec]) => ({ id,
    include: spec.include.map(glob), exclude: (spec.exclude || []).map(glob) }));
  const artifacts = (manifest.localArtifacts || []).map(glob);
  const owner = p => {
    const matches = rules.filter(r => r.include.some(re => re.test(p)) && !r.exclude.some(re => re.test(p)));
    if (matches.length > 1) throw new Error('Overlapping private product paths: ' + p);
    return matches[0]?.id;
  };
  return { root, vault, manifest, owner, protectedPath: p => owner(p) || artifacts.some(re => re.test(p)) };
}

function localFiles(w) {
  return [...new Set(names(w.root, ['ls-files', '-z', '--cached', '--others', '--exclude-standard']))]
    .filter(p => w.owner(p) && exists(path.join(w.root, p)) && !fs.lstatSync(path.join(w.root, p)).isDirectory());
}

export function productFiles(w) {
  return names(w.vault, ['ls-files', '-z', '--cached', '--others', '--exclude-standard', '--', 'products/'])
    .flatMap(p => {
      const [, id, ...rest] = p.split('/'), original = rest.join('/');
      return original && w.owner(original) === id && exists(path.join(w.vault, p)) && fs.lstatSync(path.join(w.vault, p)).isFile()
        ? [{ product: id, path: original, source: path.join(w.vault, p) }] : [];
    });
}

function link(source, destination) {
  fs.mkdirSync(path.dirname(destination), { recursive: true });
  if (exists(destination)) {
    if (fs.lstatSync(destination).isSymbolicLink() && path.resolve(path.dirname(destination), fs.readlinkSync(destination)) === source) return;
    throw new Error('Refusing to replace existing path: ' + destination);
  }
  fs.symlinkSync(path.relative(path.dirname(destination), source), destination);
}

// An inventory extension may turn a formerly shared subtree into private
// source. Replace only our own dependency links, never a user's real files.
function detachDependency(w, id, relative) {
  const parts = relative.split('/');
  for (let i = 1; i <= parts.length; i++) {
    const prefix = parts.slice(0, i).join('/');
    const target = path.join(w.vault, 'products', id, prefix);
    if (!exists(target) || !fs.lstatSync(target).isSymbolicLink()) continue;
    if (path.resolve(path.dirname(target), fs.readlinkSync(target)) !== path.join(w.root, prefix))
      throw new Error('Unexpected dependency link: ' + target);
    fs.unlinkSync(target);
    if (i < parts.length) fs.mkdirSync(target);
  }
}

function ignoreBoundary(w) {
  const file = path.join(w.root, '.gitignore');
  const before = fs.readFileSync(file, 'utf8').split(marker)[0].trimEnd();
  const patterns = [...Object.values(w.manifest.products).flatMap(s => [...s.include.map(p => '/' + p), ...(s.exclude || []).map(p => '!/' + p)]), ...(w.manifest.localArtifacts || []).map(p => '/' + p)];
  fs.writeFileSync(file, before + '\n\n' + marker + '\n' + patterns.map(p => p.replace(/\/\*\*$/, '/')).join('\n') + '\n/.private-products-installed.json\n/system/public/.products-ref\n');
}

// Recreate the old relative-import tree inside each product. Only common
// dependencies are linked back to AC; these links are ignored in the vault.
function linkDependencies(w, files) {
  const publicFiles = [...names(w.root, ['ls-files', '-z', '--cached']), 'private-products.json', 'toolchain/private-products.mjs', 'oven/private-products.mjs', 'shared/private-product.mjs']
    .filter(p => safe(p) && !w.owner(p) && exists(path.join(w.root, p)) && !p.startsWith(w.manifest.vault + '/'));
  const originalIgnore = fs.readFileSync(path.join(w.root, '.gitignore'), 'utf8').split(marker)[0];
  for (const id of Object.keys(w.manifest.products)) {
    const product = path.join(w.vault, 'products', id);
    fs.mkdirSync(product, { recursive: true });
    const owned = files.filter(f => f.product === id).map(f => f.path);
    const ignored = new Set();
    const dependencies = publicFiles.filter(p => path.basename(p) === 'package.json').map(p => path.join(path.dirname(p), 'node_modules'));
    for (const p of [...publicFiles, ...files.filter(f => f.product !== id).map(f => f.path), ...dependencies, 'node_modules', w.manifest.vault]) {
      if (!safe(p) || p === '.gitignore' || p === 'LICENSE' || p === 'README.md' || p === 'SCORE.md' || p === 'AGENTS.md') continue;
      const parts = p.split('/');
      for (let i = 1; i <= parts.length; i++) {
        const prefix = parts.slice(0, i).join('/');
        if (owned.some(o => o === prefix || o.startsWith(prefix + '/'))) continue;
        const source = path.join(w.root, prefix), destination = path.join(product, prefix);
        if (!exists(source)) break;
        // Git refuses symlinked ignore and attributes files (ELOOP).
        if (['.gitignore', '.gitattributes'].includes(path.basename(prefix))) {
          if (exists(destination) && fs.lstatSync(destination).isSymbolicLink()) fs.unlinkSync(destination);
          fs.copyFileSync(source, destination);
          ignored.add('/' + prefix);
          break;
        }
        if (!exists(destination)) link(source, destination);
        if (fs.lstatSync(destination).isSymbolicLink()) ignored.add('/' + prefix);
        break;
      }
    }
    fs.writeFileSync(path.join(product, '.gitignore'), originalIgnore.trimEnd() + '\n\n# Local AC dependency links; never export these.\n' + [...ignored].sort().join('\n') + '\n');
  }
}

export function linkProducts(w) {
  const files = productFiles(w);
  ignoreBoundary(w);
  for (const file of files) link(file.source, path.join(w.root, file.path));
  linkDependencies(w, files);
  return files;
}

export function migrate(w) {
  const indexLock = git(w.root, ['rev-parse', '--git-path', 'index.lock']).toString().trim();
  if (exists(path.resolve(w.root, indexLock))) throw new Error('Git index is locked; resolve the lock before migrating.');
  const files = localFiles(w).filter(p => !fs.lstatSync(path.join(w.root, p)).isSymbolicLink());
  const tracked = names(w.root, ['ls-files', '-z']).filter(p => w.owner(p));
  const missing = tracked.filter(p => !files.includes(p));
  if (missing.length) throw new Error('Missing tracked private source: ' + missing.join(', '));
  const staged = names(w.root, ['diff', '--cached', '--name-only', '-z']).filter(p => files.includes(p));
  if (staged.length) throw new Error('Private paths already staged; preserve/review that index before migrating: ' + staged.slice(0, 5).join(', '));
  const receiptPath = path.join(w.vault, 'products/migration.json');
  const receipt = exists(receiptPath) ? JSON.parse(fs.readFileSync(receiptPath)) : { version: 1, publicCommit: git(w.root, ['rev-parse', 'HEAD']).toString().trim(), files: [] };
  const versionedGame = 'xbox/live/oskiewar.js';
  if (w.owner(versionedGame) === 'oskiewar' && receipt.publicHistoryCounts?.[versionedGame] === undefined) {
    receipt.publicHistoryCounts ||= {};
    receipt.publicHistoryCounts[versionedGame] = Number(git(w.root, ['rev-list', '--count', receipt.publicCommit, '--', versionedGame]).toString());
  }
  const pending = [];
  for (const p of files) {
    const source = path.join(w.root, p), destination = path.join(w.vault, 'products', w.owner(p), p);
    detachDependency(w, w.owner(p), p);
    if (exists(destination)) throw new Error('Vault destination already exists: ' + destination);
    if (fs.lstatSync(source).isSymbolicLink()) throw new Error('Review existing source symlink before migrating: ' + p);
    pending.push({ path: p, product: w.owner(p), sha256: hash(fs.readFileSync(source)), mode: fs.statSync(source).mode & 0o777 });
  }
  const products = path.join(w.vault, 'products');
  fs.mkdirSync(products, { recursive: true });
  receipt.files.push(...pending);
  fs.writeFileSync(receiptPath, JSON.stringify(receipt, null, 2) + '\n');
  for (const f of pending) {
    const source = path.join(w.root, f.path), destination = path.join(products, f.product, f.path);
    fs.mkdirSync(path.dirname(destination), { recursive: true });
    fs.renameSync(source, destination);
    link(destination, source);
  }
  // Stage only removal of the exact migrated public paths. Their dirty working
  // bytes are preserved in the vault, verified against the receipt above.
  if (tracked.length) git(w.root, ['update-index', '--force-remove', '-z', '--stdin'], { input: tracked.join('\0') + '\0' });
  ignoreBoundary(w);
  linkDependencies(w, receipt.files.map(f => ({ ...f, source: path.join(products, f.product, f.path) })));
  for (const f of pending) if (hash(fs.readFileSync(path.join(products, f.product, f.path))) !== f.sha256) throw new Error('Migration checksum mismatch: ' + f.path);
  return receipt;
}

export function checkProducts(w, { links = true } = {}) {
  const leaks = names(w.root, ['ls-files', '-z']).filter(p => w.protectedPath(p));
  if (leaks.length) throw new Error('Private product source is in the public Git index: ' + leaks.slice(0, 10).join(', '));
  const files = productFiles(w);
  if (links) for (const file of files) {
    const destination = path.join(w.root, file.path);
    if (!exists(destination) || !fs.lstatSync(destination).isSymbolicLink() || fs.realpathSync(destination) !== fs.realpathSync(file.source))
      throw new Error('Missing or divergent private product link: ' + file.path);
  }
  return Object.fromEntries(Object.keys(w.manifest.products).map(id => [id, files.filter(f => f.product === id).length]));
}

export function releaseState(w) {
  const dirty = git(w.vault, ['status', '--porcelain', '--untracked-files=all', '--', 'products']).toString().trim();
  if (dirty) throw new Error('Commit the private product changes in the vault before releasing.');
  const revision = git(w.vault, ['rev-parse', 'HEAD']).toString().trim();
  if (process.env.AC_PRODUCTS_REVISION && revision !== process.env.AC_PRODUCTS_REVISION)
    throw new Error('The vault revision changed during this release; restart from one fixed revision.');
  const upstream = git(w.vault, ['rev-parse', '--abbrev-ref', '@{upstream}']).toString().trim();
  const ahead = Number(git(w.vault, ['rev-list', '--count', upstream + '..HEAD']).toString());
  if (ahead) throw new Error('Push the vault revision before releasing private products.');
  return revision;
}

export function verifyCoreRevision(w, revision) {
  const inventory = git(w.root, ['show', revision + ':private-products.json']).toString();
  if (JSON.stringify(JSON.parse(inventory)) !== JSON.stringify(w.manifest)) throw new Error('Public revision has a different private product inventory');
  const leaks = names(w.root, ['ls-tree', '-r', '--name-only', '-z', revision]).filter(p => w.protectedPath(p));
  if (leaks.length) throw new Error('Public revision has not completed the private source cutover');
}

export function composeSource(w, output, revision, paths) {
  if (exists(output)) throw new Error('Composed source destination already exists');
  if (!/^[a-f0-9]{40,64}$/.test(revision) || !paths.length || paths.some(p => !safe(p))) throw new Error('Specify a public commit and source paths');
  verifyCoreRevision(w, revision);
  const temporary = fs.mkdtempSync(path.join(os.tmpdir(), 'ac-compose-'));
  try {
    // Validate and freeze the vault before creating any output.
    const manifest = buildBundle(w, path.join(temporary, 'products.tgz'));
    const tracked = names(w.root, ['ls-tree', '-r', '--name-only', '-z', revision]);
    const publicPaths = paths.filter(p => tracked.some(f => f === p || f.startsWith(p + '/')));
    if (!publicPaths.length) throw new Error('No public source matches the requested paths');
    git(w.root, ['archive', '--output=' + path.join(temporary, 'core.tar'), revision, '--', ...publicPaths]);
    fs.mkdirSync(output, { recursive: true });
    execFileSync('tar', ['-xf', path.join(temporary, 'core.tar'), '-C', output]);
    const input = path.join(temporary, 'products'); fs.mkdirSync(input);
    execFileSync('tar', ['-xzf', path.join(temporary, 'products.tgz'), '-C', input]);
    installBundle({ ...w, root: output }, input);
    fs.writeFileSync(path.join(output, 'system/public/.commit-ref'), revision + '\n');
    return { publicCommit: revision, productsCommit: manifest.revision };
  } finally { fs.rmSync(temporary, { recursive: true, force: true }); }
}

// Read one pinned revision through one Git process. Starting `git show` for
// every asset repeatedly opens the same packs and makes fleet deploys crawl.
// Batch output goes to disk, so only one blob is held in memory at a time.
function* committedProducts(w, files, revision) {
  const temporary = fs.mkdtempSync(path.join(os.tmpdir(), 'ac-product-blobs-'));
  const fd = fs.openSync(path.join(temporary, 'objects'), 'w+');
  try {
    git(w.vault, ['cat-file', '--batch', '-Z'], {
      input: files.map(f => revision + ':products/' + f.product + '/' + f.path + '\0').join(''),
      stdio: ['pipe', fd, 'pipe'],
    });
    let offset = 0;
    const header = Buffer.alloc(128), delimiter = Buffer.alloc(1);
    for (const file of files) {
      const count = fs.readSync(fd, header, 0, header.length, offset);
      const end = header.subarray(0, count).indexOf(0);
      const match = end >= 0 && header.subarray(0, end).toString().match(/^[a-f0-9]{40,64} blob ([0-9]+)$/);
      const size = match ? Number(match[1]) : NaN;
      if (!Number.isSafeInteger(size) || size < 0 || size > 64 * 1024 * 1024)
        throw new Error('Invalid committed product blob: ' + file.path);
      offset += end + 1;
      const bytes = Buffer.allocUnsafe(size);
      let read = 0;
      while (read < size) {
        const amount = fs.readSync(fd, bytes, read, size - read, offset + read);
        if (!amount) throw new Error('Truncated committed product blob: ' + file.path);
        read += amount;
      }
      offset += size;
      if (fs.readSync(fd, delimiter, 0, 1, offset++) !== 1 || delimiter[0] !== 0)
        throw new Error('Invalid committed product delimiter: ' + file.path);
      yield bytes;
    }
  } finally {
    fs.closeSync(fd);
    fs.rmSync(temporary, { recursive: true, force: true });
  }
}

export function buildBundle(w, output, { worktree = false } = {}) {
  const revision = worktree ? 'local-test' : releaseState(w);
  const files = productFiles(w);
  for (const id of Object.keys(w.manifest.products)) if (!files.some(f => f.product === id)) throw new Error('Private product missing: ' + id);
  const stage = fs.mkdtempSync(path.join(os.tmpdir(), 'ac-products-'));
  const manifest = { version: 1, revision, files: [] };
  try {
    const committed = worktree ? null : committedProducts(w, files, revision);
    try {
      for (const f of files) {
        const destination = path.join(stage, f.path);
        const bytes = worktree ? fs.readFileSync(f.source) : committed.next().value;
        fs.mkdirSync(path.dirname(destination), { recursive: true }); fs.writeFileSync(destination, bytes, { mode: fs.statSync(f.source).mode & 0o777 });
        manifest.files.push({ path: f.path, product: f.product, sha256: hash(bytes) });
      }
    } finally { committed?.return(); }
    fs.writeFileSync(path.join(stage, '.private-products-installed.json'), JSON.stringify(manifest, null, 2) + '\n');
    fs.mkdirSync(path.dirname(path.resolve(output)), { recursive: true });
    execFileSync('tar', ['-czf', path.resolve(output), '-C', stage, '.']);
    return manifest;
  } finally { fs.rmSync(stage, { recursive: true, force: true }); }
}

// Capture the installed product revision before a public Git reset, including
// the first cutover from the former monorepo. Rollbacks restore both halves.
export function snapshotInstalled(w, output) {
  if (exists(output)) throw new Error('Snapshot destination already exists');
  const installed = path.join(w.root, '.private-products-installed.json');
  const previous = exists(installed) ? JSON.parse(fs.readFileSync(installed)) : null;
  const paths = previous ? previous.files.map(f => f.path) : localFiles(w);
  const manifest = { version: 1, revision: previous?.revision || git(w.root, ['rev-parse', 'HEAD']).toString().trim(), files: [] };
  for (const p of paths) {
    if (!safe(p) || !w.owner(p)) throw new Error('Invalid installed product path: ' + p);
    const source = path.join(w.root, p), target = path.join(output, p);
    const bytes = fs.readFileSync(source);
    fs.mkdirSync(path.dirname(target), { recursive: true });
    fs.writeFileSync(target, bytes, { mode: fs.statSync(source).mode & 0o777 });
    manifest.files.push({ path: p, product: w.owner(p), sha256: hash(bytes) });
  }
  fs.mkdirSync(output, { recursive: true });
  fs.writeFileSync(path.join(output, '.private-products-installed.json'), JSON.stringify(manifest, null, 2) + '\n');
  return manifest;
}

// Input must be an extracted release bundle. Validate all files before touching
// a deployment. Never install into developer compatibility symlinks.
export function installBundle(w, input) {
  const manifest = JSON.parse(fs.readFileSync(path.join(input, '.private-products-installed.json')));
  if (manifest.version !== 1 || !Array.isArray(manifest.files) || !/^(local-test|[a-f0-9]{40,64})$/.test(manifest.revision)) throw new Error('Invalid private product bundle');
  const noSymlinks = (root, relative) => {
    for (let target = path.join(root, relative); target !== root; target = path.dirname(target))
      if (exists(target) && fs.lstatSync(target).isSymbolicLink()) throw new Error('Bundle path is a symlink: ' + target);
  };
  const seen = new Set();
  for (const f of manifest.files) {
    if (!safe(f.path) || w.owner(f.path) !== f.product || seen.has(f.path)) throw new Error('Invalid bundle path: ' + f.path);
    seen.add(f.path);
    noSymlinks(path.resolve(input), f.path);
    const source = path.join(input, f.path);
    if (!fs.lstatSync(source).isFile() || hash(fs.readFileSync(source)) !== f.sha256) throw new Error('Invalid bundle bytes: ' + f.path);
    noSymlinks(w.root, f.path);
  }
  const oldPath = path.join(w.root, '.private-products-installed.json');
  const old = exists(oldPath) ? JSON.parse(fs.readFileSync(oldPath)) : { files: [] };
  for (const f of old.files) {
    if (!safe(f.path) || w.owner(f.path) !== f.product) throw new Error('Invalid previous bundle path');
    noSymlinks(w.root, f.path);
  }
  for (const f of old.files) {
    if (!seen.has(f.path)) fs.rmSync(path.join(w.root, f.path), { force: true });
  }
  for (const f of manifest.files) {
    const target = path.join(w.root, f.path), source = path.join(input, f.path);
    fs.mkdirSync(path.dirname(target), { recursive: true }); fs.copyFileSync(source, target); fs.chmodSync(target, fs.statSync(source).mode & 0o777);
  }
  fs.writeFileSync(oldPath, JSON.stringify(manifest, null, 2) + '\n');
  fs.mkdirSync(path.join(w.root, 'system/public'), { recursive: true });
  fs.writeFileSync(path.join(w.root, 'system/public/.products-ref'), manifest.revision + '\n');
  return manifest;
}

if (process.argv[1] && import.meta.url === pathToFileURL(path.resolve(process.argv[1])).href) {
  try {
    const [command = 'status', ...args] = process.argv.slice(2), w = workspace(process.env.AC_CORE_ROOT || coreRoot, process.env.AC_PRODUCTS_VAULT);
    if (command === 'migrate') console.log(JSON.stringify({ migrated: migrate(w).files.length }));
    else if (command === 'link') console.log(JSON.stringify({ linked: linkProducts(w).length }));
    else if (command === 'check') console.log(JSON.stringify(checkProducts(w)));
    else if (command === 'guard') {
      const leaks = names(w.root, ['ls-files', '-z']).filter(p => w.protectedPath(p));
      if (leaks.length) throw new Error('Private product paths must not be committed to public AC: ' + leaks.slice(0, 10).join(', '));
    } else if (command === 'pack') {
      if (!args[0]) throw new Error('pack requires an output .tgz path');
      const result = buildBundle(w, args[0], { worktree: args.includes('--worktree') });
      console.log(JSON.stringify({ revision: result.revision, files: result.files.length, output: args[0] }));
    } else if (command === 'install') {
      if (!args[0]) throw new Error('install requires an extracted bundle directory');
      const result = installBundle(w, path.resolve(args[0]));
      console.log(JSON.stringify({ installed: result.files.length, revision: result.revision }));
    } else if (command === 'snapshot') {
      if (!args[0]) throw new Error('snapshot requires a new destination directory');
      console.log(JSON.stringify({ saved: snapshotInstalled(w, path.resolve(args[0])).files.length }));
    } else if (command === 'source') {
      if (!args[0] || !args[1]) throw new Error('source requires a new output directory, public commit, and paths');
      console.log(JSON.stringify(composeSource(w, path.resolve(args[0]), args[1], args.slice(2))));
    } else if (command === 'root') console.log(w.root);
    else if (command === 'release-check') { verifyCoreRevision(w, args[0] || 'HEAD'); console.log(releaseState(w)); }
    else if (command === 'revision') console.log(releaseState(w));
    else if (command === 'status') {
      const privateFiles = exists(path.join(w.vault, '.git')) ? productFiles(w).map(f => f.path) : [];
      const files = [...new Set([...localFiles(w), ...privateFiles])];
      console.log(JSON.stringify(Object.fromEntries(Object.keys(w.manifest.products).map(id => [id, files.filter(p => w.owner(p) === id).length])), null, 2));
    } else throw new Error('Usage: private-products.mjs status|migrate|link|check|guard|pack|install|snapshot|source|root|revision|release-check');
  } catch (error) { console.error(error.message); process.exitCode = 1; }
}
