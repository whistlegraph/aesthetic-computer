// Build an immutable runtime from committed files, never the dirty checkout.
import { execFileSync } from 'node:child_process';
import { mkdirSync, writeFileSync, readFileSync, cpSync, readdirSync } from 'node:fs';
import { resolve, dirname, posix, join } from 'node:path';
import { createHash } from 'node:crypto';

const [ref, destination, dependencies, nativeBinary] = process.argv.slice(2);
if (!ref || !destination || !dependencies || !nativeBinary)
  throw new Error('Usage: pack.mjs REV NEW_DIRECTORY NODE_MODULES TESTED_NATIVE_BINARY');
const repo = resolve(import.meta.dirname, '../..');
const git = (...args) => execFileSync('git', ['-C', repo, ...args], { maxBuffer: 32 * 1024 * 1024 });
const revision = git('rev-parse', '--verify', ref + '^{commit}').toString().trim();
const root = resolve(destination);
mkdirSync(root); // Refuse overwriting an existing immutable release.
const hash = bytes => createHash('sha256').update(bytes).digest('hex');
const sha256 = {}, sources = new Set();
function put(path, bytes) {
  if (path.startsWith('../') || path.startsWith('/')) throw new Error('Invalid release path');
  mkdirSync(dirname(join(root, path)), { recursive: true });
  writeFileSync(join(root, path), bytes);
  sha256[path] = hash(bytes);
}
function source(path, imports = true) {
  if (sources.has(path)) return;
  sources.add(path);
  const bytes = git('show', revision + ':' + path);
  put(path, bytes);
  if (!imports || !path.endsWith('.mjs')) return;
  for (const [, spec] of bytes.toString().matchAll(/(?<![\w.])(?:from\s*|import\s*(?:\(\s*)?)["']([^"']+)["']/g)) {
    if (spec.startsWith('.')) source(posix.normalize(posix.join(posix.dirname(path), spec)));
    else if (!spec.startsWith('node:') && spec !== 'playwright-core')
      throw new Error('Unpackaged dependency: ' + spec + ' in ' + path);
  }
}
for (const file of ['slab/bin/frame.mjs', 'slab/bin/frame-mcp.mjs', 'slab/bin/puppet.mjs',
  'slab/bin/puppet-mcp.mjs', 'slab/bin/computer-use.mjs', 'slab/bin/reel.mjs',
  'slab/computer-use-release/install.mjs', 'slab/computer-use-release/verify.mjs',
  'captutor/lib/frame-client.mjs']) source(file);
// Only this verified predecessor may be replaced in an existing Captutor install.
source('captutor/captutor.mjs', false);
const captutorPreviousHash = hash(git('show', 'e678ebb766^:captutor/captutor.mjs'));
const nativeSources = {};
for (const name of ['FrameCapture', 'FrameNativeInput', 'FrameSocket']) {
  const path = `slab/menubar-swift/Sources/SlabMenubar/${name}.swift`;
  const bytes = git('show', revision + ':' + path);
  if (hash(readFileSync(join(repo, path))) !== hash(bytes)) throw new Error('Native source differs from release: ' + path);
  nativeSources[path] = hash(bytes);
}
const nativeUUID = execFileSync('/usr/bin/dwarfdump', ['--uuid', nativeBinary], { encoding: 'utf8' }).split(/\s+/)[1];
if (!/^[A-Fa-f0-9-]{36}$/.test(nativeUUID)) throw new Error('Missing native build UUID');
const pw = join(resolve(dependencies), 'playwright-core');
const version = JSON.parse(readFileSync(join(pw, 'package.json'))).version;
if (version !== '1.60.0') throw new Error('Expected tested playwright-core 1.60.0');
cpSync(pw, join(root, 'node_modules/playwright-core'), { recursive: true, dereference: true });
function hashTree(path) {
  for (const entry of readdirSync(join(root, path), { withFileTypes: true })) {
    const child = posix.join(path, entry.name);
    if (entry.isDirectory()) hashTree(child);
    else if (entry.isFile()) sha256[child] = hash(readFileSync(join(root, child)));
    else throw new Error('Unexpected dependency entry: ' + child);
  }
}
hashTree('node_modules');
writeFileSync(join(root, 'release.json'), JSON.stringify({ revision, nativeUUID, nativeSources,
  captutorPreviousHash, dependencies: { 'playwright-core': version }, sha256 }, null, 2) + '\n');
console.log(JSON.stringify({ revision, root, files: Object.keys(sha256).length, nativeUUID }));
