// npm ci --ignore-scripts --no-audit --no-fund && node build.mjs
import { build } from 'esbuild';
import { readFile, writeFile, copyFile } from 'node:fs/promises';
import { createHash } from 'node:crypto';
import { fileURLToPath } from 'node:url';
import { dirname, resolve } from 'node:path';
const dir = dirname(fileURLToPath(import.meta.url));
const output = resolve(dir, '../../public/braincells/vendor');
await build({ entryPoints:[resolve(dir, 'entry.mjs')], bundle:true, platform:'browser', format:'iife', globalName:'beacon',
  minify:true, inject:[resolve(dir, 'globals.mjs')], define:{ 'process.env.NODE_ENV':'"production"' },
  alias:{ crypto:resolve(dir, 'empty-crypto.mjs') }, outfile:resolve(output, 'beacon-sdk.min.js') });
// Normalize whitespace only in esbuild's appended license comment.
const bundlePath = resolve(output, 'beacon-sdk.min.js');
const bundle = await readFile(bundlePath, 'utf8');
const licensesAt = bundle.indexOf('\n/*! Bundled license information:');
if (licensesAt >= 0) await writeFile(bundlePath, bundle.slice(0, licensesAt) + bundle.slice(licensesAt).replace(/[ \t]+$/gm, ''));
const lock = JSON.parse(await readFile(resolve(dir, 'package-lock.json')));
const dependency = lock.packages['node_modules/@airgap/beacon-dapp'];
await copyFile(resolve(dir, 'node_modules/@airgap/beacon-dapp/LICENCE'), resolve(output, 'LICENSE.txt'));
await writeFile(resolve(output, 'beacon-sdk.json'), JSON.stringify({ package:'@airgap/beacon-dapp', version:dependency.version,
  source:dependency.resolved, integrity:dependency.integrity, build:'system/scripts/beacon-vendor/build.mjs',
  sha256:createHash('sha256').update(await readFile(resolve(output, 'beacon-sdk.min.js'))).digest('hex') }, null, 2)+'\n');
