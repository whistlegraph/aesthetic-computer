#!/usr/bin/env node
// Publish notarized Mac artifacts first, then atomically expose the update feed.
const fs = require('node:fs');
const path = require('node:path');
const crypto = require('node:crypto');
const { execFileSync } = require('node:child_process');
const { S3Client, PutObjectCommand } = require('@aws-sdk/client-s3');
const dir = path.resolve(process.argv[2] || 'dist');
const version = require('../package.json').version;
const zip = `aesel-${version}-arm64-mac.zip`;
const dmg = `aesel-${version}-arm64.dmg`;
const digest = (name, algorithm, encoding) => crypto.createHash(algorithm).update(fs.readFileSync(path.join(dir, name))).digest(encoding);
async function main() {
  if (!process.env.SPACES_KEY || !process.env.SPACES_SECRET) throw Error('Missing Spaces credentials');
  for (const name of [zip, dmg, zip + '.blockmap']) fs.accessSync(path.join(dir, name));
  execFileSync('xcrun', ['stapler', 'validate', path.join(dir, dmg)], { stdio: 'inherit' });
  execFileSync('spctl', ['-a', '-t', 'open', '--context', 'context:primary-signature', path.join(dir, dmg)], { stdio: 'inherit' });
  const zipHash = digest(zip, 'sha512', 'base64');
  // Signing/stapling changes the DMG bytes after electron-builder writes its feed.
  const manifest = `version: ${version}\nfiles:\n` + [zip, dmg].map(name => `  - url: ${name}\n    sha512: ${digest(name, 'sha512', 'base64')}\n    size: ${fs.statSync(path.join(dir, name)).size}\n`).join('') + `path: ${zip}\nsha512: ${zipHash}\nreleaseDate: '${new Date().toISOString()}'\n`;
  fs.writeFileSync(path.join(dir, 'latest-mac.yml'), manifest);
  const existing = await fetch('https://releases.aesthetic.computer/easel/desktop/SHA256SUMS', { cache: 'no-store' });
  if (!existing.ok) throw Error('Cannot preserve existing release checksums');
  const lines = (await existing.text()).trim().split('\n').filter(line => !line.endsWith('  ' + zip) && !line.endsWith('  ' + dmg));
  lines.push(...[zip, dmg].map(name => `${digest(name, 'sha256', 'hex')}  ${name}`));
  fs.writeFileSync(path.join(dir, 'SHA256SUMS'), lines.join('\n') + '\n');
  fs.copyFileSync(path.join(__dirname, 'linux-download.html'), path.join(dir, 'index.html'));
  const client = new S3Client({ endpoint: process.env.SPACES_ENDPOINT || 'https://sfo3.digitaloceanspaces.com', region: 'us-east-1', credentials: { accessKeyId: process.env.SPACES_KEY, secretAccessKey: process.env.SPACES_SECRET }, requestChecksumCalculation: 'WHEN_REQUIRED', responseChecksumValidation: 'WHEN_REQUIRED' });
  for (const name of [zip, zip + '.blockmap', dmg, 'SHA256SUMS', 'latest-mac.yml', 'index.html']) {
    await client.send(new PutObjectCommand({ Bucket: 'releases-aesthetic-computer', Key: 'easel/desktop/' + name, Body: fs.readFileSync(path.join(dir, name)), ACL: 'public-read', ContentType: name.endsWith('.html') ? 'text/html; charset=utf-8' : name.endsWith('.yml') ? 'text/yaml' : 'application/octet-stream', CacheControl: name.startsWith('aesel-') ? 'public, max-age=31536000, immutable' : 'no-cache' }));
    console.log('Published ' + name);
  }
}
main().catch(error => { console.error(error.message); process.exitCode = 1; });
