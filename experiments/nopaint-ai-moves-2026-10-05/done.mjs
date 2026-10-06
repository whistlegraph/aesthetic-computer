// One immutable artifact per Done. AC's existing publisher owns retry receipts,
// WIP ownership, recording upload, sealing, and verification of the public PNG.
import {readFile, writeFile, mkdir, rename} from 'node:fs/promises';
import {join} from 'node:path';
import {createHash} from 'node:crypto';
import {publishPicture} from '../../aesel/src/publish-picture.mjs';

const hash = bytes => createHash('sha256').update(bytes).digest('hex');
export async function finishPainting({folder, accountId, handle, token, fetch=globalThis.fetch, site}) {
  const manifest = JSON.parse(await readFile(join(folder, 'manifest.json'), 'utf8'));
  if (manifest.account_id !== accountId) throw Error('Sign into the account that started this Done.');
  if (!/^[a-f0-9-]{36}$/.test(manifest.id) || !manifest.history?.length) throw Error('Invalid Done snapshot.');
  const versions = join(folder, 'pictures', manifest.id);
  let current;
  for (const [index, image] of manifest.history.entries()) {
    const bytes = await readFile(image.path);
    if (hash(bytes) !== image.sha256) throw Error('The saved painting changed. Done was stopped.');
    const root = join(versions, `v${index+1}`);
    await mkdir(root, {recursive:true, mode:0o700});
    const revision = {files:['composite.png'], hashes:{'composite.png':image.sha256},
      createdAt:manifest.createdAt, summary:index ? 'No Paint move' : 'Starting image'};
    for (const [name, data] of Object.entries({'composite.png':bytes,
      'revision.json':JSON.stringify(revision), 'picture.json':JSON.stringify({layers:[]})})) {
      await writeFile(join(root, name+'.tmp'), data, {mode:0o600});
      await rename(join(root, name+'.tmp'), join(root, name));
    }
    current = {kind:'picture', id:manifest.id, version:index+1, root, revision};
  }
  const artifacts = {root:folder, selected:async()=>current, locked:fn=>fn()};
  return publishPicture({artifacts, session:{handle, signedIn:true, token:async()=>token,
    read:()=>({user:{sub:accountId}})}, fetch, site});
}
