#!/usr/bin/env node
// Supporting Terminal task: move only this round's generated regular files.
// No globs, overwrites, recursive deletion, or user-file cleanup.
import { lstat, realpath, link, unlink, copyFile, readFile, writeFile } from 'node:fs/promises';
import { constants } from 'node:fs';
import { join, basename, resolve } from 'node:path';
import { homedir } from 'node:os';
import { pathToFileURL } from 'node:url';
import { createHash } from 'node:crypto';

export async function sortQuest(state, downloads) {
  const root=await realpath(state.root);
  if(root!==resolve(state.root) || basename(root)!=='Finder Quest '+state.id) throw new Error('Quest root must be an unaliased generated directory');
  if(!/^[a-zA-Z0-9]{6}$/.test(state.id))throw new Error('Invalid round ID');
  const plan=[];
  for(const file of state.files){
    if(file.sorted)continue;
    if(!new RegExp('^quest-'+state.id+'-[a-z]+\\.(svg|txt|wav)$').test(file.name))throw new Error('Unexpected generated filename');
    const category={svg:'Pictures',txt:'Notes',wav:'Audio'}[file.name.split('.').at(-1)];
    let source;
    if(file.locations.length===1 && file.locations[0].valid && !file.inDownloads)source=join(root,file.locations[0].path);
    else if(!file.locations.length && file.inDownloads && file.downloadCopies===1)source=join(downloads,file.name);
    else throw new Error('Missing, changed, or duplicate file: '+file.name);
    if(![root,join(root,'Loose'),join(root,'More stuff'),join(root,category),downloads].some(parent=>source===join(parent,file.name)))throw new Error('Source outside allowed game folders');
    if(await realpath(source)!==source)throw new Error('Symlink source refused');
    const stat=await lstat(source), dest=join(root,category,file.name);
    if(!stat.isFile() || stat.size>65536)throw new Error('Unexpected source type or size');
    if(!/^[a-f0-9]{64}$/.test(file.hash||'') || createHash('sha256').update(await readFile(source)).digest('hex')!==file.hash)
      throw new Error('Source contents do not match the generated asset');
    if(await realpath(join(root,category))!==join(root,category))throw new Error('Symlink destination refused');
    try {await lstat(dest);throw new Error('Destination already exists: '+file.name);}catch(e){if(e.code!=='ENOENT')throw e;}
    plan.push({source,dest,stat,name:file.name});
  }
  const moved=[];
  for(const {source,dest,stat,name} of plan){
    const current=await lstat(source);
    if(!current.isFile() || current.ino!==stat.ino || current.mtimeMs!==stat.mtimeMs)throw new Error('Source changed before move');
    // link is an exclusive destination creation, so a concurrent destination
    // cannot be overwritten. Cross-device copies also use exclusive creation.
    try {await link(source,dest);}catch(e){
      if(e.code!=='EXDEV')throw e;
      await copyFile(source,dest,constants.COPYFILE_EXCL);
      const hash=b=>createHash('sha256').update(b).digest('hex');
      if(hash(await readFile(source))!==hash(await readFile(dest)))throw new Error('Copy verification failed; both files retained');
    }
    const after=await lstat(source);
    if(after.ino!==stat.ino || after.mtimeMs!==stat.mtimeMs)throw new Error('Source changed during move; both names retained');
    await unlink(source);moved.push(name);
  }
  return moved;
}

if(process.argv[1] && import.meta.url===pathToFileURL(resolve(process.argv[1])).href){
  const id=process.argv[process.argv.indexOf('--quest')+1];
  const receipt=process.argv[process.argv.indexOf('--receipt')+1];
  const start=performance.now();let result;
  try {
    const state=await fetch('http://127.0.0.1:7782/state').then(r=>r.json());
    if(state.id!==id)throw new Error('Active round changed; no moves attempted');
    const moved=await sortQuest(state,join(homedir(),'Downloads'));
    const verified=await fetch('http://127.0.0.1:7782/state').then(r=>r.json());
    if(verified.id!==id || verified.sorted!==12)throw new Error('Post-move score did not verify all twelve files');
    result={ok:true,moved:moved.length,sorted:verified.sorted,elapsedMs:Math.round(performance.now()-start)};
  }catch(error){result={ok:false,error:error.message};process.exitCode=1;}
  console.log(JSON.stringify(result));
  if(process.argv.includes('--receipt'))await writeFile(receipt,JSON.stringify(result));
}
