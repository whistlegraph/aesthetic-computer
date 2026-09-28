import { pictureWipRecord } from './picture-wip.mjs';
import { EventEmitter } from 'node:events';
import { randomUUID, createHash } from 'node:crypto';
import { mkdir, readFile, writeFile, rename, rm, lstat, copyFile, readdir } from 'node:fs/promises';
import { resolve, join, dirname, relative, isAbsolute } from 'node:path';

export const MEDIA = ['piece', 'picture', 'sound', 'paper', 'gameboy'];
const modules = { gameboy: () => import('./media/gameboy.mjs'), picture: () => import('./media/picture.mjs'), sound: () => import('./media/sound.mjs'), paper: () => import('./media/paper.mjs') };
const json = value => `${JSON.stringify(value, null, 2)}\n`;
async function safe(root, name) {
  if (typeof name !== 'string' || !name || isAbsolute(name)) throw new Error('Artifact paths must be relative.');
  if (name.includes('\\') || name.split('/').some(part => !part || part === '.' || part === '..')) throw new Error('Path leaves the artifact or is not canonical.');
  try { if ((await lstat(root)).isSymbolicLink()) throw new Error('Artifact root cannot be a symlink.'); }
  catch (error) { if (error.code !== 'ENOENT') throw error; }
  const path = resolve(root, name), rel = relative(root, path);
  if (!rel || rel.startsWith('../') || rel === '..') throw new Error('Path leaves the artifact.');
  let cursor = root;
  for (const part of rel.split('/')) {
    cursor = join(cursor, part);
    try { if ((await lstat(cursor)).isSymbolicLink()) throw new Error('Artifact symlinks are not supported.'); }
    catch (error) { if (error.code !== 'ENOENT') throw error; }
  }
  return path;
}

export class Artifacts extends EventEmitter {
  constructor(cwd) { super(); this.root = join(resolve(cwd), '.easel-media'); this.file = join(this.root, 'project.json'); }
  async read() {
    try { return JSON.parse(await readFile(await safe(this.root, 'project.json'), 'utf8')); }
    catch (error) { if (error.code === 'ENOENT') return { format: 1, id: null, selected: null, artifacts: [] }; throw error; }
  }
  async locked(work) {
    await mkdir(this.root, { recursive: true });
    if ((await lstat(this.root)).isSymbolicLink()) throw new Error('Project directory cannot be a symlink.');
    const lock = join(this.root, '.lock');
    try { await mkdir(lock); } catch (error) { if (error.code === 'EEXIST') throw new Error('An artifact operation is already running.'); throw error; }
    try { return await work(); } finally { await rm(lock, { recursive: true, force: true }); }
  }
  async save(project) {
    project.id ||= randomUUID();
    const tmp = join(this.root, `project.${randomUUID()}.tmp`);
    await writeFile(tmp, json(project)); await rename(tmp, this.file); this.emit('change', project);
  }
  async selected() {
    const project = await this.read();
    const artifact = project.artifacts.find(a => a.id === project.selected);
    if (!artifact) return null;
    if (!/^[a-f0-9-]{36}$/.test(artifact.id) || !Number.isSafeInteger(artifact.version) || artifact.version < 1) throw new Error('Invalid artifact identity.');
    if (!modules[artifact.kind]) throw new Error('Invalid artifact medium.');
    const root = await safe(this.root, `artifacts/${artifact.id}/v${artifact.version}`);
    const revision = JSON.parse(await readFile(await safe(root, 'revision.json'), 'utf8'));
    return { ...artifact, root, revision };
  }
  async create(kind, name = '') {
    if (!modules[kind]) throw new Error('Choose picture, sound, paper or gameboy; /medium piece returns to software.');
    return this.locked(async () => {
      const project = await this.read(), id = randomUUID();
      const artifact = { id, kind, name: name.trim().slice(0,80) || `Untitled ${kind}`, version: 0 };
      const stage = await safe(this.root, `jobs/${randomUUID()}`); await mkdir(stage, { recursive: true });
      const result = await (await modules[kind]()).create({ root: stage, name: artifact.name });
      project.artifacts.push(artifact); project.selected = id;
      return this.commit(project, artifact, stage, result);
    });
  }
  async select(id) {
    return this.locked(async () => {
      const p = await this.read();
      if (id !== 'piece' && !p.artifacts.some(a => a.id === id)) throw new Error('Unknown artifact. Use /artifacts.');
      p.selected = id === 'piece' ? null : id; await this.save(p); return this.selected();
    });
  }
  async commit(project, artifact, stage, result, restoredFrom) {
    if (!/^[a-f0-9-]{36}$/.test(artifact.id) || !Number.isSafeInteger(artifact.version) || artifact.version < 0) throw new Error('Invalid artifact identity.');
    const stageRelative = relative(this.root, stage);
    if (!/^jobs\/[a-f0-9-]{36}$/.test(stageRelative)) throw new Error('Unsafe artifact staging directory.');
    await safe(this.root, stageRelative);
    // Check every staged entry, including unlisted receipts and discarded assets.
    let stagedBytes = 0;
    async function inspect(directory) {
      for (const entry of await readdir(directory, {withFileTypes:true})) {
        const path = join(directory, entry.name), stat = await lstat(path);
        if (stat.isSymbolicLink()) throw new Error('Artifact symlinks are not supported.');
        if (stat.isDirectory()) await inspect(path);
        else if (stat.isFile()) { stagedBytes += stat.size; if (stagedBytes > 64 * 1024 * 1024) throw new Error('Artifact exceeds the 64 MiB prototype limit.'); }
        else throw new Error('Artifact output is not a regular file.');
      }
    }
    await inspect(stage);
    if (!Array.isArray(result.files) || !result.files.length) throw new Error('Operation produced no artifact files.');
    let bytes = 0; const hashes = {};
    for (const name of new Set(result.files)) {
      if (name === 'revision.json') throw new Error('Reserved artifact filename.');
      const file = await safe(stage, name), stat = await lstat(file);
      if (!stat.isFile()) throw new Error('Artifact output is not a regular file.');
      bytes += stat.size; if (bytes > 64 * 1024 * 1024) throw new Error('Artifact exceeds the 64 MiB prototype limit.');
      hashes[name] = createHash('sha256').update(await readFile(file)).digest('hex');
    }
    if (result.preview && !Object.hasOwn(hashes, result.preview.path)) throw new Error('Preview must be an artifact file.');
    const version = artifact.version + 1;
    const revision = { version, createdAt: new Date().toISOString(), files: Object.keys(hashes), hashes, preview: result.preview || null, summary: result.summary || '', status: result.status || null, metadata: result.metadata || null, sourceAhead: Boolean(result.sourceAhead ?? result.metadata?.sourceAhead), ...(restoredFrom ? {restoredFrom} : {}) };
    await writeFile(await safe(stage, 'revision.json'), json(revision));
    const parent = await safe(this.root, `artifacts/${artifact.id}`); await mkdir(parent, { recursive: true });
    await rename(stage, await safe(parent, `v${version}`));
    artifact.version = version; await this.save(project);
    return { ...result, version, artifactId: artifact.id };
  }
  async run(action, input = {}, { paid = false, reviewed = false } = {}) {
    if (action === 'qa' && !reviewed) throw new Error('Paper QA requires explicit user-reviewed visual inspection.');
    return this.locked(async () => {
      const current = await this.selected(); if (!current) throw new Error('Select a Picture, Sound, Paper or GameBoy first.');
      const project = await this.read();
      let artifact = project.artifacts.find(a => a.id === current.id);
      const sealed = current.kind === 'picture' && await pictureWipRecord(this, current.id);
      if (sealed?.status === 'done') {
        const forkStage = await safe(this.root, `jobs/${randomUUID()}`); await mkdir(forkStage, {recursive:true});
        for (const name of current.revision.files) { const dest=await safe(forkStage,name); await mkdir(dirname(dest),{recursive:true}); await copyFile(await safe(current.root,name),dest); }
        artifact={id:randomUUID(),kind:'picture',name:current.name,version:0,parent:sealed.code};
        project.artifacts.push(artifact);project.selected=artifact.id;
        await this.commit(project,artifact,forkStage,{files:current.revision.files,preview:current.revision.preview,summary:`Started from #${sealed.code}`});
      }
      const adapter = await modules[current.kind]();
      if (!adapter.actions.some(a => a.name === action)) throw new Error(`Unknown ${current.kind} action: ${action}`);
      const stage = await safe(this.root, `jobs/${randomUUID()}`); await mkdir(stage, { recursive: true });
      for (const name of current.revision.files) { const dest = await safe(stage,name); await mkdir(dirname(dest),{recursive:true}); await copyFile(await safe(current.root,name),dest); }
      // Failed attempts remain in jobs, including any paid outputs/receipts.
      const result = await adapter.run({ root: stage, action, input: { ...input, ...(action === 'generate' ? { authorized: true, jobId: randomUUID() } : {}) } });
      return this.commit(project,artifact,stage,result);
    });
  }
  async versions() {
    const current = await this.selected(); if (!current) return [];
    const list = [];
    for (let v=1;v<=current.version;v++) list.push(JSON.parse(await readFile(await safe(this.root,`artifacts/${current.id}/v${v}/revision.json`),'utf8')));
    return list;
  }
  async rollback(version) {
    if (!Number.isSafeInteger(version) || version < 1) throw new Error('Choose a saved version number.');
    return this.locked(async () => {
      const current=await this.selected(); if (!current || version>current.version) throw new Error('Version not found.');
      if(current.kind==='picture' && (await pictureWipRecord(this,current.id))?.status==='done')throw new Error('This painting is Done. Start a new copy before restoring an earlier version.');
      const root=await safe(this.root,`artifacts/${current.id}/v${version}`), rev=JSON.parse(await readFile(await safe(root,'revision.json'),'utf8'));
      const stage=await safe(this.root,`jobs/${randomUUID()}`); await mkdir(stage,{recursive:true});
      for(const name of rev.files){const dest=await safe(stage,name);await mkdir(dirname(dest),{recursive:true}); const data=await readFile(await safe(root,name)); if(createHash('sha256').update(data).digest('hex')!==rev.hashes[name])throw new Error('Saved artifact hash mismatch.');await writeFile(dest,data);}
      const p=await this.read();return this.commit(p,p.artifacts.find(a=>a.id===current.id),stage,{files:rev.files,preview:rev.preview,status:rev.status,metadata:rev.metadata,sourceAhead:rev.sourceAhead,summary:`Restored v${version}`},version);
    });
  }
  async preview() {
    const current = await this.selected();
    if (!current?.revision.preview) throw new Error('This artifact has no preview yet.');
    const preview = current.revision.preview;
    const path = await safe(current.root, preview.path);
    const bytes = await readFile(path);
    if (createHash('sha256').update(bytes).digest('hex') !== current.revision.hashes[preview.path]) throw new Error('Artifact preview hash mismatch.');
    return { ...preview, path, artifactId: current.id, version: current.version };
  }
  async export(destination) {
    if (typeof destination !== 'string' || !destination.trim()) throw new Error('Use /export DESTINATION with a filename.');
    return this.locked(async () => {
      const current = await this.selected();
      if (!current) throw new Error('Select a media artifact before exporting.');
      const name = { picture: 'composite.png', sound: 'sound.wav', paper: 'manuscript.pdf', gameboy: 'game.gb' }[current.kind];
      if (!current.revision.files.includes(name)) throw new Error('Build or render this artifact before exporting it.');
      if (current.kind === 'paper') {
        const status = await (await modules.paper()).run({root:current.root,action:'status',input:{}});
        if(status.status !== 'ready') throw new Error('Paper export requires current visual and figure/table QA. /open shows the draft PDF.');
      }
      if (current.kind === 'gameboy') {
        const build = JSON.parse(await readFile(await safe(current.root, 'build.json'),'utf8'));
        if(build.sourceHash !== current.revision.hashes['main.c']) throw new Error('Build the current GameBoy source before exporting its ROM.');
      }
      const source = await safe(current.root, name);
      const bytes = await readFile(source);
      if (createHash('sha256').update(bytes).digest('hex') !== current.revision.hashes[name]) throw new Error('Artifact export hash mismatch.');
      const target = resolve(destination);
      const inside = relative(this.root, target);
      if (!inside || (!inside.startsWith('../') && inside !== '..' && !isAbsolute(inside))) throw new Error('Export outside the versioned artifact store.');
      // An explicit destination authorizes a new copy, never overwriting a user's file.
      await writeFile(target, bytes, {flag:'wx'});
      return { path: target, source: name, artifactId: current.id, version: current.version, bytes: bytes.length };
    });
  }
  async tools() {
    const current=await this.selected(); if(!current)return [];
    return (await modules[current.kind]()).actions.filter(a=>a.name!=='qa').map(a=>({name:`artifact_${a.name}`,description:a.description,input_schema:a.inputSchema}));
  }
  async context() {
    const current=await this.selected(); if(!current)return '';
    const names=current.revision.files.filter(n=>/\.(json|tex|bib|md|c|h|asm|s)$/.test(n) && !/provenance|analysis/.test(n));
    let text=`Current ${current.kind}: ${current.name}, v${current.version}. Use artifact tools to edit it. Do not edit piece source for this medium.\n`;
    if(current.kind==='picture')text+='Use artifact_draw for AC line, fill, shapes, and filters. Fill is a real connected-region flood fill. Use artifact_generate only when the user asks for remote image generation/editing. For image edits, use composite.png as reference; preserve their picture unless instructed otherwise. Each draw/generate previews the change; accept commits it.\n';
    for(const name of names){const data=await readFile(await safe(current.root,name),'utf8');text+=`\n${name}:\n${data.slice(0,16000)}\n`;if(text.length>40000)break;}
    return text;
  }
}
