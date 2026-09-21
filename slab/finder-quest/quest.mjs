import { mkdtemp, mkdir, writeFile, readdir, readFile, lstat } from 'node:fs/promises';
import { join, relative } from 'node:path';
import { createHash, randomInt } from 'node:crypto';

export const hash = bytes => createHash('sha256').update(bytes).digest('hex');
function sound(index) {
  const samples = 4000, b = Buffer.alloc(44 + samples * 2);
  b.write('RIFF'); b.writeUInt32LE(b.length - 8, 4); b.write('WAVEfmt ', 8);
  b.writeUInt32LE(16, 16); b.writeUInt16LE(1, 20); b.writeUInt16LE(1, 22);
  b.writeUInt32LE(8000, 24); b.writeUInt32LE(16000, 28); b.writeUInt16LE(2, 32); b.writeUInt16LE(16, 34);
  b.write('data', 36); b.writeUInt32LE(samples * 2, 40);
  for (let i = 0; i < samples; i++) b.writeInt16LE(Math.round(Math.sin(i * 2 * Math.PI * (220 + index * 55) / 8000) * 4000 * (1 - i / samples)), 44 + i * 2);
  return b;
}
export async function createQuest(parent) {
  const root = await mkdtemp(join(parent, 'Finder Quest '));
  const id = root.slice(-6);
  for (const dir of ['Pictures', 'Notes', 'Audio', 'Loose', 'More stuff']) await mkdir(join(root, dir));
  const names = ['orbit', 'meadow', 'comet', 'tide'];
  const files = [];
  for (let i = 0; i < 12; i++) {
    const type = i % 3, download = i >= 9;
    const category = ['Pictures', 'Notes', 'Audio'][type];
    const extension = ['svg', 'txt', 'wav'][type];
    const name = `quest-${id}-${names[Math.floor(i / 3)]}.${extension}`;
    const content = type === 0
      ? Buffer.from(`<svg xmlns="http://www.w3.org/2000/svg" width="400" height="300"><rect width="400" height="300" fill="#e8eef6"/><circle cx="200" cy="140" r="${55 + i * 3}" fill="${['#3577a8','#b65676','#659050','#ad783b'][Math.floor(i / 3)]}"/><text x="200" y="265" text-anchor="middle" font-family="sans-serif" font-size="24">${names[Math.floor(i / 3)]}</text></svg>`)
      : type === 1 ? Buffer.from(`Finder Quest ${id}\n${names[Math.floor(i / 3)]} field notes\nA generated practice file. Sort this into Notes.\n`) : sound(i);
    const initial = download ? null : join(['', 'Loose', 'More stuff'][randomInt(3)], name);
    if (initial) await writeFile(join(root, initial), content);
    files.push({ id: String(i), name, category, initial, download, hash: hash(content), content });
  }
  return { id, root, files, uploaded: new Set(), startedAt: null, completedAt: null };
}

// Only traverse this generated game directory. Symlinks are never followed.
export async function scanQuest(quest, downloads) {
  const found = new Map();
  async function walk(dir, depth = 0) {
    if (depth > 8) return;
    for (const entry of await readdir(dir, { withFileTypes: true })) {
      const path = join(dir, entry.name);
      if (entry.isDirectory()) await walk(path, depth + 1);
      else if (entry.isFile()) {
        const expected = quest.files.find(f => f.name === entry.name);
        if (!expected) continue;
        const stat = await lstat(path);
        const valid = stat.isFile() && stat.size === expected.content.length && hash(await readFile(path)) === expected.hash;
        const list = found.get(entry.name) || [];
        list.push({ path: relative(quest.root, path), valid }); found.set(entry.name, list);
      }
    }
  }
  await walk(quest.root);
  let downloaded = [];
  if (downloads) try { downloaded = (await readdir(downloads, { withFileTypes:true })).filter(e=>e.isFile()).map(e=>e.name); } catch {}
  const files = await Promise.all(quest.files.map(async file => {
    const locations = found.get(file.name) || [];
    const destination = join(file.category, file.name);
    const dot = file.name.lastIndexOf('.'), base = file.name.slice(0,dot), extension = file.name.slice(dot);
    const copies = file.download ? downloaded.filter(name => name === file.name ||
      (name.startsWith(base+' (') && name.endsWith(')'+extension) && /^\d+$/.test(name.slice(base.length+2,-extension.length-1)))) : [];
    const inDownloads = copies.length > 0;
    const sorted = locations.length === 1 && locations[0].path === destination && locations[0].valid && !inDownloads;
    return { id: file.id, name: file.name, category: file.category, download: file.download,
      locations, inDownloads, downloadCopies: copies.length, sorted, uploaded: quest.uploaded.has(file.id) };
  }));
  const sorted = files.filter(f => f.sorted).length;
  const returned = files.filter(f => f.download && f.uploaded && f.sorted).length;
  const complete = sorted === files.length && returned === 3;
  if (complete && quest.startedAt && !quest.completedAt) quest.completedAt = Date.now();
  return { id: quest.id, root: quest.root, startedAt: quest.startedAt, completedAt: complete ? quest.completedAt : null,
    sorted, returned, complete, files };
}
