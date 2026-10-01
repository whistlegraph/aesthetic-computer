// Builds a Windows-local copy of the shared browser UI/session engine.
import {mkdir,readFile,writeFile,copyFile} from 'node:fs/promises';
import {resolve,dirname} from 'node:path';
import {fileURLToPath} from 'node:url';
import {execFileSync} from 'node:child_process';
const here=dirname(fileURLToPath(import.meta.url)), root=resolve(here,'../..');
const out=resolve(here,'www');
execFileSync(process.execPath,[resolve(root,'aesel/web/build.mjs'),resolve(out,'try')],{stdio:'inherit'});
await copyFile(resolve(here,'auth0.js'),resolve(out,'try/auth0.js'));
for(const name of ['icon.png','pixel.woff']) await copyFile(resolve(root,'system/public/aesel',name),resolve(out,name));
let html=await readFile(resolve(out,'try/index.html'),'utf8');
html=html.replace('href="/"','href="https://aesel.app/"')
  .replace('href="/privacy.html"','href="https://aesel.app/privacy.html"')
  .replace('Conversations and revisions are shared with authorized AC staff.','Conversation context goes through AC to its inference provider. Notebooks stay on this PC.')
  .replace('Saved pieces in this browser','Saved pieces on this PC')
  .replace('Drafts stay in this browser.','Drafts stay on this PC.');
await writeFile(resolve(out,'try/index.html'),html);
// Windows ICO can contain an existing PNG; no redraw or icon re-encoding.
const png=await readFile(resolve(root,'apple/aesel/Resources/Assets.xcassets/AppIcon.appiconset/mac-256.png'));
const ico=Buffer.alloc(22);ico.writeUInt16LE(1,2);ico.writeUInt16LE(1,4);ico.writeUInt16LE(1,10);ico.writeUInt16LE(32,12);ico.writeUInt32LE(png.length,14);ico.writeUInt32LE(22,18);
await mkdir(resolve(here,'assets'),{recursive:true});
await writeFile(resolve(here,'assets/aesel.ico'),Buffer.concat([ico,png]));
