import {readFile,writeFile} from 'node:fs/promises';
import {compileProject} from '../../system/public/doom-player/project.mjs';
import {lollipopProject} from './project-fixture.mjs';
const root=new URL('../../system/public/doom-player/',import.meta.url),p=lollipopProject();
const base=new Uint8Array(await readFile(new URL('vendor/freedoom1.wad',root)));
await writeFile(new URL('examples/lollipop-world-full.json',root),JSON.stringify(p,null,2)+'\n');
const wad=await compileProject(p,{base});await writeFile(new URL('examples/lollipop-world-full.wad',root),wad);
console.log('Built '+wad.length+' bytes with '+p.maps[0].rooms.length+' sectors and custom sprites/textures');
