import {readFile,writeFile,copyFile,cp,mkdir} from 'node:fs/promises';
const base=new URL('./',import.meta.url),root=new URL('../../',base);
await copyFile(new URL('shared/roblox-room.mjs',root),new URL('Resources/Web/room-schema.mjs',base));
await writeFile(new URL('Resources/Web/roblox-runtime.mjs',base),'export const ROOM_RUNTIME = '+JSON.stringify(await readFile(new URL('roblox/rooms/src/server/Room.luau',root),'utf8'))+';\n');
let html=await readFile(new URL('Resources/Web/shell.html',base),'utf8');
const host=await readFile(new URL('aesel/phone/host.html',root),'utf8');
const importMap=host.match(/<script type="importmap">[\s\S]*?<\/script>/)[0];
html=html.replace('<head>', '<head><script src="/easel/phone/shim/globals.js"></script>'+importMap);
html=html.replace('</body></html>','<script src="native.js"></script><script type="module" src="ware-engine.mjs"></script></body></html>');
await writeFile(new URL('Resources/Web/index.html',base),html);
await copyFile(new URL('system/public/papers.aesthetic.computer/foundry/fonts/ComicRelief-Regular.ttf',root),new URL('Resources/Web/ComicRelief-Regular.ttf',base));

for(const tree of ["src","phone","context"]) {
 await mkdir(new URL("Resources/Web/easel/",base),{recursive:true});
 await cp(new URL(`aesel/${tree}/`,root),new URL(`Resources/Web/easel/${tree}/`,base),{recursive:true});
}

await copyFile(new URL('apple/aesel/Resources/ComicRelief-Bold.ttf',root),new URL('Resources/Web/ComicRelief-Bold.ttf',base));

await copyFile(new URL('system/public/aesthetic.computer/lib/canvas-tape.mjs',root),new URL('Resources/Web/canvas-tape.mjs',base));
