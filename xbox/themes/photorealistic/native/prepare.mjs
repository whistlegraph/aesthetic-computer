// Deterministic format/size conversion of the approved generated masters.
import {spawnSync} from 'node:child_process';
import {readFile,writeFile} from 'node:fs/promises';
import {createHash} from 'node:crypto';
import {fileURLToPath} from 'node:url';
import path from 'node:path';
const root=path.dirname(fileURLToPath(import.meta.url));
const assets=[{id:0,name:'underpass',size:[1024,576],master:[1672,941]},
 {id:1,name:'props',size:[1024,512],master:[1774,887]},
 {id:2,name:'explosions-v1',size:[1024,512],master:[1774,887],blend:'straight-alpha',depthWrite:false},
 {id:3,name:'weapons-v2',size:[1024,1024],master:[1254,1254],blend:'straight-alpha',depthWrite:true}];
for(const a of assets){
 const source=path.join(root,'../assets',a.name+'.png');
 a.file=`${a.name}-${a.size[0]}x${a.size[1]}.rgba`;
 const result=spawnSync('ffmpeg',['-hide_banner','-loglevel','error','-y','-i',source,
  '-vf',`scale=${a.size[0]}:${a.size[1]}`,'-f','rawvideo','-pix_fmt','rgba',path.join(root,a.file)],{stdio:'inherit'});
 if(result.status!==0)throw Error(`ffmpeg failed for ${a.name}`);
 const bytes=await readFile(path.join(root,a.file));
 if(bytes.length!==a.size[0]*a.size[1]*4)throw Error('Invalid RGBA byte count');
 a.bytes=bytes.length;a.sha256=createHash('sha256').update(bytes).digest('hex');
 a.masterSha256=createHash('sha256').update(await readFile(source)).digest('hex');
}
await writeFile(path.join(root,'manifest.json'),JSON.stringify({format:'RGBA8',alpha:'straight',
 sourceCoordinates:'master pixels',textureUpload:'once at host initialization',assets},null,2)+'\n');
