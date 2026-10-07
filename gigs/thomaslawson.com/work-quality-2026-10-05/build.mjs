import {readFile,writeFile} from 'node:fs/promises';
const root=new URL('./',import.meta.url);
const [template,js,css]=await Promise.all(['quality.php','quality.js','quality.css'].map(name=>readFile(new URL(name,root),'utf8')));
await writeFile(new URL('zzz-tl-quality.php',root),template.replace('/*__CSS__*/',css).replace('/*__JS__*/',js));
