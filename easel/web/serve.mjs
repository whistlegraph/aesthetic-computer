// Serve only the public Aesel site. No tokens, repository files or dev endpoints.
import { createServer } from 'node:http';
import { readFile } from 'node:fs/promises';
import { resolve, extname, sep } from 'node:path';
import { fileURLToPath } from 'node:url';
const root=fileURLToPath(new URL('../../system/public/aesel/',import.meta.url));
const types={'.html':'text/html','.js':'text/javascript','.css':'text/css','.json':'application/json','.png':'image/png','.woff':'font/woff'};
createServer(async(req,res)=>{
  try {
    let path=decodeURIComponent(new URL(req.url,'http://localhost').pathname);
    if(path.endsWith('/')) path+='index.html';
    const file=resolve(root,'.'+path);
    if(!file.startsWith(resolve(root)+sep)) {res.writeHead(403).end();return;}
    const data=await readFile(file);
    res.writeHead(200,{'Content-Type':types[extname(file)] || 'application/octet-stream','Cache-Control':'no-store'}).end(data);
  }catch {res.writeHead(404).end('Not found');}
}).listen(Number(process.env.PORT || 8771),'127.0.0.1',()=>console.log('Aesel web → http://localhost:8771/try/'));
