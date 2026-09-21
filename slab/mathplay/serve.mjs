import {createServer} from 'node:http';
import {readFile} from 'node:fs/promises';
const port=7783;
createServer(async(req,res)=>{
  if(req.headers.host!==`127.0.0.1:${port}`){res.writeHead(403);res.end();return;}
  const path=req.url==='/'?'index.html':req.url==='/game.mjs'?'game.mjs':null;
  if(!path){res.writeHead(404);res.end();return;}
  try{res.writeHead(200,{'Content-Type':path.endsWith('.html')?'text/html; charset=utf-8':'text/javascript','Cache-Control':'no-store'});res.end(await readFile(new URL(path,import.meta.url)));}
  catch{res.end();}
}).listen(port,'127.0.0.1',()=>console.log(`Math sprint: http://127.0.0.1:${port}`));
