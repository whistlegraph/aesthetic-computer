const {test}=require('node:test');const assert=require('node:assert/strict');
const fs=require('node:fs'),os=require('node:os'),path=require('node:path');
const {localPreview}=require('../local-preview.cjs');
test('only nominated regular media project files enter the renderer',async()=>{
 const root=fs.mkdtempSync(path.join(os.tmpdir(),'easel-preview-path-'));
 try{fs.mkdirSync(path.join(root,'.easel-media'));const good=path.join(root,'.easel-media','source.txt');fs.writeFileSync(good,'hello');
 assert.equal((await localPreview(root,{path:good,mime:'text/plain'})).text,'hello');
 const outside=path.join(root,'secret.txt');fs.writeFileSync(outside,'private');
 await assert.rejects(localPreview(root,{path:outside,mime:'text/plain'}),/inside/);
 const link=path.join(root,'.easel-media','escape.txt');fs.symlinkSync(outside,link);await assert.rejects(localPreview(root,{path:link,mime:'text/plain'}),/inside/);
 assert.equal(await localPreview(root,{path:good,mime:'text/html'}),null);
 }finally{fs.rmSync(root,{recursive:true,force:true});}
});
