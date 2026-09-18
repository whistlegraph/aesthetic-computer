// Optional real-browser review: electron test/donkey-gallery-electron.cjs
// Writes fixture-only PNG contact sheets to /tmp, never opens the user's app.
const {app,BrowserWindow}=require('electron');
const {join,resolve}=require('node:path');const {writeFileSync,mkdtempSync,rmSync}=require('node:fs');const {tmpdir}=require('node:os');const assert=require('node:assert/strict');
const outputDir=process.env.EASEL_GALLERY_OUTPUT||'/tmp';
const privateDir=mkdtempSync(join(tmpdir(),'easel-gallery-electron-'));app.setPath('userData',privateDir);
app.whenReady().then(async()=>{
 const window=new BrowserWindow({show:false,width:1320,height:860,webPreferences:{sandbox:true,contextIsolation:true,nodeIntegration:false}});
 try{
  await window.loadFile(resolve(__dirname,'../desktop/donkey-gallery.html'),{query:{capture:'1'}});
  const info=await window.webContents.executeJavaScript(`(async()=>{for(let i=0;i<100;i++){if(window.donkeyGallery)return {ready:donkeyGallery.ready,error:donkeyGallery.error,count:donkeyGallery.count,ids:donkeyGallery.ids};await new Promise(r=>setTimeout(r,50));}throw Error('Gallery did not load');})()`);
  assert.equal(info.ready,true,info.error);assert.equal(info.count,128);assert.equal(new Set(info.ids).size,128);
  const files=[];
  for(let page=0;page<4;page++){
   const result=await window.webContents.executeJavaScript(`(()=>{donkeyGallery.setPage(${page});return {png:donkeyGallery.exportPage(),ids:[...document.querySelectorAll('.clip')].map(n=>n.dataset.action)};})()`);
   assert.equal(result.ids.length,32);assert.deepEqual(result.ids,info.ids.slice(page*32,(page+1)*32));
   const file=join(outputDir,`easel-donkey-actions-${page+1}.png`);writeFileSync(file,Buffer.from(result.png.split(',')[1],'base64'));files.push(file);
   await window.webContents.capturePage().then(image=>writeFileSync(join(outputDir,`easel-donkey-gallery-${page+1}.png`),image.toPNG()));
  }
  const result=await window.webContents.executeJavaScript(`(()=>{document.getElementById('state').value='sleeping';document.getElementById('state').dispatchEvent(new Event('change'));return document.getElementById('page').textContent;})()`);assert.match(result,/actions/);
  writeFileSync(join(outputDir,'easel-donkey-actions-index.html'),'<!doctype html><title>aesel — 128 action choreographies</title><style>body{margin:0;background:#241d35}img{display:block;max-width:100%;margin:auto}</style>'+files.map((file,i)=>`<img src="${file}" alt="aesel actions ${i*32+1} through ${(i+1)*32}">`).join(''));
  console.log(JSON.stringify({actions:128,pages:files,index:join(outputDir,'easel-donkey-actions-index.html')}));
 }finally{window.destroy();}
}).then(()=>app.exit(0)).catch(error=>{console.error(error.stack);app.exit(1);});
app.on('will-quit',()=>{try{rmSync(privateDir,{recursive:true,force:true});}catch{}});
