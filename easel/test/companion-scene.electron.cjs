// EASEL_DESKTOP=1 COLORTERM=truecolor electron easel/test/companion-scene.electron.cjs
const {app,BrowserWindow,ipcMain}=require('electron');const fs=require('node:fs');
(async()=>{await app.whenReady();const root=require('node:path').resolve(__dirname,'../..');const {qrcode,ErrorCorrectLevel}=await import(root+'/easel/src/vendor/qr.mjs');const {renderFrame}=await import(root+'/easel/src/render.mjs');const {FrameDiff}=await import(root+'/easel/src/frame-diff.mjs');const diff=new FrameDiff({clearOnResize:false});const url='https://aesthetic.computer/watch/?id=c41318cc97a9a587c52bfba3917efcdf';const matrix=qrcode(url,{errorCorrectLevel:ErrorCorrectLevel.L}).modules;
const win=new BrowserWindow({show:false,width:900,height:650,webPreferences:{preload:root+'/easel/desktop/preload.cjs',contextIsolation:true,sandbox:true,backgroundThrottling:false}});let cols=100,rows=35;
const state={entries:[{kind:'user',text:'Let’s make a tiny world.'},{kind:'assistant',text:'Scan the side code to watch this piece take shape.'}],input:'',account:'@jeffrey',piece:'duputo.mjs',pieceVersion:9,providerSettings:{backend:'codex',model:'gpt-6-astra',effort:'high'},status:'working'};
const draw=()=>win.webContents.send('output',diff.update(renderFrame(state,cols,rows,true),cols));ipcMain.on('size',(_,size)=>{cols=size.cols;rows=size.rows;draw()});
await win.loadFile(root+'/easel/desktop/index.html');const js=code=>win.webContents.executeJavaScript(code),delay=ms=>new Promise(r=>setTimeout(r,ms));await delay(500);win.webContents.send('output','\x1b]777;easel-phase:ready\x07');win.webContents.send('state',{medium:'picture',piece:'duputo',proxName:'sum',handle:'jeffrey',version:9,status:'working',url,qr:matrix,preview:{artifactId:'test',version:9}});await delay(500);draw();
const assert=require('node:assert/strict');
assert.equal(await js(`CompanionScene.layout().width`),112);
assert.equal(await js(`getComputedStyle(document.getElementById('qr-card')).display`),'none');
win.webContents.sendInputEvent({type:'mouseMove',x:40,y:25});await delay(250);
assert.equal(await js(`getComputedStyle(document.getElementById('qr-card')).display`),'flex');
assert.equal(await js(`document.getElementById('qr-label').textContent`),'@jeffrey/duputo');
assert.ok(await js(`document.querySelector('.xterm-screen').getBoundingClientRect().top>document.getElementById('qr-label').getBoundingClientRect().bottom`));
assert.ok(await js(`document.querySelector('.xterm-screen').getBoundingClientRect().bottom<=innerHeight`));
console.log('PASS donkey-only geometry, name-hover QR replacement, title and transcript spacing');

for(const [name,action] of [['normal','reset'],['small','smaller'],['tiny','smaller']]){
 win.webContents.send('text-size',action);await delay(180);const png=await js(`document.getElementById('aesel-donkey').toDataURL()`);fs.writeFileSync('/tmp/easel-composite-'+name+'.png',Buffer.from(png.split(',')[1],'base64'));fs.writeFileSync('/tmp/easel-scene-'+name+'.png',(await win.webContents.capturePage()).toPNG());
 console.log(name,await js(`JSON.stringify((()=>{const c=document.getElementById('aesel-donkey'),q=document.getElementById('qr-card'),r=q.getBoundingClientRect(),a=c.getBoundingClientRect();return {canvas:[c.width,c.height],visible:[a.width,a.height],qr:[r.x,r.y,r.width,r.height],nameSize:getComputedStyle(document.getElementById('qr-label')).fontSize}})())`));
}
win.webContents.sendInputEvent({type:'mouseMove',x:450,y:300});await delay(100);assert.equal(await js(`getComputedStyle(document.getElementById('qr-card')).display`),'none');assert.equal(await js(`getComputedStyle(document.getElementById('preview-viewport')).visibility`),'visible');
win.webContents.send('text-size','reset');await delay(200);
win.webContents.sendInputEvent({type:'mouseMove',x:850,y:55});await delay(350);
const hover=await js(`(()=>{const qr=document.getElementById('donkey-qr-card'),q=qr.getBoundingClientRect(),p=document.getElementById('artifact-shell').getBoundingClientRect(),d=document.getElementById('aesel-donkey').getBoundingClientRect();return {display:getComputedStyle(qr).display,size:q.width,top:q.top,bottom:q.bottom,previewBottom:p.bottom,donkeyTop:d.top}})()`);
assert.equal(hover.display,'block');assert.ok(hover.size>=123);assert.ok(hover.top>=hover.previewBottom);assert.ok(hover.bottom<=hover.donkeyTop);
fs.writeFileSync('/tmp/easel-preview-hover-qr.png',(await win.webContents.capturePage()).toPNG());console.log('PASS large QR above donkey clears preview',hover);
win.destroy();app.quit();})().catch(e=>{console.error(e);app.exit(1)});
