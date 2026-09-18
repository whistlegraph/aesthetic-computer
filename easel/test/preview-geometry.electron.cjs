// Run with: env -u ELECTRON_RUN_AS_NODE easel/desktop/node_modules/.bin/electron easel/test/preview-geometry.electron.cjs
const {app,BrowserWindow}=require('electron');
const fs=require('node:fs/promises'),os=require('node:os'),path=require('node:path'),assert=require('node:assert/strict');
let win,temporary;
const delay=ms=>new Promise(r=>setTimeout(r,ms));
(async()=>{
 await app.whenReady(); temporary=await fs.mkdtemp(path.join(os.tmpdir(),'easel-preview-test-'));
 const preload=path.join(temporary,'preload.cjs');
 await fs.writeFile(preload,`const {contextBridge}=require('electron');const callbacks={};const api={};for(const name of ['Theme','Display','Notice','TextSize','Font','Output','SelectAll','Paste','FullscreenState','PreviewMode','State','InstanceLabel','NativeTitle','SystemTextSize','Credits','BuildStatus'])api['on'+name]=fn=>callbacks[name]=fn;for(const name of ['size','input','ready','contextMenu','copyText','requestPaste','fullscreen','openPiece'])api[name]=()=>{};api.renderProxTitle=async()=>null;contextBridge.exposeInMainWorld('aesel',api);contextBridge.exposeInMainWorld('previewTest',{emit:(name,data)=>callbacks[name]?.(data)});`);
 win=new BrowserWindow({show:false,width:900,height:700,webPreferences:{preload,contextIsolation:true,sandbox:true,backgroundThrottling:false}});
 win.webContents.on('console-message', (_event, level, message) => { if(level >= 2) console.error('renderer:',message); });
 await win.loadFile(path.resolve(__dirname,'../desktop/index.html'));
 const js=code=>win.webContents.executeJavaScript(code);
 for(let i=0;i<50&&!await js('!!window.setPreviewDimensions');i++)await delay(50);
 await js(`previewTest.emit('Display',{width:1920,height:1080});document.getElementById('artifact-shell').hidden=false;window.setPreviewDimensions(320,640);`);
 await delay(250);
 const measure=()=>js(`(()=>{const v=document.getElementById('preview-viewport'),r=v.getBoundingClientRect(),a=document.getElementById('artifact').getBoundingClientRect();return {w:v.clientWidth,h:v.clientHeight,visibleW:r.width,visibleH:r.height,boxW:a.width-2,boxH:a.height-2};})()`);
 let compact=await measure();assert.equal(compact.w,320);assert.equal(compact.h,640);assert.ok(Math.abs(compact.boxW/compact.boxH-.5)<.001);
 const hoverPoint=await js(`(()=>{const r=document.getElementById('artifact').getBoundingClientRect();return {x:Math.round(r.right-10),y:Math.round(r.top+10)}})()`);
 win.webContents.sendInputEvent({type:'mouseMove',...hoverPoint});await delay(300);
 let hover=await measure();assert.equal(hover.visibleW,compact.visibleW);assert.equal(hover.w,320);assert.equal(hover.h,640);assert.ok(Math.abs(hover.visibleW/hover.visibleH-.5)<.001);
 await delay(50);
 let pinned=await measure();assert.equal(pinned.w,320);assert.equal(pinned.h,640);
 await js(`previewTest.emit('FullscreenState',{app:true,preview:true})`);await delay(250);
 let full=await measure();assert.equal(full.w,320);assert.equal(full.h,640);assert.ok(Math.abs(full.visibleW/full.visibleH-.5)<.001);
 await js(`previewTest.emit('FullscreenState',{app:false,preview:false});window.setPreviewDimensions(640,320)`);await delay(250);
 let landscape=await measure();assert.equal(landscape.w,640);assert.equal(landscape.h,320);assert.ok(Math.abs(landscape.boxW/landscape.boxH-2)<.001);
 await js(`(()=>{const canvas=document.createElement('canvas');canvas.width=96;canvas.height=144;const c=canvas.getContext('2d');c.fillStyle='#ff64ff';c.fillRect(0,0,96,144);c.fillStyle='#fff';c.fillRect(0,0,8,8);c.fillRect(88,136,8,8);window.renderMediaPreview({medium:'picture',preview:{artifactId:'test',version:1},localPreview:{mime:'image/png',data:canvas.toDataURL()}});})()`);await delay(300);
 const picture=await measure();assert.equal(picture.w,96);assert.equal(picture.h,144);assert.ok(Math.abs(picture.boxW/picture.boxH-2/3)<.001);
 await fs.writeFile(path.join(os.tmpdir(),'easel-preview-geometry.png'),(await win.webContents.capturePage()).toPNG());
 await js(`(()=>{const canvas=document.createElement('canvas');canvas.width=128;canvas.height=128;const c=canvas.getContext('2d');c.fillStyle='#fff';c.fillRect(0,0,128,128);c.strokeStyle='#000';c.strokeRect(24,24,80,80);c.fillStyle='#ffff00';c.fillRect(25,25,78,78);previewTest.emit('State',{medium:'picture',piece:'Fill test',version:7,status:'ready',url:'aesthetic.computer/#abc',qr:[[true,false,true],[false,true,false],[true,false,true]],preview:{artifactId:'fixture',version:1,publicCode:'abc'},localPreview:{mime:'image/png',data:canvas.toDataURL()}});})()`);
 await delay(250);
 assert.equal(await js(`document.getElementById('qr-label').textContent.trim()`),'Fill test');
 await js(`previewTest.emit('Output','\\x1b]777;easel-phase:ready\\x07')`);await delay(50);
 await js(`document.getElementById('qr-label').dispatchEvent(new PointerEvent('pointerenter'))`);
 const layout=await js(`(()=>{const rect=id=>{const r=document.getElementById(id).getBoundingClientRect();return {x:r.x,y:r.y,right:r.right,bottom:r.bottom}};return {name:rect('qr-label'),tv:rect('artifact'),qr:rect('qr-card'),version:rect('version'),w:innerWidth,h:innerHeight}})()`);
 assert.ok(layout.name.x<30&&layout.name.y<30);
 assert.ok(layout.tv.right>layout.w-30&&layout.tv.y<30);
 assert.equal(await js(`getComputedStyle(document.getElementById('qr-card')).display`),'none');
 assert.equal(await js(`getComputedStyle(document.getElementById('version')).display`),'block');
 assert.equal(await js(`document.getElementById('version').textContent`),'v7');

 assert.equal(await js(`document.getElementById('piece').hidden`),true);
 const qrWidth=()=>js(`document.getElementById('qr').getBoundingClientRect().width`);
 const qrBefore=await qrWidth();await js(`previewTest.emit('TextSize','smaller')`);await delay(100);
 assert.ok(await qrWidth()<=qrBefore,'Cmd-minus must not enlarge the QR');
 await js(`previewTest.emit('TextSize','larger')`);await delay(100);
 assert.equal(await qrWidth(),qrBefore,'Cmd-plus must restore QR scale');

 await fs.writeFile(path.join(os.tmpdir(),'easel-picture-wip.png'),(await win.webContents.capturePage()).toPNG());
 // Real window resizing must retain the artifact's logical pixels and avoid
 // FitAddon's explicit render-service clear (the source of blank resize frames).
 await js(`FitAddon.FitAddon.prototype.fit=()=>{throw new Error('Clearing fit path used during resize')};window.resizeErrors=[];window.addEventListener('error',e=>window.resizeErrors.push(e.message));`);
 for(const [width,height] of [[700,540],[740,570],[780,600],[820,630],[900,700]]){win.setSize(width,height);await delay(35);}
 await delay(220);
 const resized=await measure();assert.equal(resized.w,128);assert.equal(resized.h,128);assert.ok(Math.abs(resized.visibleW/resized.visibleH-1)<.001);
 assert.deepEqual(await js('window.resizeErrors'),[]);
 assert.equal(await js(`document.body.classList.contains('window-resizing')`),false);
 console.log('PASS: preview geometry, short WIP code, picture canvas, and live window resize without forced canvas clears.');
})().catch(error=>{console.error(error);process.exitCode=1;}).finally(async()=>{win?.destroy();if(temporary)await fs.rm(temporary,{recursive:true,force:true});app.quit();});
