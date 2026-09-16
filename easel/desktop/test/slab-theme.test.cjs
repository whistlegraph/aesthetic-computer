const {test}=require('node:test');const assert=require('node:assert/strict');
const {resolveTheme,FALLBACK,followSlabTheme}=require('../slab-theme.cjs');
const fs=require('node:fs'),os=require('node:os'),path=require('node:path');
const {execFileSync}=require('node:child_process');
const palette={background:[10,20,30],foreground:[210,220,230],bold:[240,245,250],cursor:[80,150,200]};
test('status uses the palette published by Slab, while retaining distinct accent colors',()=>{
 const doc={version:1,enabled:true,palettes:{working:palette,complete:{...palette,background:[100,20,30]}}};
 const working=resolveTheme(doc,'working'),complete=resolveTheme(doc,'complete');
 assert.equal(working.background,'#0a141e');assert.equal(working.white,'#d2dce6');assert.equal(working.cursor,'#5096c8');assert.notEqual(working.magenta,working.green);assert.equal(complete.background,'#64141e');
 assert.deepEqual(resolveTheme({...doc,enabled:false},'working'),FALLBACK);
 assert.deepEqual(resolveTheme({...doc,palettes:{working:{...palette,cursor:['bad',1,2]}}},'working'),FALLBACK);
});
test('atomic Slab changes update the current status without restarting',async()=>{
 const directory=fs.mkdtempSync(path.join(os.tmpdir(),'easel-theme-')),seen=[];
 const follower=followSlabTheme(t=>seen.push(t),{directory});
 try{
 follower.setStatus('working');
 fs.writeFileSync(path.join(directory,'next'),JSON.stringify({version:1,enabled:true,palettes:{working:palette}}));fs.renameSync(path.join(directory,'next'),path.join(directory,'theme.json'));
 for(let i=0;i<20&&seen.at(-1).background!== '#0a141e';i++)await new Promise(r=>setTimeout(r,10));
 assert.equal(seen.at(-1).background,'#0a141e');
 fs.unlinkSync(path.join(directory,'theme.json'));
 for(let i=0;i<20&&seen.at(-1).background!==FALLBACK.background;i++)await new Promise(r=>setTimeout(r,10));
 assert.equal(seen.at(-1).background,FALLBACK.background);
 }finally{follower.close();fs.rmSync(directory,{recursive:true,force:true});}
});
test('desktop frame uses remappable colors instead of fixed truecolor paint',()=>{
 const text=execFileSync(process.execPath,['--input-type=module','-e',`const {color}=await import(${JSON.stringify(path.resolve(__dirname,'../../src/render.mjs'))});console.log(JSON.stringify(color));`],{env:{...process.env,EASEL_THEME:'slab',EASEL_GROUND:'paint',COLORTERM:'truecolor'},encoding:'utf8'});
 const color=JSON.parse(text);assert.match(color.ground,/\x1b\[49m/);assert.equal(color.text,'\x1b[38;5;7m');assert(!text.includes('38;2;'));
});

test('light and dark Slab backgrounds retain readable multicolor semantic accents',()=>{
 const lum=color=>color.slice(1).match(/../g).map(c=>parseInt(c,16)/255).map(c=>c<=.04045?c/12.92:((c+.055)/1.055)**2.4).reduce((s,c,i)=>s+c*[.2126,.7152,.0722][i],0);
 for(const background of [[219,255,255],[255,177,171],[35,25,45],[128,128,128]]){
  const theme=resolveTheme({version:1,enabled:true,palettes:{blank:{...palette,background}}},'blank');
  const keys=['red','green','yellow','magenta','brightRed','brightYellow','brightMagenta'];
  assert.equal(new Set(keys.map(k=>theme[k])).size,keys.length);
  for(const key of keys){const a=lum(theme[key]),b=lum(theme.background);assert((Math.max(a,b)+.05)/(Math.min(a,b)+.05)>=4.5,`${key} on ${theme.background}`);}
 }
});
