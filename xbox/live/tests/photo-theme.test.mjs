import test from 'node:test';
import assert from 'node:assert/strict';
import createPhotoTheme from '../photo-theme.mjs';
function fixture(failure='') {
  const calls=[];
  const context=new Proxy({canvas:{width:1920,height:1080}, getTransform:()=>({a:1,d:1})}, {
    get:(obj,key)=>obj[key] || ((...args)=>calls.push([key,...args])),
  });
  class Image {
    naturalWidth=2000; naturalHeight=2000;
    decode=async()=>{};
    set src(value) { queueMicrotask(()=>value.includes(failure) && failure ? this.onerror() : this.onload()); }
  }
  return {calls,theme:createPhotoTheme(context,()=>calls.push(['flush']),{ImageImpl:Image})};
}
test('textures become ready once decoded, with independent optional effects', async()=>{
  const {theme}=fixture('weapons');
  assert.equal(theme.themeReady(),false);
  await theme.ready;
  assert.equal(theme.themeReady(),true);
  assert.equal(theme.themeAssetReady(3),false);
});
test('a missing core atlas leaves the vector fallback available',async()=>{
  const {theme}=fixture('props');await theme.ready;
  assert.equal(theme.themeReady(),false);
  assert.equal(theme.themeSprite(1,0,0,10,10,5,5,10,10),false);
});
test('sprites flush pending geometry, preserve facing and restore the canvas',async()=>{
  const {theme,calls}=fixture();await theme.ready;
  assert.equal(theme.themeSprite(1,80,104,285,285,100,200,60,70,.4,true),true);
  assert.deepEqual(calls.slice(0,5),[['flush'],['save'],['translate',100,200],['rotate',.4],['scale',-1,1]]);
  assert.equal(calls.at(-1)[0],'restore');
  assert.equal(theme.themeSprite(1,1995,0,10,10,0,0,10,10),false);
  assert.equal(theme.themeSprite(1,0,0,10,10,NaN,0,10,10),false);
});
test('a projected rectangle maps its atlas crop into both triangles',async()=>{
  const {theme,calls}=fixture();await theme.ready;
  assert.equal(theme.themeQuad(1,0,0,100,50,10,20,0,210,20,0,210,120,0,10,120,0),true);
  const transforms=calls.filter(c=>c[0]==='transform');
  assert.deepEqual(transforms,[['transform',2,0,0,2,10,20],['transform',2,0,0,2,10,20]]);
});

test('lighting caches highlights once and adds no pass outside a light', async()=>{
  const saved = globalThis.OffscreenCanvas;
  let cached = 0;
  globalThis.OffscreenCanvas = class {
    constructor(width,height) { this.width=width;this.height=height;cached++; }
    getContext() { return {drawImage(){},fillRect(){}}; }
  };
  try {
    const {theme,calls}=fixture(); await theme.ready;
    const count=cached;
    assert.equal(count,2, 'only props and weapons get a highlight atlas');
    theme.themeLighting([{x:100,y:200,radius:100,strength:1}]);
    theme.themeSprite(1,80,104,285,285,100,200,60,70);
    assert.equal(calls.filter(c=>c[0]==='drawImage').length,2);
    calls.length=0;
    theme.themeSprite(1,80,104,285,285,1000,200,60,70);
    assert.equal(calls.filter(c=>c[0]==='drawImage').length,1);
    assert.equal(cached,count,'rendering never allocates another highlight canvas');
    calls.length=0;theme.themeLighting([]);
    theme.themeSprite(1,80,104,285,285,100,200,60,70);
    assert.equal(calls.filter(c=>c[0]==='drawImage').length,1);
  } finally {
    if (saved === undefined) delete globalThis.OffscreenCanvas;
    else globalThis.OffscreenCanvas=saved;
  }
});
