import test from 'node:test';
import assert from 'node:assert/strict';
import sharp from 'sharp';
import { objectMetadata } from '../backend/object-metadata.mjs';
import { objectPiece } from '../public/aesthetic.computer/lib/object/viewer.mjs';
import { captureObjectFrames } from '../../oven/object-preview.mjs';
const source='; Pink sculpture\n(ink 255 90 150) (slab -.2 0 -.1 .2 3 .1) (ink 80 120 255) (orb .4 1 0 .5 6)';

test('object links have an animated OG card and an independent PNG fallback',()=>{
 const m=objectMetadata('kx9',source);
 assert.equal(m.title,'Pink sculpture');
 const image=new URL(m.image_url), still=new URL(m.twitter_image_url);
 assert.equal(image.pathname,'/grab/gif/800/420/%24kx9');
 assert.equal(still.pathname,'/grab/png/1200/630/%24kx9');
 assert.equal(image.searchParams.get('view'),'object');
 assert.equal(image.searchParams.get('fps'),image.searchParams.get('playbackFps'));
 assert.equal(m.image_type,'image/gif');
 assert.equal(objectMetadata('abc','; (orb 0 0 0 1)\n(wipe red)'),null);
 assert.equal(objectMetadata('../private',source),null);
 assert.equal(objectMetadata('abc',undefined),null);
});

test('capture framing fits a complete rotation, without changing the interactive caption',()=>{
 const view={preview:true,time:0,yaw:.6};
 const piece=objectPiece(source,'$abc',view);
 const screen={width:400,height:210,pixels:new Uint8ClampedArray(400*210*4)};
 let captions=0;
 const api={screen,ink(){return api},write(){captions++;return api}};
 piece.boot(api);
 let first,opposite;
 for(let i=0;i<=32;i++){
  view.yaw=.6+i/32*Math.PI*2;piece.paint(api);
  const pixels=screen.pixels;
  let x0=400,y0=210,x1=0,y1=0;
  for(let y=0;y<210;y++)for(let x=0;x<400;x++){
   const o=(y*400+x)*4;
   if(pixels[o]!==20||pixels[o+1]!==23||pixels[o+2]!==32){x0=Math.min(x0,x);x1=Math.max(x1,x);y0=Math.min(y0,y);y1=Math.max(y1,y)}
  }
  assert.ok(x0>8&&x1<392&&y0>8&&y1<202,`clipped at angle ${i}`);
  assert.ok(y1-y0>125,'model should dominate the frame height');
  if(i===0)first=Buffer.from(pixels);
  if(i===16)opposite=Buffer.from(pixels);
 }
 assert.equal(captions,0);
 assert.notDeepEqual(first,opposite);
 assert.deepEqual(Buffer.from(screen.pixels),first,'the turn closes without a camera jump');
 const live=objectPiece(source);live.boot(api);live.paint(api);assert.equal(captions,1);
});

test('oven renders exact-size frames and rejects non-model captures',async t=>{
 let fetched='';
 t.mock.method(globalThis,'fetch',async url=>{fetched=url;return new Response(JSON.stringify({source}))});
 const frames=await captureObjectFrames('$kx9',{width:400,height:210,duration:1000,fps:8});
 assert.equal(frames.length,8);
 assert.match(fetched,/store-kidlisp\?code=kx9$/);
 const info=await sharp(frames[0]).metadata();assert.equal(info.width,400);assert.equal(info.height,210);
 assert.notDeepEqual(frames[0],frames[4]);
 await assert.rejects(captureObjectFrames('../secret'),/saved \$code/);
 await assert.rejects(captureObjectFrames('$kx9',{frames:10000}),/frame count/);
 t.mock.method(globalThis,'fetch',async()=>new Response(JSON.stringify({source:'(wipe red)'})));
 await assert.rejects(captureObjectFrames('$abc'),/not a 3D object/);
});
