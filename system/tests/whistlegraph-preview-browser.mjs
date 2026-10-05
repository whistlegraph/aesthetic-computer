import assert from 'node:assert/strict';
import { createServer } from 'node:http';
import sharp from 'sharp';
import { renderWhistlegraphPreview } from '../../oven/whistlegraph-preview.mjs';
let leaks = 0;
const server = createServer((req,res) => { leaks++; res.end('private'); });
await new Promise(resolve => server.listen(0,'127.0.0.1',resolve));
try {
  const html = `<!doctype html><html><head></head><body style="margin:0"><canvas></canvas><script>
    const c=document.querySelector('canvas');c.width=innerWidth;c.height=innerHeight;const ctx=c.getContext('2d');
    function draw(t){ctx.fillStyle='purple';ctx.fillRect(0,0,c.width,c.height);ctx.fillStyle='green';ctx.fillRect((t/8)%c.width,10,60,200);requestAnimationFrame(draw)}requestAnimationFrame(draw);
    fetch('http://127.0.0.1:${server.address().port}/private').catch(()=>{});
    </script></body></html>`;
  const preview = await renderWhistlegraphPreview(html,'2:3', { executablePath:process.env.PUPPETEER_EXECUTABLE_PATH });
  const gif = await sharp(preview.gif,{animated:true}).metadata();
  assert.equal(gif.pages,48);assert.equal(gif.width,320);assert.equal(gif.pageHeight,480);assert.equal(gif.loop,0);
  const first=await sharp(preview.gif,{page:0}).raw().toBuffer();
  const later=await sharp(preview.gif,{page:24}).raw().toBuffer();
  assert.notDeepEqual(first,later,'GIF captures live artwork motion');
  const thumb=await sharp(preview.thumbnail).metadata();assert.equal(thumb.format,'png');assert.equal(thumb.height,350);
  assert.equal(leaks,0,'packed code cannot access server network');
  console.log('PASS: animated GIF, matching portrait thumbnail, isolated rendering');
} finally { await new Promise(resolve => server.close(resolve)); }
