// Real-device pipeline, including transparent paper over Rax and WebGL fallback.
import assert from 'node:assert/strict';
import { readFile } from 'node:fs/promises';
import { chromium } from 'playwright';

const browser = await chromium.launch({ channel: 'chrome', headless: true });
try {
  for (const fallback of [false, true]) {
    const page = await browser.newPage({deviceScaleFactor: 1});
    if (fallback) await page.addInitScript(() => Object.defineProperty(navigator, 'gpu', {value: undefined}));
    await page.route('https://paper.test/**', async route => {
      const path = new URL(route.request().url()).pathname;
      if (path === '/') return route.fulfill({contentType: 'text/html', body: '<body></body>'});
      const file = new URL('../public' + path, import.meta.url);
      return route.fulfill({contentType: /\.(m?js)$/.test(path) ? 'text/javascript' : 'text/plain', body: await readFile(file)});
    });
    await page.goto('https://paper.test');
    const report = await page.evaluate(async () => {
      const glaze = await import('/aesthetic.computer/lib/paper-glaze.mjs');
      const source = document.createElement('canvas');
      source.width = 48; source.height = 32;
      const ink = source.getContext('2d');
      ink.fillStyle = '#fcf7c5'; ink.fillRect(0,0,24,32);
      ink.fillStyle = '#281e5a'; ink.fillRect(3,2,12,7);
      ink.fillStyle = '#d54382'; ink.fillRect(31,22,14,6);
      const output = document.createElement('canvas');
      output.width = 144; output.height = 96;
      const read = output.getContext('2d', {willReadFrequently:true});
      const size = {width:144, height:96};
      const results = [];
      for (const type of ['prompt', 'prompt-webgpu']) {
        let ready = false;
        await glaze.on(48,32,size,144,96,document.body,type,()=>{ready=true;});
        if (!ready) await new Promise((resolve,reject) => {
          const until=performance.now()+5000;
          function check() { if(ready) resolve(); else if(performance.now()>until) reject(Error('glaze startup')); else setTimeout(check,20); }
          check();
        });
        glaze.update(source);
        const canvas = glaze.getCan();
        const get = (day) => {
          glaze.render(1000, {x:0,y:0}, day);
          read.clearRect(0,0,144,96);
          read.drawImage(canvas,0,0);
          return read.getImageData(0,0,144,96).data;
        };
        const day=get(true), night=get(false);
        const frozen=glaze.freeze(read);
        const again=read.getImageData(0,0,144,96).data;
        const alpha=(x,y)=>day[(144*y+x)*4+3];
        results.push({backend: canvas.dataset.backend || 'webgl2', day, night, alpha:[alpha(5,5),alpha(124,8),alpha(110,74)],
          frozen: frozen && again.some((v,i)=>i%4===3 && v===255)});
        glaze.off();
      }
      const [gl,gpu]=results;
      function error(key) {
        let sum=0, count=0;
        for(let i=0;i<gl[key].length;i++) if(i%4===3 || gl[key][i-(i%4)+3]===255) {
          sum+=Math.abs(gl[key][i]-gpu[key][i]); count++;
        }
        return sum/count;
      }
      return {backend:gpu.backend,alpha:gpu.alpha,frozen:gpu.frozen,day:error('day'),night:error('night')};
    });
    console.log(report);
    assert.equal(report.backend, fallback ? 'webgl2' : 'webgpu');
    assert.deepEqual(report.alpha,[255,0,255]);
    assert.ok(report.frozen && report.day < 2 && report.night < 3);
    await page.close();
  }
} finally { await browser.close(); }
