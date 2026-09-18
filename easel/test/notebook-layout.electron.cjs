// Run from the repo root with easel/desktop/node_modules/.bin/electron.
const { app, BrowserWindow, ipcMain } = require("electron");
const fs = require("node:fs");
const path = require("node:path");
const assert = require("node:assert/strict");
(async () => {
  await app.whenReady();
  const root = process.cwd();
  const errors = [];
  let opened = "";
  const inputs=[];ipcMain.on("input",(_event,text)=>inputs.push(text));
  ipcMain.handle("native-prox-title", (_, value) =>
    JSON.parse(
      require("node:child_process").execFileSync(
        path.join(root, "easel/desktop/native/credit-label"),
        [value.text, String(value.size), "--glyphs"],
        { encoding: "utf8" },
      ),
    ),
  );
  ipcMain.on("open-piece", (_, url) => (opened = url));
  const win = new BrowserWindow({
    show: false,
    width: 900,
    height: 650,
    webPreferences: {
      preload: path.join(root, "easel/desktop/preload.cjs"),
      contextIsolation: true,
      sandbox: true,
      backgroundThrottling: false,
    },
  });
  win.webContents.on("console-message", (_, level, message) => {
    if (level === 3) {
      errors.push(message);
      console.log("renderer:", message);
    }
  });
  await win.loadFile(path.join(root, "easel/desktop/index.html"));
  const delay = (ms) => new Promise((r) => setTimeout(r, ms));
  await delay(600);
  win.webContents.send("output", "\x1b]777;easel-phase:ready\x07");
  win.webContents.send("state", {
    medium: "piece",
    piece: "release-check",
    handle: "jeffrey",
    version: 3,
    status: "complete",
    url: "https://aesthetic.computer/blank",
  });
  await delay(300);
  await win.webContents.executeJavaScript(
    `window.updateConversation({entries:[{id:'status',kind:'notice',text:'REMOTE INFERENCE'}, {id:'u1',kind:'user',text:'Make a small moving circle.'},{id:'a1',kind:'assistant',text:'The circle follows your pointer.\\n\\nTry changing its **size** or **color**.'}]})`,
  );
  await delay(500);
  assert.equal(
    await win.webContents.executeJavaScript(
      `document.querySelectorAll('#notebook-page article').length`,
    ),
    2,
  );
  await win.webContents.executeJavaScript(
    `window.updateProviderFooter({backend:'ac',model:'Luna',status:'thinking',mode:'remote',activity:'Reading the piece',models:[],versions:[]});`,
  );
  win.webContents.send(
    "output",
    "\x1b]777;easel-prompt:" +
      JSON.stringify({
        text: "",
        cursor: 0,
        activity: "Reading the piece",
        feedback: "Thinking",
        hidden: false,
      }) +
      "\x07",
  );
  await delay(100);
  assert.equal(
    await win.webContents.executeJavaScript(
      `document.querySelector('#activity-caption')?.textContent||''`,
    ),
    "Reading the piece",
  );
  assert.equal(
    await win.webContents.executeJavaScript(
      `document.querySelector('#activity-caption').previousSibling.id`,
    ),
    "prompt-feedback",
  );
  assert.equal(
    await win.webContents.executeJavaScript(
      `document.querySelectorAll('#notebook-page article').length`,
    ),
    2,
  );
  // Real OSC token updates keep one text node, stay in the bubble, and remain
  // scrollable when a requested explanation grows beyond a short sentence.
  await win.webContents.executeJavaScript(`window.captionNode=document.getElementById('activity-text').firstChild`);
  const reply="I've made the circle smaller. ".repeat(20);
  for (const activity of ["I", "I've", "I've made", reply]) {
    win.webContents.send('output', '\x1b]777;easel-prompt:' + JSON.stringify({text:'',cursor:0,activity,feedback:'Receiving reply'}) + '\x07');
    await delay(50);
    assert.equal(await win.webContents.executeJavaScript(`document.getElementById('activity-caption').textContent`),activity);
    assert.equal(await win.webContents.executeJavaScript(`document.getElementById('activity-text').firstChild===window.captionNode`),true);
    assert.equal(await win.webContents.executeJavaScript(`document.querySelectorAll('#notebook-page article').length`),2);
  }
  assert.equal(await win.webContents.executeJavaScript(`(()=>{const t=document.getElementById('activity-text');return t.scrollHeight>t.clientHeight&&t.scrollHeight-t.clientHeight-t.scrollTop<3})()`),true);
  await win.webContents.executeJavaScript(`document.getElementById('activity-text').scrollTop=0`);
  win.webContents.send('output', '\x1b]777;easel-prompt:' + JSON.stringify({text:'',cursor:0,activity:reply+'More.',feedback:'Receiving reply'}) + '\x07');
  await delay(50);
  assert.equal(await win.webContents.executeJavaScript(`document.getElementById('activity-text').scrollTop`),0);
  win.webContents.send(
    "output",
    "\x1b]777;easel-prompt:" +
      JSON.stringify({
        text: "",
        cursor: 0,
        activity: "Stale activity",
        feedback: "",
        hidden: false,
      }) +
      "\x07",
  );
  await delay(60);
  assert.equal(
    await win.webContents.executeJavaScript(
      `document.querySelector('#activity-caption').hidden`,
    ),
    true,
  );
  assert.equal(
    await win.webContents.executeJavaScript(
      `document.querySelector('#activity-caption').textContent`,
    ),
    "",
  );
  win.webContents.send("credits", {
    text: "200,000 AC credits",
    description: "Balance",
    remaining: 200000,
    purchased: 1000000,
    dollars: { currency: "USD", free: 1, purchased: 5, total: 6 },
  });
  await delay(50);
  await win.webContents.executeJavaScript(
    `document.getElementById('credit-label').click()`,
  );
  assert.equal(
    await win.webContents.executeJavaScript(
      `document.querySelector('#provider-menu select[aria-label="Model"]')`,
    ),
    null,
  );
  assert.match(
    await win.webContents.executeJavaScript(
      `document.querySelector('.braincell-balance').textContent`,
    ),
    /\$6.00/,
  );
  assert.equal(
    await win.webContents.executeJavaScript(
      `document.querySelector('#provider-menu .inference-status').textContent`,
    ),
    "Remote inference · thinking · Reading the piece",
  );
  assert.deepEqual(await win.webContents.executeJavaScript(`Array.from(document.querySelectorAll('#provider-options [role=option]'),o=>o.textContent)`),['AC','Claude','Codex']);
  await win.webContents.executeJavaScript(`window.providerDropdown=document.querySelector('button[aria-label="Provider"]');providerDropdown.click();window.updateProviderFooter({backend:'ac',model:'Luna',status:'ready',mode:'remote',activity:'A new status',models:[],versions:[]})`);
  assert.equal(await win.webContents.executeJavaScript(`document.querySelector('button[aria-label="Provider"]')===window.providerDropdown`),true,'Status updates preserve the open dropdown');
  await win.webContents.executeJavaScript(`document.querySelectorAll('#provider-options [role=option]')[2].click()`);
  await delay(40);assert(inputs.includes('\x1b[99;2~'));
  await win.webContents.executeJavaScript(`window.updateProviderFooter({backend:'claude',model:'sonnet',selectedModel:'sonnet',models:[{id:'sonnet',label:'Sonnet'},{id:'opus',label:'Opus'}],versions:[]});document.getElementById('credit-label').click();const m=document.querySelector('select[aria-label="Model"]');m.value='1';m.dispatchEvent(new Event('change'))`);
  await delay(40);assert(inputs.includes('\x1b[99;4;1;1~'));
  assert.equal(await win.webContents.executeJavaScript(`(()=>{const a=document.querySelector('.provider-toggle').getBoundingClientRect(),b=document.querySelector('select[aria-label="Model"]').getBoundingClientRect();return Math.abs(a.top-b.top)<2&&b.left>a.right})()`),true);
  assert.equal(await win.webContents.executeJavaScript(`document.querySelectorAll('#provider-options [role=option] img').length`),3);
  await win.webContents.executeJavaScript(
    `document.querySelector('#provider-menu header button').click()`,
  );
  await win.webContents.executeJavaScript(
    `document.getElementById('qr-label').click()`,
  );
  await delay(80);
  assert.equal(opened, "https://aesthetic.computer/blank");
  for (const [name, w, h] of [
    ["normal", 900, 650],
    ["compact", 720, 540],
  ]) {
    win.setSize(w, h);
    await delay(350);
    const geometry = await win.webContents.executeJavaScript(
      `(()=>{const r=document.getElementById('conversation').getBoundingClientRect();return {width:r.width,bottom:r.bottom,height:innerHeight,text:document.getElementById('notebook-page').innerText}})()`,
    );
    assert(geometry.width > 200 && geometry.bottom <= geometry.height + 1);
    assert(geometry.text.includes("circle follows"));
  }
  await win.webContents.executeJavaScript(
    `window.updateConversation({entries:[{id:'long',kind:'assistant',text:${JSON.stringify("The circle moves through the purple sky and follows your pointer. ".repeat(90))}}]});document.getElementById('conversation').scrollTop=0`,
  );
  await delay(350);
  for (const [name, scroll] of [
    ["wrap", 0],
    ["scroll", 220],
    ["bottom", 1800],
    ["expanded", 0],
    ["resized", 160],
  ]) {
    if (name === "expanded")
      await win.webContents.executeJavaScript(
        `{const shell=document.getElementById('artifact-shell');shell.dataset.resized='true';for(const [key,value] of Object.entries({width:540,height:364,right:14,top:14}))shell.style.setProperty('--resized-preview-'+key,value+'px')}`,
      );
    if (name === "resized")
      await win.webContents.executeJavaScript(
        `{const shell=document.getElementById('artifact-shell');shell.dataset.resized='true';for(const [key,value] of Object.entries({width:280,height:210,right:30,top:70}))shell.style.setProperty('--resized-preview-'+key,value+'px')}`,
      );
    await win.webContents.executeJavaScript(
      `document.getElementById('conversation').scrollTop=${scroll}`,
    );
    await delay(250);
    const measure = await win.webContents.executeJavaScript(
      `(()=>{const box=document.getElementById('artifact-shell').getBoundingClientRect();const node=document.querySelector('article');const walk=document.createTreeWalker(node,NodeFilter.SHOW_TEXT);let overlaps=0,beside=0,below=0,text;while(text=walk.nextNode()){const range=document.createRange();for(let i=0;i<text.length;i++){if(!text.textContent[i].trim())continue;range.setStart(text,i);range.setEnd(text,i+1);const r=range.getBoundingClientRect();if(r.top<box.bottom&&r.bottom>box.top){if(r.right>box.left&&r.left<box.right)overlaps++;else beside++;}if(r.top>box.bottom&&r.right>box.left)below++;}}return {overlaps,beside,below,scroll:document.getElementById('conversation').scrollTop,box:box.toJSON()}})()`,
    );
    console.log(name, measure);
    assert.equal(measure.overlaps, 0);
    if (name !== "expanded") assert(measure.beside > 0);
    assert(measure.below > 0);
  }
  await win.webContents.executeJavaScript(
    `document.getElementById('artifact-shell').hidden=true`,
  );
  await delay(80);
  assert.equal(
    await win.webContents.executeJavaScript(
      `document.getElementById('notebook-preview-space').hidden`,
    ),
    true,
  );
  const hover = await win.webContents.executeJavaScript(
    `(()=>{const el=document.getElementById('credit-label'),s=getComputedStyle(el);return {origin:s.transformOrigin,right:s.right,width:el.offsetWidth,height:el.offsetHeight}})()`,
  );
  assert.equal(hover.right, "8px");
  assert.equal(hover.origin, hover.width / 2 + "px " + hover.height / 2 + "px");
  assert(hover.width > 15);
  await win.webContents.executeJavaScript(
    `document.getElementById('credit-label').dispatchEvent(new PointerEvent('pointerenter'))`,
  );
  await delay(850);
  console.log(
    "hover",
    await win.webContents.executeJavaScript(
      `(()=>{const e=document.getElementById('credit-label'),r=e.getBoundingClientRect(),s=getComputedStyle(e);return {right:r.right,viewport:innerWidth,transform:s.transform,origin:s.transformOrigin}})()`,
    ),
  );
  assert.deepEqual(errors, []);
  console.log(
    "PASS notebook text, title navigation, compact layout, no renderer errors",
  );
  win.destroy();
  app.quit();
})().catch((e) => {
  console.error(e);
  app.exit(1);
});
