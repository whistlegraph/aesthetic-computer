const { app, BrowserWindow, ipcMain } = require("electron");
const path = require("node:path"),
  assert = require("node:assert/strict");
const delay = (ms) => new Promise((r) => setTimeout(r, ms));
let win;
(async () => {
  await app.whenReady();
  ipcMain.handle("native-prox-title", () => null);
  win = new BrowserWindow({
    show: false,
    width: 700,
    height: 847,
    webPreferences: {
      preload: path.resolve("easel/desktop/preload.cjs"),
      contextIsolation: true,
      sandbox: true,
      backgroundThrottling: false,
    },
  });
  await win.loadFile(path.resolve("easel/desktop/index.html"));
  const js = (s) => win.webContents.executeJavaScript(s);
  win.webContents.send("state", {
    medium: "piece",
    piece: "scroll-check",
    version: 1,
    status: "complete",
    preview: {},
  });
  const entries = Array.from({ length: 14 }, (_, i) => ({
    id: String(i),
    kind: i % 2 ? "assistant" : "user",
    text:
      i % 2
        ? "The cow moves through the scrolling grass, bouncing and swishing his tail as the music plays."
        : "Give the cow a little techno.",
  }));
  await js(`window.updateConversation({entries:${JSON.stringify(entries)}})`);
  win.webContents.send(
    "output",
    "\x1b]777;easel-prompt:" +
      JSON.stringify({ text: "", cursor: 0, feedback: "", hidden: false }) +
      "\x07",
  );
  const snapshot = () =>
    js(
      `(()=>{const v=document.getElementById('conversation'),p=document.getElementById('prose-prompt').getBoundingClientRect();return {top:v.scrollTop,height:v.scrollHeight,gap:v.scrollHeight-v.scrollTop-v.clientHeight,promptBottom:p.bottom,bottom:v.getBoundingClientRect().bottom}})()`,
    );
  for (const [height, width] of [
    [847, 200],
    [420, 560],
    [340, 200],
    [340, 450],
    [650, 200],
  ]) {
    win.setSize(700, height);
    await delay(80);
    await js(
      `{const s=document.getElementById('artifact-shell');s.dataset.resized='true';for(const [k,v] of Object.entries({width:${width},height:240,right:14,top:14}))s.style.setProperty('--resized-preview-'+k,v+'px');}`,
    );
    await delay(80);
    await js(`document.getElementById('conversation').scrollTop=1e6`);
    const trace = [];
    for (let i = 0; i < 7; i++) {
      await delay(60);
      trace.push(await snapshot());
    }
    assert(
      Math.max(...trace.map((t) => t.top)) -
        Math.min(...trace.map((t) => t.top)) <=
        1,
      `scroll rebound: ${JSON.stringify(trace)}`,
    );
    assert(
      trace.at(-1).gap <= 2,
      `bottom detached: ${JSON.stringify(trace.at(-1))}`,
    );
    assert(trace.at(-1).promptBottom <= trace.at(-1).bottom + 1);
  }
  // A reader even one line above the bottom owns their scroll position.
  await js(
    `{const v=document.getElementById('conversation');v.scrollTop-=24;}`,
  );
  await delay(80);
  const before = await snapshot();
  entries.push({
    id: "next",
    kind: "assistant",
    text: "More music arrives without pulling your reading position down.",
  });
  await js(`window.updateConversation({entries:${JSON.stringify(entries)}})`);
  await delay(180);
  assert(
    Math.abs((await snapshot()).top - before.top) <= 1,
    "A new reply must not steal a reader’s scroll",
  );
  const original = await snapshot();
  await js(
    `document.getElementById('qr-label').style.transform='scale(1.7)';window.alignNotebookRuling()`,
  );
  await delay(120);
  assert.equal(
    (await snapshot()).height,
    original.height,
    "Title hover must not resize the scrolling paper",
  );
  console.log(
    "PASS: stable bottom edge, window/preview height changes, visible input, reading position, title hover.",
  );
})()
  .catch((e) => {
    console.error(e);
    process.exitCode = 1;
  })
  .finally(() => {
    win?.destroy();
    app.quit();
  });
