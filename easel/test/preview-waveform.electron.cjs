// Actual Web Audio samples cross the guest boundary into a quiet notebook line.
const { app, BrowserWindow, ipcMain } = require("electron");
const fs = require("node:fs"),
  path = require("node:path"),
  assert = require("node:assert/strict");
const delay = (ms) => new Promise((r) => setTimeout(r, ms));
app.commandLine.appendSwitch("autoplay-policy", "no-user-gesture-required");
let win;
(async () => {
  await app.whenReady();
  ipcMain.handle("native-prox-title", () => null);
  win = new BrowserWindow({
    show: false,
    width: 900,
    height: 650,
    webPreferences: {
      preload: path.resolve("easel/desktop/preload.cjs"),
      contextIsolation: true,
      sandbox: true,
      webviewTag: true,
      backgroundThrottling: false,
    },
  });
  await win.loadFile(path.resolve("easel/desktop/index.html"));
  const js = (code) => win.webContents.executeJavaScript(code);
  win.webContents.send("state", {
    medium: "piece",
    piece: "waveform",
    version: 1,
    status: "working",
    preview: {},
  });
  win.webContents.send("theme", {
    background: "#142131",
    foreground: "#fffbea",
    cursor: "#eeaacc",
  });
  await js(
    `window.updateConversation({entries:[{id:'u',kind:'user',text:'Give the cow a little techno.'}]})`,
  );
  win.webContents.send(
    "output",
    "\x1b]777;easel-prompt:" +
      JSON.stringify({
        text: "",
        cursor: 0,
        feedback: "Working",
        activity:
          "listening to the techno while the cow runs through the field",
        hidden: false,
      }) +
      "\x07",
  );
  const attached = new Promise((r) =>
    win.webContents.once("did-attach-webview", (_, guest) =>
      guest.once("did-finish-load", () => r(guest)),
    ),
  );
  await js(
    `document.getElementById('piece').src='data:text/html,<body style="background:%23334255">';`,
  );
  const guest = await attached;
  const meter = fs
    .readFileSync(
      "system/public/aesthetic.computer/lib/output-waveform.mjs",
      "utf8",
    )
    .replace("export function", "function");
  await guest.executeJavaScript(
    `${meter}\nwindow.ctx=new AudioContext();window.gain=ctx.createGain();gain.gain.value=.12;window.quiet=ctx.createGain();quiet.gain.value=0;gain.connect(quiet).connect(ctx.destination);window.osc=ctx.createOscillator();osc.frequency.value=220;osc.connect(gain);osc.start();window.AC={readOutputWaveform:createOutputWaveform(()=>[gain]),setMasterVolume:v=>{gain.gain.value=.12*v;return v}};ctx.resume();`,
  );
  await js(`window.previewAudio.set({volume:1,muted:false})`);
  await delay(550);
  assert.equal(
    await js(
      `document.getElementById('preview-waveform').classList.contains('sounding')`,
    ),
    true,
  );
  const waveform = await js(
    `document.querySelector('#preview-waveform path').getAttribute('d')`,
  );
  assert(waveform.includes("L"), "A sampled waveform, not a canned animation");
  const points = [...waveform.matchAll(/[ML][\d.]+ ([\d.]+)/g)].map(
    (m) => +m[1],
  );
  assert(Math.max(...points) - Math.min(...points) > 5);
  assert.equal(
    await js(
      `getComputedStyle(document.getElementById('preview-waveform')).pointerEvents`,
    ),
    "none",
  );
  assert.equal(
    await js(
      `document.getElementById('preview-waveform').getAttribute('aria-hidden')`,
    ),
    "true",
  );
  await js(`document.getElementById('piece').dispatchEvent(new Event('did-start-loading'));document.getElementById('piece').dispatchEvent(new Event('did-stop-loading'))`);await delay(250);
  assert.equal(await js(`document.getElementById('preview-waveform').classList.contains('sounding')`),true,'Resume polling after a load without another dom-ready');
  assert.equal(await js(`(()=>{const w=document.getElementById('preview-waveform').getBoundingClientRect();return Math.abs(w.right-(innerWidth-2))<2&&w.width===16&&Math.abs(w.height-innerHeight)<2})()`),true,'Waveform runs vertically at the right edge');
  await js(`(()=>{window.updateProviderFooter({backend:'ac',model:'Luna',busy:true,models:[],versions:[]});document.getElementById('credit-label').click();const v=document.querySelector('input[aria-label="Preview volume"]');v.value='50';v.dispatchEvent(new Event('input'))})()`);
  await delay(100);assert(Math.abs(await guest.executeJavaScript('gain.gain.value')-.06)<.001);
  await js(`document.querySelector('.preview-mute').click()`);await delay(250);
  assert.equal(guest.isAudioMuted(),true);
  assert.equal(await js(`document.getElementById('preview-waveform').classList.contains('sounding')`),false);
  await js(`(()=>{document.querySelector('.preview-mute').click();const v=document.querySelector('input[aria-label="Preview volume"]');v.value='100';v.dispatchEvent(new Event('input'));document.querySelector('#provider-menu header button').click()})()`);await delay(300);
  assert.equal(guest.isAudioMuted(),false);
  assert(Math.abs(await guest.executeJavaScript('gain.gain.value')-.12)<.001);
  for (const width of [380, 900]) {
    win.setSize(width, 650);
    await delay(220);
    const row = await js(
      `(()=>{const a=document.getElementById('prompt-feedback').getBoundingClientRect(),b=document.getElementById('activity-caption').getBoundingClientRect(),g=document.getElementById('notebook-activity').getBoundingClientRect();return {raised:b.top+b.height/2<a.top+a.height/2,beside:b.left>=a.right,width:g.width,available:innerWidth,donkey:getComputedStyle(document.getElementById('notebook-thinking-donkey')).translate,bubble:getComputedStyle(document.getElementById('activity-caption')).translate}})()`,
    );
    assert(row.raised && row.beside, JSON.stringify(row));
    assert(row.width <= row.available);
    assert.equal(
      row.donkey,
      row.bubble,
      "Donkey and bubble share their bounce",
    );
  }
  await delay(200);
  fs.writeFileSync(
    "/tmp/aesel-waveform-bubble.png",
    (await win.webContents.capturePage()).toPNG(),
  );
  guest.setAudioMuted(true);
  await delay(250);
  assert.equal(
    await js(
      `document.getElementById('preview-waveform').classList.contains('sounding')`,
    ),
    false,
  );
  guest.setAudioMuted(false);
  await guest.executeJavaScript("gain.gain.value=0");
  await delay(900);
  assert.equal(
    await js(
      `document.getElementById('preview-waveform').classList.contains('sounding')`,
    ),
    false,
    "Silence hides the line",
  );
  await guest.executeJavaScript("gain.gain.value=.12");
  await delay(300);
  assert.equal(
    await js(
      `document.getElementById('preview-waveform').classList.contains('sounding')`,
    ),
    true,
  );
  await js(`document.getElementById('piece').src='data:text/html,quiet'`);
  await delay(150);
  assert.equal(
    await js(
      `document.getElementById('preview-waveform').classList.contains('sounding')`,
    ),
    false,
    "Navigation clears old audio",
  );
  win.webContents.debugger.attach("1.3");
  await win.webContents.debugger.sendCommand("Emulation.setEmulatedMedia", {
    features: [{ name: "prefers-reduced-motion", value: "reduce" }],
  });
  await delay(180);
  assert.equal(
    await js(
      `getComputedStyle(document.getElementById('activity-caption')).translate`,
    ),
    "0px",
  );
  console.log(
    "PASS: live audio, silence, mute, navigation, narrow thought-bubble row, shared bounce, reduced motion.",
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
