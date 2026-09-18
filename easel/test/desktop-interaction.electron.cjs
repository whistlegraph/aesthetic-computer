// Real webview focus and native pointer resize, without a PTY or account.
const { app, BrowserWindow, ipcMain, webContents } = require("electron");
const assert = require("node:assert/strict"),
  path = require("node:path"),
  fs = require("node:fs");
const delay = (ms) => new Promise((resolve) => setTimeout(resolve, ms));
let win;
(async () => {
  await app.whenReady();
  const root = path.resolve(__dirname, "../desktop");
  const inputs = [],
    errors = [];
  ipcMain.on("input", (_event, text) => inputs.push(text));
  ipcMain.handle("native-prox-title", () => ({ glyphs: [] }));
  const theme = {
    background: "#152331",
    foreground: "#fffbea",
    cursor: "#eeaacc",
  };
  const start = Date.now();
  win = new BrowserWindow({
    show: false,
    width: 900,
    height: 650,
    backgroundColor: theme.background,
    webPreferences: {
      preload: path.join(root, "preload.cjs"),
      webviewTag: true,
      contextIsolation: true,
      sandbox: true,
      backgroundThrottling: false,
      additionalArguments: ["--aesel-initial-theme=" + JSON.stringify(theme)],
    },
  });
  win.webContents.on("console-message", (event) => {
    if (event.level === "error") errors.push(event.message);
  });
  const js = (code) => win.webContents.executeJavaScript(code);
  const painted = new Promise((resolve) => win.once("ready-to-show", resolve));
  await win.loadFile(path.join(root, "index.html"));
  await painted;
  assert.equal(
    await js(`getComputedStyle(document.body).backgroundColor`),
    "rgb(21, 35, 49)",
  );
  assert.equal(
    await js(`getComputedStyle(document.getElementById('terminal')).opacity`),
    "0",
  );
  assert.equal(
    await js(`document.getElementById('conversation').hidden`),
    false,
  );
  console.log(
    "Styled notebook ready in",
    Date.now() - start,
    "ms, before engine startup",
  );
  win.show();
  win.focus();
  await js(
    `window.updateConversation({entries:[{id:'u',kind:'user',text:'Make a piano.'}]});document.body.dataset.phase='ready';document.getElementById('artifact-shell').hidden=false;`,
  );
  win.webContents.send(
    "output",
    "\x1b]777;easel-prompt:" +
      JSON.stringify({
        text: "",
        cursor: 0,
        activity: "testing the keys",
        feedback: "Working",
        hidden: false,
      }) +
      "\x07",
  );
  await delay(100);
  assert.equal(
    await js(`document.getElementById('activity-caption').textContent`),
    "(testing the keys)",
  );
  assert.equal(
    await js(
      `getComputedStyle(document.getElementById('activity-caption')).display`,
    ),
    "inline-block",
  );
  fs.writeFileSync(
    "/tmp/aesel-thought-bubble.png",
    (await win.webContents.capturePage()).toPNG(),
  );
  const rect = () =>
    js(
      `(()=>{const r=document.getElementById('artifact-shell').getBoundingClientRect();return {x:r.x,y:r.y,width:r.width,height:r.height}})()`,
    );
  const before = await rect();
  win.webContents.sendInputEvent({
    type: "mouseMove",
    x: Math.round(before.x + 30),
    y: Math.round(before.y + 30),
  });
  await delay(250);
  assert.deepEqual(
    await rect(),
    before,
    "Hover must never enlarge the preview",
  );
  const grip = await js(
    `(()=>{const r=document.querySelector('[data-edge="sw"]').getBoundingClientRect();return {x:Math.round(r.x+r.width/2),y:Math.round(r.y+r.height/2)}})()`,
  );
  win.webContents.sendInputEvent({ type: "mouseMove", ...grip });
  win.webContents.sendInputEvent({
    type: "mouseDown",
    button: "left",
    clickCount: 1,
    ...grip,
  });
  win.webContents.sendInputEvent({
    type: "mouseMove",
    x: grip.x - 110,
    y: grip.y + 65,
    button: "left",
  });
  await delay(80);
  win.webContents.sendInputEvent({
    type: "mouseUp",
    button: "left",
    clickCount: 1,
    x: grip.x - 110,
    y: grip.y + 65,
  });
  await delay(100);
  const resized = await rect();
  assert(resized.width > before.width + 90);
  assert(resized.height > before.height + 45);
  win.webContents.sendInputEvent({ type: "mouseMove", x: 50, y: 400 });
  await delay(200);
  assert.deepEqual(await rect(), resized, "Leaving retains the resized bounds");
  await js(`document.querySelector('[data-edge="sw"]').focus()`);
  await delay(100);
  assert.deepEqual(await rect(), resized, "Focus does not zoom");
  const page =
    '<body tabindex="0"><canvas tabindex="0" width="300" height="180"></canvas><script>window.keys=[];addEventListener("keydown",e=>keys.push("down:"+e.key));addEventListener("keyup",e=>keys.push("up:"+e.key));</script></body>';
  const guestReady = new Promise((resolve) =>
    win.webContents.once("did-attach-webview", (_event, guest) =>
      guest.once("did-finish-load", () => resolve(guest)),
    ),
  );
  await js(
    `document.getElementById('piece').src=${JSON.stringify("data:text/html," + encodeURIComponent(page))}`,
  );
  const guest = await guestReady;
  await js(`document.getElementById('piece').focus()`);
  await delay(150);
  assert.equal(
    await js(`document.activeElement.id`),
    "piece",
    "Preview keeps focus instead of returning it to xterm",
  );
  const count = inputs.length;
  // Dispatch through Electron's actual focused WebContents, as native keys do.
  const recipient = webContents.getFocusedWebContents();
  assert.equal(
    recipient?.id,
    guest.id,
    "Native keyboard focus belongs to the guest",
  );
  recipient.sendInputEvent({ type: "keyDown", keyCode: "A" });
  recipient.sendInputEvent({ type: "keyUp", keyCode: "A" });
  await delay(100);
  assert.deepEqual(await guest.executeJavaScript("window.keys"), [
    "down:a",
    "up:a",
  ]);
  assert.equal(
    inputs.length,
    count,
    "Playing a note must not type into the prompt",
  );
  win.webContents.sendInputEvent({
    type: "mouseDown",
    button: "left",
    clickCount: 1,
    x: 55,
    y: 420,
  });
  win.webContents.sendInputEvent({
    type: "mouseUp",
    button: "left",
    clickCount: 1,
    x: 55,
    y: 420,
  });
  await delay(100);
  assert.equal(
    await js(
      `document.activeElement.classList.contains('xterm-helper-textarea')`,
    ),
    true,
    "Notebook click restores typing",
  );
  win.webContents.sendInputEvent({ type: "keyDown", keyCode: "B" });
  win.webContents.sendInputEvent({ type: "char", keyCode: "b" });
  win.webContents.sendInputEvent({ type: "keyUp", keyCode: "B" });
  await delay(100);
  assert(
    inputs.slice(count).some((text) => text.includes("b")),
    "Notebook receives prompt input",
  );
  await js(`window.currentPreviewMedium='picture';document.body.dataset.previewMedium='picture';window.setPreviewDimensions(96,144)`);
  const pictureBefore = await rect();
  await js(`document.querySelector('[data-edge="sw"]').dispatchEvent(new KeyboardEvent('keydown',{key:'ArrowLeft',bubbles:true,cancelable:true}))`);
  await delay(100);
  assert((await rect()).width > pictureBefore.width, 'Other media also retain manual resizing');
  assert.deepEqual(await js(`[getComputedStyle(document.getElementById('preview-viewport')).width,getComputedStyle(document.getElementById('preview-viewport')).height]`), ['96px','144px'], 'Resizing preserves image source pixels');
  assert.deepEqual(errors, []);
  console.log(
    "PASS thought bubble, stable hover, manual resize, guest keyboard and notebook focus",
  );
})()
  .catch((error) => {
    console.error(error);
    process.exitCode = 1;
  })
  .finally(() => {
    win?.destroy();
    app.quit();
  });
