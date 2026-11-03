const {
  app,
  BrowserWindow,
  BaseWindow,
  WebContentsView,
  Menu,
  session,
  screen,
  ipcMain,
} = require('electron');
const { randomUUID } = require('node:crypto');
const path = require('node:path');

const windows = new Set();

// The desk companion floats above everything; a full-screen game hides it
// and puts it back when the game leaves full screen or closes.
function companions(visible) {
  if (process.platform !== 'darwin') return;
  const { execFile } = require('node:child_process');
  execFile(
    'osascript',
    [
      '-e',
      `tell application "System Events" to if exists process "MacPal" then set visible of process "MacPal" to ${visible}`,
    ],
    () => {},
  );
}
function setFull(win, on) {
  if (process.platform === 'darwin') win.setSimpleFullScreen(on);
  else win.setFullScreen(on);
  win.doolFull = on;
  companions(!on);
}
let accountSession;
async function localAccountToken() {
  accountSession ||= import('../aesel/src/ac-session.mjs').then(
    ({ ACSession }) => new ACSession(),
  );
  return (await accountSession).token();
}

function gameURL(
  base = process.env.DUEL_URL || 'https://aesthetic.computer/dool/',
) {
  const url = new URL(base);
  const local = ['localhost', '127.0.0.1', '[::1]'].includes(url.hostname);
  if (
    url.username ||
    url.password ||
    !(url.protocol === 'https:' || (local && url.protocol === 'http:'))
  ) {
    throw new Error(
      'The dool rig needs HTTPS or a loopback development server.',
    );
  }
  if (!url.pathname.endsWith('/')) url.pathname += '/';
  const room = url.searchParams.get('room');
  url.search = new URLSearchParams({
    room:
      room && /^[a-z0-9-]{1,32}$/.test(room)
        ? room
        : `rig-${randomUUID().replaceAll('-', '').slice(0, 24)}`,
  });
  url.hash = '';
  return url;
}

function openDoolRig() {
  const url = gameURL();
  const count = process.env.DOOL_CLIENTS === '2' ? 2 : 1;
  const area = screen.getPrimaryDisplay().workAreaSize;
  const win = new BrowserWindow({
    title: count === 1 ? 'dool' : 'dool · two-client test',
    width: Math.min(1500, area.width),
    height: Math.min(860, area.height),
    minWidth: 320,
    minHeight: 220,
    backgroundColor: '#181818',
    show: false,
    webPreferences: {
      nodeIntegration: false,
      contextIsolation: true,
      sandbox: true,
    },
  });
  win.loadURL('data:text/html,<html style="background:%23181818"></html>');
  windows.add(win);
  const clients = Array.from({ length: count }, (_, index) => {
    // Separate player stores retain each identity and object ownership between launches.
    const store = session.fromPartition(`persist:dool-rig-player-${index}`);
    // The native shell attaches the existing AC session only to our game socket.
    // Tokens never enter renderer JavaScript, URLs, room state, or another host.
    if (
      ['localhost', '127.0.0.1', '[::1]', 'aesthetic.computer'].includes(
        url.hostname,
      )
    ) {
      const socket = new URL('play', url);
      socket.protocol = url.protocol === 'https:' ? 'wss:' : 'ws:';
      store.webRequest.onBeforeSendHeaders(
        { urls: [socket.href + '*'] },
        async (details, callback) => {
          const headers = { ...details.requestHeaders };
          try {
            headers.Authorization = `Bearer ${await localAccountToken()}`;
          } catch {}
          callback({ requestHeaders: headers });
        },
      );
    }
    const permissionAllowed = (contents, permission, origin) => {
      try {
        return (
          permission === 'pointerLock' &&
          new URL(origin).origin === url.origin &&
          new URL(contents.getURL()).origin === url.origin
        );
      } catch {
        return false;
      }
    };
    store.setPermissionCheckHandler((contents, permission, origin) =>
      permissionAllowed(contents, permission, origin),
    );
    store.setPermissionRequestHandler(
      (contents, permission, callback, details) => {
        callback(
          permissionAllowed(
            contents,
            permission,
            details.requestingUrl || contents.getURL(),
          ),
        );
      },
    );
    const view = new WebContentsView({
      webPreferences: {
        session: store,
        preload: path.join(__dirname, 'dool-preload.js'),
        nodeIntegration: false,
        contextIsolation: true,
        sandbox: true,
        backgroundThrottling: false,
      },
    });
    win.contentView.addChildView(view);
    view.setBackgroundColor('#c0c0c0');
    // Chromium views do not provide the browser window's Escape-to-unlock UI.
    view.webContents.on('before-input-event', (event, input) => {
      if (input.type === 'keyDown' && input.key === 'Escape') {
        view.webContents
          .executeJavaScript('document.exitPointerLock()')
          .catch(() => {});
      }
    });
    view.webContents.on('dom-ready', () => {
      view.webContents
        .executeJavaScript(
          `
        document.addEventListener('keydown', event => {
          if (event.key === 'Escape') document.exitPointerLock();
        }, true);
      `,
        )
        .catch(() => {});
    });
    view.webContents.setWindowOpenHandler(() => ({ action: 'deny' }));
    view.webContents.on('will-navigate', (event, destination) => {
      const next = new URL(destination);
      if (next.origin !== url.origin || next.pathname !== url.pathname)
        event.preventDefault();
    });
    return view;
  });
  function layout() {
    const [width, height] = win.getContentSize(),
      half = Math.floor((width - 1) / 2);
    if (clients.length === 1) {
      clients[0].setBounds({ x: 0, y: 0, width, height });
      return;
    }
    clients[0].setBounds({ x: 0, y: 0, width: half, height });
    clients[1].setBounds({
      x: half + 1,
      y: 0,
      width: width - half - 1,
      height,
    });
  }
  win.on('resize', layout);
  win.on('closed', () => {
    windows.delete(win);
    for (const view of clients)
      if (!view.webContents.isDestroyed()) view.webContents.close();
  });
  win.doolClients = clients;
  win.doolRoom = url.searchParams.get('room');
  win.doolReload = () =>
    clients.forEach((view) => view.webContents.reloadIgnoringCache());
  win.doolFocus = async (index) => {
    await Promise.all(
      clients.map((view) =>
        view.webContents
          .executeJavaScript('document.exitPointerLock()')
          .catch(() => {}),
      ),
    );
    if (!win.isDestroyed()) clients[index % clients.length].webContents.focus();
  };
  layout();
  win.center();
  win.show();
  if (process.env.DOOL_FULLSCREEN === '1') setFull(win, true);
  win.on('closed', () => {
    if (win.doolFull) companions(true);
  });
  app.focus({ steal: true });
  win.focus();
  clients[0].webContents.once('did-finish-load', () =>
    clients[0].webContents.focus(),
  );
  // Leave a trail in the launcher's log: page errors, crashes and reloads.
  for (const [i, view] of clients.entries()) {
    const log = (...a) => console.log(new Date().toISOString(), `dool[${i}]`, ...a);
    view.webContents.on('console-message', (event, level, message) => {
      const text = typeof event?.message === 'string' ? event.message : message;
      const lvl = typeof event?.level === 'string' ? event.level : level;
      if (lvl === 'error' || lvl === 3) log('console error:', text);
      else if (lvl === 'warning' || lvl === 2 || /webgpu|webgl|device/i.test(String(text))) log('console:', text);
    });
    view.webContents.on('render-process-gone', (_e, d) => {
      log('renderer gone:', d.reason, d.exitCode);
      if (!win.isDestroyed()) view.webContents.reload();
    });
    view.webContents.on('unresponsive', () => log('unresponsive'));
    // A load that fails (the server mid-restart) retries instead of staying blank.
    view.webContents.on('did-fail-load', (_e, code, desc, url, isMain) => {
      if (!isMain || code === -3) return;
      log('load failed:', code, desc, '- retrying');
      setTimeout(() => !win.isDestroyed() && view.webContents.loadURL(url).catch(() => {}), 1500);
    });
    view.webContents.on('did-start-loading', () => log('loading'));
  }
  for (const view of clients)
    view.webContents.loadURL(url.href).catch((error) => {
      if (!win.isDestroyed())
        win.setTitle(
          `dool · could not load game: ${error.code || error.message}`,
        );
    });
  return win;
}

function launchDoolRig() {
  const senderWindow = (sender) =>
    [...windows].find((win) =>
      win.doolClients?.some((v) => v.webContents === sender),
    );
  ipcMain.on('dool-rig:fullscreen', (event) => {
    const win = senderWindow(event.sender);
    if (!win) return;
    // Stay on the current macOS desktop so both views retain controller focus.
    setFull(win, !win.doolFull);
  });
  // A fresh process instead of an in-page reload: on some Macs a reloaded
  // WebGPU page comes back black, while a relaunch always draws.
  // A fresh renderer process (and GPU context) for the page, in the same
  // window: the render-process-gone handler reloads it. A full relaunch lost
  // the launch environment.
  ipcMain.on('dool-rig:restart', (event) => {
    console.log(new Date().toISOString(), 'dool restart (new renderer)');
    event.sender.forcefullyCrashRenderer();
  });
  // A controller pressed while another app is in front brings the game back.
  ipcMain.on('dool-rig:claim-focus', (event) => {
    const win = senderWindow(event.sender);
    if (!win || win.isDestroyed()) return;
    app.focus({ steal: true });
    win.focus();
    event.sender.focus();
  });
  ipcMain.on('dool-rig:focus-other', (event) => {
    const win = senderWindow(event.sender);
    if (win)
      void win.doolFocus(
        1 - win.doolClients.findIndex((v) => v.webContents === event.sender),
      );
  });
  // This launch path is a local testing mode: no regular app services, account
  // store, updater, launch analytics, tray or debug-listening port are started.
  app.setPath(
    'userData',
    process.env.DOOL_RIG_PROFILE ||
      path.join(app.getPath('appData'), 'Aesthetic Computer Dool Rig'),
  );
  if (!app.requestSingleInstanceLock()) {
    app.quit();
    return;
  }
  app.on('second-instance', () => {
    app.whenReady().then(openDoolRig);
  });
  app.whenReady().then(() => {
    Menu.setApplicationMenu(
      Menu.buildFromTemplate([
        ...(process.platform === 'darwin'
          ? [{ label: 'Aesthetic.Computer', submenu: [{ role: 'quit' }] }]
          : []),
        {
          label: 'Test',
          submenu: [
            {
              label: 'New Dool Rig',
              accelerator: 'CmdOrCtrl+N',
              click: openDoolRig,
            },
            {
              label: process.env.DOOL_CLIENTS === '2' ? 'Reload Both Clients' : 'Reload Game',
              accelerator: 'CmdOrCtrl+R',
              click: () => BaseWindow.getFocusedWindow()?.doolReload?.(),
            },
            { type: 'separator' },
            ...(process.env.DOOL_CLIENTS === '2' ? [{
              label: 'Focus Left Client',
              accelerator: 'CmdOrCtrl+1',
              click: () => BaseWindow.getFocusedWindow()?.doolFocus?.(0),
            },
            {
              label: 'Focus Right Client',
              accelerator: 'CmdOrCtrl+2',
              click: () => BaseWindow.getFocusedWindow()?.doolFocus?.(1),
            }] : []),
            { type: 'separator' },
            { role: 'close' },
          ],
        },
        { role: 'windowMenu' },
      ]),
    );
    openDoolRig();
  });
  app.on('activate', () => {
    if (!windows.size) openDoolRig();
  });
  app.on('window-all-closed', () => app.quit());
}

module.exports = { openDoolRig, launchDoolRig };
