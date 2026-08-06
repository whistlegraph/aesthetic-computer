#!/usr/bin/env node

function cdp(url, method, params = {}) {
  return new Promise((resolveCdp, reject) => {
    const socket = new WebSocket(url);
    socket.addEventListener("open", () => socket.send(JSON.stringify({ id: 1, method, params })));
    socket.addEventListener("error", reject);
    socket.addEventListener("message", (event) => {
      const message = JSON.parse(event.data);
      if (message.id !== 1) return;
      socket.close();
      if (message.error) reject(new Error(message.error.message));
      else resolveCdp(message.result);
    });
  });
}

const displays = [
  { name: "board", port: 9222, left: 0, top: 0, fullscreen: false },
  { name: "soup", port: 9223, left: 0, top: 1440, fullscreen: true },
];

const wait = (milliseconds) => new Promise((resolveWait) => setTimeout(resolveWait, milliseconds));

async function findTarget(display) {
  for (let attempt = 0; attempt < 80; attempt += 1) {
    const targets = await fetch(`http://127.0.0.1:${display.port}/json`).then((response) => response.json());
    const target = targets.find(({ type, url }) => type === "page" && new URL(url).pathname === `/${display.name}`);
    if (target) return target;
    await wait(100);
  }
  throw new Error(`no /${display.name} page target on port ${display.port}`);
}

async function place(display, target) {
  const current = await cdp(target.webSocketDebuggerUrl, "Browser.getWindowForTarget", { targetId: target.id });
  await cdp(target.webSocketDebuggerUrl, "Browser.setWindowBounds", {
    windowId: current.windowId,
    bounds: { windowState: "normal" },
  });
  await cdp(target.webSocketDebuggerUrl, "Browser.setWindowBounds", {
    windowId: current.windowId,
    bounds: { left: display.left, top: display.top, width: 2560, height: 1440 },
  });
  await wait(300);
  if (display.fullscreen) {
    await cdp(target.webSocketDebuggerUrl, "Browser.setWindowBounds", {
      windowId: current.windowId,
      bounds: { windowState: "fullscreen" },
    });
  }
}

const targets = new Map();
for (const display of displays) targets.set(display.name, await findTarget(display));

// Chrome applies its saved app-window placement shortly after the debugging
// target appears. Place twice after navigation settles so that late restore
// cannot move the scoreboard back off-screen during a service restart.
await wait(1500);
for (let pass = 0; pass < 2; pass += 1) {
  for (const display of displays) await place(display, targets.get(display.name));
  if (pass === 0) await wait(750);
}
