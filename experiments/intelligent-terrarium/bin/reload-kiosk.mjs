#!/usr/bin/env node

function reload(url) {
  return new Promise((resolveReload, reject) => {
    const socket = new WebSocket(url);
    socket.addEventListener("open", () => socket.send(JSON.stringify({ id: 1, method: "Page.reload", params: { ignoreCache: true } })));
    socket.addEventListener("error", reject);
    socket.addEventListener("message", (event) => {
      const message = JSON.parse(event.data);
      if (message.id !== 1) return;
      socket.close();
      if (message.error) reject(new Error(message.error.message));
      else resolveReload();
    });
  });
}

for (const port of [9222, 9223]) {
  const targets = await fetch(`http://127.0.0.1:${port}/json`).then((response) => response.json());
  const target = targets.find(({ type }) => type === "page");
  if (!target) throw new Error(`no page target on ${port}`);
  await reload(target.webSocketDebuggerUrl);
}
