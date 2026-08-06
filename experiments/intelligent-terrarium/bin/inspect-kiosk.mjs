#!/usr/bin/env node
import { mkdir, writeFile } from "node:fs/promises";
import { resolve } from "node:path";

const output = resolve(process.argv[2] || "/tmp/piecefarm-kiosk-qa");
await mkdir(output, { recursive: true });

function cdp(url, messages) {
  return new Promise((resolveCdp, reject) => {
    const socket = new WebSocket(url);
    const results = new Map();
    socket.addEventListener("open", () => messages.forEach((message, index) => socket.send(JSON.stringify({ id: index + 1, ...message }))));
    socket.addEventListener("error", reject);
    socket.addEventListener("message", (event) => {
      const message = JSON.parse(event.data);
      if (!message.id) return;
      results.set(message.id, message.result || { error: message.error });
      if (results.size === messages.length) { socket.close(); resolveCdp(messages.map((_, index) => results.get(index + 1))); }
    });
  });
}

const report = {};
for (const [name, port] of [["board", 9222], ["soup", 9223]]) {
  const [target] = await fetch(`http://127.0.0.1:${port}/json`).then((response) => response.json());
  if (!target) throw new Error(`no ${name} Chrome target on ${port}`);
  const [window, screenshot, metrics] = await cdp(target.webSocketDebuggerUrl, [
    { method: "Browser.getWindowForTarget", params: { targetId: target.id } },
    { method: "Page.captureScreenshot", params: { format: "png", captureBeyondViewport: false } },
    { method: "Performance.getMetrics", params: {} },
  ]);
  await writeFile(resolve(output, `${name}.png`), Buffer.from(screenshot.data, "base64"));
  report[name] = {
    title: target.title,
    url: target.url,
    bounds: window.bounds,
    metrics: Object.fromEntries((metrics.metrics || []).filter(({ name: key }) => ["Documents", "Frames", "JSEventListeners", "Nodes", "JSHeapUsedSize"].includes(key)).map(({ name: key, value }) => [key, value])),
  };
}

console.log(JSON.stringify(report, null, 2));
