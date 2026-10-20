// notepaint widget, 26.10.19
// The painting card Claude and ChatGPT draw under a notepaint_paint call. It
// is an MCP App (the shared ui:// standard both hosts render): a sandboxed
// page that talks to its host over postMessage JSON-RPC. It shows the
// painting large, says whose it is and what it cost, and carries the way back
// to notepaint.art, so a painting shown in a chat leads people to the studio.
//
// The PNG arrives in the tool result's _meta (seen by the widget, never the
// model); content's image item is the fallback for hosts that drop _meta.

// A new URI whenever the page changes: hosts cache it by URI.
export const NOTEPAINT_WIDGET_URI = "ui://notepaint/painting-v1.html";
export const NOTEPAINT_WIDGET_MIME = "text/html;profile=mcp-app";

export const NOTEPAINT_WIDGET_HTML = `<!doctype html>
<html>
<head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<style>
  :root {
    --bg: #ffffff; --ink: #171717; --soft: #6b6b6b; --line: #e4e4e4;
    --accent: #e8590c; --field: #f6f6f6;
    color-scheme: light dark;
  }
  @media (prefers-color-scheme: dark) {
    :root:not([data-theme="light"]) {
      --bg: #1b1b1b; --ink: #f2f2f2; --soft: #a0a0a0; --line: #333; --field: #262626;
    }
  }
  :root[data-theme="dark"] {
    --bg: #1b1b1b; --ink: #f2f2f2; --soft: #a0a0a0; --line: #333; --field: #262626;
  }
  * { box-sizing: border-box; }
  html, body { margin: 0; background: var(--bg); color: var(--ink);
    font: 14px/1.4 ui-sans-serif, system-ui, -apple-system, sans-serif; }
  main { max-width: 420px; padding: 12px; }
  .frame { position: relative; aspect-ratio: 1; border-radius: 10px; overflow: hidden;
    background: var(--field); border: 1px solid var(--line); }
  .frame img { display: block; width: 100%; height: 100%; object-fit: contain;
    image-rendering: auto; }
  .frame.small img { image-rendering: pixelated; }
  .wait { position: absolute; inset: 0; display: grid; place-items: center;
    padding: 24px; text-align: center; color: var(--soft); }
  .wait b { display: block; color: var(--ink); font-weight: 600; margin-bottom: 4px; }
  .dot { display: inline-block; width: 8px; height: 8px; border-radius: 50%;
    background: var(--accent); margin-right: 6px; animation: pulse 1s infinite alternate; }
  @keyframes pulse { from { opacity: .25 } to { opacity: 1 } }
  .meta { display: flex; justify-content: space-between; gap: 8px; margin: 10px 2px 0;
    color: var(--soft); font-size: 12px; }
  .brand { color: var(--ink); font-weight: 650; letter-spacing: -.01em; }
  .prompt { margin: 6px 2px 0; }
  form { display: flex; gap: 6px; margin-top: 10px; }
  input { flex: 1; min-width: 0; padding: 8px 10px; border-radius: 8px;
    border: 1px solid var(--line); background: var(--field); color: var(--ink); font: inherit; }
  button { padding: 8px 12px; border-radius: 8px; border: 1px solid var(--line);
    background: var(--field); color: var(--ink); font: inherit; cursor: pointer; white-space: nowrap; }
  button.go { background: var(--accent); border-color: var(--accent); color: #fff; }
  button:disabled { opacity: .5; cursor: default; }
  .row { display: flex; gap: 6px; margin-top: 6px; }
  .row button { flex: 1; }
  .err { color: #c92a2a; }
</style>
</head>
<body>
<main>
  <div class="frame" id="frame">
    <div class="wait" id="wait"><div><b><span class="dot"></span>notepaint is painting</b><span id="asked"></span></div></div>
  </div>
  <div class="meta"><span class="brand">notepaint</span><span id="cost"></span></div>
  <p class="prompt" id="prompt" hidden></p>
  <form id="again" hidden>
    <input id="change" maxlength="190" placeholder="Change something…" autocomplete="off">
    <button class="go" id="repaint">Repaint</button>
  </form>
  <div class="row"><button id="studio" type="button">Paint more at notepaint.art ↗</button></div>
</main>
<script>
(() => {
  const $ = (id) => document.getElementById(id);
  const STUDIO = "https://notepaint.art";
  let nextId = 1, painting = null;
  const pending = new Map();

  const post = (message) => window.parent.postMessage({ jsonrpc: "2.0", ...message }, "*");
  const request = (method, params) => new Promise((resolve, reject) => {
    const id = nextId++;
    pending.set(id, { resolve, reject });
    post({ id, method, params });
  });
  const notify = (method, params) => post({ method, params });

  const theme = (context) => {
    if (context?.theme) document.documentElement.dataset.theme = context.theme;
  };

  const show = (result) => {
    const meta = result?._meta?.notepaint || {};
    const facts = result?.structuredContent || {};
    const image = meta.image ? "data:image/png;base64," + meta.image
      : (result?.content || []).filter((item) => item.type === "image")
          .map((item) => "data:" + (item.mimeType || "image/png") + ";base64," + item.data)[0];
    if (result?.isError || !image) {
      const text = (result?.content || []).find((item) => item.type === "text")?.text;
      $("wait").innerHTML = '<div class="err">' + (text || "No painting came back.").replace(/[<&]/g, (c) => c === "<" ? "&lt;" : "&amp;") + "</div>";
      return;
    }
    painting = facts;
    const img = new Image();
    img.alt = facts.prompt || "A notepaint painting";
    img.src = image;
    $("frame").classList.toggle("small", (facts.size || 256) < 512);
    $("frame").replaceChildren(img);
    if (facts.prompt) { $("prompt").textContent = "“" + facts.prompt + "”"; $("prompt").hidden = false; }
    const bits = [facts.painter, facts.braincells != null ? facts.braincells.toLocaleString() + " Braincells" : null];
    $("cost").textContent = bits.filter(Boolean).join(" · ");
    $("again").hidden = !facts.id;
  };

  const open = (url) => request("ui/open-link", { url }).catch(() => window.open(url, "_blank", "noopener"));

  $("studio").onclick = () => open(STUDIO);
  $("again").onsubmit = (event) => {
    event.preventDefault();
    const change = $("change").value.replace(/\\s+/g, " ").trim();
    if (!change || !painting?.id) return;
    $("repaint").disabled = true;
    request("ui/message", { role: "user", content: { type: "text",
      text: 'Repaint notepaint painting ' + painting.id + ': ' + change } })
      .then(() => { $("change").value = ""; })
      .catch(() => {})
      .finally(() => { $("repaint").disabled = false; });
  };

  window.addEventListener("message", (event) => {
    const message = event.data;
    if (!message || message.jsonrpc !== "2.0") return;
    if (message.id != null && pending.has(message.id) && !message.method) {
      const { resolve, reject } = pending.get(message.id);
      pending.delete(message.id);
      return message.error ? reject(message.error) : resolve(message.result);
    }
    switch (message.method) {
      case "ui/notifications/tool-input":
        if (message.params?.arguments?.prompt) $("asked").textContent = "“" + message.params.arguments.prompt + "”";
        break;
      case "ui/notifications/tool-result":
        show(message.params);
        break;
      case "ui/notifications/host-context-changed":
        theme(message.params);
        break;
      case "ui/resource-teardown":
        post({ id: message.id, result: {} });
        break;
      case "ping":
        post({ id: message.id, result: {} });
        break;
    }
  });

  new ResizeObserver(() => notify("ui/notifications/size-changed", {
    width: document.documentElement.scrollWidth,
    height: document.documentElement.scrollHeight,
  })).observe(document.body);

  request("ui/initialize", {
    protocolVersion: "2026-01-26",
    appInfo: { name: "notepaint", version: "1.0.0" },
    appCapabilities: { availableDisplayModes: ["inline"] },
  }).then((result) => {
    theme(result?.hostContext);
    notify("ui/notifications/initialized", {});
  }).catch(() => notify("ui/notifications/initialized", {}));

  // ChatGPT also hands the result over as window.openai, in case the bridge is late.
  const openai = window.openai;
  if (openai?.toolOutput && !painting) show({ structuredContent: openai.toolOutput, _meta: openai.toolResponseMetadata });
})();
</script>
</body>
</html>`;
