const DEFAULT_MONACO_LOADER =
  "https://cdn.jsdelivr.net/npm/monaco-editor@0.52.0/min/vs/loader.min.js";
const DEFAULT_MONACO_BASE = "https://cdn.jsdelivr.net/npm/monaco-editor@0.52.0/min/vs";

const DEFAULT_THEME_VARS = {
  light: {
    "--try-bg": "#ffffff",
    "--try-ink": "#111111",
    "--try-ink-soft": "#2b2b2b",
    "--try-line": "#111111",
    "--try-panel": "rgba(255, 255, 255, 0.95)",
    "--try-control-bg": "#ffffff",
    "--try-head-bg": "#ffee57",
    "--try-code-bg": "#000000",
    "--try-code-ink": "#ffd700",
    "--try-accent": "#ffd700",
    "--try-accent-soft": "#ffee57",
    "--try-dot-a": "rgba(255, 215, 0, 0.32)",
    "--try-dot-b": "rgba(255, 215, 0, 0.16)",
  },
  dark: {
    "--try-bg": "#0a0a0a",
    "--try-ink": "#f4f4f4",
    "--try-ink-soft": "#c8c8c8",
    "--try-line": "#f6d648",
    "--try-panel": "rgba(14, 14, 14, 0.92)",
    "--try-control-bg": "#121212",
    "--try-head-bg": "#2a250a",
    "--try-code-bg": "#020202",
    "--try-code-ink": "#f6d648",
    "--try-accent": "#f6d648",
    "--try-accent-soft": "#685b1b",
    "--try-dot-a": "rgba(246, 214, 72, 0.18)",
    "--try-dot-b": "rgba(246, 214, 72, 0.09)",
  },
};

const DEFAULT_MONACO_THEMES = {
  dark: {
    base: "vs-dark",
    inherit: true,
    rules: [
      { token: "keyword", foreground: "ffd700" },
      { token: "identifier", foreground: "f6f6f6" },
      { token: "comment", foreground: "999999" },
      { token: "string", foreground: "ffee57" },
    ],
    colors: {
      "editor.background": "#060606",
      "editor.foreground": "#f6f6f6",
      "editorCursor.foreground": "#ffd700",
      "editor.lineHighlightBackground": "#111111",
      "editorLineNumber.foreground": "#666666",
      "editorLineNumber.activeForeground": "#ffd700",
      "editor.selectionBackground": "#ffd70033",
    },
  },
  light: {
    base: "vs",
    inherit: true,
    rules: [
      { token: "keyword", foreground: "8b6500" },
      { token: "identifier", foreground: "1a1a1a" },
      { token: "comment", foreground: "6c6c6c" },
      { token: "string", foreground: "945f00" },
    ],
    colors: {
      "editor.background": "#fffdf2",
      "editor.foreground": "#111111",
      "editorCursor.foreground": "#111111",
      "editor.lineHighlightBackground": "#fff6cf",
      "editorLineNumber.foreground": "#8f8f8f",
      "editorLineNumber.activeForeground": "#8b6500",
      "editor.selectionBackground": "#f6d64844",
    },
  },
};

function escapeHtml(text) {
  const div = document.createElement("div");
  div.textContent = String(text ?? "");
  return div.innerHTML;
}

function ensureSharedCss() {
  if (document.getElementById("ac-try-shared-css")) return;
  const link = document.createElement("link");
  link.id = "ac-try-shared-css";
  link.rel = "stylesheet";
  link.href = new URL("./shared-page.css", import.meta.url).href;
  document.head.appendChild(link);
}

function applyThemeVars(themeVars) {
  const styleId = "ac-try-theme-vars";
  const style = document.getElementById(styleId) || document.createElement("style");
  style.id = styleId;

  const light = { ...DEFAULT_THEME_VARS.light, ...(themeVars?.light || {}) };
  const dark = { ...DEFAULT_THEME_VARS.dark, ...(themeVars?.dark || {}) };

  const block = (vars) =>
    Object.entries(vars)
      .map(([name, value]) => `${name}: ${value};`)
      .join("\n");

  style.textContent = `
    :root {
      ${block(light)}
    }

    @media (prefers-color-scheme: dark) {
      :root {
        ${block(dark)}
      }
    }
  `;

  if (!style.parentNode) {
    document.head.appendChild(style);
  }
}

function applyFontFacesCss(fontFacesCss) {
  if (!fontFacesCss) return;
  const styleId = "ac-try-font-faces";
  const style = document.getElementById(styleId) || document.createElement("style");
  style.id = styleId;
  style.textContent = fontFacesCss;
  if (!style.parentNode) {
    document.head.appendChild(style);
  }
}

function setMeta(name, content) {
  if (!content) return;
  let el = document.querySelector(`meta[name="${name}"]`);
  if (!el) {
    el = document.createElement("meta");
    el.setAttribute("name", name);
    document.head.appendChild(el);
  }
  el.setAttribute("content", content);
}

function isAbsoluteUrl(value) {
  return /^https?:\/\//.test(value);
}

function asAbsoluteUrl(urlOrPath, origin) {
  if (!urlOrPath) return origin;
  if (isAbsoluteUrl(urlOrPath)) return urlOrPath;
  if (urlOrPath.startsWith("/")) return `${origin}${urlOrPath}`;
  return `${origin}/${urlOrPath}`;
}

function withFreshTimestamp(urlString) {
  try {
    const url = new URL(urlString, window.location.origin);
    url.searchParams.set("t", Date.now());
    return url.toString();
  } catch {
    const joiner = urlString.includes("?") ? "&" : "?";
    return `${urlString}${joiner}t=${Date.now()}`;
  }
}

function onColorSchemeChange(callback) {
  if (!window.matchMedia) return;
  const media = window.matchMedia("(prefers-color-scheme: dark)");
  if (typeof media.addEventListener === "function") {
    media.addEventListener("change", callback);
    return;
  }
  if (typeof media.addListener === "function") {
    media.addListener(callback);
  }
}

function ensureScript(src) {
  return new Promise((resolve, reject) => {
    const existing = document.querySelector(`script[src="${src}"]`);
    if (existing) {
      if (window.require) {
        resolve();
        return;
      }
      if (existing.dataset.loaded === "true") {
        resolve();
        return;
      }
      existing.addEventListener("load", () => resolve(), { once: true });
      existing.addEventListener("error", () => reject(new Error(`Failed to load ${src}`)), {
        once: true,
      });
      return;
    }

    const script = document.createElement("script");
    script.src = src;
    script.async = true;
    script.dataset.loaded = "false";
    script.addEventListener(
      "load",
      () => {
        script.dataset.loaded = "true";
        resolve();
      },
      { once: true },
    );
    script.addEventListener("error", () => reject(new Error(`Failed to load ${src}`)), {
      once: true,
    });
    document.head.appendChild(script);
  });
}

function buildFrameSrc(runtime, acOrigin) {
  const base = asAbsoluteUrl(runtime.framePath || runtime.frameUrl, acOrigin);
  const url = new URL(base, window.location.origin);
  const flags = {
    nogap: "true",
    nolabel: "true",
    noauth: "true",
    popout: "true",
    ...(runtime.frameFlags || {}),
  };
  Object.entries(flags).forEach(([k, v]) => {
    if (v !== undefined && v !== null) {
      url.searchParams.set(k, String(v));
    }
  });
  url.searchParams.set("t", String(Date.now()));
  return url.toString();
}

function renderLayout(config) {
  const heading = config.brand?.heading || "Try on AC";
  const logoUrl = config.brand?.logoUrl || "";
  const logoAlt = config.brand?.logoAlt || "logo";
  const heroLink = config.brand?.heroLink;

  const footerLinks = Array.isArray(config.brand?.footerLinks) ? config.brand.footerLinks : [];
  const footerHtml = footerLinks
    .map(
      (link) =>
        `<a href="${escapeHtml(link.href)}" target="_blank" rel="noopener">${escapeHtml(link.label)}</a>`,
    )
    .join(" &middot; ");

  return `
    <main class="try-wrap">
      <section class="try-panel try-hero">
        <div class="try-hero-top">
          <div class="try-brand">
            <div class="try-logo-wrap">
              <img id="try-brand-logo" src="${escapeHtml(logoUrl)}" alt="${escapeHtml(logoAlt)}" />
            </div>
            <div>
              <h1 class="try-heading">${escapeHtml(heading)}</h1>
            </div>
          </div>
          ${
            heroLink
              ? `<a class="try-btn" href="${escapeHtml(heroLink.href)}" target="_blank" rel="noopener" title="${escapeHtml(heroLink.title || "")}">${escapeHtml(heroLink.label || "Learn")}</a>`
              : ""
          }
        </div>
      </section>

      <section class="try-playground">
        <div class="try-editor-shell">
          <div class="try-editor-head">
            <div class="try-controls">
              <select id="try-example-select" aria-label="Choose example"></select>
              <a id="try-example-source" class="try-btn" href="#" target="_blank" rel="noopener">Source</a>
              <button id="try-reset-btn" type="button">Reset</button>
              <button id="try-copy-btn" type="button">Copy</button>
              <button id="try-run-btn" class="try-btn-primary" type="button">Run</button>
            </div>
          </div>
          <div id="try-editor"></div>
        </div>

        <div class="try-preview-shell">
          <div class="try-preview-head">
            <a id="try-open-link" class="try-btn" href="#" target="_blank" rel="noopener">Open in AC</a>
          </div>
          <div class="try-preview-host">
            <iframe id="try-frame" title="Runtime preview" loading="eager" allow="autoplay; clipboard-write"></iframe>
          </div>
          <div class="try-runtime-status" id="try-runtime-status">Booting AC iframe...</div>
        </div>

        <div class="try-doc-panel" id="try-doc-panel"></div>
      </section>

      <p class="try-muted" style="padding: 0 2px;">
        ${escapeHtml(config.helperText || "Live reload on edit. Cmd/Ctrl+Enter to force run. Zoom: Cmd/Ctrl +/-/0 in editor.")}
      </p>
    </main>

    <footer class="try-footer-wrap">
      <div class="try-footer-credits try-muted">
        ${footerHtml}
      </div>
    </footer>
  `;
}

export async function createTryPage(config) {
  const settings = {
    acOrigin: config.acOrigin || "https://aesthetic.computer",
    runtime: {
      framePath: config.runtime?.framePath,
      frameUrl: config.runtime?.frameUrl,
      frameFlags: config.runtime?.frameFlags,
      openPath: config.runtime?.openPath,
      openUrl: config.runtime?.openUrl,
      readyTypes: config.runtime?.readyTypes || ["ready", "kidlisp-ready"],
      targetOrigin: config.runtime?.targetOrigin,
      buildReloadMessage: config.runtime?.buildReloadMessage,
    },
    docs: config.docs || {},
    examples: Array.isArray(config.examples) ? config.examples : [],
    monaco: config.monaco || {},
    brand: config.brand || {},
    themeVars: config.themeVars,
    fontFacesCss: config.fontFacesCss,
    helperText: config.helperText,
    languageLabel: config.languageLabel || "Code",
    source: {
      defaultUrl: config.source?.defaultUrl || "https://aesthetic.computer",
      labelForExample:
        config.source?.labelForExample ||
        ((example) => (example.sourceUrl ? "Source" : "Source")),
      urlForExample:
        config.source?.urlForExample ||
        ((example, fallback) => example.sourceUrl || fallback),
    },
    mountId: config.mountId || "try-page-root",
    defaultDocMessage: config.defaultDocMessage || "Hover over a function to see its docs.",
    onLogoError: config.onLogoError,
  };

  if (!settings.examples.length) {
    settings.examples.push({ id: "default", title: "Example", code: "" });
  }

  document.title = config.page?.title || settings.brand?.heading || "Try on AC";
  if (config.page?.description) {
    setMeta("description", config.page.description);
  }

  ensureSharedCss();
  applyThemeVars(settings.themeVars);
  applyFontFacesCss(settings.fontFacesCss);

  const mount = document.getElementById(settings.mountId);
  if (mount) {
    mount.innerHTML = renderLayout(settings);
  } else {
    document.body.innerHTML = renderLayout(settings);
  }

  const frameSrc = buildFrameSrc(settings.runtime, settings.acOrigin);
  const frameOrigin = new URL(frameSrc).origin;
  const openUrl =
    settings.runtime.openUrl ||
    asAbsoluteUrl(settings.runtime.openPath || settings.runtime.framePath || settings.runtime.frameUrl, settings.acOrigin);

  const state = {
    editor: null,
    fallbackEditor: null,
    iframeReady: false,
    pendingCode: null,
    selectedExampleId: settings.examples[0].id,
  };

  const runtimeStatus = document.getElementById("try-runtime-status");
  const docPanel = document.getElementById("try-doc-panel");
  const openLink = document.getElementById("try-open-link");
  const logo = document.getElementById("try-brand-logo");
  openLink.href = openUrl;
  docPanel.textContent = settings.defaultDocMessage;

  if (logo && typeof settings.onLogoError === "function") {
    logo.addEventListener(
      "error",
      () => {
        settings.onLogoError(logo);
      },
      { once: true },
    );
  }

  function setRuntimeStatus(message, level = "") {
    runtimeStatus.textContent = message;
    runtimeStatus.classList.remove("ok", "error");
    if (level) runtimeStatus.classList.add(level);
  }

  function getExampleById(id) {
    return settings.examples.find((item) => item.id === id) || settings.examples[0];
  }

  function getEditorValue() {
    if (state.editor) return state.editor.getValue();
    if (state.fallbackEditor) return state.fallbackEditor.value;
    return "";
  }

  function setEditorValue(value) {
    if (state.editor) {
      state.editor.setValue(value);
      return;
    }
    if (state.fallbackEditor) {
      state.fallbackEditor.value = value;
    }
  }

  function sendToIframe(code) {
    const frame = document.getElementById("try-frame");
    if (!frame.contentWindow) {
      setRuntimeStatus("AC iframe unavailable.", "error");
      return;
    }

    const message =
      typeof settings.runtime.buildReloadMessage === "function"
        ? settings.runtime.buildReloadMessage(code)
        : {
            type: "ac:try:reload",
            source: code,
          };

    const targetOrigin = settings.runtime.targetOrigin || frameOrigin;
    frame.contentWindow.postMessage(message, targetOrigin);
    setRuntimeStatus(`${settings.languageLabel} sent to AC runtime.`, "ok");
  }

  function queueOrRun(code) {
    if (!code || !code.trim()) {
      setRuntimeStatus("Cannot run empty source.", "error");
      return;
    }

    if (!state.iframeReady) {
      state.pendingCode = code;
      setRuntimeStatus("Iframe not ready yet. Code queued...", "");
      return;
    }

    sendToIframe(code);
  }

  function initExampleControls() {
    const select = document.getElementById("try-example-select");
    const source = document.getElementById("try-example-source");
    const resetBtn = document.getElementById("try-reset-btn");
    const copyBtn = document.getElementById("try-copy-btn");
    const runBtn = document.getElementById("try-run-btn");

    const updateSource = (picked) => {
      source.href = settings.source.urlForExample(picked, settings.source.defaultUrl);
      source.textContent = settings.source.labelForExample(picked);
    };

    select.innerHTML = settings.examples
      .map((item) => `<option value="${escapeHtml(item.id)}">${escapeHtml(item.title)}</option>`)
      .join("");

    select.value = state.selectedExampleId;
    updateSource(getExampleById(state.selectedExampleId));

    select.addEventListener("change", () => {
      state.selectedExampleId = select.value;
      const picked = getExampleById(state.selectedExampleId);
      updateSource(picked);
      setEditorValue(picked.code);
    });

    resetBtn.addEventListener("click", () => {
      const picked = getExampleById(state.selectedExampleId);
      setEditorValue(picked.code);
      setRuntimeStatus(`Reset to ${picked.title}.`, "");
    });

    copyBtn.addEventListener("click", async () => {
      try {
        await navigator.clipboard.writeText(getEditorValue());
        const old = copyBtn.textContent;
        copyBtn.textContent = "Copied";
        setTimeout(() => {
          copyBtn.textContent = old;
        }, 900);
      } catch {
        setRuntimeStatus("Clipboard write failed.", "error");
      }
    });

    runBtn.addEventListener("click", () => {
      queueOrRun(getEditorValue());
    });

    window.addEventListener("keydown", (event) => {
      if ((event.metaKey || event.ctrlKey) && event.key === "Enter") {
        event.preventDefault();
        queueOrRun(getEditorValue());
      }
    });
  }

  function updateDocPanel(word) {
    const doc = word ? settings.docs[word] : null;
    if (doc) {
      docPanel.innerHTML =
        `<span class="doc-cat">${escapeHtml(doc.cat || "")}</span>` +
        `<span class="doc-sig">${escapeHtml(doc.sig || word)}</span>` +
        `<span class="doc-desc">${escapeHtml(doc.desc || "")}</span>`;
      return;
    }

    docPanel.textContent = settings.defaultDocMessage;
  }

  function createFallbackEditor(initial) {
    const container = document.getElementById("try-editor");
    const textarea = document.createElement("textarea");
    textarea.className = "try-editor-fallback";
    textarea.value = initial;
    container.replaceWith(textarea);
    state.fallbackEditor = textarea;
    setRuntimeStatus("Monaco unavailable. Using fallback editor.", "error");
  }

  async function initMonacoEditor() {
    const initial = getExampleById(state.selectedExampleId).code;
    const loaderUrl = settings.monaco.loaderUrl || DEFAULT_MONACO_LOADER;
    const baseUrl = settings.monaco.baseUrl || DEFAULT_MONACO_BASE;

    try {
      await ensureScript(loaderUrl);
    } catch {
      createFallbackEditor(initial);
      return;
    }

    if (!window.require) {
      createFallbackEditor(initial);
      return;
    }

    const container = document.getElementById("try-editor");

    const workerCode = `self.MonacoEnvironment={baseUrl:'${baseUrl}/'};importScripts('${baseUrl}/base/worker/workerMain.js');`;
    const workerBlob = new Blob([workerCode], { type: "application/javascript" });
    const workerUrl = URL.createObjectURL(workerBlob);

    window.MonacoEnvironment = {
      getWorkerUrl: () => workerUrl,
    };

    window.require.config({ paths: { vs: baseUrl } });

    await new Promise((resolve, reject) => {
      window.require(
        ["vs/editor/editor.main"],
        () => resolve(),
        (err) => reject(err),
      );
    }).catch(() => {
      createFallbackEditor(initial);
    });

    if (!window.monaco) {
      if (!state.fallbackEditor) {
        createFallbackEditor(initial);
      }
      return;
    }

    const themes = settings.monaco.themes || DEFAULT_MONACO_THEMES;
    const themeNames = settings.monaco.themeNames || {
      dark: "ac-try-dark",
      light: "ac-try-light",
    };

    monaco.editor.defineTheme(themeNames.dark, themes.dark);
    monaco.editor.defineTheme(themeNames.light, themes.light);

    const applyEditorTheme = () => {
      const isDark =
        window.matchMedia && window.matchMedia("(prefers-color-scheme: dark)").matches;
      monaco.editor.setTheme(isDark ? themeNames.dark : themeNames.light);
    };

    state.editor = monaco.editor.create(container, {
      value: initial,
      language: settings.monaco.language || "javascript",
      theme: themeNames.dark,
      minimap: { enabled: false },
      automaticLayout: true,
      tabSize: 2,
      insertSpaces: true,
      fontFamily: settings.monaco.fontFamily || "Berkeley Mono Variable, Menlo, monospace",
      fontSize: settings.monaco.fontSizeDefault || 13,
      lineHeight: 20,
      scrollBeyondLastLine: false,
      lineNumbers: "on",
      glyphMargin: false,
      folding: false,
      lineDecorationsWidth: 0,
      lineNumbersMinChars: 3,
      ...(settings.monaco.options || {}),
    });

    applyEditorTheme();
    onColorSchemeChange(applyEditorTheme);

    let editorFontSize = settings.monaco.fontSizeDefault || 13;
    const fontMin = settings.monaco.fontSizeMin || 8;
    const fontMax = settings.monaco.fontSizeMax || 32;
    const fontDefault = settings.monaco.fontSizeDefault || 13;

    state.editor.onKeyDown((e) => {
      const isMeta = e.metaKey || e.ctrlKey;
      if (!isMeta) return;

      const isPlus = e.keyCode === monaco.KeyCode.Equal || e.keyCode === monaco.KeyCode.NumpadAdd;
      const isMinus =
        e.keyCode === monaco.KeyCode.Minus || e.keyCode === monaco.KeyCode.NumpadSubtract;
      const isZero = e.keyCode === monaco.KeyCode.Digit0 || e.keyCode === monaco.KeyCode.Numpad0;

      if (isPlus || isMinus || isZero) {
        e.preventDefault();
        e.stopPropagation();
        if (isPlus && editorFontSize < fontMax) editorFontSize += 2;
        else if (isMinus && editorFontSize > fontMin) editorFontSize -= 2;
        else if (isZero) editorFontSize = fontDefault;
        state.editor.updateOptions({ fontSize: editorFontSize });
      }
    });

    let liveTimer = null;
    state.editor.onDidChangeModelContent(() => {
      clearTimeout(liveTimer);
      liveTimer = setTimeout(() => {
        queueOrRun(state.editor.getValue());
      }, settings.monaco.liveDebounceMs || 400);
    });

    monaco.languages.registerHoverProvider(settings.monaco.language || "javascript", {
      provideHover(model, position) {
        const word = model.getWordAtPosition(position);
        if (!word) return null;
        const doc = settings.docs[word.word];
        if (!doc) return null;

        return {
          range: new monaco.Range(
            position.lineNumber,
            word.startColumn,
            position.lineNumber,
            word.endColumn,
          ),
          contents: [
            { value: `**${doc.cat || "Docs"}**` },
            { value: "```\n" + (doc.sig || word.word) + "\n```" },
            { value: doc.desc || "" },
          ],
        };
      },
    });

    monaco.languages.registerCompletionItemProvider(settings.monaco.language || "javascript", {
      provideCompletionItems() {
        return {
          suggestions: Object.entries(settings.docs).map(([name, doc]) => ({
            label: name,
            kind: String(doc.sig || "").includes("(")
              ? monaco.languages.CompletionItemKind.Function
              : monaco.languages.CompletionItemKind.Variable,
            insertText: name,
            detail: doc.sig || name,
            documentation: doc.desc || "",
          })),
        };
      },
    });

    state.editor.onDidChangeCursorPosition((event) => {
      const model = state.editor.getModel();
      const word = model.getWordAtPosition(event.position);
      updateDocPanel(word?.word || null);
    });
  }

  function initIframeRunner() {
    const frame = document.getElementById("try-frame");

    frame.addEventListener("load", () => {
      setRuntimeStatus("AC iframe loaded. Waiting for runtime ready signal...", "");
    });

    window.addEventListener("message", (event) => {
      if (event.origin !== frameOrigin) return;
      const type = event.data?.type;
      if (!settings.runtime.readyTypes.includes(type)) return;

      state.iframeReady = true;
      setRuntimeStatus("AC runtime ready.", "ok");

      if (state.pendingCode) {
        sendToIframe(state.pendingCode);
        state.pendingCode = null;
      }
    });

    frame.src = frameSrc;
  }

  function initLiveReload() {
    const isDev =
      location.hostname === "localhost" || location.hostname.endsWith(".app.github.dev");
    if (!isDev) return;

    const protocol = location.protocol === "https:" ? "wss:" : "ws:";
    const wsPort = settings.monaco.liveReloadPort || 8889;
    const wsUrl = location.hostname.endsWith(".app.github.dev")
      ? `${protocol}//${location.hostname.replace("-8888", `-${wsPort}`)}`
      : `${protocol}//localhost:${wsPort}`;

    let ws;
    function connect() {
      ws = new WebSocket(wsUrl);
      ws.onmessage = (event) => {
        try {
          const msg = JSON.parse(event.data);
          if (msg.type === "reload" && msg.content?.piece === "*refresh*") {
            location.reload();
          }
        } catch {
          // ignore non-JSON messages
        }
      };
      ws.onclose = () => {
        setTimeout(connect, 2000);
      };
    }

    connect();
  }

  initExampleControls();
  initIframeRunner();
  await initMonacoEditor();
  initLiveReload();

  return {
    run: () => queueOrRun(getEditorValue()),
    setCode: (code) => setEditorValue(code),
    getCode: () => getEditorValue(),
    refreshPreview: () => {
      const frame = document.getElementById("try-frame");
      frame.src = withFreshTimestamp(frame.src || frameSrc);
    },
  };
}
