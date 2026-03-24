(function initACMediaModal(global) {
  if (global.ACMediaModal) return;

  function ensureStyles() {
    if (document.getElementById("ac-media-modal-styles")) return;
    const style = document.createElement("style");
    style.id = "ac-media-modal-styles";
    style.textContent = `
      .ac-media-modal {
        position: fixed;
        inset: 0;
        z-index: 9999;
        display: none;
      }
      .ac-media-modal.active {
        display: block;
      }
      .ac-media-modal-backdrop {
        position: absolute;
        inset: 0;
        background: rgba(0, 0, 0, 0.75);
      }
      .ac-media-modal-panel {
        position: relative;
        z-index: 1;
        width: min(1000px, calc(100vw - 2rem));
        max-height: calc(100vh - 2rem);
        margin: 1rem auto;
        overflow: hidden;
        background: #fff;
        color: #000;
        border: 2px solid rgba(205, 92, 155, 0.55);
        border-radius: 8px;
        display: flex;
        flex-direction: column;
        box-shadow: 0 24px 80px rgba(0, 0, 0, 0.5);
      }
      .ac-media-modal-header {
        padding: 0.9rem 1rem;
        border-bottom: 1px solid rgba(205, 92, 155, 0.25);
        background: rgba(205, 92, 155, 0.1);
      }
      .ac-media-modal-title {
        margin: 0;
        font-size: 1rem;
        line-height: 1.3;
      }
      .ac-media-modal-subtitle {
        margin-top: 0.3rem;
        opacity: 0.7;
        font-size: 0.8rem;
      }
      .ac-media-modal-close {
        position: absolute;
        top: 0.45rem;
        right: 0.55rem;
        border: 1px solid rgba(205, 92, 155, 0.45);
        border-radius: 4px;
        background: #fff;
        color: rgb(205, 92, 155);
        font-family: monospace;
        font-size: 0.8rem;
        cursor: pointer;
        padding: 0.25rem 0.5rem;
      }
      .ac-media-modal-main {
        padding: 0.9rem 1rem 1rem;
        overflow: auto;
        display: grid;
        gap: 0.8rem;
      }
      .ac-media-modal-body {
        font-size: 0.85rem;
        line-height: 1.5;
      }
      .ac-media-modal-iframe-wrap {
        border: 1px solid rgba(205, 92, 155, 0.25);
        border-radius: 6px;
        overflow: hidden;
        min-height: 320px;
      }
      .ac-media-modal-iframe {
        width: 100%;
        height: min(60vh, 560px);
        border: none;
        display: block;
        background: #fff;
      }
      .ac-media-modal-actions {
        display: flex;
        flex-wrap: wrap;
        gap: 0.5rem;
      }
      .ac-media-modal-action {
        display: inline-block;
        padding: 0.38rem 0.65rem;
        border-radius: 4px;
        border: 1px solid rgba(205, 92, 155, 0.45);
        background: rgba(205, 92, 155, 0.12);
        color: rgb(205, 92, 155);
        text-decoration: none;
        font-size: 0.78rem;
        font-family: monospace;
      }
      .ac-media-modal-action:hover {
        background: rgba(205, 92, 155, 0.2);
      }
      @media (prefers-color-scheme: dark) {
        .ac-media-modal-panel {
          background: rgb(42, 37, 50);
          color: rgba(255, 255, 255, 0.92);
          border-color: rgba(205, 92, 155, 0.6);
        }
        .ac-media-modal-header {
          background: rgba(205, 92, 155, 0.15);
        }
        .ac-media-modal-close {
          background: rgba(255, 255, 255, 0.08);
        }
        .ac-media-modal-iframe-wrap {
          border-color: rgba(255, 255, 255, 0.12);
        }
      }
    `;
    document.head.appendChild(style);
  }

  function ensureModal() {
    let modal = document.getElementById("ac-media-modal");
    if (modal) return modal;

    modal = document.createElement("div");
    modal.id = "ac-media-modal";
    modal.className = "ac-media-modal";
    modal.innerHTML = `
      <div class="ac-media-modal-backdrop"></div>
      <div class="ac-media-modal-panel" role="dialog" aria-modal="true">
        <button class="ac-media-modal-close" type="button" aria-label="Close">Close</button>
        <div class="ac-media-modal-header">
          <h3 class="ac-media-modal-title"></h3>
          <div class="ac-media-modal-subtitle"></div>
        </div>
        <div class="ac-media-modal-main">
          <div class="ac-media-modal-body"></div>
          <div class="ac-media-modal-iframe-wrap" hidden>
            <iframe class="ac-media-modal-iframe" loading="lazy" referrerpolicy="no-referrer"></iframe>
          </div>
          <div class="ac-media-modal-actions"></div>
        </div>
      </div>
    `;

    const close = function close() {
      modal.classList.remove("active");
      const iframe = modal.querySelector(".ac-media-modal-iframe");
      if (iframe) iframe.src = "";
      document.body.style.overflow = "";
      document.removeEventListener("keydown", onEscape);
    };

    const onEscape = function onEscape(event) {
      if (event.key === "Escape") close();
    };

    modal.querySelector(".ac-media-modal-close").addEventListener("click", close);
    modal
      .querySelector(".ac-media-modal-backdrop")
      .addEventListener("click", close);

    modal.__acClose = close;
    modal.__acOnEscape = onEscape;

    document.body.appendChild(modal);
    return modal;
  }

  function open(options) {
    const opts = options || {};
    ensureStyles();
    const modal = ensureModal();

    const titleEl = modal.querySelector(".ac-media-modal-title");
    const subtitleEl = modal.querySelector(".ac-media-modal-subtitle");
    const bodyEl = modal.querySelector(".ac-media-modal-body");
    const iframeWrap = modal.querySelector(".ac-media-modal-iframe-wrap");
    const iframeEl = modal.querySelector(".ac-media-modal-iframe");
    const actionsEl = modal.querySelector(".ac-media-modal-actions");

    titleEl.textContent = opts.title || "Media";
    subtitleEl.textContent = opts.subtitle || "";
    bodyEl.innerHTML = opts.bodyHtml || "";

    const iframeUrl = typeof opts.iframeUrl === "string" ? opts.iframeUrl : "";
    if (iframeUrl) {
      iframeEl.src = iframeUrl;
      iframeWrap.hidden = false;
    } else {
      iframeEl.src = "";
      iframeWrap.hidden = true;
    }

    const actions = Array.isArray(opts.actions) ? opts.actions : [];
    actionsEl.innerHTML = actions
      .filter((action) => action && typeof action.url === "string" && action.url)
      .map((action) => {
        const label = String(action.label || "Open");
        const href = String(action.url);
        return `<a class="ac-media-modal-action" href="${href}" target="_blank" rel="noopener noreferrer">${label}</a>`;
      })
      .join("");

    modal.classList.add("active");
    document.body.style.overflow = "hidden";
    document.removeEventListener("keydown", modal.__acOnEscape);
    document.addEventListener("keydown", modal.__acOnEscape);
  }

  function close() {
    const modal = document.getElementById("ac-media-modal");
    if (!modal || !modal.__acClose) return;
    modal.__acClose();
  }

  global.ACMediaModal = { open, close };
})(window);
