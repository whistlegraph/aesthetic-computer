// p5-iframe.mjs
// Option D from reports/p5js-integration-exploration.md.
//
// When disk.mjs resolves a piece as `.js` (a plain p5-style sketch) it asks
// this factory for a synthesized piece module that mounts an <iframe> via the
// dom.html helper. The iframe loads /aesthetic.computer/dep/p5/p5-host.html
// which in turn imports p5.js and runs the sketch in global mode.
//
// Trade-offs are documented in the exploration doc — this path keeps the AC
// worker untouched at the cost of recording/preview integration. We can layer
// Option B (OffscreenCanvas-patched p5 inside the worker) on top later.

export function makeP5IframeModule({ slug, sourceUrl }) {
  const hostBase = "/aesthetic.computer/dep/p5/p5-host.html";
  // Bust the host fetch on every reload so dev edits to the sketch show up.
  const hostUrl = `${hostBase}?src=${encodeURIComponent(sourceUrl)}&t=${Date.now()}`;

  return {
    boot: ({ dom: { html, css } }) => {
      css`
        iframe#ac-p5-host {
          position: absolute;
          inset: 0;
          width: 100%;
          height: 100%;
          border: 0;
          background: transparent;
          display: block;
        }
      `;
      html`<iframe
        id="ac-p5-host"
        title="${slug || "p5 sketch"}"
        src="${hostUrl}"
        allowtransparency="true"
        allowfullscreen
      ></iframe>`;
    },
    paint: ({ wipe }) => {
      wipe(0, 0, 0);
      return false;
    },
    act: () => {},
    leave: ({ dom }) => {
      dom?.clear?.();
    },
  };
}
