// Seed the native and web backgrounds from the rendered artwork before resizing.
// Freeze frames retain their aspect until the runtime delivers fresh pixels.
(() => {
  let freeze = null, backdrop = null, mode = 'idle', lastColor = null;
  const sample = document.createElement('canvas');
  sample.width = sample.height = 8;
  const sampleContext = sample.getContext('2d', {willReadFrequently: true});

  function edgeColor(source) {
    try {
      sample.width = 8; // Reset origin-clean state after a previous tainted frame.
      sampleContext.drawImage(source, 0, 0, 8, 8);
      const pixels = sampleContext.getImageData(0, 0, 8, 8).data;
      const colors = [];
      for (let y = 0; y < 8; y++) for (let x = 0; x < 8; x++) {
        if (x !== 0 && x !== 7 && y !== 0 && y !== 7) continue;
        const i = (y * 8 + x) * 4;
        if (pixels[i + 3] >= 240) colors.push(Array.from(pixels.slice(i, i + 3)));
      }
      if (colors.length < 20) return null;
      const rgb = [0, 1, 2].map(channel => colors.map(color => color[channel]).sort((a, b) => a - b)[Math.floor(colors.length / 2)]);
      const uniform = colors.every(color => color.every((value, channel) => Math.abs(value - rgb[channel]) <= 20));
      return {rgb, uniform};
    } catch { return null; } // A cross-origin canvas can be copied but not sampled.
  }

  function rememberColor(color, wrapper) {
    if (!color || color.rgb.join(',') === lastColor?.join(',')) return;
    lastColor = color.rgb;
    const css = `rgb(${color.rgb.join(',')})`;
    document.documentElement.style.setProperty('--aesel-preview-background', css);
    window.webkit?.messageHandlers?.previewBackdrop?.postMessage(color.rgb);
  }

  function sampleLive(wrapper) {
    // Resizing clears the live framebuffer briefly. Keep its last good color
    // while the freeze frame is present; never infer a new color from a clear.
    if (freeze || window.preloaded === false) return;
    for (const selector of ['canvas[data-type="webgpu"]', 'canvas[data-type="webgl-composite"]', 'canvas:not([data-type])']) {
      const canvas = wrapper.querySelector(selector);
      if (!canvas || !canvas.width || !canvas.height) continue;
      const style = getComputedStyle(canvas);
      if (style.display === 'none' || style.visibility === 'hidden' || Number(style.opacity) === 0) continue;
      const color = edgeColor(canvas);
      if (color) { rememberColor(color, wrapper); return; }
    }
  }

  function update(wrapper) {
    freeze = wrapper.querySelector('canvas[data-type="freeze"]');
    if (!freeze || !freeze.width || !freeze.height) {
      backdrop?.remove(); backdrop = null; mode = 'idle';
      return;
    }
    // The native corner preview is top-right anchored. Centering the old
    // picture here made its right edge slide while WebKit caught up.
    if (freeze.style.objectPosition !== 'right top') freeze.style.objectPosition = 'right top';
    if (!backdrop) {
      backdrop = document.createElement('canvas');
      backdrop.dataset.aeselContinuity = 'true';
      backdrop.setAttribute('aria-hidden', 'true');
      Object.assign(backdrop.style, {
        position: 'absolute', left: '0', top: '0', width: '100%', height: '100%',
        zIndex: '9', pointerEvents: 'none', objectFit: 'cover', objectPosition: 'right top', imageRendering: 'auto',
      });
      wrapper.insertBefore(backdrop, freeze);
    }
    // A small blurred image is sufficient for the edges, even in fullscreen.
    const factor = Math.min(1, 256 / Math.max(freeze.width, freeze.height));
    backdrop.width = Math.max(1, Math.round(freeze.width * factor));
    backdrop.height = Math.max(1, Math.round(freeze.height * factor));
    const context = backdrop.getContext('2d');
    const color = edgeColor(freeze);
    if (color?.uniform) {
      context.fillStyle = `rgb(${color.rgb.join(',')})`;
      context.fillRect(0, 0, backdrop.width, backdrop.height);
      Object.assign(backdrop.style, {filter: 'none', left: '0', top: '0', width: '100%', height: '100%'});
      mode = 'color';
    } else {
      context.drawImage(freeze, 0, 0, backdrop.width, backdrop.height);
      // Fixed overscan avoids a centered scale that shifts as the box changes.
      Object.assign(backdrop.style, {filter: 'blur(12px)', left: '-36px', top: '-36px', width: 'calc(100% + 72px)', height: 'calc(100% + 72px)'});
      mode = 'blur';
    }
    rememberColor(color, wrapper);
    // Follow the runtime's ready-pixels fade, including a resize that interrupts
    // an in-progress fade. No separate timeout can remove this frame early.
    backdrop.style.transition = freeze.style.transition;
    backdrop.style.opacity = freeze.style.opacity || '1';
  }

  function attach(wrapper) {
    const style = document.createElement('style');
    style.textContent = 'html,body,#aesthetic-computer{background-color:var(--aesel-preview-background,transparent)!important}';
    (document.head || document.documentElement).append(style);
    // Seed from the first opaque frame, without waiting for the first resize.
    let attempts = 0;
    const warmup = () => {
      sampleLive(wrapper);
      if (!lastColor && ++attempts < 120) requestAnimationFrame(warmup);
    };
    requestAnimationFrame(warmup);
    // Tiny (8x8) bounded samples also follow a new source or animated wipe.
    setInterval(() => sampleLive(wrapper), 250);
    const observer = new MutationObserver(records => {
      if (records.some(record => record.target === freeze ||
          [...record.addedNodes || [], ...record.removedNodes || []].some(node => node.dataset?.type === 'freeze'))) update(wrapper);
      sampleLive(wrapper);
    });
    observer.observe(wrapper, {childList: true, subtree: true, attributes: true, attributeFilter: ['style', 'width', 'height']});
    update(wrapper);
    sampleLive(wrapper);
  }
  const rootObserver = new MutationObserver(() => {
    const wrapper = document.getElementById('aesthetic-computer');
    if (wrapper) { rootObserver.disconnect(); attach(wrapper); }
  });
  const wrapper = document.getElementById('aesthetic-computer');
  if (wrapper) attach(wrapper);
  else rootObserver.observe(document, {childList: true, subtree: true});
  window.__aeselContinuity = {inspect: () => ({active: !!backdrop, mode, color: lastColor})};
})();
