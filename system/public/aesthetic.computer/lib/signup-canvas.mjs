import { signupLayout } from './onboarding-ui.mjs';
// Keep browser editing/autofill and accessible semantics. All visible pixels
// are drawn by the join piece; no email, code or token enters an analytics event.
export function createSignupCanvas(win, doc, audio) {
  let view, observer, dimensions, scroll = 0, last = '', launched = false;
  let buttons = [], field, extraStyle;
  const send = content => win.acDISK_SEND?.({ type: 'signup:view', content });
  const snapshot = () => ({
    title: view?.querySelector('h1')?.textContent?.replace('Where should we send a code?', 'Your email') || '',
    description: view?.querySelector('.description')?.textContent || '',
    status: view?.querySelector('[role=status]')?.textContent || '',
    check: view?.querySelector('.check')?.textContent || '',
    field: field ? { value: field.value, label: view.querySelector(`label[for="${field.id}"]`)?.textContent || '', focused: doc.activeElement === field } : null,
    buttons: buttons.map((b, i) => ({ id: b.classList.contains('dismiss') ? 'close' : String(i), label: ({'I already have an account':'Log in','Use a password instead':'Use password','New here? Pick a handle':'Pick a handle','Log in with my password':'Use password'})[b.textContent] || b.textContent, disabled: b.disabled, primary: b.classList.contains('primary'), close: b.classList.contains('dismiss') })),
    music: audio.enabled,
  });
  function publish() {
    if (!view) return;
    field = view.querySelector('input'); buttons = [...view.querySelectorAll('button:not([data-music])')];
    const data = snapshot();
    const canvas = doc.querySelector('canvas[data-ac-visit-canvas]');
    if (canvas && dimensions) {
      const rect = canvas.getBoundingClientRect(), scale = rect.width / dimensions.width;
      const viewport = win.visualViewport;
      const top = Math.max(rect.top, viewport?.offsetTop || 0);
      const bottom = Math.min(rect.bottom, (viewport?.offsetTop || 0) + (viewport?.height || win.innerHeight));
      data.height = Math.max(90, Math.floor((bottom - top) / scale));
      data.top = Math.max(0, Math.floor((top - rect.top) / scale));
      const layout = signupLayout(data, dimensions.width, data.height, scroll); scroll = layout.scroll;
      data.scroll = scroll;
      const nodes = new Map(buttons.map((b, i) => [b.classList.contains('dismiss') ? 'close' : String(i), b]));
      nodes.set('field', field); nodes.set('music', view.querySelector('[data-music]'));
      for (const [id, node] of nodes) {
        if (!node) continue;
        const box = layout.items.find(i => i.id === id);
        const visible = box && (id === 'close' || id === 'music' || box.y >= 34 && box.y + box.h <= data.height);
        node.style.setProperty('display', box ? 'block' : 'none', 'important');
        node.style.setProperty('pointer-events', visible ? 'auto' : 'none', 'important');
        if (!box) continue;
        Object.assign(node.style, { left: `${rect.left + box.x * scale}px`, top: `${top + box.y * scale}px`, width: `${box.w * scale}px`, height: `${box.h * scale}px` });
      }
    }
    const serialized = JSON.stringify(data);
    if (serialized !== last) { last = serialized; send(data); }
  }
  const resize = () => { last = ''; publish(); };
  win.visualViewport?.addEventListener('resize', resize);
  win.visualViewport?.addEventListener('scroll', resize);
  win.addEventListener('resize', resize);
  return {
    mount(node) {
      this.unmount(); view = node; scroll = 0; last = ''; audio.enter();
      view.classList.add('ac-signup-pixel');
      extraStyle = doc.createElement('style');
      extraStyle.textContent = `.ac-signup-pixel{position:fixed!important;inset:0!important;width:100%!important;height:0!important;max-height:none!important;overflow:visible!important;border:0!important;padding:0!important;margin:0!important;background:transparent!important;box-shadow:none!important;z-index:10000}.ac-signup-pixel *{opacity:0!important;pointer-events:none!important}.ac-signup-pixel input,.ac-signup-pixel button{visibility:visible!important;opacity:0!important;position:fixed!important;box-sizing:border-box!important;margin:0!important;padding:0!important;min-width:0!important;max-width:none!important;font-size:16px!important;pointer-events:auto!important}.ac-signup-pixel::backdrop{background:transparent!important}`;
      doc.head.append(extraStyle); view.setAttribute('open', '');
      const music = doc.createElement('button'); music.type = 'button'; music.dataset.music = ''; music.textContent = 'Music'; music.setAttribute('aria-label', 'Toggle onboarding music');
      music.onclick = () => { audio.toggle(); music.setAttribute('aria-pressed', String(audio.enabled)); publish(); };
      view.append(music);
      for (const event of ['input', 'focusin', 'focusout', 'selectionchange']) view.addEventListener(event, publish);
      view.addEventListener('keydown', event => {
        if (event.key === 'Escape') { buttons.find(b => b.classList.contains('dismiss'))?.click(); event.preventDefault(); }
        if (event.key !== 'Tab') return;
        const nodes = [...view.querySelectorAll('input,button')].filter(n => !n.disabled);
        const at = nodes.indexOf(doc.activeElement), next = (at + (event.shiftKey ? -1 : 1) + nodes.length) % nodes.length;
        const node = nodes[next];
        if (node && dimensions) {
          const data = snapshot(), canvas = doc.querySelector('canvas[data-ac-visit-canvas]');
          const rect = canvas?.getBoundingClientRect(), scale = rect ? rect.width / dimensions.width : 2;
          const h = Math.floor((win.visualViewport?.height || win.innerHeight) / scale);
          const id = node === field ? 'field' : node.dataset.music !== undefined ? 'music' : node.classList.contains('dismiss') ? 'close' : String(buttons.indexOf(node));
          const b = signupLayout(data, dimensions.width, h, scroll).items.find(i => i.id === id);
          if (b && id !== 'close' && id !== 'music') { if (b.y < 34) scroll -= 34 - b.y; if (b.y + b.h > h) scroll += b.y + b.h - h; publish(); }
        }
        node?.focus({ preventScroll: true }); event.preventDefault();
      });
      view.addEventListener('click', () => { audio.tap(); queueMicrotask(publish); }, true);
      // Let a finger scroll the pixel form, including over transparent controls.
      let touchY;
      view.addEventListener('touchstart', e => { touchY = e.touches[0]?.clientY; }, { passive: true });
      view.addEventListener('touchmove', e => { if (touchY === undefined || !dimensions) return; const y = e.touches[0]?.clientY; if (Number.isFinite(y)) { scroll += (touchY - y) / 2; touchY = y; publish(); } }, { passive: true });
      observer = new win.MutationObserver(publish);
      observer.observe(view, { childList: true, subtree: true, characterData: true, attributes: true, attributeFilter: ['disabled', 'class'] });
      if (!launched) { launched = true; if (!/^\/join\/?$/.test(win.location.pathname)) win.postMessage({ type: 'load-piece', slug: 'join' }, win.location.origin); }
      queueMicrotask(publish);
    },
    unmount() { observer?.disconnect(); observer = null; extraStyle?.remove(); view = null; last = ''; },
    handle(message = {}) {
      if (message.action === 'ready') { dimensions = message; if (!view) win.acSignup?.open('signup'); last = ''; publish(); }
      if (message.action === 'scroll') { scroll += Number(message.delta) || 0; publish(); }
      if (message.action === 'leave') { launched = false; dimensions = null; win.acSignup?.close(); audio.leave(); }
    },
  };
}
