// effects — declarative visual emphasis for Captutor screenplays.
//
// These are page-side filming marks, not product UI: a screenplay can dim the
// frame around one control, draw an outer accent ring, attach a short label,
// burst a few glyphs, or ease the page camera toward a target. Everything is
// pointer-events:none, isolated, tokenized against stale timers, and reversible.

const INSTALL = `(() => {
  if (window.__captutorFx) return true;
  const host = document.createElement('div');
  host.id = '__captutor_fx';
  Object.assign(host.style, {
    position: 'fixed', inset: '0', pointerEvents: 'none', zIndex: '2147483645',
  });
  document.documentElement.appendChild(host);
  const root = host.attachShadow({ mode: 'open' });
  root.innerHTML = \`
    <style>
      svg { position:absolute; inset:0; width:100vw; height:100vh; overflow:visible; }
      .shade { opacity:0; transition:opacity .28s ease-out; }
      .ring { opacity:0; transition:opacity .28s ease-out;
              filter:drop-shadow(0 0 6px color-mix(in srgb, var(--accent) 68%, transparent))
                     drop-shadow(0 0 18px color-mix(in srgb, var(--accent) 28%, transparent)); }
      .label { position:absolute; opacity:0; transform:translateY(4px);
               transition:opacity .18s ease-out,transform .18s ease-out;
               padding:6px 9px; border-radius:8px; color:white;
               background:rgba(12,12,14,.82); font:600 13px/1.2 Arial,sans-serif;
               box-shadow:0 2px 12px rgba(0,0,0,.25); white-space:nowrap; }
      .show { opacity:1; transform:translateY(0); }
      .particles { position:absolute; inset:0; overflow:hidden; }
      .particle { position:absolute; translate:-50% -50%; font:700 22px/1 Arial,sans-serif;
                  text-shadow:0 1px 3px rgba(0,0,0,.35); will-change:transform,opacity; }
    </style>
    <svg>
      <defs>
        <filter id="feather"><feGaussianBlur class="feather-blur" stdDeviation="14"/></filter>
        <mask id="cutout"><rect class="mask-bg" fill="white"/>
          <rect class="cut" fill="black" filter="url(#feather)"/></mask>
      </defs>
      <rect class="shade" mask="url(#cutout)"/>
      <rect class="ring" fill="none" stroke-width="3"/>
    </svg>
    <div class="label"></div><div class="particles"></div>\`;

  const svg = root.querySelector('svg');
  const maskBg = root.querySelector('.mask-bg');
  const cut = root.querySelector('.cut');
  const shade = root.querySelector('.shade');
  const ring = root.querySelector('.ring');
  const label = root.querySelector('.label');
  const particles = root.querySelector('.particles');
  const featherBlur = root.querySelector('.feather-blur');
  let token = 0;
  let cameraAnimation = null;

  const resolve = (selector) => {
    if (selector.startsWith('js=')) return Function('return (' + selector.slice(3) + ')')();
    if (selector.startsWith('text=')) {
      const want = selector.slice(5).toLowerCase();
      return [...document.querySelectorAll('button,[role=button],a,[role=option],[role=menuitem]')]
        .find((el) => (el.innerText || '').trim().toLowerCase().startsWith(want));
    }
    return document.querySelector(selector);
  };
  const attrs = (el, values) => {
    for (const [name, value] of Object.entries(values)) el.setAttribute(name, value);
  };
  const hide = () => {
    token += 1;
    shade.style.opacity = '0';
    ring.style.opacity = '0';
    label.classList.remove('show');
    particles.replaceChildren();
    return true;
  };
  const resetCamera = (options = {}) => {
    cameraAnimation?.cancel(); cameraAnimation = null;
    const body = document.body;
    if (!body) return true;
    const ms = Math.max(0, Number(options.durationMs ?? 420));
    const from = getComputedStyle(body).transform;
    cameraAnimation = body.animate([
      { transform: from === 'none' ? 'none' : from },
      { transform: 'translate3d(0,0,0) scale(1)' },
    ], { duration:ms, easing:'cubic-bezier(.22,.75,.22,1)', fill:'forwards' });
    cameraAnimation.finished.then(() => {
      body.style.transform = '';
      body.style.transformOrigin = '';
      document.documentElement.style.overflow = '';
      cameraAnimation = null;
    }).catch(() => {});
    return true;
  };
  const box = (selector) => {
    const el = resolve(selector);
    if (!el) throw new Error('Captutor effect target not found: ' + selector);
    el.scrollIntoView({ block:'center', inline:'center', behavior:'instant' });
    const r = el.getBoundingClientRect();
    return { x:r.x, y:r.y, width:r.width, height:r.height };
  };

  const spotlight = (selector, options = {}) => {
    const own = ++token;
    const r = box(selector);
    const pad = Number(options.padding ?? 12);
    const x = Math.max(0, r.x - pad), y = Math.max(0, r.y - pad);
    const w = Math.min(innerWidth - x, r.width + pad * 2);
    const h = Math.min(innerHeight - y, r.height + pad * 2);
    const radius = Number(options.radius ?? 12);
    const feather = Math.max(0, Number(options.feather ?? 16));
    const color = options.color || '#7c3aed';
    const dim = Math.max(0, Math.min(.82, Number(options.dim ?? .54)));
    attrs(svg, { viewBox:'0 0 ' + innerWidth + ' ' + innerHeight });
    attrs(maskBg, { x:0, y:0, width:innerWidth, height:innerHeight });
    attrs(cut, { x, y, width:w, height:h, rx:radius, ry:radius });
    attrs(shade, { x:0, y:0, width:innerWidth, height:innerHeight,
      fill:'rgba(0,0,0,' + dim + ')' });
    attrs(ring, { x, y, width:w, height:h, rx:radius, ry:radius, stroke:color });
    featherBlur.setAttribute('stdDeviation', String(feather));
    ring.style.setProperty('--accent', color);
    shade.style.opacity = dim > 0 ? '1' : '0';
    ring.style.opacity = options.ring === false ? '0' : '1';
    label.textContent = options.label || '';
    label.style.left = Math.max(8, Math.min(innerWidth - 220, x)) + 'px';
    label.style.top = (y >= 42 ? y - 34 : y + h + 8) + 'px';
    label.classList.toggle('show', !!options.label);
    const ms = Number(options.durationMs ?? 2200);
    if (ms > 0) setTimeout(() => { if (own === token) hide(); }, ms);
    return r;
  };

  const zoom = (selector, options = {}) => {
    const r = box(selector);
    const scale = Math.max(1, Math.min(1.7, Number(options.scale ?? 1.16)));
    const ms = Math.max(120, Number(options.durationMs ?? 680));
    const cx = r.x + r.width / 2, cy = r.y + r.height / 2;
    const tx = innerWidth / 2 - cx * scale;
    const ty = innerHeight / 2 - cy * scale;
    const body = document.body;
    if (!body) return r;
    cameraAnimation?.cancel();
    document.documentElement.style.overflow = 'hidden';
    body.style.transformOrigin = '0 0';
    const from = getComputedStyle(body).transform;
    cameraAnimation = body.animate([
      { transform: from === 'none' ? 'translate3d(0,0,0) scale(1)' : from },
      { transform:'translate3d(' + tx + 'px,' + ty + 'px,0) scale(' + scale + ')' },
    ], { duration:ms, easing:'cubic-bezier(.22,.75,.22,1)', fill:'forwards' });
    cameraAnimation.finished.then(() => {
      body.style.transform = 'translate3d(' + tx + 'px,' + ty + 'px,0) scale(' + scale + ')';
      cameraAnimation = null;
    }).catch(() => {});
    const resetMs = Number(options.resetAfterMs ?? 0);
    if (resetMs > 0) setTimeout(() => resetCamera({ durationMs:options.resetDurationMs }), resetMs);
    return r;
  };

  const burst = (selector, options = {}) => {
    const r = box(selector);
    const x = r.x + r.width / 2, y = r.y + r.height / 2;
    const glyph = String(options.glyph || '?').slice(0, 3);
    const color = options.color || '#facc15';
    const count = Math.max(1, Math.min(24, Number(options.count ?? 9)));
    const ms = Math.max(300, Number(options.durationMs ?? 1050));
    for (let i = 0; i < count; i += 1) {
      const p = document.createElement('span');
      p.className = 'particle'; p.textContent = glyph; p.style.color = color;
      p.style.left = x + 'px'; p.style.top = y + 'px'; particles.appendChild(p);
      const a = -Math.PI / 2 + i * 2.3999632297;
      const d = 32 + (i % 4) * 14;
      p.animate([
        { transform:'translate(0,0) scale(.65)', opacity:0 },
        { transform:'translate(' + Math.cos(a)*d*.35 + 'px,' + Math.sin(a)*d*.35 + 'px) scale(1)', opacity:1, offset:.2 },
        { transform:'translate(' + Math.cos(a)*d + 'px,' + Math.sin(a)*d + 'px) scale(.82)', opacity:0 },
      ], { duration:ms, easing:'cubic-bezier(.2,.75,.25,1)', fill:'forwards' });
      setTimeout(() => p.remove(), ms + 60);
    }
    return r;
  };

  window.__captutorFx = {
    spotlight, outline:(s,o={}) => spotlight(s,{...o,dim:0}), burst,
    zoom, resetCamera, clear:() => { hide(); resetCamera(); return true; },
  };
  return true;
})()`;

async function ready(cdp) {
  await cdp.eval(INSTALL);
}

export async function spotlight(cdp, selector, options = {}) {
  await ready(cdp);
  return cdp.eval(`window.__captutorFx.spotlight(${JSON.stringify(selector)},${JSON.stringify(options)})`);
}

export async function outline(cdp, selector, options = {}) {
  await ready(cdp);
  return cdp.eval(`window.__captutorFx.outline(${JSON.stringify(selector)},${JSON.stringify(options)})`);
}

export async function burst(cdp, selector, options = {}) {
  await ready(cdp);
  return cdp.eval(`window.__captutorFx.burst(${JSON.stringify(selector)},${JSON.stringify(options)})`);
}

export async function zoom(cdp, selector, options = {}) {
  await ready(cdp);
  return cdp.eval(`window.__captutorFx.zoom(${JSON.stringify(selector)},${JSON.stringify(options)})`);
}

export async function resetCamera(cdp, options = {}) {
  return cdp.eval(`window.__captutorFx?.resetCamera(${JSON.stringify(options)}) ?? true`);
}

export async function clearEffects(cdp) {
  return cdp.eval(`window.__captutorFx?.clear() ?? true`);
}
