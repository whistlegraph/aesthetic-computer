// effects — declarative visual emphasis for Captutor screenplays.
//
// These are page-side filming marks, not product UI: a screenplay can dim the
// frame around one control, draw an outer accent ring, attach a short label,
// or burst a few glyphs. Everything is
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
      .ring,.ring-glow { opacity:0; transition:opacity .32s ease-out; }
      .ring { filter:blur(var(--ring-blur,.45px))
                     drop-shadow(0 0 var(--ring-shadow-blur,7px) var(--ring-shadow-color)); }
      .ring-glow { filter:blur(var(--glow-blur,8px))
                          drop-shadow(0 0 var(--glow-shadow-blur,18px) var(--glow-shadow-color)); }
      .label { position:absolute; display:block; opacity:0;
               transition:opacity .16s ease-out; color:var(--label-color,#fff);
               background:var(--label-background,AccentColor);
               border:1px solid var(--label-border-color,rgba(255,255,255,.94));
               border-radius:3px;
               box-shadow:var(--label-box-shadow,
                 4px 5px 0 rgba(0,0,0,.92),0 10px 22px rgba(0,0,0,.44));
               font:900 22px/1 system-ui,-apple-system,BlinkMacSystemFont,
                    "PingFang SC","Noto Sans CJK SC",sans-serif;
               font-kerning:normal; font-variant-ligatures:none;
               white-space:nowrap;
               text-shadow:var(--label-shadow); }
      .label.show { opacity:1; }
      .label-glyph { position:absolute; left:0; top:0; display:block;
                     transform-origin:center 72%; will-change:transform,opacity; }
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
      <rect class="ring-glow" fill="none" stroke-width="9"/>
      <rect class="ring" fill="none" stroke-width="2"/>
    </svg>
    <div class="label"></div><div class="particles"></div>\`;

  const svg = root.querySelector('svg');
  const maskBg = root.querySelector('.mask-bg');
  const cut = root.querySelector('.cut');
  const shade = root.querySelector('.shade');
  const ringGlow = root.querySelector('.ring-glow');
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
    ringGlow.style.opacity = '0';
    ring.style.opacity = '0';
    label.classList.remove('show');
    particles.replaceChildren();
    return true;
  };
  const showLabel = (text, options, color, r) => {
    label.replaceChildren();
    const value = String(text || '').replace(
      /(^|[\\s:–—-])([\\p{L}\\p{N}])/gu,
      (_, boundary, character) => boundary + character.toLocaleUpperCase(),
    );
    if (!value) {
      label.classList.remove('show');
      return;
    }
    const padX = Number(options.labelPadX ?? 11);
    const padY = Number(options.labelPadY ?? 7);
    label.style.setProperty('--label-color', options.labelColor || '#fff');
    label.style.setProperty('--label-background', options.labelBackground || color);
    label.style.setProperty('--label-border-color', options.labelBorderColor ||
      'rgba(255,255,255,.94)');
    label.style.setProperty('--label-box-shadow', options.labelBoxShadow ||
      '4px 5px 0 rgba(0,0,0,.92), 0 10px 22px rgba(0,0,0,.44)');
    label.style.setProperty('--label-shadow', options.labelShadow ||
      '0 1px 0 rgba(0,0,0,.72)');
    const style = getComputedStyle(label);
    const measure = document.createElement('canvas').getContext('2d');
    measure.font = style.font ||
      (style.fontWeight + ' ' + style.fontSize + ' ' + style.fontFamily);
    if ('fontKerning' in measure) measure.fontKerning = 'normal';
    const glyphs = [];
    let prefix = '';
    let glyphIndex = 0;
    for (const char of value) {
      if (!/\\s/.test(char)) {
        const glyph = document.createElement('span');
        glyph.className = 'label-glyph';
        glyph.textContent = char;
        // Prefix measurement preserves pair kerning while each glyph remains
        // independently animatable. Flexed spans cannot kern across elements.
        const withCharacter = prefix + char;
        glyph.style.left = (padX + Math.max(0,
          measure.measureText(withCharacter).width - measure.measureText(char).width)) + 'px';
        glyph.style.top = padY + 'px';
        label.appendChild(glyph);
        glyphs.push({ glyph, index:glyphIndex });
        glyphIndex += 1;
      }
      prefix += char;
    }
    const metrics = measure.measureText(value);
    const fontSize = parseFloat(style.fontSize) || 22;
    label.style.width = Math.ceil(metrics.width + padX * 2) + 'px';
    label.style.height = Math.ceil(Math.max(fontSize,
      (metrics.actualBoundingBoxAscent || fontSize * .8) +
      (metrics.actualBoundingBoxDescent || fontSize * .2)) + padY * 2) + 'px';
    for (const { glyph, index } of glyphs) {
      const drift = ((index * 17) % 9) - 4;
      const tilt = ((index * 11) % 7) - 3;
      const delay = index * 34;
      glyph.animate([
        { opacity:0, transform:'translate3d(' + drift + 'px,11px,0) rotate(' + tilt + 'deg) scale(.72)' },
        { opacity:1, transform:'translate3d(0,-2px,0) rotate(' + (-tilt * .3) + 'deg) scale(1.08)', offset:.72 },
        { opacity:1, transform:'translate3d(0,0,0) rotate(0) scale(1)' },
      ], {
        duration:620, delay, fill:'both',
        easing:'cubic-bezier(.18,.82,.2,1)',
      });
      // Once landed, every letter keeps a barely-there independent buoyancy.
      // The short amplitude reads as alive, not as a novelty title effect.
      glyph.animate([
        { translate:'0 -1px', rotate:'-0.55deg' },
        { translate:'0 1.5px', rotate:'0.55deg' },
      ], {
        duration:1700 + (index % 5) * 170,
        delay:620 + delay,
        direction:'alternate', iterations:Infinity,
        easing:'ease-in-out', composite:'add',
      });
    }
    label.setAttribute('aria-label', value);
    label.style.left = '0px';
    label.style.top = '0px';
    label.classList.add('show');
    const measured = label.getBoundingClientRect();
    let left;
    let top;
    if (options.labelPosition === 'above') {
      left = r.x + (r.width - measured.width) / 2;
      top = r.y - measured.height - Number(options.labelGap ?? 12);
    } else if (options.labelPosition === 'side' &&
        r.x + r.width + measured.width + 24 <= innerWidth) {
      left = r.x + r.width + 12;
      top = r.y + (r.height - measured.height) / 2;
    } else if (options.labelPosition === 'side' && r.x >= measured.width + 24) {
      left = r.x - measured.width - 12;
      top = r.y + (r.height - measured.height) / 2;
    } else {
      left = Math.max(12, Math.min(innerWidth - measured.width - 12, r.x));
      top = r.y >= measured.height + 20
        ? r.y - measured.height - 12
        : Math.min(innerHeight - measured.height - 12, r.y + r.height + 12);
    }
    left += Number(options.labelOffsetX ?? 0);
    top += Number(options.labelOffsetY ?? 0);
    label.style.left = Math.max(12, Math.min(innerWidth - measured.width - 12, left)) + 'px';
    label.style.top = Math.max(12, Math.min(innerHeight - measured.height - 12, top)) + 'px';
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
  const box = (selector, options = {}) => {
    const el = resolve(selector);
    if (!el) throw new Error('Captutor effect target not found: ' + selector);
    if (options.scrollIntoView !== false) {
      el.scrollIntoView({ block:'center', inline:'center', behavior:'instant' });
    }
    const r = el.getBoundingClientRect();
    // React Flow applies radius before its viewport scale, while our SVG lives
    // in screen pixels outside that transform. Inspect descendants whose box
    // exactly matches the requested node, take their strongest authored corner,
    // and scale it into the overlay's coordinate system. This catches Fuser's
    // rounded inner card even when the outer React Flow wrapper is less rounded.
    const candidates = [el, ...el.querySelectorAll(':scope > *, :scope > * > *, :scope > * > * > *')];
    let inferredRadius = 0;
    for (const candidate of candidates) {
      const q = candidate.getBoundingClientRect();
      const tolerance = 3;
      const sameBox = Math.abs(q.left - r.left) <= tolerance &&
        Math.abs(q.top - r.top) <= tolerance &&
        Math.abs(q.right - r.right) <= tolerance &&
        Math.abs(q.bottom - r.bottom) <= tolerance;
      if (!sameBox) continue;
      const style = getComputedStyle(candidate);
      const scaleX = candidate.offsetWidth ? q.width / candidate.offsetWidth : 1;
      const scaleY = candidate.offsetHeight ? q.height / candidate.offsetHeight : 1;
      const scale = Math.min(scaleX, scaleY);
      const corners = [
        style.borderTopLeftRadius, style.borderTopRightRadius,
        style.borderBottomRightRadius, style.borderBottomLeftRadius,
      ];
      for (const corner of corners) {
        const authored = parseFloat(corner) || 0;
        inferredRadius = Math.max(inferredRadius, authored * scale);
      }
    }
    inferredRadius = Math.min(inferredRadius, r.width / 2, r.height / 2);
    return { x:r.x, y:r.y, width:r.width, height:r.height, radius:inferredRadius };
  };

  const spotlight = (selector, options = {}) => {
    const own = ++token;
    const r = box(selector, options);
    const pad = Number(options.padding ?? 12);
    const x = Math.max(0, r.x - pad), y = Math.max(0, r.y - pad);
    const w = Math.min(innerWidth - x, r.width + pad * 2);
    const h = Math.min(innerHeight - y, r.height + pad * 2);
    // Expanding a rounded rectangle by the pad expands its radius by the same
    // amount; using the raw node radius would still pinch the frame's corners.
    const radius = Number(options.radius ?? (r.radius ? r.radius + pad : 12));
    const feather = Math.max(0, Number(options.feather ?? 16));
    // Follow the filming machine by default. A screenplay can merge a client
    // effectTheme into these options without changing the generic effect.
    const color = options.color || 'AccentColor';
    const ringColor = options.ringColor || color;
    const glowColor = options.glowColor || color;
    const dim = Math.max(0, Math.min(.82, Number(options.dim ?? .54)));
    attrs(svg, { viewBox:'0 0 ' + innerWidth + ' ' + innerHeight });
    attrs(maskBg, { x:0, y:0, width:innerWidth, height:innerHeight });
    attrs(cut, { x, y, width:w, height:h, rx:radius, ry:radius });
    attrs(shade, { x:0, y:0, width:innerWidth, height:innerHeight,
      fill:'rgba(0,0,0,' + dim + ')' });
    attrs(ring, { x, y, width:w, height:h, rx:radius, ry:radius, stroke:ringColor });
    attrs(ringGlow, { x, y, width:w, height:h, rx:radius, ry:radius, stroke:glowColor });
    featherBlur.setAttribute('stdDeviation', String(feather));
    ring.style.setProperty('--accent', color);
    ringGlow.style.setProperty('--accent', color);
    ring.style.setProperty('--ring-blur', Number(options.ringBlur ?? .45) + 'px');
    ring.style.setProperty('--ring-shadow-blur', Number(options.ringShadowBlur ?? 7) + 'px');
    ring.style.setProperty('--ring-shadow-color', options.ringShadowColor ||
      'color-mix(in srgb, ' + color + ' 55%, transparent)');
    ringGlow.style.setProperty('--glow-blur', Number(options.glowBlur ?? 8) + 'px');
    ringGlow.style.setProperty('--glow-shadow-blur', Number(options.glowShadowBlur ?? 18) + 'px');
    ringGlow.style.setProperty('--glow-shadow-color', options.glowShadowColor ||
      'color-mix(in srgb, ' + color + ' 34%, transparent)');
    shade.style.opacity = dim > 0 ? '1' : '0';
    ring.style.opacity = options.ring === false ? '0' : String(options.ringOpacity ?? .82);
    ringGlow.style.opacity = options.ring === false ? '0' : String(options.glowOpacity ?? .46);
    showLabel(options.label, options, color, { x, y, width:w, height:h });
    const ms = Number(options.durationMs ?? 2200);
    if (ms > 0) setTimeout(() => { if (own === token) hide(); }, ms);
    return r;
  };

  // Kept as a compatibility alias for older screenplays. Transforming <body>
  // changes sticky/fixed containing blocks and clips real product UI, so visual
  // emphasis is now always a feathered outline + dim with no layout mutation.
  const zoom = (selector, options = {}) => spotlight(selector, {
    ...options,
    dim: Number(options.dim ?? .30),
    feather: Number(options.feather ?? 26),
    ring: true,
  });

  const burst = (selector, options = {}) => {
    const r = box(selector, options);
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
