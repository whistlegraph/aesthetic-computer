// Slab's PromptSigilOverlay.rebuildName lettering, expressed as compositor CSS.
// Font names refer only to installed system fonts; no proprietary font ships.
(() => {
  const encoder = new TextEncoder();
  const segmenter = typeof Intl.Segmenter === 'function'
    ? new Intl.Segmenter(undefined, { granularity: 'grapheme' }) : null;
  const labels = new WeakMap();
  const known = new Set(['working', 'rendering', 'awaiting', 'complete', 'interrupted', 'stale']);
  const aliases = { thinking: 'working', streaming: 'working', writing: 'working',
    queued: 'rendering', building: 'rendering', idle: 'complete', ready: 'complete',
    done: 'complete', waiting: 'awaiting', error: 'stale', failed: 'stale', cancelled: 'interrupted' };
  function fnv(text) {
    let hash = 2166136261;
    for (const byte of encoder.encode(text)) hash = Math.imul(hash ^ byte, 16777619) >>> 0;
    return hash;
  }
  window.updateQrLabel = (name, status = '') => {
    const label = document.getElementById('qr-label');
    if (!label) return;
    const raw = typeof name === 'string' ? name.replace(/[\u0000-\u001f\u007f]/g, '') : '';
    const characters = (segmenter ? Array.from(segmenter.segment(raw), part => part.segment) : Array.from(raw)).slice(0, 80);
    const text = characters.join('');
    const normalized = typeof status === 'string' ? status.toLowerCase() : '';
    const state = known.has(normalized) ? normalized : (Object.hasOwn(aliases, normalized) ? aliases[normalized] : 'unknown');
    // Updating colour must not reset each letter's animation or replace its DOM.
    if (label.dataset.status !== state) label.dataset.status = state;
    if (labels.get(label) === text) return;
    labels.set(label, text);
    label.hidden = !text;
    label.setAttribute('role', 'img');
    label.setAttribute('aria-label', text);
    const fragment = document.createDocumentFragment();
    characters.forEach((character, index) => {
      const hash = fnv(`rock${index}${text}`);
      const letter = document.createElement('span');
      const ink = document.createElement('span');
      letter.className = 'qr-letter';
      const suffix = / v[0-9]+$/.exec(text);
      if(suffix && characters.slice(0,index).join('').length >= suffix.index)letter.classList.add('qr-version');
      ink.className = 'qr-letter-ink';
      letter.setAttribute('aria-hidden', 'true');
      // Native y/rotation coordinates point oppositely to CSS coordinates.
      letter.style.setProperty('--qr-dy', `${-(hash % 5 / 2 - 1)}px`);
      letter.style.setProperty('--qr-tilt', `${-(((hash >>> 8) % 9 - 4) * .9)}deg`);
      letter.style.setProperty('--qr-delay', `${index * .12}s`);
      ink.textContent = character;
      letter.appendChild(ink);
      fragment.appendChild(letter);
    });
    label.replaceChildren(fragment);
  };
})();
