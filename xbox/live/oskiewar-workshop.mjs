import { validateMap } from './oskiewar-map.mjs';

export async function mountWorkshop() {
  globalThis.__oskiewarValidateMap = validateMap;
  globalThis.__oskiewarWorkshopEnabled = true;
  const style = document.createElement('style');
  style.textContent = `
    #workshop-entry { position:fixed; z-index:12;
      top:max(12px,env(safe-area-inset-top)); right:max(12px,env(safe-area-inset-right));
      display:grid; place-items:center; width:46px; height:46px; border:2px solid #6e768d;
      border-radius:10px; color:#171b28; background:#d9dfee; box-shadow:2px 3px #14192655; }
    #workshop-entry:hover { background:#fff; }
    #workshop-entry:focus-visible, #workshop-panel :focus-visible { outline:3px solid #00bfea; outline-offset:3px; }
    body:not(.title-open):not(.workshop-open) #workshop-entry { display:none; }
    #workshop-panel { position:fixed; z-index:12; top:max(68px,calc(env(safe-area-inset-top) + 56px));
      right:max(12px,env(safe-area-inset-right)); width:min(280px,calc(100vw - 24px));
      max-height:calc(100dvh - 90px); overflow:auto; padding:16px; border:2px solid #6e768d;
      border-radius:12px; background:#ebf1f8f5; color:#171b28; font:16px/1.4 'Comic Relief',sans-serif; }
    #workshop-panel[hidden] { display:none; }
    #workshop-panel h2 { margin:0 0 12px; font-size:22px; }
    #workshop-panel button, #workshop-panel input { font:inherit; padding:9px; min-height:42px; }
    #workshop-panel button { border:1px solid #6e768d; border-radius:6px; background:#d9dfee; color:#171b28; }
    #workshop-panel input { width:100%; border:1px solid #6e768d; margin:12px 0 8px; }
    #workshop-actions { display:grid; grid-template-columns:1fr 1fr; gap:8px; }
    #workshop-note { margin:12px 0 0; overflow-wrap:anywhere; }
    #workshop-note:empty { display:none; }
  `;
  document.head.append(style);
  const entry = document.createElement('a');
  entry.id = 'workshop-entry'; entry.href = '/workshop';
  entry.title = 'Workshop'; entry.setAttribute('aria-label', 'Workshop');
  entry.innerHTML = '<svg width="27" height="27" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round" aria-hidden="true"><path d="M14.5 6.3 17.7 9.5 21 6.2a6 6 0 0 1-7.6 7.6L6.2 21a2.3 2.3 0 0 1-3.2-3.2l7.2-7.2A6 6 0 0 1 17.8 3z"/></svg>';
  const panel = document.createElement('section');
  panel.id = 'workshop-panel'; panel.setAttribute('aria-label', 'Workshop tools');
  panel.innerHTML = `<h2>Workshop</h2><div id="workshop-actions">
    <button data-op="reset-round">Reset round</button><button data-op="restart-level">Restart level</button>
    <button data-op="highlight" aria-pressed="false">Show spawns</button><button data-op="undo">Undo</button>
    <button data-op="invite" style="grid-column:1/-1">Copy coach invite</button></div>
    <input id="workshop-name" aria-label="Map name" placeholder="Map name" maxlength="60">
    <button data-op="save">Save</button> <button data-op="publish">Publish</button>
    <p id="workshop-note" role="status" aria-live="polite"></p>`;
  document.body.append(entry, panel);
  const note = panel.querySelector('#workshop-note');
  const onWorkshop = () => /^\/workshop\/?$/.test(location.pathname);
  const show = () => {
    panel.hidden = !onWorkshop();
    document.body.classList.toggle('workshop-open', !panel.hidden);
    entry.setAttribute('aria-expanded', String(!panel.hidden));
  };
  entry.addEventListener('click', event => {
    event.preventDefault(); event.stopPropagation();
    const room = globalThis.__oskiewarVersusRoom;
    const target = onWorkshop() ? (room ? '/' + room : '/')
      : '/workshop' + (room ? '?room=' + encodeURIComponent(room) : '');
    history.pushState(null, '', target); show();
  });
  for (const element of [entry, panel]) for (const event of ['pointerdown', 'pointerup', 'keydown', 'keyup'])
    element.addEventListener(event, e => e.stopPropagation());
  addEventListener('popstate', show); show();
  panel.addEventListener('click', async event => {
    const button = event.target.closest('button');
    if (!button) return;
    const op = button.dataset.op;
    button.disabled = true; note.textContent = '';
    try {
      const current = await globalThis.__oskiewarWorkshopRequest({ op: 'inspect' });
      if (op === 'invite') {
        await navigator.clipboard.writeText('Join my Oskiewar room ' + current.room +
          ' as my coach. Setup: https://oskiewar.com/coach — use coach_workshop to edit the room while I play.');
        note.textContent = 'Coach invitation copied.';
      } else {
        const result = await globalThis.__oskiewarWorkshopRequest({ op, revision: current.revision,
          enabled: !current.highlights, name: panel.querySelector('#workshop-name').value || current.map.name });
        if (op === 'highlight') button.setAttribute('aria-pressed', String(result.highlights));
        if (result.url) {
          const link = document.createElement('a'); link.href = result.url; link.textContent = 'Play published map';
          note.replaceChildren(link);
        } else note.textContent = op === 'save' ? 'Map saved.' : '';
      }
    } catch (error) { note.textContent = error.message; }
    finally { button.disabled = false; }
  });
  const params = new URLSearchParams(location.search);
  const mapId = params.get('map') || params.get('workshop');
  if (mapId && mapId !== '1') {
    try {
      if (!/^[a-f0-9]{64}$/.test(mapId)) throw new Error('Invalid map link');
      const response = await fetch('/api/oskiewar-maps?id=' + mapId,
        { signal: AbortSignal.timeout(10000) });
      const body = await response.json();
      if (!response.ok) throw new Error(body.message || 'Map unavailable');
      globalThis.__oskiewarPublishedMap = validateMap(body.map);
    } catch (error) { note.textContent = error.message; panel.hidden = false; }
  }
  let busy = false;
  globalThis.__oskiewarWorkshopRequest = async command => {
    if (!globalThis.__oskiewarWorkshopEnabled) throw new Error('Room editing is locked');
    if (busy) throw new Error('Another workshop command is running');
    busy = true;
    try {
      const game = globalThis.__oskiewarWorkshopCommand;
      if (!game) throw new Error('Game is not ready');
      const current = game({ op: 'inspect' }); // Also checks play mode.
      if (!['save', 'publish', 'load', 'list'].includes(command.op)) return game(command);
      if (command.op === 'load') {
        const saved = await api('GET', { id: command.id });
        return game({ op: 'apply', revision: command.revision, map: saved.map });
      }
      if (command.op === 'list') return await api('GET', { mine: command.mine === true ? '1' : '0' });
      if (command.revision !== current.revision) throw new Error('Map changed; inspect before saving');
      const map = validateMap({ ...current.map, name: command.name ?? current.map.name });
      return await api('POST', {}, { map, publish: command.op === 'publish' });
    } finally { busy = false; }
  };
  async function api(method, query, body) {
    const token = await globalThis.__oskiewarAccount?.bearer();
    if ((method === 'POST' || query.mine === '1') && !token) throw new Error('Sign in to save maps');
    if (!globalThis.__oskiewarWorkshopEnabled) throw new Error('Room editing is locked');
    const response = await fetch('/api/oskiewar-maps?' + new URLSearchParams(query), {
      method, headers: { 'Content-Type': 'application/json',
        ...(token ? { authorization: 'Bearer ' + token } : {}) },
      ...(body ? { body: JSON.stringify(body) } : {}), signal: AbortSignal.timeout(10000),
    });
    const result = await response.json();
    if (!response.ok) throw new Error(result.message || 'Map request failed');
    return result;
  }
}
