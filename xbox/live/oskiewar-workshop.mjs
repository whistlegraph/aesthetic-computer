import { validateMap } from './oskiewar-map.mjs';

export async function mountWorkshop() {
  globalThis.__oskiewarValidateMap = validateMap;
  const label = document.createElement('label');
  label.style.cssText = 'position:fixed;bottom:8px;left:8px;z-index:30;background:#111;color:white;padding:8px;font:14px sans-serif';
  const toggle = document.createElement('input');
  toggle.type = 'checkbox';
  label.append(toggle, ' Coach editing');
  label.title = 'Allow attached coaches to edit this local game and save or publish maps with your account. Network matches are excluded.';
  const practice = document.createElement('a');
  practice.href = '/?workshop=1';
  practice.textContent = ' · Map workshop';
  practice.style.color = '#8eeaff';
  label.append(practice);
  for (const event of ['pointerdown', 'keydown', 'keyup'])
    label.addEventListener(event, e => e.stopPropagation());
  document.body.append(label);
  const mapId = new URLSearchParams(location.search).get('workshop');
  if (mapId && mapId !== '1') {
    try {
      if (!/^[a-f0-9]{64}$/.test(mapId)) throw new Error('Invalid map link');
      const response = await fetch('/api/oskiewar-maps?id=' + mapId,
        { signal: AbortSignal.timeout(10000) });
      const body = await response.json();
      if (!response.ok) throw new Error(body.message || 'Map unavailable');
      globalThis.__oskiewarPublishedMap = validateMap(body.map);
    } catch (error) { label.append(' · ' + error.message); }
  }
  toggle.addEventListener('change', () => {
    globalThis.__oskiewarWorkshopEnabled = toggle.checked;
  });
  let busy = false;
  globalThis.__oskiewarWorkshopRequest = async command => {
    if (!toggle.checked) throw new Error('Turn on Coach editing in the game first');
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
    if (!toggle.checked) throw new Error('Coach editing was turned off');
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
