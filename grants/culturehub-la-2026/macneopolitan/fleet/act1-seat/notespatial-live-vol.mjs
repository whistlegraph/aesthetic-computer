import * as rehearsal from './notespatial-performance-263da9-vol.mjs';   // the installed performance module + the room-volume follower (fresh names: the runtime caches modules)

const SSID = 'CULTUREHUB LA';
const SEAT_FILE = '/mnt/culturehub-seat.json';
const DISPLAY_FILE = '/mnt/culturehub-display.json';
let concert = true, flipY = false, renderBuffer, flipRow;
let bootApi, selected = false, credential, retryAt = -Infinity, message = '';
function read(system, path) {
  try { return JSON.parse(system.readFile(path)); } catch { return null; }
}
function assign(system, seat, persist = false) {
  if (!Number.isInteger(seat) || seat < 1 || seat > 6) return false;
  if (persist && !system.writeFile(SEAT_FILE, JSON.stringify({ seat }))) {
    message = 'Could not save seat to USB';
    return false;
  }
  const config = { seat: seat - 1, seats: 6, machineName: `culturehub-${seat}`, maxSeconds: 0 };
  if (!system.writeFile('/pieces/spatial-rehearsal-config.json', JSON.stringify(config))) {
    message = 'Could not configure rehearsal';
    return false;
  }
  // Discard previous commands. A boot or seat selection must never start music.
  system.writeFile('/pieces/spatial-rehearsal-command.json', JSON.stringify({ id: 'boot-arm', action: 'arm' }));
  rehearsal.boot({ ...bootApi, colon: [], params: [] });
  selected = true;
  return true;
}
export function boot(api) {
  bootApi = api;
  const display = read(api.system, DISPLAY_FILE) || {};
  concert = display.concert !== false; flipY = display.flipY === true;
  api.sound.microphone.close();
  api.sound.volume.setMono?.(true);
  api.sound.volume.setMonoOutput?.('left');
  api.sound.master?.set({ enabled: true, normalize: true, gainDb: 0,
    targetDb: -18, thresholdDb: -12, ratio: 3, makeupDb: 0, ceilingDb: -1 });
  api.system.startSSH?.();
  credential = (read(api.system, '/mnt/wifi_creds.json') || []).find(c => c.ssid === SSID);
  if (!credential?.pass) message = 'CultureHub Wi-Fi credential missing';
  assign(api.system, (read(api.system, SEAT_FILE) || read(api.system, '/pieces/culturehub-seat.json'))?.seat);
}
export function sim(api) {
  const { wifi, sound, system } = api;
  // Target the venue directly; retry without disturbing an in-progress connection.
  if (credential?.pass && wifi && !(wifi.connected && wifi.ssid === SSID)
      && wifi.state !== 3 && wifi.state !== 4 && sound.time >= retryAt) {
    wifi.connect(SSID, credential.pass);
    retryAt = sound.time + 30;
  }
  if (selected) rehearsal.sim(api);
}
export function paint(api) {
  if (selected) {
    const view = concert ? { ...api, write: () => {} } : api;
    if (!flipY) return rehearsal.paint(view);
    if (!renderBuffer || renderBuffer.width !== api.screen.width || renderBuffer.height !== api.screen.height) {
      renderBuffer = api.painting(api.screen.width, api.screen.height);
      flipRow = new Uint8Array(api.screen.width * 4);
    }
    api.page(renderBuffer);
    try { rehearsal.paint(view); } finally { api.page(); }
    const pixels = renderBuffer.pixels, stride = api.screen.width * 4;
    for (let y = 0; y < Math.floor(api.screen.height / 2); y++) {
      const top = y * stride, bottom = (api.screen.height - 1 - y) * stride;
      flipRow.set(pixels.subarray(top, top + stride));
      pixels.copyWithin(top, bottom, bottom + stride);
      pixels.set(flipRow, bottom);
    }
    api.paste(renderBuffer, 0, 0);
    return;
  }
  const { wipe, ink, write, screen, wifi } = api;
  wipe(12, 18, 28);
  ink(240, 245, 250);
  write('Choose position: 1 2 3 4 5 6', { x: 10, y: screen.height / 2 - 22, font: '6x10' });
  write('1-5 ring / 6 held center', { x: 10, y: screen.height / 2 - 6, font: '6x10' });
  write('Then wait for the remote cue', { x: 10, y: screen.height / 2 + 10, font: '6x10' });
  ink(160, 210, 190);
  write(message || (wifi?.connected ? `${wifi.ssid} / ${wifi.ip}` : 'Connecting to CULTUREHUB LA'),
    { x: 10, y: screen.height - 18, font: '6x10' });
}
export function act(api) {
  if (api.event.is('keyboard:down') && String(api.event.key).toLowerCase() === 'c') {
    concert = !concert;
    api.system.writeFile(DISPLAY_FILE, JSON.stringify({ concert, flipY }));
    return;
  }
  if (!selected && api.event.is('keyboard:down') && /^[1-6]$/.test(api.event.key)) {
    assign(api.system, Number(api.event.key), true);
  } else if (selected) rehearsal.act(api);
}
export function leave() { if (selected) rehearsal.leave(); }
