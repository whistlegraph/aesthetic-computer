import {createBatteryWatch} from './battery-watch-power-v2.mjs';
import {createScoreBrightness} from './score-brightness-audio-v2.mjs';
import {candlelightRgb} from './candlelight.mjs';
const brightnessControl=createScoreBrightness();
import * as rehearsal from './notespatial-performance-optimized.mjs';

const batteryWatch = createBatteryWatch();
let batteryReportAt=-Infinity;

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
 brightnessControl.boot(api.system);
 api.sound.volume.setMix(.25);
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
function paintPerformance(api) {
  if (selected) {
    const view = { ...api, overlayWrite: api.write, ...(concert ? { write: () => {} } : {}) };
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
function paintOriginal(api) {
 paintPerformance(api);
 const batteryStatus=batteryWatch.update(api);
 if(api.sound.time-batteryReportAt>=1){batteryReportAt=api.sound.time;api.system.writeFile('/pieces/battery-display-status.json',JSON.stringify({...batteryStatus,at:api.sound.time}));}
 batteryWatch.paint(api);
}
function leaveOriginal() { batteryWatch.leave(); if (selected) rehearsal.leave(); }

let candleSystem, candleNext=0, lastRgb='', controlsAt=-Infinity, reportAt=-Infinity;
export function paint(api) {
 if(api.sound.time-controlsAt>=.25){
  controlsAt=api.sound.time;
  try{const controls=JSON.parse(api.system.readFile('/pieces/performance-controls.json'));rehearsal.setVisualMode(controls.noteLabels?'notes':'frames');}catch{}
 }
 paintOriginal(api);
 const status=rehearsal.getPerformanceVisualState();
 const t=status.phase==='playing'?status.scoreTime:-1;
 brightnessControl.update(api.system,api.sound.time,t,Math.max(api.sound.speaker?.amplitudes?.left||0,api.sound.speaker?.amplitudes?.right||0));
 if(api.sound.time-reportAt>=1){reportAt=api.sound.time;api.system.writeFile('/pieces/performance-screen-status.json',JSON.stringify({...status,at:api.sound.time}));}
 if(status.seat!==5||api.sound.time<candleNext)return;
 candleNext=api.sound.time+.04;candleSystem=api.system;
 const gain=t<0?0:Math.min(1,t,Math.max(0,status.scoreDuration-t));
 const rgb=candlelightRgb(t,5).map(v=>Math.round(v*gain*4));
 const key=rgb.join(',');if(key===lastRgb)return;
 const slots=new Array(64).fill(0);rgb.forEach((v,i)=>slots[40+i]=v);
 const ok=api.system.dmxSend(slots);
 if(ok)lastRgb=key;else candleNext=api.sound.time+1;
 api.system.writeFile('/pieces/center-dmx-live.json',JSON.stringify({ok,active:rgb.some(v=>v>0),scoreTime:t,rgb,address:41}));
}
export function leave(){candleSystem?.dmxSend(new Array(64).fill(0));leaveOriginal();}
