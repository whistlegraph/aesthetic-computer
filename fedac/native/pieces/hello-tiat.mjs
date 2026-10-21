let config = {}, frames = 0, lastStatus = -10;
let battery = {}, t = 0;
export function boot({ system, sound }) {
  sound.microphone?.close?.();
  try { config = JSON.parse(system.readFile('/pieces/hello-tiat-config.json')); } catch (_) {}
}
export function sim({ system, sound, wifi, screen }) {
  t = sound.time;
  battery = system.battery || {};
  frames++;
  if (t - lastStatus >= 1 || t < lastStatus) {
    lastStatus = t;
    system.writeFile('/pieces/hello-tiat-status.json', JSON.stringify({
      message: 'hello tiat!', device: config.label, ip: wifi?.ip,
      percent: battery.percent, charging: battery.charging,
      minutesLeft: battery.minutesLeft, frames, audioTime: t,
      screen: { width: screen.width, height: screen.height }
    }));
  }
}
export function paint({ wipe, ink, write, box, screen }) {
  const w = screen.width, h = screen.height;
  wipe(7, 9, 18);
  const color = config.color || [140, 230, 180];
  const size = Math.max(1, Math.floor(Math.min(w / 36, h / 40)));
  const bob = Math.sin(t * 2) * Math.max(2, h * .025);
  const glow = .8 + .2 * Math.sin(t * 1.5);
  ink(...color.map(c => Math.round(c * glow)));
  write('hello', { x: Math.round((w - 5 * 6 * size) / 2), y: Math.round(h * .17 + bob), size, font: '6x10' });
  write('tiat!', { x: Math.round((w - 5 * 6 * size) / 2), y: Math.round(h * .17 + 11 * size + bob), size, font: '6x10' });
  const pct = Number.isFinite(battery.percent) && battery.percent >= 0 ? Math.round(battery.percent) + '%' : '--%';
  const status = battery.charging ? 'charging' : battery.status === 'Full' ? 'full' : 'battery';
  const label = pct + ' ' + status;
  const bs = Math.max(1, Math.floor(Math.min(w / (label.length * 6 + 8), h / 60)));
  ink(255, 255, 255);
  write(label, { x: Math.round((w - label.length * 6 * bs) / 2), y: Math.round(h * .73), size: bs, font: '6x10' });
  const mins = battery.minutesLeft;
  const estimate = Number.isFinite(mins) && mins > 0 ? ' ~' + Math.floor(mins / 60) + 'h ' + (mins % 60) + 'm' : '';
  const detail = (config.label || 'AC OS') + estimate;
  const ds = Math.max(1, Math.floor(Math.min(w / (detail.length * 6 + 12), h / 120)));
  ink(...color);
  write(detail, { x: Math.round((w - detail.length * 6 * ds) / 2), y: Math.round(h * .9), size: ds, font: '6x10' });
  const bar = Math.max(4, Math.round(w / 8));
  box(Math.round((Math.sin(t * 1.8) + 1) * .5 * (w - bar)), h - 4, bar, 4, 'fill');
}
export function act({ event, system }) {
  if (event.is('keyboard:down:escape')) system.jump('prompt');
}
