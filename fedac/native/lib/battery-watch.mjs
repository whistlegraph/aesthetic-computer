// Visible charge and a sine bell that accelerates below 10%.
export function batteryWarningInterval(percent) {
  return Number.isFinite(percent) && percent >= 0 && percent <= 10
    ? Math.max(1, percent * 3)
    : null;
}

export function createBatteryWatch() {
  let lastDing = -Infinity, lastTime = -Infinity;
  let percent = null, charging = false, pluggedIn = false, interval = null, now = 0;
  let powerAt = -Infinity, powerPaths;
  let voices = [];

  function update({ system, sound }) {
    const value = system?.battery?.percent;
    percent = Number.isFinite(value) && value >= 0 && value <= 100 ? value : null;
    charging = system?.battery?.charging === true;
    interval = batteryWarningInterval(percent);
    now = Number.isFinite(sound?.time) ? sound.time : 0;
    if (now < powerAt || now - powerAt >= 1) {
      powerAt = now;
      let known = false, online = false;
      try {
        powerPaths = (system?.listDir?.('/sys/class/power_supply') || [])
          .map(entry => `/sys/class/power_supply/${entry.name}/online`);
        for (const path of powerPaths) {
          const value = system?.readFile?.(path)?.trim();
          if (value === '0' || value === '1') { known = true; online ||= value === '1'; }
        }
      } catch {}
      pluggedIn = typeof system?.battery?.pluggedIn === 'boolean'
        ? system.battery.pluggedIn : known ? online : charging;
    }
    if (now < lastTime || interval === null) lastDing = -Infinity;
    lastTime = now;
    if (interval !== null && sound?.synth && now - lastDing >= interval) {
      lastDing = now;
      voices = [
        sound.synth({ type: 'sine', tone: 880, volume: .16, duration: .45, attack: .004, decay: .42 }),
        sound.synth({ type: 'sine', tone: 2200, volume: .045, duration: .22, attack: .002, decay: .20 }),
      ];
    }
    return { percent, charging, pluggedIn, interval };
  }

  function paint({ ink, box, write, screen }) {
    const low = interval !== null;
    const label = percent === null ? '--%' : `${Math.round(percent)}%`;
    const size = Math.max(2, Math.floor(Math.min(screen.width / (low ? 48 : 96), screen.height / (low ? 18 : 48))));
    const unit = Math.max(1, Math.floor(size / 2));
    const pad = Math.max(6, unit * 2);
    const bellWidth = low ? unit * 13 : 0;
    const boltWidth = pluggedIn ? unit * 10 : 0;
    const width = label.length * 6 * size + pad * 2 + bellWidth + boltWidth;
    const height = 10 * size + pad * 2;
    const x = Math.max(0, screen.width - width - pad), y = pad;
    const pulseHz = low ? 1 + (10 - percent) / 10 : 0;
    const bright = low && Math.floor(now * pulseHz * 2) % 2 === 0;
    ink(...(bright ? [185, 12, 35] : [8, 10, 18]));
    box(x, y, width, height, 'fill');
    ink(...(low ? (bright ? [255, 255, 230] : [255, 90, 100]) : (charging ? [145, 255, 190] : [255, 255, 255])));
    box(x, y, width, height, 'outline');
    write(label, { x: x + pad + bellWidth, y: y + pad, font: '6x10', size });
    if (pluggedIn) {
      const bx = x + pad + bellWidth + label.length * 6 * size + unit;
      const by = y + Math.floor((height - 10 * unit) / 2);
      for (const [row, start, length] of [[0,4,3],[1,3,3],[2,2,3],[3,1,5],[4,0,7],[5,3,3],[6,2,3],[7,1,3],[8,1,2],[9,0,2]])
        box(bx + start * unit, by + row * unit, length * unit, unit, 'fill');
    }
    if (low) {
      const bx = x + pad + Math.round(Math.sin(now * pulseHz * Math.PI * 2) * unit);
      const by = y + Math.floor((height - 10 * unit) / 2);
      // Run-length rows of a pixel bell; nine filled rectangles per frame.
      for (const [row, start, length] of [[0,4,2],[1,3,4],[2,2,6],[3,2,6],[4,2,6],[5,1,8],[6,0,10],[8,4,2],[9,4,2]])
        box(bx + start * unit, by + row * unit, length * unit, unit, 'fill');
    }
  }

  function leave() {
    for (const voice of voices) voice?.kill?.(.03);
    voices = [];
  }

  return { update, paint, leave };
}
