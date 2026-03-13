// machine.mjs — Hardware & software info for the current device
// Left column: hardware (model, CPU, RAM, battery, devices)
// Right column: software (OS version, wifi, uptime, governor)
// Press escape to return.

let frame = 0;

function act({ event: e, system }) {
  if (e.is("keyboard:down:escape") || e.is("keyboard:down:backspace")) {
    system?.jump?.("prompt");
  }
}

function paint({ wipe, ink, box, line, write, screen, system, wifi }) {
  frame++;
  const w = screen.width, h = screen.height;
  const pad = 6;
  const font = "6x10";
  const charW = 6;
  const lineH = 11;

  wipe(14, 14, 20);

  // Title
  ink(160, 170, 200);
  write("machine", { x: pad, y: 4, size: 1, font });

  // Divider under title
  ink(40, 45, 55);
  line(pad, 15, w - pad, 15);

  // Two-column layout
  const wide = w > 200;
  const colW = wide ? Math.floor((w - pad * 3) / 2) : w - pad * 2;
  const col2X = wide ? pad + colW + pad : pad;

  // ─── LEFT: HARDWARE ───
  let ly = 20;
  ink(100, 140, 180);
  write("hardware", { x: pad, y: ly, size: 1, font });
  ly += lineH + 2;

  const hw = system?.hw;
  const bat = system?.battery;

  // Model / vendor
  if (hw?.model && hw.model !== "unknown") {
    ink(180, 190, 200);
    let label = hw.vendor && hw.vendor !== "unknown"
      ? hw.vendor.replace("LENOVO", "Lenovo") + " " + hw.model
      : hw.model;
    const maxC = Math.floor(colW / charW);
    write(label.slice(0, maxC), { x: pad, y: ly, size: 1, font });
    ly += lineH;
  }

  // CPU
  if (hw?.cpu && hw.cpu !== "unknown") {
    ink(140, 150, 170);
    let cpuShort = hw.cpu
      .replace("Intel(R) Core(TM) ", "")
      .replace(" CPU", "")
      .replace(/ +/g, " ");
    const maxC = Math.floor(colW / charW);
    write(cpuShort.slice(0, maxC), { x: pad, y: ly, size: 1, font });
    ly += lineH;
  }

  // Cores
  if (hw?.cores > 0) {
    ink(110, 120, 140);
    write(hw.cores + " cores", { x: pad, y: ly, size: 1, font });
    ly += lineH;
  }

  // RAM with bar
  if (hw?.ramTotalMB > 0) {
    const usedMB = hw.ramTotalMB - (hw.ramAvailMB || 0);
    const pct = Math.round((usedMB / hw.ramTotalMB) * 100);
    ink(130, 150, 140);
    write(`ram ${usedMB}/${hw.ramTotalMB}MB (${pct}%)`, { x: pad, y: ly, size: 1, font });
    ly += lineH;

    // Bar
    const barW = Math.min(colW, 100);
    const fill = Math.round(barW * (usedMB / hw.ramTotalMB));
    ink(30, 40, 35);
    box(pad, ly, barW, 5, true);
    ink(pct > 80 ? 220 : 60, pct > 80 ? 80 : 140, pct > 80 ? 80 : 80);
    box(pad, ly, fill, 5, true);
    ly += 9;
  }

  // Battery
  if (bat?.percent >= 0) {
    const charging = bat.charging ? "+" : "";
    const col = bat.percent < 20 ? [220, 80, 80] : bat.charging ? [80, 200, 120] : [130, 140, 130];
    ink(...col);
    let batStr = `bat ${charging}${bat.percent}%`;
    if (bat.minutesLeft > 0) batStr += ` ~${bat.minutesLeft}m`;
    write(batStr, { x: pad, y: ly, size: 1, font });
    ly += lineH;
  }

  // Devices
  const devs = hw?.devices;
  if (devs && devs.length > 0) {
    ly += 4;
    ink(90, 100, 120);
    write("devices", { x: pad, y: ly, size: 1, font });
    ly += lineH;

    const devMaxC = Math.floor(colW / charW);
    const maxDevs = Math.floor((h - ly - 14) / (lineH - 1));

    // Auto-scroll if too many
    const visible = Math.min(devs.length, maxDevs);
    let scrollOff = 0;
    if (devs.length > visible) {
      scrollOff = Math.floor(frame * 0.02) % devs.length;
    }

    for (let slot = 0; slot < visible; slot++) {
      const i = (slot + scrollOff) % devs.length;
      const d = devs[i];
      const icon = d.type === "usb" ? "U" : d.type === "input" ? "I"
                 : d.type === "camera" ? "C" : d.type === "audio" ? "A"
                 : d.type === "disk" ? "D" : d.type === "display" ? "M" : "?";
      let label = `${icon} ${d.name || d.id}`;
      if (d.type === "disk" && d.sizeGB > 0) label += ` ${d.sizeGB}GB`;
      if (d.type === "disk" && d.removable) label += " *";
      if (d.type === "display") label += d.connected ? " on" : " off";

      const isActive = (d.type === "display" && d.connected);
      ink(isActive ? 80 : 60, isActive ? 160 : 75, isActive ? 80 : 85);
      write(label.slice(0, devMaxC), { x: pad, y: ly, size: 1, font });
      ly += lineH - 1;
    }
  }

  // ─── RIGHT: SOFTWARE ───
  let ry = wide ? 20 : ly + 8;
  const sx = wide ? col2X : pad;

  ink(140, 120, 180);
  write("software", { x: sx, y: ry, size: 1, font });
  ry += lineH + 2;

  // OS version
  const ver = system?.version || "unknown";
  ink(180, 180, 180);
  const verMaxC = Math.floor(colW / charW);
  write(ver.slice(0, verMaxC), { x: sx, y: ry, size: 1, font });
  ry += lineH;

  // Handle
  const handle = system?.config?.handle;
  if (handle) {
    ink(180, 150, 255);
    write("@" + handle, { x: sx, y: ry, size: 1, font });
    ry += lineH;
  }

  // Boot piece
  const piece = system?.config?.piece || "notepat";
  ink(110, 130, 120);
  write("boot: " + piece, { x: sx, y: ry, size: 1, font });
  ry += lineH;

  // WiFi
  if (wifi) {
    if (wifi.connected) {
      ink(80, 180, 120);
      write("wifi: " + (wifi.ssid || "connected"), { x: sx, y: ry, size: 1, font });
      ry += lineH;
      if (wifi.ip) {
        ink(100, 120, 110);
        write(wifi.ip, { x: sx, y: ry, size: 1, font });
        ry += lineH;
      }
    } else {
      ink(160, 100, 80);
      write("wifi: " + (wifi.status || "off"), { x: sx, y: ry, size: 1, font });
      ry += lineH;
    }
  }

  // Load average
  if (hw?.load1 !== undefined) {
    ink(100, 110, 120);
    write(`load ${hw.load1.toFixed(2)} ${(hw.load5 || 0).toFixed(2)} ${(hw.load15 || 0).toFixed(2)}`, { x: sx, y: ry, size: 1, font });
    ry += lineH;
  }

  // Processes
  if (hw?.processes > 0) {
    ink(90, 100, 110);
    write(hw.processes + " processes", { x: sx, y: ry, size: 1, font });
    ry += lineH;
  }

  // Governor
  if (hw?.governor) {
    ink(80, 90, 100);
    write("gov: " + hw.governor, { x: sx, y: ry, size: 1, font });
    ry += lineH;
  }

  // Boot device
  if (system?.bootDevice) {
    ink(80, 90, 100);
    write("boot dev: " + system.bootDevice, { x: sx, y: ry, size: 1, font });
    ry += lineH;
  }

  // Footer
  ink(50, 55, 65);
  write("esc: back", { x: pad, y: h - 12, size: 1, font });
}

export { act, paint };
